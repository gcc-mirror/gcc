/**
 * Takes a token stream from the lexer, and parses it into an abstract syntax tree.
 *
 * Specification: $(LINK2 https://dlang.org/spec/grammar.html, D Grammar)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/parse.d, _parse.d)
 * Documentation:  https://dlang.org/phobos/dmd_parse.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/parse.d
 */

module dmd.parse;

import core.stdc.stdio;
import core.stdc.string;

import dmd.astenums;
import dmd.errorsink;
import dmd.id;
import dmd.identifier;
import dmd.lexer;
import dmd.location;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.rootobject;
import dmd.root.string;
import dmd.tokens;

alias CompileEnv = dmd.lexer.CompileEnv;

/***********************************************************
 */
class Parser(AST, Lexer = dmd.lexer.Lexer) : Lexer
{
    AST.ModuleDeclaration* md;

    protected
    {
        AST.Module mod;
        LINK linkage;
        Loc linkLoc;
        CPPMANGLE cppmangle;
        Loc endloc; // set to location of last right curly
        int inBrackets; // inside [] of array index or slice
        Loc lookingForElse; // location of lonely if looking for an else
        bool doUnittests; // parse unittest blocks
    }

    /*********************
     * Use this constructor for string mixins.
     * Input:
     *      loc = location in source file of mixin
     */
    extern (D) this(Loc loc, AST.Module _module, const(char)[] input, bool doDocComment,
        ErrorSink errorSink, const CompileEnv* compileEnv, const bool doUnittests) scope
    {
        //printf("Parser::Parser()1 %d\n", doUnittests);
        this(_module, input, doDocComment, errorSink, compileEnv, doUnittests);
        scanloc = loc;
    }

    /**************************************************
     * Main Parser constructor.
     */
    extern (D) this(AST.Module _module, const(char)[] input, bool doDocComment, ErrorSink errorSink,
        const CompileEnv* compileEnv, const bool doUnittests) scope
    {
        super(_module ? _module.srcfile.toChars() : null, input.ptr, 0, input.length, doDocComment, false,
              errorSink,
              compileEnv);

        //printf("Parser::Parser()2 %d\n", doUnittests);
        this.mod = _module;
        this.linkage = LINK.d;
        this.doUnittests = doUnittests;
    }

    /++
     + Parse a module, i.e. the optional `module x.y.z` declaration and all declarations
     + found in the current file.
     +
     + Returns: the list of declarations or an empty list in case of malformed declarations,
     +          the module declaration will be stored as `this.md` if found
     +/
    AST.Dsymbols* parseModule()
    {
        if (!parseModuleDeclaration())
            return errorReturn();

        return parseModuleContent();
    }

    /++
     + Parse the optional module declaration
     +
     + Returns: false if a malformed module declaration was found
     +/
    final bool parseModuleDeclaration()
    {
        const comment = token.blockComment;
        bool isdeprecated = false;
        AST.Expression msg = null;

        // Parse optional module attributes
        parseModuleAttributes(msg, isdeprecated);

        // ModuleDeclaration leads off
        if (token.value != TOK.module_)
            return true;

        const loc = token.loc;
        nextToken();

        /* parse ModuleFullyQualifiedName
         * https://dlang.org/spec/module.html#ModuleFullyQualifiedName
         */

        if (token.value != TOK.identifier)
        {
            error("identifier expected following `module`");
            return false;
        }

        Identifier[] a;
        Identifier id = token.ident;

        while (nextToken() == TOK.dot)
        {
            a ~= id;
            nextToken();
            if (token.value != TOK.identifier)
            {
                error("identifier expected following `package`");
                return false;
            }
            id = token.ident;
        }

        md = new AST.ModuleDeclaration(loc, a, id, msg, isdeprecated);

        if (token.value != TOK.semicolon)
            error("`;` expected following module declaration instead of `%s`", token.toChars());
        nextToken();
        addComment(mod, comment);

        return true;
    }

    /++
     + Parse the content of a module, i.e. all declarations found until the end of file.
     +
     + Returns: the list of declarations or an empty list in case of malformed declarations
     +/
    final AST.Dsymbols* parseModuleContent()
    {
        AST.Dsymbol lastDecl = mod;
        AST.Dsymbols* decldefs = parseDeclDefs(0, &lastDecl);

        if (token.value == TOK.rightCurly)
        {
            error("unmatched closing brace");
            return errorReturn();
        }

        if (token.value != TOK.endOfFile)
        {
            error("unrecognized declaration");
            return errorReturn();
        }
        return decldefs;
    }

    /++
     + Skips to the end of the current declaration - denoted by either `;` or EOF
     +
     + Returns: An empty list of Dsymbols
     +/
    private AST.Dsymbols* errorReturn()
    {
        while (token.value != TOK.semicolon && token.value != TOK.endOfFile)
            nextToken();
        nextToken();
        return new AST.Dsymbols();
    }

    /**********************************
     * Parse the ModuleAttributes preceding a module declaration.
     * ModuleDeclaration:
     *    ModuleAttributes(opt) module ModuleFullyQualifiedName ;
     * https://dlang.org/spec/module.html#ModuleAttributes
     * Params:
     *  msg = set to the AssignExpression from DeprecatedAttribute https://dlang.org/spec/module.html#DeprecatedAttribute
     *  isdeprecated = set to true if a DeprecatedAttribute is seen
     */
    private
    void parseModuleAttributes(out AST.Expression msg, out bool isdeprecated)
    {
        Token* tk;
        if (!(skipAttributes(&token, &tk) && tk.value == TOK.module_))
            return;             // no module attributes

        AST.Expressions* udas = null;
        while (token.value != TOK.module_)
        {
            switch (token.value)
            {
            case TOK.deprecated_:
                {
                    // deprecated (...) module ...
                    if (isdeprecated)
                        error("there is only one deprecation attribute allowed for module declaration");
                    isdeprecated = true;
                    nextToken();
                    if (token.value == TOK.leftParenthesis)
                    {
                        check(TOK.leftParenthesis);
                        msg = parseAssignExp();
                        check(TOK.rightParenthesis);
                    }
                    break;
                }
            case TOK.at:
                {
                    AST.Expressions* exps = null;
                    const stc = parseAttribute(exps);
                    if (stc & atAttrGroup)
                    {
                        error("`@%s` attribute for module declaration is not supported", token.toChars());
                    }
                    else
                    {
                        static if (is(typeof(mod.edition)))
                            if (exps && exps.length > 0)
                                if (auto id = (*exps)[0].isIdentifierExp())
                                    if (id.ident == Id.__edition_latest_do_not_use)
                                    {
                                        mod.edition = Edition.latest;
                                        continue;
                                    }

                        udas = AST.UserAttributeDeclaration.concat(udas, exps);
                    }
                    if (stc)
                        nextToken();
                    break;
                }
            default:
                {
                    error("`module` expected instead of `%s`", token.toChars());
                    nextToken();
                    break;
                }
            }
        }

        if (udas)
        {
            auto a = new AST.Dsymbols();
            auto udad = new AST.UserAttributeDeclaration(udas, a);
            mod.userAttribDecl = udad;
        }
    }

  final:

    /**
     * Parses a `deprecated` declaration
     *
     * Params:
     *   msg = Deprecated message, if any.
     *         Used to support overriding a deprecated storage class with
     *         a deprecated declaration with a message, but to error
     *         if both declaration have a message.
     *
     * Returns:
     *   Whether the deprecated declaration has a message
     */
    private bool parseDeprecatedAttribute(ref AST.Expression msg)
    {
        if (peekNext() != TOK.leftParenthesis)
            return false;

        nextToken();
        check(TOK.leftParenthesis);
        AST.Expression e = parseAssignExp();
        check(TOK.rightParenthesis);
        if (msg)
        {
            error(token.loc, "conflicting storage class `deprecated(%s)` and `deprecated(%s)`", msg.toChars(), e.toChars());
        }
        msg = e;
        return true;
    }

    /************************************
     * Parse declarations and definitions
     * Params:
     *  once = !=0 means parse exactly one decl or def
     *  pLastDecl = set to last decl or def parsed
     *  pAttrs = keep track of attributes
     * Returns:
     *  array of declared symbols
     */
    AST.Dsymbols* parseDeclDefs(int once, AST.Dsymbol* pLastDecl = null, PrefixAttributes!AST* pAttrs = null)
    {
        AST.Dsymbol lastDecl = null; // used to link unittest to its previous declaration
        if (!pLastDecl)
            pLastDecl = &lastDecl;

        const linksave = linkage; // save global state

        //printf("Parser::parseDeclDefs()\n");
        auto decldefs = new AST.Dsymbols();
        do
        {
            // parse result
            AST.Dsymbol s = null;
            AST.Dsymbols* a = null;

            PrefixAttributes!AST attrs;
            if (!once || !pAttrs)
            {
                pAttrs = &attrs;
                pAttrs.comment = token.blockComment.ptr;
            }
            AST.Visibility.Kind prot;
            STC stc;
            AST.Condition condition;

            linkage = linksave;

            Loc startloc;
            Loc scdLoc;

            switch (token.value)
            {
            case TOK.enum_:
                {
                    /* Determine if this is a manifest constant declaration,
                     * or a conventional enum.
                     */
                    const tv = peekNext();
                    if (tv == TOK.leftCurly || tv == TOK.colon)
                        s = parseEnum();
                    else if (tv != TOK.identifier)
                        goto Ldeclaration;
                    else
                    {
                        const nextv = peekNext2();
                        if (nextv == TOK.leftCurly || nextv == TOK.colon || nextv == TOK.semicolon)
                            s = parseEnum();
                        else
                            goto Ldeclaration;
                    }
                    break;
                }
            case TOK.import_:
                a = parseImport();
                // keep pLastDecl
                break;

            case TOK.template_:
                s = cast(AST.Dsymbol)parseTemplateDeclaration();
                break;

            case TOK.mixin_:
                {
                    const loc = token.loc;
                    switch (peekNext())
                    {
                    case TOK.leftParenthesis:
                        {
                            // MixinType
                            if (isDeclaration(&token, NeedDeclaratorId.mustIfDstyle, TOK.reserved, null))
                                goto Ldeclaration;
                            // mixin(string)
                            nextToken();
                            auto exps = parseArguments();
                            check(TOK.semicolon);
                            s = new AST.MixinDeclaration(loc, exps);
                            break;
                        }
                    case TOK.template_:
                        // mixin template
                        nextToken();
                        s = cast(AST.Dsymbol)parseTemplateDeclaration(true);
                        break;

                    default:
                        s = parseMixin();
                        break;
                    }
                    break;
                }
            case TOK.wchar_:
            case TOK.dchar_:
            case TOK.bool_:
            case TOK.char_:
            case TOK.int8:
            case TOK.uns8:
            case TOK.int16:
            case TOK.uns16:
            case TOK.int32:
            case TOK.uns32:
            case TOK.int64:
            case TOK.uns64:
            case TOK.int128:
            case TOK.uns128:
            case TOK.float32:
            case TOK.float64:
            case TOK.float80:
            case TOK.imaginary32:
            case TOK.imaginary64:
            case TOK.imaginary80:
            case TOK.complex32:
            case TOK.complex64:
            case TOK.complex80:
            case TOK.void_:
            case TOK.alias_:
            case TOK.identifier:
            case TOK.super_:
            case TOK.typeof_:
            case TOK.dot:
            case TOK.vector:
            case TOK.struct_:
            case TOK.union_:
            case TOK.class_:
            case TOK.interface_:
            case TOK.traits:
            Ldeclaration:
                a = parseDeclarations(false, pAttrs, pAttrs.comment);
                if (a && a.length)
                    *pLastDecl = (*a)[a.length - 1];
                break;

            case TOK.this_:
                if (peekNext() == TOK.dot)
                    goto Ldeclaration;
                s = parseCtor(pAttrs);
                break;

            case TOK.tilde:
                s = parseDtor(pAttrs);
                break;

            case TOK.invariant_:
                const tv = peekNext();
                if (tv == TOK.leftParenthesis || tv == TOK.leftCurly)
                {
                    // invariant { statements... }
                    // invariant() { statements... }
                    // invariant (expression);
                    s = parseInvariant(pAttrs);
                    break;
                }
                error("invariant body expected, not `%s`", token.toChars());
                goto Lerror;

            case TOK.unittest_:
                /**
                 * Ignore unittests in non-root modules.
                 *
                 * This mainly means that unittests *inside templates* are only
                 * ever instantiated if the module lexically declaring the
                 * template is one of the root modules.
                 *
                 * E.g., compiling some project with `-unittest` does NOT
                 * compile and later run any unittests in instantiations of
                 * templates declared in other libraries.
                 *
                 * Declaring unittests *inside* templates is considered an anti-
                 * pattern. In almost all cases, the unittests don't depend on
                 * the template parameters, but instantiate the template with
                 * fixed arguments (e.g., Nullable!T unittests instantiating
                 * Nullable!int), so compiling and running identical tests for
                 * each template instantiation is hardly desirable.
                 * But adding a unittest right below some function being tested
                 * is arguably good for locality, so unittests end up inside
                 * templates.
                 * To make sure a template's unittests are run, it should be
                 * instantiated in the same module, e.g., some module-level
                 * unittest.
                 *
                 * Another reason for ignoring unittests in templates from non-
                 * root modules is for template codegen culling via
                 * TemplateInstance.needsCodegen(). If the compiler decides not
                 * to emit some Nullable!bool because there's an existing
                 * instantiation in some non-root module, it has no idea whether
                 * that module was compiled with -unittest too, and so whether
                 * Nullable!int (instantiated in some unittest inside the
                 * Nullable template) can be culled too. By ignoring unittests
                 * in non-root modules, the compiler won't consider any
                 * template instantiations in these unittests as candidates for
                 * further codegen culling.
                 */
                // The isRoot check is here because it can change after parsing begins (see dmodule.d)
                if (doUnittests && mod.isRoot())
                {
                    linkage = LINK.d; // unittests have D linkage
                    s = parseUnitTest(pAttrs);
                    if (*pLastDecl)
                        (*pLastDecl).ddocUnittest = cast(AST.UnitTestDeclaration)s;
                }
                else
                {
                    // Skip over unittest block by counting { }
                    Loc loc = token.loc;
                    int braces = 0;
                    while (1)
                    {
                        nextToken();
                        switch (token.value)
                        {
                        case TOK.leftCurly:
                            ++braces;
                            continue;

                        case TOK.rightCurly:
                            if (--braces)
                                continue;
                            nextToken();
                            break;

                        case TOK.endOfFile:
                            /* { */
                            error(loc, "closing `}` of unittest not found before end of file");
                            goto Lerror;

                        default:
                            continue;
                        }
                        break;
                    }
                    // Workaround 14894. Add an empty unittest declaration to keep
                    // the number of symbols in this scope independent of -unittest.
                    s = new AST.UnitTestDeclaration(loc, token.loc, STC.none, null);
                }
                break;

            case TOK.new_:
                s = parseNewDeclaration(pAttrs);
                break;

            case TOK.colon:
            case TOK.leftCurly:
                error("declaration expected, not `%s`", token.toChars());
                goto Lerror;

            case TOK.rightCurly:
            case TOK.endOfFile:
                if (once)
                    error("declaration expected, not `%s`", token.toChars());
                return decldefs;

            case TOK.static_:
                {
                    const next = peekNext();
                    if (next == TOK.this_)
                        s = parseStaticCtor(pAttrs);
                    else if (next == TOK.tilde)
                        s = parseStaticDtor(pAttrs);
                    else if (next == TOK.assert_)
                        s = parseStaticAssert();
                    else if (next == TOK.if_)
                    {
                        const Loc loc = token.loc;
                        condition = parseStaticIfCondition();
                        AST.Dsymbols* athen;
                        if (token.value == TOK.colon)
                            athen = parseBlock(pLastDecl);
                        else
                        {
                            const lookingForElseSave = lookingForElse;
                            lookingForElse = token.loc;
                            athen = parseBlock(pLastDecl);
                            lookingForElse = lookingForElseSave;
                        }
                        AST.Dsymbols* aelse = null;
                        if (token.value == TOK.else_)
                        {
                            const elseloc = token.loc;
                            nextToken();
                            aelse = parseBlock(pLastDecl);
                            checkDanglingElse(elseloc);
                        }
                        s = new AST.StaticIfDeclaration(loc, condition, athen, aelse);
                    }
                    else if (next == TOK.import_)
                    {
                        a = parseImport();
                        // keep pLastDecl
                    }
                    else if (next == TOK.foreach_ || next == TOK.foreach_reverse_)
                    {
                        s = parseForeach!(AST.StaticForeachDeclaration)(token.loc, pLastDecl);
                    }
                    else
                    {
                        stc = STC.static_;
                        goto Lstc;
                    }
                    break;
                }
            case TOK.const_:
                if (peekNext() == TOK.leftParenthesis)
                    goto Ldeclaration;
                stc = STC.const_;
                goto Lstc;

            case TOK.immutable_:
                if (peekNext() == TOK.leftParenthesis)
                    goto Ldeclaration;
                stc = STC.immutable_;
                goto Lstc;

            case TOK.shared_:
                {
                    const next = peekNext();
                    if (next == TOK.leftParenthesis)
                        goto Ldeclaration;
                    if (next == TOK.static_)
                    {
                        TOK next2 = peekNext2();
                        if (next2 == TOK.this_)
                        {
                            s = parseSharedStaticCtor(pAttrs);
                            break;
                        }
                        if (next2 == TOK.tilde)
                        {
                            s = parseSharedStaticDtor(pAttrs);
                            break;
                        }
                    }
                    stc = STC.shared_;
                    goto Lstc;
                }
            case TOK.inout_:
                if (peekNext() == TOK.leftParenthesis)
                    goto Ldeclaration;
                stc = STC.wild;
                goto Lstc;

            case TOK.final_:
                stc = STC.final_;
                goto Lstc;

            case TOK.auto_:
                stc = STC.auto_;
                if (peekNext() == TOK.ref_)
                    stc |= STC.autoref;
                goto Lstc;

            case TOK.scope_:
                stc = STC.scope_;
                goto Lstc;

            case TOK.override_:
                stc = STC.override_;
                goto Lstc;

            case TOK.abstract_:
                stc = STC.abstract_;
                goto Lstc;

            case TOK.synchronized_:
                stc = STC.synchronized_;
                goto Lstc;

            case TOK.nothrow_:
                stc = STC.nothrow_;
                goto Lstc;

            case TOK.pure_:
                stc = STC.pure_;
                goto Lstc;

            case TOK.ref_:
                stc = STC.ref_;
                goto Lstc;

            case TOK.gshared:
                stc = STC.gshared;
                goto Lstc;

            case TOK.at:
                {
                    AST.Expressions* exps = null;
                    stc = parseAttribute(exps);
                    if (stc)
                        goto Lstc; // it's a predefined attribute
                    // no redundant/conflicting check for UDAs
                    pAttrs.udas = AST.UserAttributeDeclaration.concat(pAttrs.udas, exps);
                    goto Lautodecl;
                }
            Lstc:
                pAttrs.storageClass = appendStorageClass(pAttrs.storageClass, stc);
                scdLoc = token.loc;
                nextToken();

            Lautodecl:

                /* Look for auto initializers:
                 *      storage_class identifier = initializer;
                 *      storage_class identifier(...) = initializer;
                 */
                if (token.value == TOK.identifier && hasOptionalParensThen(peek(&token), TOK.assign))
                {
                    a = parseAutoDeclarations(getStorageClass!AST(pAttrs), pAttrs.comment);
                    if (a && a.length)
                        *pLastDecl = (*a)[a.length - 1];
                    if (pAttrs.udas)
                    {
                        s = new AST.UserAttributeDeclaration(pAttrs.udas, a);
                        pAttrs.udas = null;
                    }
                    break;
                }

                /* Look for return type inference for template functions.
                 */
                Token* tk;
                if (token.value == TOK.identifier && skipParens(peek(&token), &tk) && skipAttributes(tk, &tk) &&
                    (tk.value == TOK.leftParenthesis || tk.value == TOK.leftCurly || tk.value == TOK.in_ ||
                     tk.value == TOK.out_ || tk.value == TOK.do_ || tk.value == TOK.goesTo ||
                     tk.value == TOK.identifier && tk.ident == Id._body))
                {
                    if (tk.value == TOK.identifier && tk.ident == Id._body)
                        usageOfBodyKeyword();

                    a = parseDeclarations(true, pAttrs, pAttrs.comment);
                    if (a && a.length)
                        *pLastDecl = (*a)[a.length - 1];
                    if (pAttrs.udas)
                    {
                        s = new AST.UserAttributeDeclaration(pAttrs.udas, a);
                        pAttrs.udas = null;
                    }
                    break;
                }

                a = parseBlock(pLastDecl, pAttrs);
                auto stc2 = getStorageClass!AST(pAttrs);
                if (stc2 != STC.none)
                {
                    s = new AST.StorageClassDeclaration(scdLoc, stc2, a);
                }
                if (pAttrs.udas)
                {
                    if (s)
                    {
                        a = new AST.Dsymbols();
                        a.push(s);
                    }
                    s = new AST.UserAttributeDeclaration(pAttrs.udas, a);
                    pAttrs.udas = null;
                }
                break;

            case TOK.deprecated_:
                {
                    stc |= STC.deprecated_;
                    if (!parseDeprecatedAttribute(pAttrs.depmsg))
                        goto Lstc;

                    a = parseBlock(pLastDecl, pAttrs);
                    s = new AST.DeprecatedDeclaration(pAttrs.depmsg, a);
                    pAttrs.depmsg = null;
                    break;
                }
            case TOK.leftBracket:
                {
                    if (peekNext() == TOK.rightBracket)
                        error("empty attribute list is not allowed");
                    error("use `@(attributes)` instead of `[attributes]`");
                    AST.Expressions* exps = parseArguments();
                    // no redundant/conflicting check for UDAs

                    pAttrs.udas = AST.UserAttributeDeclaration.concat(pAttrs.udas, exps);
                    a = parseBlock(pLastDecl, pAttrs);
                    if (pAttrs.udas)
                    {
                        s = new AST.UserAttributeDeclaration(pAttrs.udas, a);
                        pAttrs.udas = null;
                    }
                    break;
                }
            case TOK.extern_:
                {
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.extern_;
                        goto Lstc;
                    }

                    const linkLoc = token.loc;
                    auto res = parseLinkage();
                    if (pAttrs.link != LINK.default_)
                    {
                        if (pAttrs.link != res.link)
                        {
                            error(token.loc, "conflicting linkage `extern (%s)` and `extern (%s)`", AST.linkageToChars(pAttrs.link), AST.linkageToChars(res.link));
                        }
                        else if (res.idents || res.identExps || res.cppmangle != CPPMANGLE.def)
                        {
                            // Allow:
                            //      extern(C++, foo) extern(C++, bar) void foo();
                            // to be equivalent with:
                            //      extern(C++, foo.bar) void foo();
                            // Allow also:
                            //      extern(C++, "ns") extern(C++, class) struct test {}
                            //      extern(C++, class) extern(C++, "ns") struct test {}
                        }
                        else
                            error("redundant linkage `extern (%s)`", AST.linkageToChars(pAttrs.link));
                    }
                    pAttrs.link = res.link;
                    this.linkage = res.link;
                    this.linkLoc = linkLoc;
                    a = parseBlock(pLastDecl, pAttrs);
                    if (res.idents)
                    {
                        assert(res.link == LINK.cpp);
                        assert(res.idents.length);
                        for (size_t i = res.idents.length; i;)
                        {
                            Identifier id = (*res.idents)[--i];
                            if (s)
                            {
                                a = new AST.Dsymbols();
                                a.push(s);
                            }
                            s = new AST.Nspace(linkLoc, id, null, a);
                        }
                        pAttrs.link = LINK.default_;
                    }
                    else if (res.identExps)
                    {
                        assert(res.link == LINK.cpp);
                        assert(res.identExps.length);
                        for (size_t i = res.identExps.length; i;)
                        {
                            AST.Expression exp = (*res.identExps)[--i];
                            if (s)
                            {
                                a = new AST.Dsymbols();
                                a.push(s);
                            }
                            s = new AST.CPPNamespaceDeclaration(linkLoc, exp, a);
                        }
                        pAttrs.link = LINK.default_;
                    }
                    else if (res.cppmangle != CPPMANGLE.def)
                    {
                        assert(res.link == LINK.cpp);
                        s = new AST.CPPMangleDeclaration(linkLoc, res.cppmangle, a);
                    }
                    else if (pAttrs.link != LINK.default_)
                    {
                        s = new AST.LinkDeclaration(linkLoc, pAttrs.link, a);
                        pAttrs.link = LINK.default_;
                    }
                    break;
                }

            case TOK.private_:
                prot = AST.Visibility.Kind.private_;
                goto Lprot;

            case TOK.package_:
                prot = AST.Visibility.Kind.package_;
                goto Lprot;

            case TOK.protected_:
                prot = AST.Visibility.Kind.protected_;
                goto Lprot;

            case TOK.public_:
                prot = AST.Visibility.Kind.public_;
                goto Lprot;

            case TOK.export_:
                prot = AST.Visibility.Kind.export_;
                goto Lprot;
            Lprot:
                {
                    if (pAttrs.visibility.kind != AST.Visibility.Kind.undefined)
                    {
                        if (pAttrs.visibility.kind != prot)
                            error(token.loc, "conflicting visibility attribute `%s` and `%s`", AST.visibilityToChars(pAttrs.visibility.kind), AST.visibilityToChars(prot));
                        else
                            error("redundant visibility attribute `%s`", AST.visibilityToChars(prot));
                    }
                    pAttrs.visibility.kind = prot;
                    const attrloc = token.loc;

                    nextToken();

                    // optional qualified package identifier to bind
                    // visibility to
                    Identifier[] pkg_prot_idents;
                    if (pAttrs.visibility.kind == AST.Visibility.Kind.package_ && token.value == TOK.leftParenthesis)
                    {
                        pkg_prot_idents = parseQualifiedIdentifier("protection package");
                        if (pkg_prot_idents)
                            check(TOK.rightParenthesis);
                        else
                        {
                            while (token.value != TOK.semicolon && token.value != TOK.endOfFile)
                                nextToken();
                            nextToken();
                            break;
                        }
                    }

                    a = parseBlock(pLastDecl, pAttrs);
                    if (pAttrs.visibility.kind != AST.Visibility.Kind.undefined)
                    {
                        if (pAttrs.visibility.kind == AST.Visibility.Kind.package_ && pkg_prot_idents)
                            s = new AST.VisibilityDeclaration(attrloc, pkg_prot_idents, a);
                        else
                            s = new AST.VisibilityDeclaration(attrloc, pAttrs.visibility, a);

                        pAttrs.visibility = AST.Visibility(AST.Visibility.Kind.undefined);
                    }
                    break;
                }
            case TOK.align_:
                {
                    const attrLoc = token.loc;

                    AST.Expression e = parseAlign();

                    if (pAttrs.setAlignment)
                    {
                        if (e)
                            error("redundant alignment attribute `align(%s)`", e.toChars());
                        else
                            error("redundant alignment attribute `align(default)`");
                    }

                    pAttrs.setAlignment = true;
                    pAttrs.ealign = e;
                    a = parseBlock(pLastDecl, pAttrs);
                    if (pAttrs.setAlignment)
                    {
                        s = new AST.AlignDeclaration(attrLoc, pAttrs.ealign, a);
                        pAttrs.setAlignment = false;
                        pAttrs.ealign = null;
                    }
                    break;
                }
            case TOK.pragma_:
                {
                    AST.Expressions* args = null;
                    const loc = token.loc;

                    nextToken();
                    check(TOK.leftParenthesis);
                    if (token.value != TOK.identifier)
                    {
                        error("`pragma(identifier)` expected");
                        goto Lerror;
                    }
                    Identifier ident = token.ident;
                    nextToken();
                    if (token.value == TOK.comma && peekNext() != TOK.rightParenthesis)
                        args = parseArguments(); // pragma(identifier, args...)
                    else
                        check(TOK.rightParenthesis); // pragma(identifier)

                    AST.Dsymbols* a2 = null;
                    if (token.value == TOK.semicolon)
                    {
                        /* https://issues.dlang.org/show_bug.cgi?id=2354
                         * Accept single semicolon as an empty
                         * DeclarationBlock following attribute.
                         *
                         * Attribute DeclarationBlock
                         * Pragma    DeclDef
                         *           ;
                         */
                        nextToken();
                    }
                    else
                        a2 = parseBlock(pLastDecl);
                    s = new AST.PragmaDeclaration(loc, ident, args, a2);
                    break;
                }
            case TOK.debug_:
                startloc = token.loc;
                nextToken();
                if (token.value == TOK.assign)
                {
                    s = parseDebugSpecification();
                    break;
                }
                condition = parseDebugCondition();
                goto Lcondition;

            case TOK.version_:
                startloc = token.loc;
                nextToken();
                if (token.value == TOK.assign)
                {
                    s = parseVersionSpecification();
                    break;
                }
                condition = parseVersionCondition();
                goto Lcondition;

            Lcondition:
                {
                    AST.Dsymbols* athen;
                    if (token.value == TOK.colon)
                        athen = parseBlock(pLastDecl);
                    else
                    {
                        const lookingForElseSave = lookingForElse;
                        lookingForElse = token.loc;
                        athen = parseBlock(pLastDecl);
                        lookingForElse = lookingForElseSave;
                    }
                    AST.Dsymbols* aelse = null;
                    if (token.value == TOK.else_)
                    {
                        const elseloc = token.loc;
                        nextToken();
                        aelse = parseBlock(pLastDecl);
                        checkDanglingElse(elseloc);
                    }
                    s = new AST.ConditionalDeclaration(startloc, condition, athen, aelse);
                    break;
                }
            case TOK.semicolon:
                // empty declaration
                //error("empty declaration");
                nextToken();
                continue;

            // The following are all errors, the cases are just for better error messages than the default case
            case TOK.return_:
            case TOK.goto_:
            case TOK.break_:
            case TOK.continue_:
                error("`%s` statement must be inside function scope", token.toChars());
                goto Lerror;
            case TOK.asm_:
            case TOK.do_:
            case TOK.for_:
            case TOK.foreach_:
            case TOK.foreach_reverse_:
            case TOK.if_:
            case TOK.switch_:
            case TOK.try_:
            case TOK.while_:
                error("`%s` statement must be inside function scope", token.toChars());
                if (peekNext() == TOK.leftParenthesis || peekNext() == TOK.leftCurly)
                {
                    parseStatement(0);
                    s = null;
                    continue;
                }
                goto Lerror;
            default:
                error("declaration expected, not `%s`", token.toChars());
            Lerror:
                while (token.value != TOK.semicolon && token.value != TOK.endOfFile)
                    nextToken();
                nextToken();
                s = null;
                continue;
            }

            if (s)
            {
                if (!s.isAttribDeclaration())
                    *pLastDecl = s;
                decldefs.push(s);
                addComment(s, pAttrs.comment);
            }
            else if (a && a.length)
            {
                decldefs.append(a);
            }
        }
        while (!once);

        linkage = linksave;

        return decldefs;
    }

    /*****************************************
     * Parse auto declarations of the form:
     *   storageClass ident = init, ident = init, ... ;
     * and return the array of them.
     * Starts with token on the first ident.
     * Ends with scanner past closing ';'
     */
    private AST.Dsymbols* parseAutoDeclarations(STC storageClass, const(char)* comment)
    {
        //printf("parseAutoDeclarations\n");
        auto a = new AST.Dsymbols();

        while (1)
        {
            const loc = token.loc;
            Identifier ident = token.ident;
            nextToken(); // skip over ident

            AST.TemplateParameters* tpl = null;
            if (token.value == TOK.leftParenthesis)
                tpl = parseTemplateParameterList();

            check(TOK.assign);   // skip over '='
            AST.Initializer _init = parseInitializer();
            auto v = new AST.VarDeclaration(loc, null, ident, _init, storageClass);

            AST.Dsymbol s = v;
            if (tpl)
            {
                auto a2 = new AST.Dsymbols();
                a2.push(v);
                auto tempdecl = new AST.TemplateDeclaration(loc, ident, tpl, null, a2, 0);
                s = tempdecl;
            }
            a.push(s);
            switch (token.value)
            {
            case TOK.semicolon:
                nextToken();
                addComment(s, comment);
                break;

            case TOK.comma:
                nextToken();
                if (!(token.value == TOK.identifier && hasOptionalParensThen(peek(&token), TOK.assign)))
                {
                    error("identifier expected following comma");
                    break;
                }
                addComment(s, comment);
                continue;

            default:
                error("semicolon expected following auto declaration, not `%s`", token.toChars());
                break;
            }
            break;
        }
        return a;
    }

    /********************************************
     * Parse declarations after an align, visibility, or extern decl.
     */
    private AST.Dsymbols* parseBlock(AST.Dsymbol* pLastDecl, PrefixAttributes!AST* pAttrs = null)
    {
        AST.Dsymbols* a = null;

        //printf("parseBlock()\n");
        switch (token.value)
        {
        case TOK.semicolon:
            error("declaration expected following attribute, not `;`");
            nextToken();
            break;

        case TOK.endOfFile:
            error("declaration expected following attribute, not end of file");
            break;

        case TOK.leftCurly:
            {
                const lcLoc = token.loc;
                const lookingForElseSave = lookingForElse;
                lookingForElse = Loc();

                nextToken();
                a = parseDeclDefs(0, pLastDecl);
                if (token.value != TOK.rightCurly)
                {
                    /* left curly brace */
                    error("matching `}` expected, not `%s`", token.toChars());
                    eSink.errorSupplemental(lcLoc, "unmatched `{`");
                }
                else
                    nextToken();
                lookingForElse = lookingForElseSave;
                break;
            }
        case TOK.colon:
            nextToken();
            a = parseDeclDefs(0, pLastDecl); // grab declarations up to closing curly bracket
            break;

        default:
            a = parseDeclDefs(1, pLastDecl, pAttrs);
            break;
        }
        return a;
    }

    /**
     * Provide an error message if `added` contains storage classes which are
     * redundant with those in `orig`; otherwise, return the combination.
     *
     * Params:
     *   orig = The already applied storage class.
     *   added = The new storage class to add to `orig`.
     *
     * Returns:
     *   The combination of both storage classes (`orig | added`).
     */
    private STC appendStorageClass(STC orig, STC added)
    {
        void checkConflictSTCGroup(bool at = false)(STC group)
        {
            if (added & group && orig & group & ((orig & group) - 1))
                error(
                    at ? "conflicting attribute `@%s`"
                       : "conflicting attribute `%s`",
                    token.toChars());
        }

        if (orig & added)
        {
            OutBuffer buf;
            AST.stcToBuffer(buf, added);
            error("redundant attribute `%s`", buf.peekChars());
            return orig | added;
        }

        const Redundant = (STC.const_ | STC.scope_ | STC.ref_);
        orig |= added;

        if ((orig & STC.in_) && (added & Redundant))
        {
            if (added & STC.const_)
                error("attribute `const` is redundant with previously-applied `in`");
            else if (compileEnv.previewIn)
            {
                error("attribute `%s` is redundant with previously-applied `in`",
                      (orig & STC.scope_) ? "scope".ptr : "ref".ptr);
            }
            else if (added & STC.ref_)
            {
                // accept using `in ref` for legacy compatibility
            }
            else
            {
                version (IN_GCC)
                    error("attribute `scope` cannot be applied with `in`, use `-fpreview=in` instead");
                else
                    error("attribute `scope` cannot be applied with `in`, use `-preview=in` instead");
            }
            return orig;
        }

        if ((added & STC.in_) && (orig & Redundant))
        {
            if (orig & STC.const_)
                error("attribute `in` cannot be added after `const`: remove `const`");
            else if (compileEnv.previewIn)
            {
                // Windows `printf` does not support `%1$s`
                const(char*) stc_str = (orig & STC.scope_) ? "scope".ptr : "ref".ptr;
                error(token.loc, "attribute `in` cannot be added after `%s`: remove `%s`",
                      stc_str, stc_str);
            }
            else if (orig & STC.ref_)
            {
                // accept using `in ref` for legacy compatibility
            }
            else
            {
                version (IN_GCC)
                    error("attribute `in` cannot be added after `scope`: remove `scope` and use `-fpreview=in`");
                else
                    error("attribute `in` cannot be added after `scope`: remove `scope` and use `-preview=in`");
            }
            return orig;
        }

        checkConflictSTCGroup(STC.const_ | STC.immutable_ | STC.manifest);
        checkConflictSTCGroup(STC.gshared | STC.shared_);
        checkConflictSTCGroup!true(STC.safeGroup);

        return orig;
    }

    /***********************************************
     * Parse attribute(s), lexer is on '@'.
     *
     * Attributes can be builtin (e.g. `@safe`, `@nogc`, etc...),
     * or be user-defined (UDAs). In the former case, we return the storage
     * class via the return value, while in thelater case we return `0`
     * and set `pudas`.
     *
     * Params:
     *   pudas = An array of UDAs to append to
     *
     * Returns:
     *   If the attribute is builtin, the return value will be non-zero.
     *   Otherwise, 0 is returned, and `pudas` will be appended to.
     */
    private STC parseAttribute(ref AST.Expressions* udas)
    {
        nextToken();
        if (token.value == TOK.identifier)
        {
            // If we find a builtin attribute, we're done, return immediately.
            if (STC stc = isBuiltinAtAttribute(token.ident))
                return stc;

            // Allow identifier, template instantiation, or function call
            // for `@Argument` (single UDA) form.
            AST.Expression exp = parsePrimaryExp();
            if (token.value == TOK.leftParenthesis)
            {
                const loc = token.loc;
                AST.Expressions* args = new AST.Expressions();
                AST.Identifiers* names = new AST.Identifiers();
                parseNamedArguments(args, names);
                exp = new AST.CallExp(loc, exp, args, names);
            }

            if (udas is null)
                udas = new AST.Expressions();
            udas.push(exp);
            return STC.none;
        }

        AST.Expression templateArgToExp(RootObject o, Loc loc)
        {
            switch (o.dyncast)
            {
                case DYNCAST.expression:
                    return cast(AST.Expression) o;
                case DYNCAST.type:
                    return new AST.TypeExp(loc, cast(AST.Type)o);
                default:
                    assert(0);
            }
        }

        if (token.value == TOK.leftParenthesis)
        {
            // Multi-UDAs ( `@( ArgumentList )`) form, concatenate with existing
            if (peekNext() == TOK.rightParenthesis)
                error("empty attribute list is not allowed");

            if (udas is null)
                udas = new AST.Expressions();
            auto args = parseTemplateArgumentList();
            foreach (arg; *args)
                udas.push(templateArgToExp(arg, token.loc));
            return STC.none;
        }

        if (auto o = parseTemplateSingleArgument())
        {
            if (udas is null)
                udas = new AST.Expressions();
            udas.push(templateArgToExp(o, token.loc));
            return STC.none;
        }

        if (token.isKeyword())
            error("`%s` is a keyword, not an `@` attribute", token.toChars());
        else
            error("`@identifier` or `@(ArgumentList)` expected, not `@%s`", token.toChars());

        return STC.none;
    }

    /***********************************************
     * Parse const/immutable/shared/inout/nothrow/pure postfix
     */
    private STC parsePostfix(STC storageClass, AST.Expressions** pudas)
    {
        while (1)
        {
            STC stc;
            switch (token.value)
            {
            case TOK.const_:
                stc = STC.const_;
                break;

            case TOK.immutable_:
                stc = STC.immutable_;
                break;

            case TOK.shared_:
                stc = STC.shared_;
                break;

            case TOK.inout_:
                stc = STC.wild;
                break;

            case TOK.nothrow_:
                stc = STC.nothrow_;
                break;

            case TOK.pure_:
                stc = STC.pure_;
                break;

            case TOK.return_:
                stc = STC.return_;
                if (peekNext() == TOK.scope_)
                    stc |= STC.returnScope;     // recognize `return scope`
                break;

            case TOK.scope_:
                stc = STC.scope_;
                break;

            case TOK.rvalue:
                stc = STC.rvalue;
                break;

            case TOK.at:
                {
                    AST.Expressions* udas = null;
                    stc = parseAttribute(udas);
                    if (udas)
                    {
                        if (pudas)
                            *pudas = AST.UserAttributeDeclaration.concat(*pudas, udas);
                        else
                        {
                            // Disallow:
                            //      void function() @uda fp;
                            //      () @uda { return 1; }
                            error("user-defined attributes cannot appear as postfixes");
                        }
                        continue;
                    }
                    break;
                }
            default:
                Token* tk;
                if (skipAttributes(&token, &tk) && tk.ptr != token.ptr ||
                    token.value == TOK.static_ || token.value == TOK.extern_)
                {
                    error("`%s` token is not allowed in postfix position",
                        Token.toChars(token.value));
                    nextToken();
                    continue;
                }
                return storageClass;
            }
            storageClass = appendStorageClass(storageClass, stc);
            nextToken();
        }
    }

    private STC parseTypeCtor()
    {
        STC storageClass = STC.none;

        while (1)
        {
            if (peekNext() == TOK.leftParenthesis)
                return storageClass;

            STC stc;
            switch (token.value)
            {
            case TOK.const_:
                stc = STC.const_;
                break;

            case TOK.immutable_:
                stc = STC.immutable_;
                break;

            case TOK.shared_:
                stc = STC.shared_;
                break;

            case TOK.inout_:
                stc = STC.wild;
                break;

            default:
                return storageClass;
            }
            storageClass = appendStorageClass(storageClass, stc);
            nextToken();
        }
    }

    /**************************************
     * Parse constraint.
     * Constraint is of the form:
     *      if ( ConstraintExpression )
     */
    private AST.Expression parseConstraint()
    {
        AST.Expression e = null;
        if (token.value == TOK.if_)
        {
            nextToken(); // skip over 'if'
            check(TOK.leftParenthesis);
            e = parseExpression();
            check(TOK.rightParenthesis);
        }
        return e;
    }

    /**************************************
     * Parse a TemplateDeclaration.
     */
    private AST.TemplateDeclaration parseTemplateDeclaration(bool ismixin = false)
    {
        AST.TemplateDeclaration tempdecl;
        Identifier id;
        AST.TemplateParameters* tpl;
        AST.Dsymbols* decldefs;
        AST.Expression constraint = null;
        const loc = token.loc;

        nextToken();
        if (token.value != TOK.identifier)
        {
            error("identifier expected following `template`");
            return null;
        }
        id = token.ident;
        nextToken();
        tpl = parseTemplateParameterList();
        if (!tpl)
            return null;

        constraint = parseConstraint();

        if (token.value != TOK.leftCurly)
        {
            error("`{` expected after template parameter list, not `%s`", token.toChars()); /* } */
            nextToken();
            return null;
        }
        decldefs = parseBlock(null);

        tempdecl = new AST.TemplateDeclaration(loc, id, tpl, constraint, decldefs, ismixin);
        return tempdecl;
    }

    /******************************************
     * Parse template parameter list.
     * Input:
     *      flag    0: parsing "( list )"
     *              1: parsing non-empty "list $(RPAREN)"
     */
    private AST.TemplateParameters* parseTemplateParameterList(int flag = 0)
    {
        auto tpl = new AST.TemplateParameters();

        if (!flag && token.value != TOK.leftParenthesis)
        {
            error("parenthesized template parameter list expected following template identifier");
            goto Lerr;
        }
        nextToken();

        // Get array of TemplateParameters
        if (flag || token.value != TOK.rightParenthesis)
        {
            while (token.value != TOK.rightParenthesis)
            {
                AST.TemplateParameter tp;
                Loc loc;
                Identifier tp_ident = null;
                AST.Type tp_spectype = null;
                AST.Type tp_valtype = null;
                AST.Type tp_defaulttype = null;
                AST.Expression tp_specvalue = null;
                AST.Expression tp_defaultvalue = null;

                // Get TemplateParameter

                // First, look ahead to see if it is a TypeParameter or a ValueParameter
                const tv = peekNext();
                if (token.value == TOK.alias_)
                {
                    // AliasParameter
                    nextToken();
                    loc = token.loc; // todo
                    AST.Type spectype = null;
                    if (isDeclaration(&token, NeedDeclaratorId.must, TOK.reserved, null))
                    {
                        spectype = parseType(&tp_ident);
                    }
                    else
                    {
                        if (token.value != TOK.identifier)
                        {
                            error("identifier expected for template `alias` parameter");
                            goto Lerr;
                        }
                        tp_ident = token.ident;
                        nextToken();
                    }
                    RootObject spec = null;
                    if (token.value == TOK.colon) // : Type
                    {
                        nextToken();
                        if (isDeclaration(&token, NeedDeclaratorId.no, TOK.reserved, null))
                            spec = parseType();
                        else
                            spec = parseCondExp();
                    }
                    RootObject def = null;
                    if (token.value == TOK.assign) // = Type
                    {
                        nextToken();
                        if (isDeclaration(&token, NeedDeclaratorId.no, TOK.reserved, null))
                            def = parseType();
                        else
                            def = parseCondExp();
                    }
                    tp = new AST.TemplateAliasParameter(loc, tp_ident, spectype, spec, def);
                }
                else if (tv == TOK.colon || tv == TOK.assign || tv == TOK.comma || tv == TOK.rightParenthesis)
                {
                    // TypeParameter
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected for template type parameter");
                        goto Lerr;
                    }
                    loc = token.loc;
                    tp_ident = token.ident;
                    nextToken();
                    if (token.value == TOK.colon) // : Type
                    {
                        nextToken();
                        tp_spectype = parseType();
                    }
                    if (token.value == TOK.assign) // = Type
                    {
                        nextToken();
                        tp_defaulttype = parseType();
                    }
                    tp = new AST.TemplateTypeParameter(loc, tp_ident, tp_spectype, tp_defaulttype);
                }
                else if (token.value == TOK.identifier && tv == TOK.dotDotDot)
                {
                    // ident...
                    loc = token.loc;
                    tp_ident = token.ident;
                    nextToken();
                    nextToken();
                    tp = new AST.TemplateTupleParameter(loc, tp_ident);
                }
                else if (token.value == TOK.this_)
                {
                    // ThisParameter
                    nextToken();
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected for template `this` parameter");
                        goto Lerr;
                    }
                    loc = token.loc;
                    tp_ident = token.ident;
                    nextToken();
                    if (token.value == TOK.colon) // : Type
                    {
                        nextToken();
                        tp_spectype = parseType();
                    }
                    if (token.value == TOK.assign) // = Type
                    {
                        nextToken();
                        tp_defaulttype = parseType();
                    }
                    tp = new AST.TemplateThisParameter(loc, tp_ident, tp_spectype, tp_defaulttype);
                }
                else
                {
                    // ValueParameter
                    loc = token.loc; // todo
                    tp_valtype = parseType(&tp_ident);
                    if (!tp_ident)
                    {
                        error("identifier expected for template value parameter");
                        tp_ident = Identifier.idPool("error");
                    }
                    if (token.value == TOK.colon) // : CondExpression
                    {
                        nextToken();
                        tp_specvalue = parseCondExp();
                    }
                    if (token.value == TOK.assign) // = CondExpression
                    {
                        nextToken();
                        tp_defaultvalue = parseAssignExp();
                    }
                    tp = new AST.TemplateValueParameter(loc, tp_ident, tp_valtype, tp_specvalue, tp_defaultvalue);
                }
                tpl.push(tp);
                if (token.value != TOK.comma)
                    break;
                nextToken();
            }
        }
        check(TOK.rightParenthesis);

    Lerr:
        return tpl;
    }

    /******************************************
     * Parse template mixin.
     *      mixin Foo;
     *      mixin Foo!(args);
     *      mixin a.b.c!(args).Foo!(args);
     *      mixin typeof(expr).identifier!(args);
     *      mixin Foo!(args) identifier;
     *      mixin identifier = Foo!(args);
     */
    private AST.Dsymbol parseMixin()
    {
        AST.TemplateMixin tm;
        Identifier id, name;
        AST.Objects* tiargs;

        //printf("parseMixin()\n");
        const locMixin = token.loc;
        nextToken(); // skip 'mixin'

        // mixin Identifier = MixinTemplateName TemplateArguments;
        if (token.value == TOK.identifier && peekNext() == TOK.assign)
        {
            name = token.ident;
            nextToken();
            nextToken();
        }

        auto loc = token.loc;
        AST.TypeQualified tqual = null;
        if (token.value == TOK.dot)
        {
            id = Id.empty;
        }
        else
        {
            if (token.value == TOK.typeof_)
            {
                tqual = parseTypeof();
                check(TOK.dot);
            }
            if (token.value != TOK.identifier)
            {
                error("identifier expected, not `%s`", token.toChars());
                id = Id.empty;
            }
            else
                id = token.ident;
            nextToken();
        }

        while (1)
        {
            tiargs = null;
            if (token.value == TOK.not)
            {
                tiargs = parseTemplateArguments();
            }

            if (tiargs && token.value == TOK.dot)
            {
                auto tempinst = new AST.TemplateInstance(loc, id, tiargs);
                if (!tqual)
                    tqual = new AST.TypeInstance(loc, tempinst);
                else
                    tqual.addInst(tempinst);
                tiargs = null;
            }
            else
            {
                if (!tqual)
                    tqual = new AST.TypeIdentifier(loc, id);
                else
                    tqual.addIdent(id);
            }

            if (token.value != TOK.dot)
                break;

            nextToken();
            if (token.value != TOK.identifier)
            {
                error("identifier expected following `.` instead of `%s`", token.toChars());
                break;
            }
            loc = token.loc;
            id = token.ident;
            nextToken();
        }

        // mixin MixinTemplateName TemplateArguments Identifier;
        if (!name && token.value == TOK.identifier)
        {
            name = token.ident;
            nextToken();
        }

        tm = new AST.TemplateMixin(locMixin, name, tqual, tiargs);
        if (token.value != TOK.semicolon)
            error("`;` expected after `mixin`");
        nextToken();

        return tm;
    }

    /******************************************
     * Parse template arguments.
     * Input:
     *      current token is opening '!'
     * Output:
     *      current token is one after closing '$(RPAREN)'
     */
    private AST.Objects* parseTemplateArguments()
    {
        AST.Objects* tiargs;

        nextToken();
        if (token.value == TOK.leftParenthesis)
        {
            // ident!(template_arguments)
            tiargs = parseTemplateArgumentList();
        }
        else
        {
            // ident!template_argument
            RootObject o = parseTemplateSingleArgument();
            if (!o)
            {
                error("template argument expected following `!`");
            }
            else
            {
                tiargs = new AST.Objects();
                tiargs.push(o);
            }
        }
        if (token.value == TOK.not)
        {
            TOK tok = peekNext();
            if (tok != TOK.is_ && tok != TOK.in_)
            {
                error("multiple ! arguments are not allowed");
            Lagain:
                nextToken();
                if (token.value == TOK.leftParenthesis)
                    parseTemplateArgumentList();
                else
                    parseTemplateSingleArgument();
                if (token.value == TOK.not && (tok = peekNext()) != TOK.is_ && tok != TOK.in_)
                    goto Lagain;
            }
        }
        return tiargs;
    }

    /******************************************
     * Parse template argument list.
     * Input:
     *      current token is opening '$(LPAREN)',
     *          or ',' for __traits
     * Output:
     *      current token is one after closing '$(RPAREN)'
     */
    private AST.Objects* parseTemplateArgumentList()
    {
        //printf("Parser::parseTemplateArgumentList()\n");
        auto tiargs = new AST.Objects();
        TOK endtok = TOK.rightParenthesis;
        assert(token.value == TOK.leftParenthesis || token.value == TOK.comma);
        nextToken();

        // Get TemplateArgumentList
        while (token.value != endtok)
        {
            tiargs.push(parseTypeOrAssignExp());
            if (token.value != TOK.comma)
                break;
            nextToken();
        }
        check(endtok, "template argument list");
        return tiargs;
    }

    /***************************************
     * Parse a Type or an Expression
     * Returns:
     *  RootObject representing the AST
     */
    RootObject parseTypeOrAssignExp(TOK endtoken = TOK.reserved)
    {
        return isDeclaration(&token, NeedDeclaratorId.no, endtoken, null)
            ? parseType()           // argument is a type
            : parseAssignExp();     // argument is an expression
    }

    /*****************************
     * Parse single template argument, to support the syntax:
     *      foo!arg
     * Input:
     *      current token is the arg
     * Returns: An AST.Type, AST.Expression, or `null` on error
     */
    private RootObject parseTemplateSingleArgument()
    {
        //printf("parseTemplateSingleArgument()\n");
        AST.Type ta;
        switch (token.value)
        {
        case TOK.identifier:
            ta = new AST.TypeIdentifier(token.loc, token.ident);
            goto LabelX;

        case TOK.vector:
            ta = parseVector();
            goto LabelX;

        case TOK.void_:
            ta = AST.Type.tvoid;
            goto LabelX;

        case TOK.int8:
            ta = AST.Type.tint8;
            goto LabelX;

        case TOK.uns8:
            ta = AST.Type.tuns8;
            goto LabelX;

        case TOK.int16:
            ta = AST.Type.tint16;
            goto LabelX;

        case TOK.uns16:
            ta = AST.Type.tuns16;
            goto LabelX;

        case TOK.int32:
            ta = AST.Type.tint32;
            goto LabelX;

        case TOK.uns32:
            ta = AST.Type.tuns32;
            goto LabelX;

        case TOK.int64:
            ta = AST.Type.tint64;
            goto LabelX;

        case TOK.uns64:
            ta = AST.Type.tuns64;
            goto LabelX;

        case TOK.int128:
            ta = AST.Type.tint128;
            goto LabelX;

        case TOK.uns128:
            ta = AST.Type.tuns128;
            goto LabelX;

        case TOK.float32:
            ta = AST.Type.tfloat32;
            goto LabelX;

        case TOK.float64:
            ta = AST.Type.tfloat64;
            goto LabelX;

        case TOK.float80:
            ta = AST.Type.tfloat80;
            goto LabelX;

        case TOK.imaginary32:
            ta = AST.Type.timaginary32;
            goto LabelX;

        case TOK.imaginary64:
            ta = AST.Type.timaginary64;
            goto LabelX;

        case TOK.imaginary80:
            ta = AST.Type.timaginary80;
            goto LabelX;

        case TOK.complex32:
            ta = AST.Type.tcomplex32;
            goto LabelX;

        case TOK.complex64:
            ta = AST.Type.tcomplex64;
            goto LabelX;

        case TOK.complex80:
            ta = AST.Type.tcomplex80;
            goto LabelX;

        case TOK.bool_:
            ta = AST.Type.tbool;
            goto LabelX;

        case TOK.char_:
            ta = AST.Type.tchar;
            goto LabelX;

        case TOK.wchar_:
            ta = AST.Type.twchar;
            goto LabelX;

        case TOK.dchar_:
            ta = AST.Type.tdchar;
            goto LabelX;
        LabelX:
            nextToken();
            return ta;

        case TOK.int32Literal:
        case TOK.uns32Literal:
        case TOK.int64Literal:
        case TOK.uns64Literal:
        case TOK.int128Literal:
        case TOK.uns128Literal:
        case TOK.float32Literal:
        case TOK.float64Literal:
        case TOK.float80Literal:
        case TOK.imaginary32Literal:
        case TOK.imaginary64Literal:
        case TOK.imaginary80Literal:
        case TOK.null_:
        case TOK.true_:
        case TOK.false_:
        case TOK.charLiteral:
        case TOK.wcharLiteral:
        case TOK.dcharLiteral:
        case TOK.string_:
        case TOK.interpolated:
        case TOK.hexadecimalString:
        case TOK.file:
        case TOK.fileFullPath:
        case TOK.line:
        case TOK.moduleString:
        case TOK.functionString:
        case TOK.prettyFunction:
        case TOK.this_:
            {
                // Template argument is an expression
                return parsePrimaryExp();
            }
        default:
            return null;
        }
    }

    /**********************************
     * Parse a static assertion.
     * Current token is 'static'.
     */
    private AST.StaticAssert parseStaticAssert()
    {
        const loc = token.loc;
        AST.Expression exp;
        AST.Expressions* msg = null;

        //printf("parseStaticAssert()\n");
        nextToken();
        nextToken();
        check(TOK.leftParenthesis);
        exp = parseAssignExp();
        if (token.value == TOK.comma)
        {
            if (peekNext() == TOK.rightParenthesis)
            {
                nextToken(); // consume `,`
                nextToken(); // consume `)`
            }
            else
                msg = parseArguments();
        }
        else
            check(TOK.rightParenthesis);
        check(TOK.semicolon, "static assert");
        return new AST.StaticAssert(loc, exp, msg);
    }

    /***********************************
     * Parse typeof(expression).
     * Current token is on the 'typeof'.
     */
    private AST.TypeQualified parseTypeof()
    {
        AST.TypeQualified t;
        const loc = token.loc;

        nextToken();
        check(TOK.leftParenthesis);
        if (token.value == TOK.return_) // typeof(return)
        {
            nextToken();
            t = new AST.TypeReturn(loc);
        }
        else
        {
            AST.Expression exp = parseExpression(); // typeof(expression)
            t = new AST.TypeTypeof(loc, exp);
        }
        check(TOK.rightParenthesis);
        return t;
    }

    /***********************************
     * Parse __vector(type).
     * Current token is on the '__vector'.
     */
    private AST.Type parseVector()
    {
        nextToken();
        check(TOK.leftParenthesis);
        AST.Type tb = parseType();
        check(TOK.rightParenthesis);
        return new AST.TypeVector(tb);
    }

    /***********************************
     * Parse:
     *      extern (linkage)
     *      extern (C++, namespaces)
     *      extern (C++, "namespace", "namespaces", ...)
     *      extern (C++, (StringExp))
     * The parser is on the 'extern' token.
     */
    private ParsedLinkage!(AST) parseLinkage()
    {
        ParsedLinkage!(AST) result;
        nextToken();
        assert(token.value == TOK.leftParenthesis);
        nextToken();
        ParsedLinkage!(AST) returnLinkage(LINK link)
        {
            check(TOK.rightParenthesis);
            result.link = link;
            return result;
        }
        ParsedLinkage!(AST) invalidLinkage()
        {
            error("valid linkage identifiers are `D`, `C`, `C++`, `Objective-C`, `Windows`, `System`");
            return returnLinkage(LINK.d);
        }

        if (token.value != TOK.identifier)
            return returnLinkage(LINK.d);

        Identifier id = token.ident;
        nextToken();
        if (id == Id.Windows)
            return returnLinkage(LINK.windows);
        if (id == Id.D)
            return returnLinkage(LINK.d);
        if (id == Id.System)
            return returnLinkage(LINK.system);
        if (id == Id.Objective) // Looking for tokens "Objective-C"
        {
            if (token.value != TOK.min)
                return invalidLinkage();

            nextToken();
            if (token.ident != Id.C)
                return invalidLinkage();

            nextToken();
            return returnLinkage(LINK.objc);
        }
        else if (id != Id.C)
            return invalidLinkage();

        if (token.value != TOK.plusPlus)
            return returnLinkage(LINK.c);

        nextToken();
        if (token.value != TOK.comma) // , namespaces or class or struct
            return returnLinkage(LINK.cpp);

        nextToken();

        if (token.value == TOK.rightParenthesis)
            return returnLinkage(LINK.cpp); // extern(C++,)

        if (token.value == TOK.class_ || token.value == TOK.struct_)
        {
            result.cppmangle = token.value == TOK.class_ ? CPPMANGLE.asClass : CPPMANGLE.asStruct;
            nextToken();
        }
        else if (token.value == TOK.identifier) // named scope namespace
        {
            result.idents = new AST.Identifiers();
            while (1)
            {
                Identifier idn = token.ident;
                result.idents.push(idn);
                nextToken();
                if (token.value == TOK.dot)
                {
                    nextToken();
                    if (token.value == TOK.identifier)
                        continue;
                    error("identifier expected for C++ namespace");
                    result.idents = null;  // error occurred, invalidate list of elements.
                }
                break;
            }
        }
        else // non-scoped StringExp namespace
        {
            result.identExps = new AST.Expressions();
            while (1)
            {
                result.identExps.push(parseCondExp());
                if (token.value != TOK.comma)
                    break;
                nextToken();
                // Allow trailing commas as done for argument lists, arrays, ...
                if (token.value == TOK.rightParenthesis)
                    break;
            }
        }
        return returnLinkage(LINK.cpp);
    }

    /***********************************
     * Parse ident1.ident2.ident3
     *
     * Params:
     *  entity = what qualified identifier is expected to resolve into.
     *     Used only for better error message
     *
     * Returns:
     *     array of identifiers with actual qualified one stored last
     */
    private Identifier[] parseQualifiedIdentifier(const(char)* entity)
    {
        Identifier[] qualified;

        do
        {
            nextToken();
            if (token.value != TOK.identifier)
            {
                error(token.loc, "`%s` expected as dot-separated identifiers, got `%s`", entity, token.toChars());
                return qualified;
            }

            Identifier id = token.ident;
            qualified ~= id;

            nextToken();
        }
        while (token.value == TOK.dot);

        return qualified;
    }

    private AST.DebugSymbol parseDebugSpecification()
    {
        AST.DebugSymbol s;
        nextToken();
        if (token.value == TOK.identifier)
            s = new AST.DebugSymbol(token.loc, token.ident);
        else
        {
            error("identifier expected, not `%s`", token.toChars());
            s = null;
        }
        nextToken();
        if (token.value != TOK.semicolon)
            error("semicolon expected");
        nextToken();
        return s;
    }

    /**************************************
     * Parse a debug conditional
     */
    private AST.Condition parseDebugCondition()
    {
        Identifier id = null;
        Loc loc = token.loc;

        if (token.value == TOK.leftParenthesis)
        {
            nextToken();

            if (token.value == TOK.identifier)
                id = token.ident;
            else
                error("identifier expected inside `debug(...)`, not `%s`", token.toChars());
            loc = token.loc;
            nextToken();
            check(TOK.rightParenthesis);
        }
        return new AST.DebugCondition(loc, mod, id);
    }

    /**************************************
     * Parse a version specification
     */
    private AST.VersionSymbol parseVersionSpecification()
    {
        AST.VersionSymbol s;
        nextToken();
        if (token.value == TOK.identifier)
            s = new AST.VersionSymbol(token.loc, token.ident);
        else
        {
            error("identifier expected, not `%s`", token.toChars());
            s = null;
        }
        nextToken();
        if (token.value != TOK.semicolon)
            error("semicolon expected");
        nextToken();
        return s;
    }

    /**************************************
     * Parse a version conditional
     */
    private AST.Condition parseVersionCondition()
    {
        Identifier id = null;
        Loc loc;

        if (token.value == TOK.leftParenthesis)
        {
            nextToken();
            /* Allow:
             *    version (unittest)
             *    version (assert)
             * even though they are keywords
             */
            loc = token.loc;
            if (token.value == TOK.identifier)
                id = token.ident;
            else if (token.value == TOK.unittest_)
                id = Identifier.idPool(Token.toString(TOK.unittest_));
            else if (token.value == TOK.assert_)
                id = Identifier.idPool(Token.toString(TOK.assert_));
            else
                error("identifier expected inside `version(...)`, not `%s`", token.toChars());
            nextToken();
            check(TOK.rightParenthesis);
        }
        else
            error("(condition) expected following `version`");
        return new AST.VersionCondition(loc, mod, id);
    }

    /***********************************************
     *      static if (expression)
     *          body
     *      else
     *          body
     * Current token is 'static'.
     */
    private AST.Condition parseStaticIfCondition()
    {
        AST.Expression exp;
        AST.Condition condition;
        const loc = token.loc;

        nextToken();
        nextToken();
        if (token.value == TOK.leftParenthesis)
        {
            nextToken();
            exp = parseAssignExp();
            check(TOK.rightParenthesis);
        }
        else
        {
            error("(expression) expected following `static if`");
            exp = null;
        }
        condition = new AST.StaticIfCondition(loc, exp);
        return condition;
    }

    /*****************************************
     * Parse a constructor definition:
     *      this(parameters) { body }
     * or postblit:
     *      this(this) { body }
     * or constructor template:
     *      this(templateparameters)(parameters) { body }
     * Current token is 'this'.
     */
    private AST.Dsymbol parseCtor(PrefixAttributes!AST* pAttrs)
    {
        AST.Expressions* udas = null;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        if (token.value == TOK.leftParenthesis && peekNext() == TOK.this_ && peekNext2() == TOK.rightParenthesis)
        {
            // this(this) { ... }
            nextToken();
            nextToken();
            check(TOK.rightParenthesis);

            stc = parsePostfix(stc, &udas);
            if (stc & STC.immutable_)
                deprecation("`immutable` postblit is deprecated. Please use an unqualified postblit.");
            if (stc & STC.shared_)
                deprecation("`shared` postblit is deprecated. Please use an unqualified postblit.");
            if (stc & STC.const_)
                deprecation("`const` postblit is deprecated. Please use an unqualified postblit.");
            if (stc & STC.static_)
                error(loc, "postblit cannot be `static`");

            auto f = new AST.PostBlitDeclaration(loc, Loc.initial, stc, Id.postblit);
            AST.Dsymbol s = parseContracts(f);
            if (udas)
            {
                auto a = new AST.Dsymbols();
                a.push(f);
                s = new AST.UserAttributeDeclaration(udas, a);
            }
            return s;
        }

        /* Look ahead to see if:
         *   this(...)(...)
         * which is a constructor template
         */
        AST.TemplateParameters* tpl = null;
        if (token.value == TOK.leftParenthesis && peekPastParen(&token).value == TOK.leftParenthesis)
        {
            tpl = parseTemplateParameterList();
        }

        /* Just a regular constructor
         */
        auto parameterList = parseParameterList(null);
        stc = parsePostfix(stc, &udas);

        if (parameterList.varargs != VarArg.none || AST.Parameter.dim(parameterList.parameters) != 0)
        {
            if (stc & STC.static_)
                error(loc, "constructor cannot be static");
        }
        else if (STC ss = stc & (STC.shared_ | STC.static_)) // this()
        {
            if (ss == STC.static_)
                error(loc, "use `static this()` to declare a static constructor");
            else if (ss == (STC.shared_ | STC.static_))
                error(loc, "use `shared static this()` to declare a shared static constructor");
        }

        AST.Expression constraint = tpl ? parseConstraint() : null;

        AST.Type tf = new AST.TypeFunction(parameterList, null, linkage, stc); // ReturnType -> auto
        tf = tf.addSTC(stc);

        auto f = new AST.CtorDeclaration(loc, Loc.initial, stc, tf);
        AST.Dsymbol s = parseContracts(f, !!tpl);
        if (udas)
        {
            auto a = new AST.Dsymbols();
            a.push(f);
            s = new AST.UserAttributeDeclaration(udas, a);
        }

        if (tpl)
        {
            // Wrap a template around it
            auto decldefs = new AST.Dsymbols();
            decldefs.push(s);
            s = new AST.TemplateDeclaration(loc, f.ident, tpl, constraint, decldefs);
        }

        return s;
    }

    /*****************************************
     * Parse a destructor definition:
     *      ~this() { body }
     * Current token is '~'.
     */
    private AST.Dsymbol parseDtor(PrefixAttributes!AST* pAttrs)
    {
        AST.Expressions* udas = null;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        check(TOK.this_);
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);

        stc = parsePostfix(stc, &udas);
        if (STC ss = stc & (STC.shared_ | STC.static_))
        {
            if (ss == STC.static_)
                error(loc, "use `static ~this()` to declare a static destructor");
            else if (ss == (STC.shared_ | STC.static_))
                error(loc, "use `shared static ~this()` to declare a shared static destructor");
        }

        auto f = new AST.DtorDeclaration(loc, Loc.initial, stc, Id.dtor);
        AST.Dsymbol s = parseContracts(f);
        if (udas)
        {
            auto a = new AST.Dsymbols();
            a.push(f);
            s = new AST.UserAttributeDeclaration(udas, a);
        }
        return s;
    }

    /*****************************************
     * Parse a static constructor definition:
     *      static this() { body }
     * Current token is 'static'.
     */
    private AST.Dsymbol parseStaticCtor(PrefixAttributes!AST* pAttrs)
    {
        //Expressions *udas = NULL;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        nextToken();
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);

        stc = parsePostfix(stc & ~STC.TYPECTOR, null) | stc;
        if (stc & STC.shared_)
            error(loc, "use `shared static this()` to declare a shared static constructor");
        else if (stc & STC.static_)
            appendStorageClass(stc, STC.static_); // complaint for the redundancy
        else if (STC modStc = stc & STC.TYPECTOR)
        {
            OutBuffer buf;
            AST.stcToBuffer(buf, modStc);
            error(loc, "static constructor cannot be `%s`", buf.peekChars());
        }
        stc &= ~(STC.static_ | STC.TYPECTOR);

        auto f = new AST.StaticCtorDeclaration(loc, Loc.initial, stc);
        AST.Dsymbol s = parseContracts(f);
        return s;
    }

    /*****************************************
     * Parse a static destructor definition:
     *      static ~this() { body }
     * Current token is 'static'.
     */
    private AST.Dsymbol parseStaticDtor(PrefixAttributes!AST* pAttrs)
    {
        AST.Expressions* udas = null;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        nextToken();
        check(TOK.this_);
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);

        stc = parsePostfix(stc & ~STC.TYPECTOR, &udas) | stc;
        if (stc & STC.shared_)
            error(loc, "use `shared static ~this()` to declare a shared static destructor");
        else if (stc & STC.static_)
            appendStorageClass(stc, STC.static_); // complaint for the redundancy
        else if (STC modStc = stc & STC.TYPECTOR)
        {
            OutBuffer buf;
            AST.stcToBuffer(buf, modStc);
            error(loc, "static destructor cannot be `%s`", buf.peekChars());
        }
        stc &= ~(STC.static_ | STC.TYPECTOR);

        auto f = new AST.StaticDtorDeclaration(loc, Loc.initial, stc);
        AST.Dsymbol s = parseContracts(f);
        if (udas)
        {
            auto a = new AST.Dsymbols();
            a.push(f);
            s = new AST.UserAttributeDeclaration(udas, a);
        }
        return s;
    }

    /*****************************************
     * Parse a shared static constructor definition:
     *      shared static this() { body }
     * Current token is 'shared'.
     */
    private AST.Dsymbol parseSharedStaticCtor(PrefixAttributes!AST* pAttrs)
    {
        //Expressions *udas = NULL;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        nextToken();
        nextToken();
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);

        stc = parsePostfix(stc & ~STC.TYPECTOR, null) | stc;
        if (STC ss = stc & (STC.shared_ | STC.static_))
            appendStorageClass(stc, ss); // complaint for the redundancy
        else if (STC modStc = stc & STC.TYPECTOR)
        {
            OutBuffer buf;
            AST.stcToBuffer(buf, modStc);
            error(loc, "shared static constructor cannot be `%s`", buf.peekChars());
        }
        stc &= ~(STC.static_ | STC.TYPECTOR);

        auto f = new AST.SharedStaticCtorDeclaration(loc, Loc.initial, stc);
        AST.Dsymbol s = parseContracts(f);
        return s;
    }

    /*****************************************
     * Parse a shared static destructor definition:
     *      shared static ~this() { body }
     * Current token is 'shared'.
     */
    private AST.Dsymbol parseSharedStaticDtor(PrefixAttributes!AST* pAttrs)
    {
        AST.Expressions* udas = null;
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        nextToken();
        nextToken();
        check(TOK.this_);
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);

        stc = parsePostfix(stc & ~STC.TYPECTOR, &udas) | stc;
        if (STC ss = stc & (STC.shared_ | STC.static_))
            appendStorageClass(stc, ss); // complaint for the redundancy
        else if (STC modStc = stc & STC.TYPECTOR)
        {
            OutBuffer buf;
            AST.stcToBuffer(buf, modStc);
            error(loc, "shared static destructor cannot be `%s`", buf.peekChars());
        }
        stc &= ~(STC.static_ | STC.TYPECTOR);

        auto f = new AST.SharedStaticDtorDeclaration(loc, Loc.initial, stc);
        AST.Dsymbol s = parseContracts(f);
        if (udas)
        {
            auto a = new AST.Dsymbols();
            a.push(f);
            s = new AST.UserAttributeDeclaration(udas, a);
        }
        return s;
    }

    /*****************************************
     * Parse an invariant definition:
     *      invariant { statements... }
     *      invariant() { statements... }
     *      invariant (expression);
     * Current token is 'invariant'.
     */
    private AST.Dsymbol parseInvariant(PrefixAttributes!AST* pAttrs)
    {
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();
        if (token.value == TOK.leftParenthesis) // optional () or invariant (expression);
        {
            nextToken();
            if (token.value != TOK.rightParenthesis) // invariant (expression);
            {
                AST.Expression e = parseAssignExp(), msg = null;
                if (token.value == TOK.comma)
                {
                    nextToken();
                    if (token.value != TOK.rightParenthesis)
                    {
                        msg = parseAssignExp();
                        if (token.value == TOK.comma)
                            nextToken();
                    }
                }
                check(TOK.rightParenthesis);
                check(TOK.semicolon, "invariant");
                e = new AST.AssertExp(loc, e, msg);
                auto fbody = new AST.ExpStatement(loc, e);
                auto f = new AST.InvariantDeclaration(loc, token.loc, stc, null, fbody);
                return f;
            }
            nextToken();
        }

        auto fbody = parseStatement(ParseStatementFlags.curly);
        auto f = new AST.InvariantDeclaration(loc, token.loc, stc, null, fbody);
        return f;
    }

    /*****************************************
     * Parse a unittest definition:
     *      unittest { body }
     * Current token is 'unittest'.
     */
    private AST.Dsymbol parseUnitTest(PrefixAttributes!AST* pAttrs)
    {
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);

        nextToken();

        const(char)* begPtr = token.ptr + 1; // skip left curly brace
        const(char)* endPtr = null;
        AST.Statement sbody = parseStatement(ParseStatementFlags.curly, &endPtr);

        /** Extract unittest body as a string. Must be done eagerly since memory
         will be released by the lexer before doc gen. */
        char* docline = null;
        if (compileEnv.ddocOutput && endPtr > begPtr)
        {
            /* Remove trailing whitespaces */
            for (const(char)* p = endPtr - 1; begPtr <= p && (*p == ' ' || *p == '\r' || *p == '\n' || *p == '\t'); --p)
            {
                endPtr = p;
            }

            size_t len = endPtr - begPtr;
            if (len > 0)
            {
                docline = cast(char*)mem.xmalloc_noscan(len + 2);
                memcpy(docline, begPtr, len);
                docline[len] = '\n'; // Terminate all lines by LF
                docline[len + 1] = '\0';
            }
        }

        auto f = new AST.UnitTestDeclaration(loc, token.loc, stc, docline);
        f.fbody = sbody;
        return f;
    }

    /*****************************************
     * Parse a new definition:
     *      @disable new();
     * Current token is 'new'.
     */
    private AST.Dsymbol parseNewDeclaration(PrefixAttributes!AST* pAttrs)
    {
        const loc = token.loc;
        STC stc = getStorageClass!AST(pAttrs);
        if (!(stc & STC.disable))
        {
            error("`new` allocator must be annotated with `@disabled`");
        }
        nextToken();
        check(TOK.leftParenthesis);
        check(TOK.rightParenthesis);
        check(TOK.semicolon);
        return new AST.NewDeclaration(loc, stc);
    }

    /**********************************************
     * Parse parameter list.
     */
    private AST.ParameterList parseParameterList(AST.TemplateParameters** tpl)
    {
        auto parameters = new AST.Parameters();
        VarArg varargs = VarArg.none;
        STC varargsStc;

        // Attributes allowed for ...
        enum VarArgsStc = STC.const_ | STC.immutable_ | STC.shared_ | STC.scope_ | STC.return_ | STC.returnScope;

        check(TOK.leftParenthesis);
        while (1)
        {
            Identifier ai = null;
            AST.Type at;
            STC storageClass = STC.none;
            STC stc;
            AST.Expression ae;
            AST.Expressions* udas = null;
            for (; 1; nextToken())
            {
            L3:
                switch (token.value)
                {
                case TOK.rightParenthesis:
                    if (storageClass != 0 || udas !is null)
                        error("basic type expected, not `)`");
                    break;

                case TOK.dotDotDot:
                    varargs = VarArg.variadic;
                    varargsStc = storageClass;
                    if (varargsStc & ~VarArgsStc)
                    {
                        OutBuffer buf;
                        AST.stcToBuffer(buf, varargsStc & ~VarArgsStc);
                        error("variadic parameter cannot have attributes `%s`", buf.peekChars());
                        varargsStc &= VarArgsStc;
                    }
                    nextToken();
                    break;

                case TOK.const_:
                    if (peekNext() == TOK.leftParenthesis)
                        goto default;
                    stc = STC.const_;
                    goto L2;

                case TOK.immutable_:
                    if (peekNext() == TOK.leftParenthesis)
                        goto default;
                    stc = STC.immutable_;
                    goto L2;

                case TOK.shared_:
                    if (peekNext() == TOK.leftParenthesis)
                        goto default;
                    stc = STC.shared_;
                    goto L2;

                case TOK.inout_:
                    if (peekNext() == TOK.leftParenthesis)
                        goto default;
                    stc = STC.wild;
                    goto L2;
                case TOK.at:
                    {
                        AST.Expressions* exps = null;
                        STC stc2 = parseAttribute(exps);
                        if (stc2 & atAttrGroup)
                        {
                            error("`@%s` attribute for function parameter is not supported", token.toChars());
                        }
                        else
                        {
                            udas = AST.UserAttributeDeclaration.concat(udas, exps);
                        }
                        if (token.value == TOK.dotDotDot)
                            error("variadic parameter cannot have user-defined attributes");
                        if (stc2)
                            nextToken();
                        goto L3;
                        // Don't call nextToken again.
                    }
                case TOK.in_:
                    if (compileEnv.transitionIn)
                        eSink.message(scanloc, "Usage of 'in' on parameter");
                    stc = STC.in_;
                    if (compileEnv.previewIn)
                        stc |= STC.constscoperef;
                    goto L2;

                case TOK.out_:
                    stc = STC.out_;
                    goto L2;

                case TOK.ref_:
                    stc = STC.ref_;
                    goto L2;

                case TOK.lazy_:
                    stc = STC.lazy_;
                    goto L2;

                case TOK.scope_:
                    stc = STC.scope_;
                    goto L2;

                case TOK.final_:
                    stc = STC.final_;
                    goto L2;

                case TOK.auto_:
                    stc = STC.auto_;
                    if (peekNext() == TOK.ref_)
                        stc |= STC.autoref;
                    goto L2;

                case TOK.return_:
                    stc = STC.return_;
                    if (peekNext() == TOK.scope_)
                        stc |= STC.returnScope;
                    goto L2;
                L2:
                    storageClass = appendStorageClass(storageClass, stc);
                    continue;

                default:
                    {
                        const stcx = storageClass & (STC.in_ | STC.ref_ | STC.out_ | STC.lazy_);
                        // if stcx is not a power of 2
                        if (stcx & (stcx - 1) && !(stcx == (STC.in_ | STC.ref_)))
                            error("incompatible parameter storage classes");

                        // Deprecated in 2.111
                        if ((storageClass & STC.auto_) && (storageClass & STC.ref_) && !(storageClass & STC.autoref))
                            deprecation("`auto` and `ref` storage classes should be adjacent");

                        const tv = peekNext();
                        Loc loc;
                        if (tpl && token.value == TOK.identifier &&
                            (tv == TOK.comma || tv == TOK.rightParenthesis || tv == TOK.dotDotDot))
                        {
                            Identifier id = Identifier.generateId("__T");
                            loc = token.loc;
                            at = new AST.TypeIdentifier(loc, id);
                            if (!*tpl)
                                *tpl = new AST.TemplateParameters();
                            AST.TemplateParameter tp = new AST.TemplateTypeParameter(loc, id, null, null);
                            (*tpl).push(tp);

                            ai = token.ident;
                            nextToken();
                        }
                        else
                        {
                            at = parseType(&ai, null, &loc);
                        }
                        ae = null;
                        if (token.value == TOK.assign) // = defaultArg
                        {
                            nextToken();
                            ae = parseAssignExp();
                        }
                        auto param = new AST.Parameter(loc, storageClass | STC.parameter, at, ai, ae, null);
                        if (udas)
                        {
                            auto a = new AST.Dsymbols();
                            auto udad = new AST.UserAttributeDeclaration(udas, a);
                            param.userAttribDecl = udad;
                        }
                        if (token.value == TOK.at)
                        {
                            AST.Expressions* exps = null;
                            STC stc2 = parseAttribute(exps);
                            if (stc2 & atAttrGroup)
                            {
                                error("`@%s` attribute for function parameter is not supported", token.toChars());
                            }
                            else
                            {
                                error("user-defined attributes cannot appear as postfixes", token.toChars());
                            }
                            if (stc2)
                                nextToken();
                        }
                        if (token.value == TOK.dotDotDot)
                        {
                            /* This is:
                             *      at ai ...
                             */
                            if (storageClass & (STC.out_ | STC.ref_))
                                error("variadic argument cannot be `out` or `ref`");
                            varargs = VarArg.typesafe;
                            parameters.push(param);
                            nextToken();
                            break;
                        }
                        parameters.push(param);
                        if (token.value == TOK.comma)
                        {
                            nextToken();
                            goto L1;
                        }
                        break;
                    }
                }
                break;
            }
            break;

        L1:
        }
        check(TOK.rightParenthesis);
        return AST.ParameterList(parameters, varargs, varargsStc);
    }

    /*************************************
     */
    private AST.EnumDeclaration parseEnum()
    {
        AST.EnumDeclaration e;
        Identifier id;
        AST.Type memtype;
        auto loc = token.loc;

        // printf("Parser::parseEnum()\n");
        nextToken();
        id = null;
        if (token.value == TOK.identifier)
        {
            id = token.ident;
            nextToken();
        }

        memtype = null;
        if (token.value == TOK.colon)
        {
            nextToken();
            int alt = 0;
            const typeLoc = token.loc;
            memtype = parseBasicType();
            memtype = parseDeclarator(memtype, alt, null);
            checkCstyleTypeSyntax(typeLoc, memtype, alt, null);
        }

        e = new AST.EnumDeclaration(loc, id, memtype);
        // opaque type
        if (token.value == TOK.semicolon && id)
            nextToken();
        else if (token.value == TOK.leftCurly)
        {
            bool isAnonymousEnum = !id;

            //printf("enum definition\n");
            e.members = new AST.Dsymbols();
            nextToken();
            const(char)[] comment = token.blockComment;
            while (token.value != TOK.rightCurly)
            {
                /* Can take the following forms...
                 *  1. ident
                 *  2. ident = value
                 *  3. type ident = value
                 *  ... prefixed by valid attributes
                 */
                loc = token.loc;

                AST.Type type = null;
                Identifier ident = null;

                AST.Expressions* udas;
                STC stc;
                AST.Expression deprecationMessage;
                enum attributeErrorMessage = "`%s` is not a valid attribute for enum members";
            Lattrs:
                while (1)
                {
                    switch (token.value)
                    {
                        case TOK.at:
                            if (STC _stc = parseAttribute(udas))
                            {
                                if (_stc == STC.disable)
                                    stc |= _stc;
                                else
                                {
                                    OutBuffer buf;
                                    AST.stcToBuffer(buf, _stc);
                                    error(attributeErrorMessage, buf.peekChars());
                                }
                                nextToken();
                            }
                            break;
                        case TOK.deprecated_:
                            stc |= STC.deprecated_;
                            if (!parseDeprecatedAttribute(deprecationMessage))
                            {
                                nextToken();
                            }
                            break;
                        default:
                            break Lattrs;
                    }
                }
                if (token.value == TOK.identifier)
                {
                    const tv = peekNext();
                    if (tv == TOK.assign || tv == TOK.comma || tv == TOK.rightCurly)
                    {
                        ident = token.ident;
                        type = null;
                        nextToken();
                    }
                    else
                    {
                        if (isAnonymousEnum)
                            goto Ltype;

                        nextToken();
                        error("expected `,` or `=` after identifier, not `%s`", token.toChars());
                    }
                }
                else
                {
                    if (isAnonymousEnum)
                    {
                    Ltype:
                        // Type identifier
                        type = parseType(&ident, null);
                        if (type == AST.Type.terror)
                        {
                            type = null;
                            nextToken();
                        }
                        else if (!ident)
                        {
                            error("no identifier for declarator `%s`", type.toChars());
                            type = null;
                        }
                        else
                        {
                            const tv = token.value;
                            if (tv != TOK.assign && tv != TOK.comma && tv != TOK.rightCurly)
                            {
                                error("expected `,` or `=` after identifier, not `%s`", token.toChars());
                                nextToken();
                            }
                        }
                    }
                    else
                    {
                        Token* t = &token;
                        if (isBasicType(&t))
                        {
                            error("named enum cannot declare member with type", (*t).toChars());
                            nextToken();
                        }
                        else
                            check(TOK.identifier);

                        // avoid extra error messages
                        const tv = token.value;
                        if (tv != TOK.assign && tv != TOK.comma && tv != TOK.rightCurly && tv != TOK.endOfFile)
                            continue;
                    }
                }

                AST.Expression value;
                if (token.value == TOK.assign)
                {
                    nextToken();
                    value = parseAssignExp();
                }
                else
                {
                    value = null;
                    if (type && isAnonymousEnum)
                        error("initializer required after `%s` when type is specified", ident.toChars());
                }

                AST.DeprecatedDeclaration dd;
                if (deprecationMessage)
                {
                    dd = new AST.DeprecatedDeclaration(deprecationMessage, null);
                    stc |= STC.deprecated_;
                }

                auto em = new AST.EnumMember(loc, ident, value, type, stc, null, dd);
                e.members.push(em);

                if (udas)
                {
                    auto uad = new AST.UserAttributeDeclaration(udas, new AST.Dsymbols());
                    em.userAttribDecl = uad;
                }

                if (token.value != TOK.rightCurly)
                {
                    addComment(em, comment);
                    comment = null;
                    check(TOK.comma);
                }
                addComment(em, comment);
                comment = token.blockComment;

                if (token.value == TOK.endOfFile)
                {
                    error("premature end of file");
                    break;
                }
            }
            nextToken();
        }
        else
        {
            nextToken();
            error("expected `{`, not `%s` for enum declaration", token.toChars());
        }
        //printf("-parseEnum() %s\n", e.toChars());
        return e;
    }

    /********************************
     * Parse struct, union, interface, class.
     */
    private AST.Dsymbol parseAggregate()
    {
        AST.TemplateParameters* tpl = null;
        AST.Expression constraint;
        const loc = token.loc;
        TOK tok = token.value;

        //printf("Parser::parseAggregate()\n");
        nextToken();
        Identifier id;
        if (token.value != TOK.identifier)
        {
            id = null;
        }
        else
        {
            id = token.ident;
            nextToken();

            if (token.value == TOK.leftParenthesis)
            {
                // struct/class template declaration.
                tpl = parseTemplateParameterList();
                constraint = parseConstraint();
            }
        }

        // Collect base class(es)
        AST.BaseClasses* baseclasses = null;
        if (token.value == TOK.colon)
        {
            if (tok != TOK.interface_ && tok != TOK.class_)
                error("base classes are not allowed for `%s`, did you mean `;`?", Token.toChars(tok));
            nextToken();
            baseclasses = parseBaseClasses();
        }

        if (token.value == TOK.if_)
        {
            if (constraint)
                error("template constraints appear both before and after BaseClassList, put them before");
            constraint = parseConstraint();
        }
        if (constraint)
        {
            if (!id)
                error("template constraints not allowed for anonymous `%s`", Token.toChars(tok));
            if (!tpl)
                error("template constraints only allowed for templates");
        }

        AST.Dsymbols* members = null;
        if (token.value == TOK.leftCurly)
        {
            //printf("aggregate definition\n");
            const lookingForElseSave = lookingForElse;
            lookingForElse = Loc();
            nextToken();
            members = parseDeclDefs(0);
            lookingForElse = lookingForElseSave;
            if (token.value != TOK.rightCurly)
            {
                /* { */
                error(token.loc, "`}` expected following members in `%s` declaration",
                    Token.toChars(tok));
                if (id)
                    eSink.errorSupplemental(loc, "%s `%s` starts here",
                        Token.toChars(tok), id.toChars());
                else
                    eSink.errorSupplemental(loc, "%s starts here",
                        Token.toChars(tok));
            }
            nextToken();
        }
        else if (token.value == TOK.semicolon && id)
        {
            if (baseclasses || constraint)
                error("members expected");
            nextToken();
        }
        else
        {
            error(token.loc, "{ } expected following `%s` declaration", Token.toChars(tok));
        }

        AST.AggregateDeclaration a;
        switch (tok)
        {
        case TOK.interface_:
            if (!id)
                error(loc, "anonymous interfaces not allowed");
            a = new AST.InterfaceDeclaration(loc, id, baseclasses);
            a.members = members;
            break;

        case TOK.class_:
            if (!id)
                error(loc, "anonymous classes not allowed");
            bool inObject = md && !md.packages && md.id == Id.object;
            a = new AST.ClassDeclaration(loc, id, baseclasses, members, inObject);
            break;

        case TOK.struct_:
            if (id)
            {
                bool inObject = md && !md.packages && md.id == Id.object;
                a = new AST.StructDeclaration(loc, id, inObject);
                a.members = members;
            }
            else
            {
                /* Anonymous structs/unions are more like attributes.
                 */
                assert(!tpl);
                return new AST.AnonDeclaration(loc, false, members);
            }
            break;

        case TOK.union_:
            if (id)
            {
                a = new AST.UnionDeclaration(loc, id);
                a.members = members;
            }
            else
            {
                /* Anonymous structs/unions are more like attributes.
                 */
                assert(!tpl);
                return new AST.AnonDeclaration(loc, true, members);
            }
            break;

        default:
            assert(0);
        }

        if (tpl)
        {
            // Wrap a template around the aggregate declaration
            auto decldefs = new AST.Dsymbols();
            decldefs.push(a);
            auto tempdecl = new AST.TemplateDeclaration(loc, id, tpl, constraint, decldefs);
            return tempdecl;
        }
        return a;
    }

    /*******************************************
     */
    private AST.BaseClasses* parseBaseClasses()
    {
        auto baseclasses = new AST.BaseClasses();

        for (; 1; nextToken())
        {
            auto b = new AST.BaseClass(parseBasicType());
            baseclasses.push(b);
            if (token.value != TOK.comma)
                break;
        }
        return baseclasses;
    }

    AST.Dsymbols* parseImport()
    {
        auto decldefs = new AST.Dsymbols();
        Identifier aliasid = null;

        int isstatic = token.value == TOK.static_;
        if (isstatic)
            nextToken();

        //printf("Parser::parseImport()\n");
        do
        {
        L1:
            nextToken();
            if (token.value != TOK.identifier)
            {
                error("identifier expected following `import`");
                break;
            }

            const loc = token.loc;
            Identifier id = token.ident;
            Identifier[] a;
            nextToken();
            if (!aliasid && token.value == TOK.assign)
            {
                aliasid = id;
                goto L1;
            }
            while (token.value == TOK.dot)
            {
                a ~= id;
                nextToken();
                if (token.value != TOK.identifier)
                {
                    error("identifier expected following `package`");
                    break;
                }
                id = token.ident;
                nextToken();
            }

            auto s = new AST.Import(loc, a, id, aliasid, isstatic);
            decldefs.push(s);

            /* Look for
             *      : alias=name, alias=name;
             * syntax.
             */
            if (token.value == TOK.colon)
            {
                do
                {
                    nextToken();
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected following `:`");
                        break;
                    }
                    Identifier _alias = token.ident;
                    Identifier name;
                    nextToken();
                    if (token.value == TOK.assign)
                    {
                        nextToken();
                        if (token.value != TOK.identifier)
                        {
                            error("identifier expected following `%s=`", _alias.toChars());
                            break;
                        }
                        name = token.ident;
                        nextToken();
                    }
                    else
                    {
                        name = _alias;
                        _alias = null;
                    }
                    if (s.isstatic)
                        error(loc, "static import `%s` cannot have an import bind list", s.toPrettyChars());
                    if (!s.aliasId)
                        s.ident = null; // make it an anonymous import
                    s.names.push(name);
                    s.aliases.push(_alias);
                }
                while (token.value == TOK.comma);
                break; // no comma-separated imports of this form
            }
            aliasid = null;
        }
        while (token.value == TOK.comma);

        if (token.value == TOK.semicolon)
            nextToken();
        else
        {
            error("`;` expected");
            nextToken();
        }

        return decldefs;
    }

    /* Parse a type and optional identifier
     * Params:
     *  pident       = set to Identifier if there is one, null if not
     *  ptpl         = if !null, then set to TemplateParameterList
     *  pdeclLoc     = if !null, then set to location of the declarator
     */
    AST.Type parseType(Identifier* pident = null, AST.TemplateParameters** ptpl = null, Loc* pdeclLoc = null)
    {
        /* Take care of the storage class prefixes that
         * serve as type attributes:
         *               const type
         *           immutable type
         *              shared type
         *               inout type
         *         inout const type
         *        shared const type
         *        shared inout type
         *  shared inout const type
         */
        STC stc = STC.none;
        while (1)
        {
            switch (token.value)
            {
            case TOK.const_:
                if (peekNext() == TOK.leftParenthesis)
                    break; // const as type constructor
                stc |= STC.const_; // const as storage class
                nextToken();
                continue;

            case TOK.immutable_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc |= STC.immutable_;
                nextToken();
                continue;

            case TOK.shared_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc |= STC.shared_;
                nextToken();
                continue;

            case TOK.inout_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc |= STC.wild;
                nextToken();
                continue;

            default:
                break;
            }
            break;
        }

        const typeLoc = token.loc;

        AST.Type t;
        t = parseBasicType();

        if (pdeclLoc)
            *pdeclLoc = token.loc;
        int alt = 0;
        t = parseDeclarator(t, alt, pident, ptpl);
        checkCstyleTypeSyntax(typeLoc, t, alt, pident ? *pident : null);

        t = t.addSTC(stc);
        return t;
    }

    private AST.Type parseBasicType(bool dontLookDotIdents = false)
    {
        AST.Type t;
        Loc loc;
        Identifier id;
        //printf("parseBasicType()\n");
        switch (token.value)
        {
        case TOK.void_:
            t = AST.Type.tvoid;
            goto LabelX;

        case TOK.int8:
            t = AST.Type.tint8;
            goto LabelX;

        case TOK.uns8:
            t = AST.Type.tuns8;
            goto LabelX;

        case TOK.int16:
            t = AST.Type.tint16;
            goto LabelX;

        case TOK.uns16:
            t = AST.Type.tuns16;
            goto LabelX;

        case TOK.int32:
            t = AST.Type.tint32;
            goto LabelX;

        case TOK.uns32:
            t = AST.Type.tuns32;
            goto LabelX;

        case TOK.int64:
            t = AST.Type.tint64;
            nextToken();
            if (token.value == TOK.int64)   // if `long long`
            {
                error("use `long` for a 64 bit integer instead of `long long`");
                nextToken();
            }
            else if (token.value == TOK.float64)   // if `long double`
            {
                error("use `real` instead of `long double`");
                t = AST.Type.tfloat80;
                nextToken();
            }
            break;

        case TOK.uns64:
            t = AST.Type.tuns64;
            goto LabelX;

        case TOK.int128:
            t = AST.Type.tint128;
            goto LabelX;

        case TOK.uns128:
            t = AST.Type.tuns128;
            goto LabelX;

        case TOK.float32:
            t = AST.Type.tfloat32;
            goto LabelX;

        case TOK.float64:
            t = AST.Type.tfloat64;
            goto LabelX;

        case TOK.float80:
            t = AST.Type.tfloat80;
            goto LabelX;

        case TOK.imaginary32:
            t = AST.Type.timaginary32;
            goto LabelX;

        case TOK.imaginary64:
            t = AST.Type.timaginary64;
            goto LabelX;

        case TOK.imaginary80:
            t = AST.Type.timaginary80;
            goto LabelX;

        case TOK.complex32:
            t = AST.Type.tcomplex32;
            goto LabelX;

        case TOK.complex64:
            t = AST.Type.tcomplex64;
            goto LabelX;

        case TOK.complex80:
            t = AST.Type.tcomplex80;
            goto LabelX;

        case TOK.bool_:
            t = AST.Type.tbool;
            goto LabelX;

        case TOK.char_:
            t = AST.Type.tchar;
            goto LabelX;

        case TOK.wchar_:
            t = AST.Type.twchar;
            goto LabelX;

        case TOK.dchar_:
            t = AST.Type.tdchar;
            goto LabelX;
        LabelX:
            nextToken();
            break;

        case TOK.this_:
        case TOK.super_:
        case TOK.identifier:
            loc = token.loc;
            id = token.ident;
            nextToken();
            if (token.value == TOK.not)
            {
                // ident!(template_arguments)
                auto tempinst = new AST.TemplateInstance(loc, id, parseTemplateArguments());
                t = parseBasicTypeStartingAt(new AST.TypeInstance(loc, tempinst), dontLookDotIdents);
            }
            else
            {
                t = parseBasicTypeStartingAt(new AST.TypeIdentifier(loc, id), dontLookDotIdents);
            }
            break;

        case TOK.mixin_:
            // https://dlang.org/spec/expression.html#mixin_types
            loc = token.loc;
            nextToken();
            if (token.value != TOK.leftParenthesis)
                error(token.loc, "found `%s` when expecting `%s` following `mixin`", token.toChars(), Token.toChars(TOK.leftParenthesis));
            auto exps = parseArguments();
            t = new AST.TypeMixin(loc, exps);
            break;

        case TOK.dot:
            // Leading . as in .foo
            t = parseBasicTypeStartingAt(new AST.TypeIdentifier(token.loc, Id.empty), dontLookDotIdents);
            break;

        case TOK.typeof_:
            // typeof(expression)
            t = parseBasicTypeStartingAt(parseTypeof(), dontLookDotIdents);
            break;

        case TOK.vector:
            t = parseVector();
            break;

        case TOK.traits:
            if (AST.TraitsExp te = cast(AST.TraitsExp) parsePrimaryExp())
                if (te.ident)
                {
                    t = new AST.TypeTraits(token.loc, te);
                    break;
                }
            t = new AST.TypeError;
            break;

        case TOK.const_:
            // const(type)
            nextToken();
            check(TOK.leftParenthesis);
            t = parseType().addSTC(STC.const_);
            check(TOK.rightParenthesis);
            break;

        case TOK.immutable_:
            // immutable(type)
            nextToken();
            check(TOK.leftParenthesis);
            t = parseType().addSTC(STC.immutable_);
            check(TOK.rightParenthesis);
            break;

        case TOK.shared_:
            // shared(type)
            nextToken();
            check(TOK.leftParenthesis);
            t = parseType().addSTC(STC.shared_);
            check(TOK.rightParenthesis);
            break;

        case TOK.inout_:
            // wild(type)
            nextToken();
            check(TOK.leftParenthesis);
            t = parseType().addSTC(STC.wild);
            check(TOK.rightParenthesis);
            break;

        default:
            error("basic type expected, not `%s`", token.toChars());
            if (token.value == TOK.else_)
                eSink.errorSupplemental(token.loc, "There's no `static else`, use `else` instead.");
            t = AST.Type.terror;
            break;
        }
        return t;
    }

    private AST.Type parseBasicTypeStartingAt(AST.TypeQualified tid, bool dontLookDotIdents)
    {
        AST.Type maybeArray = null;
        // See https://issues.dlang.org/show_bug.cgi?id=1215
        // A basic type can look like MyType (typical case), but also:
        //  MyType.T -> A type
        //  MyType[expr] -> Either a static array of MyType or a type (iif MyType is a Ttuple)
        //  MyType[expr].T -> A type.
        //  MyType[expr].T[expr] ->  Either a static array of MyType[expr].T or a type
        //                           (iif MyType[expr].T is a Ttuple)
        while (1)
        {
            switch (token.value)
            {
            case TOK.dot:
                {
                    nextToken();
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected following `.` instead of `%s`", token.toChars());
                        break;
                    }
                    if (maybeArray)
                    {
                        // This is actually a TypeTuple index, not an {a/s}array.
                        // We need to have a while loop to unwind all index taking:
                        // T[e1][e2].U   ->  T, addIndex(e1), addIndex(e2)
                        AST.Objects dimStack;
                        AST.Type t = maybeArray;
                        while (true)
                        {
                            if (t.ty == Tsarray)
                            {
                                // The index expression is an Expression.
                                AST.TypeSArray a = cast(AST.TypeSArray)t;
                                dimStack.push(a.dim.syntaxCopy());
                                t = a.next.syntaxCopy();
                            }
                            else if (t.ty == Taarray)
                            {
                                // The index expression is a Type. It will be interpreted as an expression at semantic time.
                                AST.TypeAArray a = cast(AST.TypeAArray)t;
                                dimStack.push(a.index.syntaxCopy());
                                t = a.next.syntaxCopy();
                            }
                            else
                            {
                                break;
                            }
                        }
                        assert(dimStack.length > 0);
                        // We're good. Replay indices in the reverse order.
                        tid = cast(AST.TypeQualified)t;
                        while (dimStack.length)
                        {
                            tid.addIndex(dimStack.pop());
                        }
                        maybeArray = null;
                    }
                    const loc = token.loc;
                    Identifier id = token.ident;
                    nextToken();
                    if (token.value == TOK.not)
                    {
                        auto tempinst = new AST.TemplateInstance(loc, id, parseTemplateArguments());
                        tid.addInst(tempinst);
                    }
                    else
                        tid.addIdent(id);
                    continue;
                }
            case TOK.leftBracket:
                {
                    if (dontLookDotIdents) // workaround for https://issues.dlang.org/show_bug.cgi?id=14911
                        goto Lend;

                    nextToken();
                    AST.Type t = maybeArray ? maybeArray : cast(AST.Type)tid;
                    if (token.value == TOK.rightBracket)
                    {
                        // It's a dynamic array, and we're done:
                        // T[].U does not make sense.
                        t = new AST.TypeDArray(t);
                        nextToken();
                        return t;
                    }
                    else if (isDeclaration(&token, NeedDeclaratorId.no, TOK.rightBracket, null))
                    {
                        // This can be one of two things:
                        //  1 - an associative array declaration, T[type]
                        //  2 - an associative array declaration, T[expr]
                        // These  can only be disambiguated later.
                        AST.Type index = parseType(); // [ type ]
                        maybeArray = new AST.TypeAArray(t, index);
                        check(TOK.rightBracket);
                    }
                    else
                    {
                        // This can be one of three things:
                        //  1 - an static array declaration, T[expr]
                        //  2 - a slice, T[expr .. expr]
                        //  3 - a template parameter pack index expression, T[expr].U
                        // 1 and 3 can only be disambiguated later.
                        //printf("it's type[expression]\n");
                        inBrackets++;
                        AST.Expression e = parseAssignExp(); // [ expression ]
                        if (token.value == TOK.slice)
                        {
                            // It's a slice, and we're done.
                            nextToken();
                            AST.Expression e2 = parseAssignExp(); // [ exp .. exp ]
                            t = new AST.TypeSlice(t, e, e2);
                            inBrackets--;
                            check(TOK.rightBracket);
                            return t;
                        }
                        else
                        {
                            maybeArray = new AST.TypeSArray(t, e);
                            inBrackets--;
                            check(TOK.rightBracket);
                            continue;
                        }
                    }
                    break;
                }
            default:
                goto Lend;
            }
        }
    Lend:
        return maybeArray ? maybeArray : cast(AST.Type)tid;
    }

    /******************************************
     * Parse suffixes to type t.
     *      *
     *      []
     *      [AssignExpression]
     *      [AssignExpression .. AssignExpression]
     *      [Type]
     *      delegate Parameters MemberFunctionAttributes(opt)
     *      function Parameters FunctionAttributes(opt)
     * Params:
     *      t = the already parsed type
     * Returns:
     *      t with the suffixes added
     * See_Also:
     *      https://dlang.org/spec/declaration.html#TypeSuffixes
     */
    private AST.Type parseTypeSuffixes(AST.Type t)
    {
        //printf("parseTypeSuffixes()\n");
        while (1)
        {
            switch (token.value)
            {
            case TOK.mul:
                t = new AST.TypePointer(t);
                nextToken();
                continue;

            case TOK.leftBracket:
                // Handle []. Make sure things like
                //     int[3][1] a;
                // is (array[1] of array[3] of int)
                nextToken();
                if (token.value == TOK.rightBracket)
                {
                    t = new AST.TypeDArray(t); // []
                    nextToken();
                }
                else if (isDeclaration(&token, NeedDeclaratorId.no, TOK.rightBracket, null))
                {
                    // It's an associative array declaration
                    //printf("it's an associative array\n");
                    AST.Type index = parseType(); // [ type ]
                    t = new AST.TypeAArray(t, index);
                    check(TOK.rightBracket);
                }
                else
                {
                    //printf("it's type[expression]\n");
                    inBrackets++;
                    AST.Expression e = parseAssignExp(); // [ expression ]
                    if (!e)
                    {
                        inBrackets--;
                        check(TOK.rightBracket);
                        continue;
                    }
                    if (token.value == TOK.slice)
                    {
                        nextToken();
                        AST.Expression e2 = parseAssignExp(); // [ exp .. exp ]
                        t = new AST.TypeSlice(t, e, e2);
                    }
                    else
                    {
                        t = new AST.TypeSArray(t, e);
                    }
                    inBrackets--;
                    check(TOK.rightBracket);
                }
                continue;

            case TOK.delegate_:
            case TOK.function_:
                {
                    // Handle delegate declaration:
                    //      t delegate(parameter list) nothrow pure
                    //      t function(parameter list) nothrow pure
                    const save = token.value;
                    nextToken();

                    auto parameterList = parseParameterList(null);

                    STC stc = parsePostfix(STC.none, null);
                    auto tf = new AST.TypeFunction(parameterList, t, linkage, stc);
                    if (stc & (STC.const_ | STC.immutable_ | STC.shared_ | STC.wild | STC.return_))
                    {
                        if (save == TOK.function_)
                            error("`const`/`immutable`/`shared`/`inout`/`return` attributes are only valid for non-static member functions");
                        else
                            tf = cast(AST.TypeFunction)tf.addSTC(stc);
                    }
                    t = save == TOK.delegate_ ? new AST.TypeDelegate(tf) : new AST.TypePointer(tf); // pointer to function
                    continue;
                }
            default:
                return t;
            }
            assert(0);
        }
        assert(0);
    }

    /**********************
     * Parse Declarator
     * Params:
     *  t            = base type to start with
     *  palt         = OR in 1 for C-style function pointer declaration syntax,
     *                 2 for C-style array declaration syntax, otherwise don't modify
     *  pident       = set to Identifier if there is one, null if not
     *  tpl          = if !null, then set to TemplateParameterList
     *  storageClass = any storage classes seen so far
     *  pdisable     = set to true if @disable seen
     *  pudas        = any user defined attributes seen so far. Merged with any more found
     * Returns:
     *  type declared
     * Reference: https://dlang.org/spec/declaration.html#Declarator
     */
    private AST.Type parseDeclarator(AST.Type t, ref int palt, Identifier* pident,
        AST.TemplateParameters** tpl = null, STC storageClass = STC.none,
        bool* pdisable = null, AST.Expressions** pudas = null)
    {
        //printf("parseDeclarator(tpl = %p)\n", tpl);
        t = parseTypeSuffixes(t);
        AST.Type ts;
        switch (token.value)
        {
        case TOK.identifier:
            if (pident)
                *pident = token.ident;
            else
                error("unexpected identifier `%s` in declarator", token.ident.toChars());
            ts = t;
            nextToken();
            break;

        case TOK.leftParenthesis:
            {
                // like: T (*fp)();
                // like: T ((*fp))();
                if (peekNext() == TOK.mul || peekNext() == TOK.leftParenthesis)
                {
                    /* Parse things with parentheses around the identifier, like:
                     *  int (*ident[3])[]
                     * although the D style would be:
                     *  int[]*[3] ident
                     */
                    palt |= 1;
                    nextToken();
                    ts = parseDeclarator(t, palt, pident);
                    check(TOK.rightParenthesis);
                    break;
                }
                ts = t;

                Token* peekt = &token;
                /* Completely disallow C-style things like:
                 *   T (a);
                 * Improve error messages for the common bug of a missing return type
                 * by looking to see if (a) looks like a parameter list.
                 */
                if (isParameters(&peekt))
                {
                    error("function declaration without return type. (Note that constructors are always named `this`)");
                }
                else
                    error("unexpected `(` in declarator");
                break;
            }
        default:
            ts = t;
            break;
        }

        // parse DeclaratorSuffixes
        while (1)
        {
            switch (token.value)
            {
                static if (CARRAYDECL)
                {
                    /* Support C style array syntax:
                     *   int ident[]
                     * as opposed to D-style:
                     *   int[] ident
                     */
                case TOK.leftBracket:
                    {
                        // This is the old C-style post [] syntax.
                        AST.TypeNext ta;
                        nextToken();
                        if (token.value == TOK.rightBracket)
                        {
                            // It's a dynamic array
                            ta = new AST.TypeDArray(t); // []
                            nextToken();
                            palt |= 2;
                        }
                        else if (isDeclaration(&token, NeedDeclaratorId.no, TOK.rightBracket, null))
                        {
                            // It's an associative array
                            //printf("it's an associative array\n");
                            AST.Type index = parseType(); // [ type ]
                            check(TOK.rightBracket);
                            ta = new AST.TypeAArray(t, index);
                            palt |= 2;
                        }
                        else
                        {
                            //printf("It's a static array\n");
                            AST.Expression e = parseAssignExp(); // [ expression ]
                            ta = new AST.TypeSArray(t, e);
                            check(TOK.rightBracket);
                            palt |= 2;
                        }

                        /* Insert ta into
                         *   ts -> ... -> t
                         * so that
                         *   ts -> ... -> ta -> t
                         */
                        AST.Type* pt;
                        for (pt = &ts; *pt != t; pt = &(cast(AST.TypeNext)*pt).next)
                        {
                        }
                        *pt = ta;
                        continue;
                    }
                }
            case TOK.leftParenthesis:
                {
                    if (tpl)
                    {
                        Token* tk = peekPastParen(&token);
                        if (tk.value == TOK.leftParenthesis)
                        {
                            /* Look ahead to see if this is (...)(...),
                             * i.e. a function template declaration
                             */
                            //printf("function template declaration\n");

                            // Gather template parameter list
                            *tpl = parseTemplateParameterList();
                        }
                        else if (tk.value == TOK.assign)
                        {
                            /* or (...) =,
                             * i.e. a variable template declaration
                             */
                            //printf("variable template declaration\n");
                            *tpl = parseTemplateParameterList();
                            break;
                        }
                    }

                    auto parameterList = parseParameterList(null);

                    /* Parse const/immutable/shared/inout/nothrow/pure/return postfix
                     */
                    // merge prefix storage classes
                    STC stc = parsePostfix(storageClass, pudas);

                    AST.Type tf = new AST.TypeFunction(parameterList, t, linkage, stc);
                    tf = tf.addSTC(stc);
                    if (pdisable)
                        *pdisable = stc & STC.disable ? true : false;

                    /* Insert tf into
                     *   ts -> ... -> t
                     * so that
                     *   ts -> ... -> tf -> t
                     */
                    AST.Type* pt;
                    for (pt = &ts; *pt != t; pt = &(cast(AST.TypeNext)*pt).next)
                    {
                    }
                    *pt = tf;
                    break;
                }
            default:
                break;
            }
            break;
        }
        return ts;
    }

    private void parseStorageClasses(ref STC storage_class, ref LINK link,
        ref bool setAlignment, ref AST.Expression ealign, ref AST.Expressions* udas,
        out Loc linkloc)
    {
        STC stc;
        bool sawLinkage = false; // seen a linkage declaration

        linkloc = Loc.initial;

        while (1)
        {
            switch (token.value)
            {
            case TOK.const_:
                if (peekNext() == TOK.leftParenthesis)
                    break; // const as type constructor
                stc = STC.const_; // const as storage class
                goto L1;

            case TOK.immutable_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc = STC.immutable_;
                goto L1;

            case TOK.shared_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc = STC.shared_;
                goto L1;

            case TOK.inout_:
                if (peekNext() == TOK.leftParenthesis)
                    break;
                stc = STC.wild;
                goto L1;

            case TOK.static_:
                stc = STC.static_;
                goto L1;

            case TOK.final_:
                stc = STC.final_;
                goto L1;

            case TOK.auto_:
                stc = STC.auto_;
                if (peekNext() == TOK.ref_)
                    stc |= STC.autoref;
                goto L1;

            case TOK.scope_:
                stc = STC.scope_;
                goto L1;

            case TOK.override_:
                stc = STC.override_;
                goto L1;

            case TOK.abstract_:
                stc = STC.abstract_;
                goto L1;

            case TOK.synchronized_:
                stc = STC.synchronized_;
                goto L1;

            case TOK.deprecated_:
                stc = STC.deprecated_;
                goto L1;

            case TOK.nothrow_:
                stc = STC.nothrow_;
                goto L1;

            case TOK.pure_:
                stc = STC.pure_;
                goto L1;

            case TOK.ref_:
                stc = STC.ref_;
                goto L1;

            case TOK.gshared:
                stc = STC.gshared;
                goto L1;

            case TOK.enum_:
                {
                    const tv = peekNext();
                    if (tv == TOK.leftCurly || tv == TOK.colon)
                        break;
                    if (tv == TOK.identifier)
                    {
                        const nextv = peekNext2();
                        if (nextv == TOK.leftCurly || nextv == TOK.colon || nextv == TOK.semicolon)
                            break;
                    }
                    stc = STC.manifest;
                    goto L1;
                }

            case TOK.at:
                {
                    stc = parseAttribute(udas);
                    if (stc)
                        goto L1;
                    continue;
                }
            L1:
                storage_class = appendStorageClass(storage_class, stc);
                nextToken();
                continue;

            case TOK.extern_:
                {
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.extern_;
                        goto L1;
                    }

                    if (sawLinkage)
                        error("redundant linkage declaration");
                    sawLinkage = true;
                    linkloc = token.loc;
                    auto res = parseLinkage();
                    link = res.link;
                    if (res.idents || res.identExps)
                    {
                        error("C++ name spaces not allowed here");
                    }
                    if (res.cppmangle != CPPMANGLE.def)
                    {
                        error("C++ mangle declaration not allowed here");
                    }
                    continue;
                }
            case TOK.align_:
                {
                    setAlignment = true;
                    ealign = parseAlign();
                    continue;
                }
            default:
                break;
            }
            break;
        }
    }

    /**
     * Parse `align` or `align(n)`
     * Returns:
     *  expression `n` if it is present, or `null` otherwise.
     */
    private AST.Expression parseAlign()
    {
        assert(token.value == TOK.align_);
        AST.Expression e = null;
        nextToken();
        if (token.value == TOK.leftParenthesis)
        {
            nextToken();
            if (token.value == TOK.default_)
                nextToken();
            else
                e = parseAssignExp();
            check(TOK.rightParenthesis);
        }
        return e;
    }
    /**********************************
     * Parse Declarations.
     * These can be:
     *      1. declarations at global/class level
     *      2. declarations at statement level
     * Returns:
     *  array of Declarations.
     */
    private AST.Dsymbols* parseDeclarations(bool autodecl, PrefixAttributes!AST* pAttrs, const(char)* comment)
    {
        STC storage_class = STC.none;
        LINK link = linkage;
        Loc linkloc = this.linkLoc;
        bool setAlignment = false;
        AST.Expression ealign;
        AST.Expressions* udas = null;

        //printf("parseDeclarations() %s\n", token.toChars());
        if (!comment)
            comment = token.blockComment.ptr;

        /* Look for AliasReassignment
         */
        if (token.value == TOK.identifier && peekNext() == TOK.assign)
            return parseAliasReassignment(comment);

        /* Declarations that start with `alias`
         */
        bool isAliasDeclaration = false;
        auto aliasLoc = token.loc;
        if (token.value == TOK.alias_)
        {
            if (auto a = parseAliasDeclarations(comment))
                return a;
            /* Handle these later:
             *   alias StorageClasses type ident;
             */
            isAliasDeclaration = true;
        }

        AST.Type ts;

        if (!autodecl)
        {
            parseStorageClasses(storage_class, link, setAlignment, ealign, udas, linkloc);

            if (token.value == TOK.enum_)
            {
                AST.Dsymbol d = parseEnum();
                auto a = new AST.Dsymbols();
                a.push(d);

                if (udas)
                {
                    d = new AST.UserAttributeDeclaration(udas, a);
                    a = new AST.Dsymbols();
                    a.push(d);
                }

                addComment(d, comment);
                return a;
            }
            if (token.value == TOK.struct_ ||
                     token.value == TOK.union_ ||
                     token.value == TOK.class_ ||
                     token.value == TOK.interface_)
            {
                AST.Dsymbol s = parseAggregate();
                auto a = new AST.Dsymbols();
                a.push(s);

                if (storage_class)
                {
                    s = new AST.StorageClassDeclaration(storage_class, a);
                    a = new AST.Dsymbols();
                    a.push(s);
                }
                if (setAlignment)
                {
                    s = new AST.AlignDeclaration(s.loc, ealign, a);
                    a = new AST.Dsymbols();
                    a.push(s);
                }
                if (link != linkage)
                {
                    s = new AST.LinkDeclaration(linkloc, link, a);
                    a = new AST.Dsymbols();
                    a.push(s);
                }
                if (udas)
                {
                    s = new AST.UserAttributeDeclaration(udas, a);
                    a = new AST.Dsymbols();
                    a.push(s);
                }

                addComment(s, comment);
                return a;
            }

            /* Look for auto initializers:
             *  storage_class identifier = initializer;
             *  storage_class identifier(...) = initializer;
             */
            if ((storage_class || udas) && token.value == TOK.identifier && hasOptionalParensThen(peek(&token), TOK.assign))
            {
                AST.Dsymbols* a = parseAutoDeclarations(storage_class, comment);
                if (udas)
                {
                    AST.Dsymbol s = new AST.UserAttributeDeclaration(udas, a);
                    a = new AST.Dsymbols();
                    a.push(s);
                }
                return a;
            }

            /* Look for return type inference for template functions.
             */
            {
                Token* tk;
                if ((storage_class || udas) && token.value == TOK.identifier && skipParens(peek(&token), &tk) &&
                    skipAttributes(tk, &tk) &&
                    (tk.value == TOK.leftParenthesis || tk.value == TOK.leftCurly || tk.value == TOK.in_ || tk.value == TOK.out_ || tk.value == TOK.goesTo ||
                     tk.value == TOK.do_ || tk.value == TOK.identifier && tk.ident == Id._body))
                {
                    if (tk.value == TOK.identifier && tk.ident == Id._body)
                        usageOfBodyKeyword();

                    ts = null;
                }
                else
                {
                    ts = parseBasicType();
                    ts = parseTypeSuffixes(ts);
                }
            }
        }

        if (pAttrs)
        {
            storage_class |= pAttrs.storageClass;
            //pAttrs.storageClass = STC.none;
        }

        AST.Type tfirst = null;
        auto a = new AST.Dsymbols();

        while (1)
        {
            AST.TemplateParameters* tpl = null;
            bool disable;
            int alt = 0;

            const loc = token.loc;
            Identifier ident;
            auto t = parseDeclarator(ts, alt, &ident, &tpl, storage_class, &disable, &udas);
            assert(t);
            if (!tfirst)
                tfirst = t;
            else if (t != tfirst)
                error(token.loc, "multiple declarations must have the same type, not `%s` and `%s`", tfirst.toChars(), t.toChars());

            if (token.value == TOK.colon && !ident && t.ty != Tfunction)
            {
                // Unnamed bit field
                ident = Identifier.generateAnonymousId("BitField");
            }

            bool isThis = (t.ty == Tident && (cast(AST.TypeIdentifier)t).ident == Id.This && token.value == TOK.assign);
            if (ident)
                checkCstyleTypeSyntax(loc, t, alt, ident);
            else if (!isThis && (t != AST.Type.terror))
                noIdentifierForDeclarator(t, token);

            if (isAliasDeclaration)
            {
                AST.Declaration v;
                AST.Initializer _init = null;

                /* Aliases can no longer have multiple declarators, storage classes,
                 * linkages, or auto declarations.
                 * These never made any sense, anyway.
                 * The code below needs to be fixed to reject them.
                 * The grammar has already been fixed to preclude them.
                 */

                if (udas)
                    error("user-defined attributes not allowed for `alias` declarations");

                if (token.value == TOK.assign)
                {
                    nextToken();
                    _init = parseInitializer();
                }
                if (_init)
                {
                    error("alias cannot have initializer");
                }
                v = new AST.AliasDeclaration(aliasLoc, ident, t);

                v.storage_class = storage_class;
                if (pAttrs)
                {
                    /* AliasDeclaration distinguish @safe, @system, @trusted attributes
                     * on prefix and postfix.
                     *   @safe alias void function() FP1;
                     *   alias @safe void function() FP2;    // FP2 is not @safe
                     *   alias void function() @safe FP3;
                     */
                    pAttrs.storageClass &= STC.safeGroup;
                }
                AST.Dsymbol s = v;

                if (link != linkage)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(v);
                    s = new AST.LinkDeclaration(linkloc, link, ax);
                }
                a.push(s);
                switch (token.value)
                {
                case TOK.semicolon:
                    nextToken();
                    addComment(s, comment);
                    break;

                case TOK.comma:
                    nextToken();
                    addComment(s, comment);
                    continue;

                default:
                    error("semicolon expected to close `alias` declaration, not `%s`", token.toChars());
                    nextToken();
                    break;
                }
            }
            else if (t.ty == Tfunction)
            {
                /* @@@DEPRECATED_2.115@@@
                 * change to error, deprecated in 2.105.1 */
                if (storage_class & STC.manifest)
                    deprecation("function cannot have enum storage class");

                AST.Expression constraint = null;
                //printf("%s funcdecl t = %s, storage_class = x%lx\n", loc.toChars(), t.toChars(), storage_class);
                auto f = new AST.FuncDeclaration(loc, Loc.initial, ident, storage_class | (disable ? STC.disable : STC.none), t);
                if (pAttrs)
                    pAttrs.storageClass = STC.none;
                if (tpl)
                    constraint = parseConstraint();
                AST.Dsymbol s = parseContracts(f, !!tpl);
                auto tplIdent = s.ident;

                if (link != linkage)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(s);
                    s = new AST.LinkDeclaration(linkloc, link, ax);
                }
                if (udas)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(s);
                    s = new AST.UserAttributeDeclaration(udas, ax);
                }

                /* A template parameter list means it's a function template
                 */
                if (tpl)
                {
                    // @@@DEPRECATED_2.114@@@
                    // Both deprecated in 2.104, change to error
                    if (storage_class & STC.override_)
                        deprecation(loc, "a function template is not virtual so cannot be marked `override`");
                    else if (storage_class & STC.abstract_)
                        deprecation(loc, "a function template is not virtual so cannot be marked `abstract`");

                    // Wrap a template around the function declaration
                    auto decldefs = new AST.Dsymbols();
                    decldefs.push(s);
                    auto tempdecl = new AST.TemplateDeclaration(loc, tplIdent, tpl, constraint, decldefs);
                    s = tempdecl;

                    STC stc2 = STC.none;
                    if (storage_class & STC.static_)
                    {
                        assert(f.storage_class & STC.static_);
                        f.storage_class &= ~STC.static_;
                        stc2 |= STC.static_;
                    }
                    if (storage_class & STC.deprecated_)
                    {
                        assert(f.storage_class & STC.deprecated_);
                        f.storage_class &= ~STC.deprecated_;
                        stc2 |= STC.deprecated_;
                    }
                    if (stc2 != STC.none)
                    {
                        auto ax = new AST.Dsymbols();
                        ax.push(s);
                        s = new AST.StorageClassDeclaration(stc2, ax);
                    }
                }
                a.push(s);
                addComment(s, comment);
            }
            else if (ident)
            {
                AST.Expression width;
                if (token.value == TOK.colon)
                {
                    nextToken();
                    width = parseCondExp();
                }

                AST.Initializer _init = null;
                if (token.value == TOK.assign)
                {
                    nextToken();
                    _init = parseInitializer();
                }

                AST.Dsymbol s;
                if (width)
                {
                    if (_init)
                        error("initializer not allowed for bit-field declaration");
                    if (storage_class)
                        error("storage class not allowed for bit-field declaration");
                    s = new AST.BitFieldDeclaration(width.loc, t, ident, width);
                }
                else
                {
                    auto v = new AST.VarDeclaration(loc, t, ident, _init);
                    v.storage_class = storage_class;
                    if (pAttrs)
                        pAttrs.storageClass = STC.none;
                    s = v;
                }

                if (tpl && _init)
                {
                    auto a2 = new AST.Dsymbols();
                    a2.push(s);
                    auto tempdecl = new AST.TemplateDeclaration(loc, ident, tpl, null, a2, 0);
                    s = tempdecl;
                }
                if (setAlignment)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(s);
                    s = new AST.AlignDeclaration(s.loc, ealign, ax);
                }
                if (link != linkage)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(s);
                    s = new AST.LinkDeclaration(linkloc, link, ax);
                }
                if (udas)
                {
                    auto ax = new AST.Dsymbols();
                    ax.push(s);
                    s = new AST.UserAttributeDeclaration(udas, ax);
                }
                a.push(s);
                switch (token.value)
                {
                case TOK.semicolon:
                    nextToken();
                    addComment(s, comment);
                    break;

                case TOK.comma:
                    nextToken();
                    addComment(s, comment);
                    continue;

                default:
                    if (loc.linnum != token.loc.linnum)
                    {
                        error(token.loc, "semicolon needed to end declaration of `%s`, instead of `%s`", s.toChars(), token.toChars());
                        eSink.errorSupplemental(loc, "`%s` declared here", s.toChars());
                    }
                    else
                    {
                        error(token.loc, "semicolon needed to end declaration of `%s` instead of `%s`", s.toChars(), token.toChars());
                    }
                    break;
                }
            }
            break;
        }
        return a;
    }

    /// Report an error that a declaration of type `t` is missing an identifier and got `tok` instead
    /// The parser is expected to sit on the next token after the type.
    private void noIdentifierForDeclarator(AST.Type t, Token tok)
    {
        error("variable name expected after type `%s`, not `%s`", t.toChars(), tok.toChars);

        // A common mistake is to use a reserved keyword as an identifier, e.g. `in` or `out`
        if (token.isKeyword)
        {
            eSink.errorSupplemental(token.loc, "`%s` is a keyword, perhaps append `_` to make it an identifier", token.toChars());
            nextToken();
        }
    }

    /********************************
     * Parse AliasReassignment:
     *   identifier = type;
     * Parser is sitting on the identifier.
     * https://dlang.org/spec/declaration.html#alias-reassignment
     * Params:
     *  comment = if not null, comment to attach to symbol
     * Returns:
     *  array of symbols
     */
    private AST.Dsymbols* parseAliasReassignment(const(char)* comment)
    {
        const loc = token.loc;
        auto ident = token.ident;
        nextToken();
        nextToken();        // advance past =
        auto t = parseType();
        AST.Dsymbol s = new AST.AliasAssign(loc, ident, t, null);
        check(TOK.semicolon, "alias reassignment");
        addComment(s, comment);
        auto a = new AST.Dsymbols();
        a.push(s);
        return a;
    }

    /********************************
     * Parse declarations that start with `alias`
     * Parser is sitting on the `alias`.
     * https://dlang.org/spec/declaration.html#alias
     * Params:
     *  comment = if not null, comment to attach to symbol
     * Returns:
     *  array of symbols
     */
    private AST.Dsymbols* parseAliasDeclarations(const(char)* comment)
    {
        const loc = token.loc;
        nextToken();
        Loc linkloc = this.linkLoc;
        AST.Expressions* udas;
        LINK link = linkage;
        STC storage_class = STC.none;
        AST.Expression ealign;
        bool setAlignment = false;

        /* Look for:
         *   alias Identifier this;
         * https://dlang.org/spec/class.html#alias-this
         */
        if (token.value == TOK.identifier && peekNext() == TOK.this_)
        {
            auto s = new AST.AliasThis(loc, token.ident);
            nextToken();
            check(TOK.this_);
            check(TOK.semicolon, "`alias Identifier this`");
            auto a = new AST.Dsymbols();
            a.push(s);
            addComment(s, comment);
            return a;
        }
        /* Look for:
         *  alias this = identifier;
         */
        if (token.value == TOK.this_ && peekNext() == TOK.assign && peekNext2() == TOK.identifier)
        {
            check(TOK.this_);
            check(TOK.assign);
            auto s = new AST.AliasThis(loc, token.ident);
            nextToken();
            check(TOK.semicolon, "`alias this = Identifier`");
            auto a = new AST.Dsymbols();
            a.push(s);
            addComment(s, comment);
            return a;
        }
        /* Look for:
         *  alias identifier = type;
         *  alias identifier(...) = type;
         * https://dlang.org/spec/declaration.html#alias
         */
        if (token.value == TOK.identifier && hasOptionalParensThen(peek(&token), TOK.assign))
        {
            auto a = new AST.Dsymbols();
            while (1)
            {
                auto ident = token.ident;
                nextToken();
                AST.TemplateParameters* tpl = null;
                if (token.value == TOK.leftParenthesis)
                    tpl = parseTemplateParameterList();
                check(TOK.assign);

                bool hasParsedAttributes;
                void parseAttributes()
                {
                    if (hasParsedAttributes) // only parse once
                        return;
                    hasParsedAttributes = true;
                    udas = null;
                    storage_class = STC.none;
                    link = linkage;
                    linkloc = this.linkLoc;
                    setAlignment = false;
                    ealign = null;
                    parseStorageClasses(storage_class, link, setAlignment, ealign, udas, linkloc);
                }

                if (token.value == TOK.at)
                    parseAttributes;

                AST.Declaration v;
                AST.Dsymbol s;

                bool attributesAppended;
                const STC funcStc = parseTypeCtor();
                Token* tk;
                // function literal?
                if (token.value == TOK.function_ ||
                    token.value == TOK.delegate_ ||
                    token.value == TOK.leftParenthesis &&
                        skipAttributes(peekPastParen(&token), &tk) &&
                        (tk.value == TOK.goesTo || tk.value == TOK.leftCurly) ||
                    token.value == TOK.leftCurly ||
                    token.value == TOK.identifier && peekNext() == TOK.goesTo ||
                    token.value == TOK.ref_ && peekNext() == TOK.leftParenthesis &&
                        skipAttributes(peekPastParen(peek(&token)), &tk) &&
                        (tk.value == TOK.goesTo || tk.value == TOK.leftCurly) ||
                    token.value == TOK.auto_ &&
                        (peekNext() == TOK.leftParenthesis || // for better error
                            peekNext() == TOK.ref_ &&
                            peekNext2() == TOK.leftParenthesis)
                   )
                {
                    // function (parameters) { statements... }
                    // delegate (parameters) { statements... }
                    // (parameters) { statements... }
                    // (parameters) => expression
                    // { statements... }
                    // identifier => expression
                    // ref (parameters) { statements... }
                    // ref (parameters) => expression
                    // auto ref (parameters) { statements... }
                    // auto ref (parameters) => expression

                    s = parseFunctionLiteral();

                    if (udas !is null)
                    {
                        if (storage_class != 0)
                            error("cannot put a storage-class in an `alias` declaration.");
                        // parseAttributes shouldn't have set these variables
                        assert(link == linkage && !setAlignment && ealign is null);
                        auto tpl_ = cast(AST.TemplateDeclaration) s;
                        if (tpl_ is null || tpl_.members.length != 1)
                        {
                            error("user-defined attributes are not allowed on `alias` declarations");
                        }
                        else
                        {
                            auto fd = cast(AST.FuncLiteralDeclaration) (*tpl_.members)[0];
                            auto tf = cast(AST.TypeFunction) fd.type;
                            assert(tf.parameterList.parameters.length > 0);
                            auto as = new AST.Dsymbols();
                            (*tf.parameterList.parameters)[0].userAttribDecl = new AST.UserAttributeDeclaration(udas, as);
                        }
                    }

                    v = new AST.AliasDeclaration(loc, ident, s);
                }
                else
                {
                    // type
                    parseAttributes();
                    if (udas)
                        error("user-defined attributes not allowed for `alias` declarations");

                    auto t = parseBasicType();
                    t = parseTypeSuffixes(t);
                    if (token.value == TOK.identifier)
                    {
                        error("unexpected identifier `%s` after `%s`",
                            token.ident.toChars(), t.toChars());
                        nextToken();
                    }
                    else if (token.value == TOK.leftParenthesis)
                    {
                        // function type:
                        // StorageClasses Type ( Parameters ) MemberFunctionAttributes
                        auto parameterList = parseParameterList(null);
                        udas = null;
                        parseStorageClasses(storage_class, link, setAlignment, ealign, udas, linkloc);
                        if (udas)
                            error("user-defined attributes not allowed for `alias` declarations");

                        attributesAppended = true;
                        // Note: method types can have a TypeCtor attribute
                        storage_class = appendStorageClass(storage_class, funcStc);
                        t = new AST.TypeFunction(parameterList, t, link, storage_class);
                    }

                    // Disallow meaningless storage classes on type aliases
                    if (storage_class)
                    {
                        // Don't raise errors for STC that are part of a function/delegate type, e.g.
                        // `alias F = ref pure nothrow @nogc @safe int function();`
                        const remStc = t.isTypeFunction ?
                            storage_class & ~(STC.FUNCATTR | STC.TYPECTOR) : {
                            auto tp = t.isTypePointer;
                            const isFuncType = (tp && tp.next.isTypeFunction) || t.isTypeDelegate;
                            return isFuncType ? (storage_class & ~STC.FUNCATTR) : storage_class;
                        }();

                        if (remStc)
                        {
                            OutBuffer buf;
                            AST.stcToBuffer(buf, remStc);
                            // @@@DEPRECATED_2.103@@@
                            // Deprecated in 2020-07, can be made an error in 2.103
                            eSink.deprecation(token.loc, "storage class `%s` has no effect in type aliases", buf.peekChars());
                        }
                    }

                    v = new AST.AliasDeclaration(loc, ident, t);
                }
                if (!attributesAppended)
                    storage_class = appendStorageClass(storage_class, funcStc);
                v.storage_class = storage_class;

                s = v;
                if (tpl)
                {
                    auto a2 = new AST.Dsymbols();
                    a2.push(s);
                    auto tempdecl = new AST.TemplateDeclaration(loc, ident, tpl, null, a2);
                    s = tempdecl;
                }
                if (link != linkage)
                {
                    auto a2 = new AST.Dsymbols();
                    a2.push(s);
                    s = new AST.LinkDeclaration(linkloc, link, a2);
                }
                a.push(s);

                switch (token.value)
                {
                case TOK.semicolon:
                    nextToken();
                    addComment(s, comment);
                    break;

                case TOK.comma:
                    nextToken();
                    addComment(s, comment);
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected following comma, not `%s`", token.toChars());
                        break;
                    }
                    if (peekNext() != TOK.assign && peekNext() != TOK.leftParenthesis)
                    {
                        error("`=` expected following identifier");
                        nextToken();
                        break;
                    }
                    continue;

                default:
                    error("semicolon expected to close `alias` declaration, not `%s`", token.toChars());
                    nextToken();
                    break;
                }
                break;
            }
            return a;
        }

        // alias StorageClasses type ident;
        return null;
    }

    private AST.Dsymbol parseFunctionLiteral()
    {
        const loc = token.loc;
        AST.TemplateParameters* tpl = null;
        AST.ParameterList parameterList;
        AST.Type tret = null;
        STC stc = STC.none;
        TOK save = TOK.reserved;

        switch (token.value)
        {
        case TOK.function_:
        case TOK.delegate_:
            save = token.value;
            nextToken();
            if (token.value == TOK.auto_)
            {
                nextToken();
                if (token.value == TOK.ref_)
                {
                    // function auto ref (parameters) { statements... }
                    // delegate auto ref (parameters) { statements... }
                    stc = STC.auto_ | STC.ref_ | STC.autoref;
                    nextToken();
                }
                else
                    error("`auto` can only be used as part of `auto ref` for function literal return values");
            }
            else if (token.value == TOK.ref_)
            {
                // function ref (parameters) { statements... }
                // delegate ref (parameters) { statements... }
                stc = STC.ref_;
                nextToken();
            }
            if (token.value != TOK.leftParenthesis && token.value != TOK.leftCurly &&
                token.value != TOK.goesTo)
            {
                // function type (parameters) { statements... }
                // delegate type (parameters) { statements... }
                tret = parseBasicType();
                tret = parseTypeSuffixes(tret); // function return type
            }

            if (token.value == TOK.leftParenthesis)
            {
                // function (parameters) { statements... }
                // delegate (parameters) { statements... }
            }
            else
            {
                // function { statements... }
                // delegate { statements... }
                break;
            }
            goto case TOK.leftParenthesis;

        case TOK.auto_:
            {
                nextToken();
                if (token.value == TOK.ref_)
                {
                    // auto ref (parameters) => expression
                    // auto ref (parameters) { statements... }
                    stc = STC.auto_ | STC.ref_ | STC.autoref;
                    nextToken();
                }
                else
                    error("`auto` can only be used as part of `auto ref` for function literal return values");
                goto case TOK.leftParenthesis;
            }
        case TOK.ref_:
            {
                // ref (parameters) => expression
                // ref (parameters) { statements... }
                stc = STC.ref_;
                nextToken();
                goto case TOK.leftParenthesis;
            }
        case TOK.leftParenthesis:
            {
                // (parameters) => expression
                // (parameters) { statements... }
                parameterList = parseParameterList(&tpl);
                stc = parsePostfix(stc, null);
                if (STC modStc = stc & STC.TYPECTOR)
                {
                    if (save == TOK.function_)
                    {
                        OutBuffer buf;
                        AST.stcToBuffer(buf, modStc);
                        error("function literal cannot be `%s`", buf.peekChars());
                    }
                    else
                        save = TOK.delegate_;
                }
                break;
            }
        case TOK.leftCurly:
            // { statements... }
            break;

        case TOK.identifier:
            {
                // identifier => expression
                parameterList.parameters = new AST.Parameters();
                Identifier id = Identifier.generateId("__T");
                AST.Type t = new AST.TypeIdentifier(loc, id);
                parameterList.parameters.push(new AST.Parameter(loc, STC.parameter, t, token.ident, null, null));

                tpl = new AST.TemplateParameters();
                AST.TemplateParameter tp = new AST.TemplateTypeParameter(loc, id, null, null);
                tpl.push(tp);

                nextToken();
                break;
            }
        default:
            assert(0);
        }

        auto tf = new AST.TypeFunction(parameterList, tret, linkage, stc);
        tf = cast(AST.TypeFunction)tf.addSTC(stc);
        auto fd = new AST.FuncLiteralDeclaration(loc, Loc.initial, tf, save, null, null, stc & STC.auto_);

        if (token.value == TOK.goesTo)
        {
            check(TOK.goesTo);
            if (token.value == TOK.leftCurly)
            {
                deprecation("using `(args) => { ... }` to create a delegate that returns a delegate is error-prone.");
                deprecationSupplemental("Use `(args) { ... }` for a multi-statement function literal or use `(args) => () { }` if you intended for the lambda to return a delegate.");
            }
            const returnloc = token.loc;
            AST.Expression ae = parseAssignExp();
            fd.fbody = new AST.ReturnStatement(returnloc, ae);
            fd.endloc = token.loc;
        }
        else
        {
            parseContracts(fd);
        }

        if (tpl)
        {
            // Wrap a template around function fd
            auto decldefs = new AST.Dsymbols();
            decldefs.push(fd);
            return new AST.TemplateDeclaration(fd.loc, fd.ident, tpl, null, decldefs, false, true);
        }
        return fd;
    }

    /*****************************************
     * Parse contracts following function declaration.
     */
    private AST.FuncDeclaration parseContracts(AST.FuncDeclaration f, bool isTemplateFunction = false)
    {
        LINK linksave = linkage;

        bool literal = f.isFuncLiteralDeclaration() !is null;

        // The following is irrelevant, as it is overridden by sc.linkage in
        // TypeFunction::semantic
        linkage = LINK.d; // nested functions have D linkage
        bool requireDo = false;
    L1:
        switch (token.value)
        {
        case TOK.goesTo:
            if (requireDo)
                error("missing `do { ... }` after `in` or `out`");
            const returnloc = token.loc;
            nextToken();
            if (f.isCtorDeclaration)
                f.fbody = new AST.ExpStatement(returnloc, parseExpression());
            else
                f.fbody = new AST.ReturnStatement(returnloc, parseExpression());
            f.endloc = token.loc;
            check(TOK.semicolon);
            break;

        case TOK.leftCurly:
            if (requireDo)
                error("missing `do { ... }` after `in` or `out`");
            f.fbody = parseStatement(0);
            f.endloc = endloc;
            break;

        case TOK.identifier:
            if (token.ident == Id._body)
            {
                usageOfBodyKeyword();
                goto case TOK.do_;
            }
            goto default;

        case TOK.do_:
            nextToken();
            f.fbody = parseStatement(ParseStatementFlags.curly);
            f.endloc = endloc;
            break;

            version (none)
            {
                // Do we want this for function declarations, so we can do:
                // int x, y, foo(), z;
            case TOK.comma:
                nextToken();
                continue;
            }

        case TOK.in_:
            // in { statements... }
            // in (expression)
            auto loc = token.loc;
            nextToken();
            if (!f.frequires)
            {
                f.frequires = new AST.Statements;
            }
            if (token.value == TOK.leftParenthesis)
            {
                nextToken();
                AST.Expression e = parseAssignExp(), msg = null;
                if (token.value == TOK.comma)
                {
                    nextToken();
                    if (token.value != TOK.rightParenthesis)
                    {
                        msg = parseAssignExp();
                        if (token.value == TOK.comma)
                            nextToken();
                    }
                }
                check(TOK.rightParenthesis);
                e = new AST.AssertExp(loc, e, msg);
                f.frequires.push(new AST.ExpStatement(loc, e));
                requireDo = false;
            }
            else
            {
                auto ret = parseStatement(ParseStatementFlags.curly | ParseStatementFlags.scope_);
                assert(ret);
                f.frequires.push(ret);
                requireDo = true;
            }
            goto L1;

        case TOK.out_:
            // out { statements... }
            // out (; expression)
            // out (identifier) { statements... }
            // out (identifier; expression)
            auto loc = token.loc;
            nextToken();
            if (!f.fensures)
            {
                f.fensures = new AST.Ensures;
            }
            Identifier id = null;
            if (token.value != TOK.leftCurly)
            {
                check(TOK.leftParenthesis);
                if (token.value != TOK.identifier && token.value != TOK.semicolon)
                    error("`(identifier) { ... }` or `(identifier; expression)` following `out` expected, not `%s`", token.toChars());
                if (token.value != TOK.semicolon)
                {
                    id = token.ident;
                    nextToken();
                }
                if (token.value == TOK.semicolon)
                {
                    nextToken();
                    AST.Expression e = parseAssignExp(), msg = null;
                    if (token.value == TOK.comma)
                    {
                        nextToken();
                        if (token.value != TOK.rightParenthesis)
                        {
                            msg = parseAssignExp();
                            if (token.value == TOK.comma)
                                nextToken();
                        }
                    }
                    check(TOK.rightParenthesis);
                    e = new AST.AssertExp(loc, e, msg);
                    f.fensures.push(AST.Ensure(id, new AST.ExpStatement(loc, e)));
                    requireDo = false;
                    goto L1;
                }
                check(TOK.rightParenthesis);
            }
            f.fensures.push(AST.Ensure(id, parseStatement(ParseStatementFlags.curly | ParseStatementFlags.scope_)));
            requireDo = true;
            goto L1;

        case TOK.semicolon:
            if (!literal)
            {
                // https://issues.dlang.org/show_bug.cgi?id=15799
                // Semicolon becomes a part of function declaration
                // only when 'do' is not required
                if (!requireDo)
                    nextToken();
                break;
            }
            goto default;

        default:
            if (literal)
            {
                const(char)* sbody = requireDo ? "do " : "";
                error("missing `%s{ ... }` for function literal", sbody);
            }
            else if (!requireDo) // allow contracts even with no body
            {
                TOK t = token.value;
                if (t == TOK.const_ || t == TOK.immutable_ || t == TOK.inout_ || t == TOK.return_ ||
                        t == TOK.shared_ || t == TOK.nothrow_ || t == TOK.pure_)
                    error("'%s' cannot be placed after a template constraint", token.toChars);
                else if (t == TOK.at)
                    error("attributes cannot be placed after a template constraint");
                else if (t == TOK.if_)
                {
                    if (isTemplateFunction)
                        error("template constraint must follow parameter lists and attributes");
                    else
                        error("cannot use function constraints for non-template functions. Use `static if` instead");

                    parseConstraint();
                }
                else
                {
                    error("semicolon expected following function declaration, not `%s`", token.toChars());
                    nextToken();
                }
            }
            break;
        }
        if (literal && !f.fbody)
        {
            // Set empty function body for error recovery
            f.fbody = new AST.CompoundStatement(Loc.initial, cast(AST.Statement)null);
        }

        linkage = linksave;

        return f;
    }

    /*****************************************
     */
    private void checkDanglingElse(Loc elseloc)
    {
        if (token.value != TOK.else_ && token.value != TOK.catch_ && token.value != TOK.finally_ && lookingForElse.isValid)
        {
            eSink.warning(elseloc, "else is dangling, add { } after condition at %s", lookingForElse.toChars());
        }
    }

    /* *************************
     * Issue errors if C-style syntax
     * Params:
     *  alt = !=0 for C-style syntax
     */
    private void checkCstyleTypeSyntax(Loc loc, AST.Type t, int alt, Identifier ident)
    {
        if (!alt)
            return;

        const(char)* sp = !ident ? "" : " ";
        const(char)* s = !ident ? "" : ident.toChars();
        error(loc, "instead of C-style syntax, use D-style `%s%s%s`", t.toChars(), sp, s);
    }

    /*****************************
     * Ad-hoc error message for missing or extra parens that close a condition.
     * Params:
     *  start = "if", "while", etc. Must be 0 terminated.
     *  param = if the condition is a declaration, this will be non-null
     *  condition = if param is null, then this is the conditional Expression. If condition is null,
     *      then an error in the condition was already reported.
     */
    private void closeCondition(string start, AST.Parameter param, AST.Expression condition)
    {
        string format;
        if (token.value != TOK.rightParenthesis && condition)
        {
            format = "missing closing `)` after `%s (%s`";
        }
        else
            check(TOK.rightParenthesis);
        if (token.value == TOK.rightParenthesis)
        {
            if (condition) // if not an error in condition
                format = "extra `)` after `%s (%s)`";
            nextToken();
        }
        if (format)
            error(token.loc, format.ptr, start.ptr, param ? "declaration".ptr : condition.toChars());
    }

    /*****************************************
     * Parses `foreach` statements, `static foreach` statements and
     * `static foreach` declarations.
     * Params:
     *  Foreach = one of Statement, StaticForeachStatement, StaticForeachDeclaration
     *  loc = location of foreach
     *  pLastDecl = non-null for StaticForeachDeclaration
     * Returns:
     *  the Foreach generated
     */
    private Foreach parseForeach(alias Foreach)(Loc loc, AST.Dsymbol* pLastDecl)
    {
        static if (is(Foreach == AST.StaticForeachStatement) || is(Foreach == AST.StaticForeachDeclaration))
        {
            nextToken();
        }

        TOK op = token.value;

        nextToken();
        check(TOK.leftParenthesis);

        auto parameters = new AST.Parameters();
        Identifier lastai;
        while (1)
        {
            Identifier ai = null;
            AST.Type at;
            Loc aloc;

            STC storageClass = STC.none;
            STC stc = STC.none;
        Lagain:
            if (stc)
            {
                storageClass = appendStorageClass(storageClass, stc);
                nextToken();
            }
            switch (token.value)
            {
                case TOK.ref_:
                    stc = STC.ref_;
                    goto Lagain;

                case TOK.scope_:
                    stc = STC.scope_;
                    goto Lagain;

                case TOK.out_:
                    error("cannot declare `out` loop variable, use `ref` instead");
                    stc = STC.out_;
                    goto Lagain;

                case TOK.auto_:
                    error("cannot declare `auto` loop variable, omit `auto` to still get type inference");
                    stc = STC.auto_;
                    goto Lagain;

                case TOK.enum_:
                    stc = STC.manifest;
                    goto Lagain;

                case TOK.alias_:
                    storageClass = appendStorageClass(storageClass, STC.alias_);
                    nextToken();
                    break;

                case TOK.const_:
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.const_;
                        goto Lagain;
                    }
                    break;

                case TOK.immutable_:
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.immutable_;
                        goto Lagain;
                    }
                    break;

                case TOK.shared_:
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.shared_;
                        goto Lagain;
                    }
                    break;

                case TOK.inout_:
                    if (peekNext() != TOK.leftParenthesis)
                    {
                        stc = STC.wild;
                        goto Lagain;
                    }
                    break;

                default:
                    break;
            }
            if (token.value == TOK.identifier)
            {
                const tv = peekNext();
                if (tv == TOK.comma || tv == TOK.semicolon || tv == TOK.rightParenthesis)
                {
                    lastai = token.ident;
                    ai = token.ident;
                    at = null; // infer argument type
                    aloc = token.loc;
                    nextToken();
                    goto Larg;
                }
            }
            at = parseType(&ai);
            if (!ai)
                noIdentifierForDeclarator(at, token);
        Larg:
            auto p = new AST.Parameter(aloc, storageClass, at, ai, null, null);
            parameters.push(p);
            if (token.value == TOK.comma)
            {
                nextToken();
                continue;
            }
            break;
        }
        if (token.value != TOK.semicolon)
        {
            error("missing `; expression` before `)` of `foreach`");
            nextToken();
            if (lastai && parameters.length >= 2)
            {
                eSink.errorSupplemental(loc, "perhaps the `;` goes before `%s`", lastai.toChars());
            }
            return null;
        }
        nextToken();

        AST.Expression aggr = parseExpression();
        if (token.value == TOK.slice && parameters.length == 1)
        {
            AST.Parameter p = (*parameters)[0];
            nextToken();
            AST.Expression upr = parseExpression();
            check(TOK.rightParenthesis);
            Loc endloc;
            static if (is(Foreach == AST.Statement) || is(Foreach == AST.StaticForeachStatement))
            {
                AST.Statement _body = parseStatement(0, null, &endloc);
            }
            else
            {
                AST.Statement _body = null;
            }
            auto rangefe = new AST.ForeachRangeStatement(loc, op, p, aggr, upr, _body, endloc);
            static if (is(Foreach == AST.Statement))
            {
                return rangefe;
            }
            else static if(is(Foreach == AST.StaticForeachDeclaration))
            {
                return new AST.StaticForeachDeclaration(new AST.StaticForeach(loc, null, rangefe), parseBlock(pLastDecl));
            }
            else static if (is(Foreach == AST.StaticForeachStatement))
            {
                return new AST.StaticForeachStatement(loc, new AST.StaticForeach(loc, null, rangefe));
            }
        }
        else
        {
            check(TOK.rightParenthesis);
            Loc endloc;
            static if (is(Foreach == AST.Statement) || is(Foreach == AST.StaticForeachStatement))
            {
                AST.Statement _body = parseStatement(0, null, &endloc);
            }
            else
            {
                AST.Statement _body = null;
            }
            auto aggrfe = new AST.ForeachStatement(loc, op, parameters, aggr, _body, endloc);
            static if (is(Foreach == AST.Statement))
            {
                return aggrfe;
            }
            else static if(is(Foreach == AST.StaticForeachDeclaration))
            {
                return new AST.StaticForeachDeclaration(new AST.StaticForeach(loc, aggrfe, null), parseBlock(pLastDecl));
            }
            else static if (is(Foreach == AST.StaticForeachStatement))
            {
                return new AST.StaticForeachStatement(loc, new AST.StaticForeach(loc, aggrfe, null));
            }
        }

    }

    /***
     * Parse an assignment condition for `if`, `switch` or `while` statements.
     *
     * Returns:
     *      The variable that is declared inside the condition
     */
    AST.Parameter parseAssignCondition()
    {
        AST.Parameter param = null;
        STC storageClass = STC.none;
        STC stc = STC.none;
    Lwhile:
        while (1)
        {
            switch (token.value)
            {
            // parse ref for better error
            case TOK.ref_:
                stc = STC.ref_;
                break;

            case TOK.scope_:
                stc = STC.scope_;
                break;

            case TOK.auto_:
                stc = STC.auto_;
                if (peekNext() == TOK.ref_)
                    stc |= STC.autoref;
                break;

            case TOK.const_:
                if (peekNext() != TOK.leftParenthesis)
                {
                    stc = STC.const_;
                    break;
                }
                goto default;

            case TOK.immutable_:
                if (peekNext() != TOK.leftParenthesis)
                {
                    stc = STC.immutable_;
                    break;
                }
                goto default;

            case TOK.shared_:
                if (peekNext() != TOK.leftParenthesis)
                {
                    stc = STC.shared_;
                    break;
                }
                goto default;

            case TOK.inout_:
                if (peekNext() != TOK.leftParenthesis)
                {
                    stc = STC.wild;
                    break;
                }
                goto default;

            default:
                break Lwhile;
            }
            storageClass = appendStorageClass(storageClass, stc);
            nextToken();
        }
        auto n = peek(&token);
        if (storageClass != 0 && token.value == TOK.identifier && n.value == TOK.assign)
        {
            Identifier ai = token.ident;
            AST.Type at = null; // infer parameter type
            const aloc = token.loc;
            nextToken();
            check(TOK.assign);
            param = new AST.Parameter(aloc, storageClass, at, ai, null, null);
        }
        else if (isDeclaration(&token, NeedDeclaratorId.must, TOK.assign, null))
        {
            Identifier ai;
            const aloc = token.loc;
            AST.Type at = parseType(&ai);
            check(TOK.assign);
            param = new AST.Parameter(aloc, storageClass, at, ai, null, null);
        }
        else if (storageClass != 0)
            error("found `%s` while expecting `=` or identifier", n.toChars());

        return param;
    }

    /*****************************************
     * Input:
     *      flags   PSxxxx
     * Output:
     *      pEndloc if { ... statements ... }, store location of closing brace, otherwise loc of last token of statement
     */
    AST.Statement parseStatement(int flags, const(char)** endPtr = null, Loc* pEndloc = null)
    {
        AST.Statement s;
        AST.Condition cond;
        AST.Statement ifbody;
        AST.Statement elsebody;
        bool isfinal;
        const loc = token.loc;

        //printf("parseStatement()\n");
        if (flags & ParseStatementFlags.curly && token.value != TOK.leftCurly)
            error("statement expected to be `{ }`, not `%s`", token.toChars());

        switch (token.value)
        {
        case TOK.identifier:
            {
                /* A leading identifier can be a declaration, label, or expression.
                 * The easiest case to check first is label:
                 */
                if (peekNext() == TOK.colonColon)
                {
                    // skip ident::
                    nextToken();
                    nextToken();
                    error("use `.` for member lookup, not `::`");
                    break;
                }

                if (peekNext() == TOK.colon)
                {
                    // It's a label
                    Identifier ident = token.ident;
                    nextToken();
                    nextToken();
                    if (token.value == TOK.rightCurly)
                        s = null;
                    else if (token.value == TOK.leftCurly)
                        s = parseStatement(ParseStatementFlags.curly | ParseStatementFlags.scope_);
                    else if (flags & ParseStatementFlags.curlyScope)
                        s = parseStatement(ParseStatementFlags.semiOk | ParseStatementFlags.curlyScope);
                    else
                        s = parseStatement(ParseStatementFlags.semiOk);
                    s = new AST.LabelStatement(loc, ident, s);
                    break;
                }
                goto case TOK.dot;
            }
        case TOK.dot:
        case TOK.typeof_:
        case TOK.vector:
        case TOK.traits:
            /* https://issues.dlang.org/show_bug.cgi?id=15163
             * If tokens can be handled as
             * old C-style declaration or D expression, prefer the latter.
             */
            if (isDeclaration(&token, NeedDeclaratorId.mustIfDstyle, TOK.reserved, null))
                goto Ldeclaration;
            goto Lexp;

        case TOK.assert_:
        case TOK.this_:
        case TOK.super_:
        case TOK.int32Literal:
        case TOK.uns32Literal:
        case TOK.int64Literal:
        case TOK.uns64Literal:
        case TOK.int128Literal:
        case TOK.uns128Literal:
        case TOK.float32Literal:
        case TOK.float64Literal:
        case TOK.float80Literal:
        case TOK.imaginary32Literal:
        case TOK.imaginary64Literal:
        case TOK.imaginary80Literal:
        case TOK.charLiteral:
        case TOK.wcharLiteral:
        case TOK.dcharLiteral:
        case TOK.null_:
        case TOK.true_:
        case TOK.false_:
        case TOK.string_:
        case TOK.interpolated:
        case TOK.hexadecimalString:
        case TOK.leftParenthesis:
        case TOK.cast_:
        case TOK.mul:
        case TOK.min:
        case TOK.add:
        case TOK.tilde:
        case TOK.not:
        case TOK.plusPlus:
        case TOK.minusMinus:
        case TOK.new_:
        case TOK.delegate_:
        case TOK.function_:
        case TOK.typeid_:
        case TOK.is_:
        case TOK.leftBracket:
        case TOK.file:
        case TOK.fileFullPath:
        case TOK.line:
        case TOK.moduleString:
        case TOK.functionString:
        case TOK.prettyFunction:
        case TOK.rvalue:
        Lexp:
            {
                AST.Expression exp = parseExpression();
                /* https://issues.dlang.org/show_bug.cgi?id=15103
                 * Improve declaration / initialization syntax error message
                 * Error: found 'foo' when expecting ';' following expression
                 * becomes Error: found `(` when expecting `;` or `=`, did you mean `Foo foo = 42`?
                 */
                if (token.value == TOK.identifier && exp.op == EXP.identifier)
                {
                    error(token.loc, "found `%s` when expecting `;` or `=`, did you mean `%s %s = %s`?", peek(&token).toChars(), exp.toChars(), token.toChars(), peek(peek(&token)).toChars());
                    nextToken();
                }
                else
                {
                    /*
                     * https://issues.dlang.org/show_bug.cgi?id=22529
                     * Avoid empty declaration error in case of missing semicolon
                     * followed by another token and another semicolon. E.g.:
                     *
                     *  foo()
                     *  return;
                     *
                     * When the missing `;` error is emitted, token is sitting on return.
                     * If we simply use `check` to emit the error, the token is advanced
                     * to `;` and the empty statement error would follow. To avoid that,
                     * we check if the next token is a semicolon and simply output the error,
                     * otherwise we fall back on the old path (advancing the token).
                     */
                    if (token.value != TOK.semicolon && peek(&token).value == TOK.semicolon)
                        error("found `%s` when expecting `;` following expression", token.toChars());
                    else
                    {
                        if (token.value != TOK.semicolon)
                        {
                            error("found `%s` when expecting `;` following expression", token.toChars());
                            eSink.errorSupplemental(exp.loc, "expression: `%s`", exp.toChars());
                        }
                        nextToken();
                    }
                }
                s = new AST.ExpStatement(loc, exp);
                break;
            }
        case TOK.static_:
            {
                // Look ahead to see if it's static assert() or static if()
                const tv = peekNext();
                if (tv == TOK.assert_)
                {
                    s = new AST.StaticAssertStatement(parseStaticAssert());
                    break;
                }
                if (tv == TOK.if_)
                {
                    cond = parseStaticIfCondition();
                    goto Lcondition;
                }
                if (tv == TOK.foreach_ || tv == TOK.foreach_reverse_)
                {
                    s = parseForeach!(AST.StaticForeachStatement)(loc, null);
                    if (flags & ParseStatementFlags.scope_)
                        s = new AST.ScopeStatement(loc, s, token.loc);
                    break;
                }
                if (tv == TOK.import_)
                {
                    AST.Dsymbols* imports = parseImport();
                    s = new AST.ImportStatement(loc, imports);
                    if (flags & ParseStatementFlags.scope_)
                        s = new AST.ScopeStatement(loc, s, token.loc);
                    break;
                }
                goto Ldeclaration;
            }
        case TOK.final_:
            if (peekNext() == TOK.switch_)
            {
                nextToken();
                isfinal = true;
                goto Lswitch;
            }
            goto Ldeclaration;

        case TOK.wchar_:
        case TOK.dchar_:
        case TOK.bool_:
        case TOK.char_:
        case TOK.int8:
        case TOK.uns8:
        case TOK.int16:
        case TOK.uns16:
        case TOK.int32:
        case TOK.uns32:
        case TOK.int64:
        case TOK.uns64:
        case TOK.int128:
        case TOK.uns128:
        case TOK.float32:
        case TOK.float64:
        case TOK.float80:
        case TOK.imaginary32:
        case TOK.imaginary64:
        case TOK.imaginary80:
        case TOK.complex32:
        case TOK.complex64:
        case TOK.complex80:
        case TOK.void_:
            // bug 7773: int.max is always a part of expression
            if (peekNext() == TOK.dot)
                goto Lexp;
            if (peekNext() == TOK.leftParenthesis)
                goto Lexp;
            goto case;

        // FunctionLiteral `auto ref (`
        case TOK.auto_:
            if (peekNext() == TOK.ref_ && peekNext2() == TOK.leftParenthesis)
                goto Lexp;
            goto Ldeclaration;
        case TOK.ref_:
            if (peekNext() == TOK.leftParenthesis)
                goto Lexp;
            goto Ldeclaration;

        case TOK.alias_:
        case TOK.const_:
        case TOK.abstract_:
        case TOK.extern_:
        case TOK.align_:
        case TOK.immutable_:
        case TOK.shared_:
        case TOK.inout_:
        case TOK.deprecated_:
        case TOK.nothrow_:
        case TOK.pure_:
        case TOK.gshared:
        case TOK.at:
        case TOK.struct_:
        case TOK.union_:
        case TOK.class_:
        case TOK.interface_:
        Ldeclaration:
            {
                AST.Dsymbols* a = parseDeclarations(false, null, null);
                if (a.length > 1)
                {
                    auto as = new AST.Statements();
                    as.reserve(a.length);
                    foreach (i; 0 .. a.length)
                    {
                        AST.Dsymbol d = (*a)[i];
                        s = new AST.ExpStatement(loc, d);
                        as.push(s);
                    }
                    s = new AST.CompoundDeclarationStatement(loc, as);
                }
                else if (a.length == 1)
                {
                    AST.Dsymbol d = (*a)[0];
                    s = new AST.ExpStatement(loc, d);
                }
                else
                    s = new AST.ExpStatement(loc, cast(AST.Expression)null);
                if (flags & ParseStatementFlags.scope_)
                    s = new AST.ScopeStatement(loc, s, token.loc);
                break;
            }
        case TOK.enum_:
            {
                /* Determine if this is a manifest constant declaration,
                 * or a conventional enum.
                 */
                AST.Dsymbol d;
                const tv = peekNext();
                if (tv == TOK.leftCurly || tv == TOK.colon)
                    d = parseEnum();
                else if (tv != TOK.identifier)
                    goto Ldeclaration;
                else
                {
                    const nextv = peekNext2();
                    if (nextv == TOK.leftCurly || nextv == TOK.colon || nextv == TOK.semicolon)
                        d = parseEnum();
                    else
                        goto Ldeclaration;
                }
                s = new AST.ExpStatement(loc, d);
                if (flags & ParseStatementFlags.scope_)
                    s = new AST.ScopeStatement(loc, s, token.loc);
                break;
            }
        case TOK.mixin_:
            {
                if (isDeclaration(&token, NeedDeclaratorId.mustIfDstyle, TOK.reserved, null))
                    goto Ldeclaration;
                const tv = peekNext();
                if (tv == TOK.leftParenthesis)
                {
                    // mixin(string)
                    AST.Expression e = parseAssignExp();
                    check(TOK.semicolon, "mixin");
                    if (e.op == EXP.mixin_)
                    {
                        AST.MixinExp cpe = cast(AST.MixinExp)e;
                        s = new AST.MixinStatement(loc, cpe.exps);
                    }
                    else
                    {
                        s = new AST.ExpStatement(loc, e);
                    }
                    break;
                }
                else if (tv == TOK.template_)
                {
                    // mixin template
                    nextToken();
                    AST.Dsymbol d = parseTemplateDeclaration(true);
                    s = new AST.ExpStatement(loc, d);
                    break;
                }
                AST.Dsymbol d = parseMixin();
                s = new AST.ExpStatement(loc, d);
                if (flags & ParseStatementFlags.scope_)
                    s = new AST.ScopeStatement(loc, s, token.loc);
                break;
            }
        case TOK.leftCurly:
            {
                const lcLoc = token.loc;
                const lookingForElseSave = lookingForElse;
                lookingForElse = Loc.initial;

                nextToken();
                //if (token.value == TOK.semicolon)
                //    error("use `{ }` for an empty statement, not `;`");
                auto statements = new AST.Statements();
                while (token.value != TOK.rightCurly && token.value != TOK.endOfFile)
                {
                    statements.push(parseStatement(ParseStatementFlags.curlyScope | ParseStatementFlags.semiOk));
                }
                if (endPtr)
                    *endPtr = token.ptr;
                endloc = token.loc;
                if (pEndloc)
                {
                    *pEndloc = token.loc;
                    pEndloc = null; // don't set it again
                }
                s = new AST.CompoundStatement(loc, statements);
                if (flags & (ParseStatementFlags.scope_ | ParseStatementFlags.curlyScope))
                    s = new AST.ScopeStatement(loc, s, token.loc);
                if (token.value != TOK.rightCurly)
                {
                    error(token.loc, "matching `}` expected following compound statement, not `%s`",
                        token.toChars());
                    eSink.errorSupplemental(lcLoc, "unmatched `{`");
                }
                else
                    nextToken();
                lookingForElse = lookingForElseSave;
                break;
            }
        case TOK.while_:
            {
                nextToken();
                check(TOK.leftParenthesis);
                auto param = parseAssignCondition();
                auto condition = parseExpression();
                closeCondition("while", param, condition);

                Loc endloc;
                AST.Statement _body = parseStatement(ParseStatementFlags.scope_, null, &endloc);
                s = new AST.WhileStatement(loc, condition, _body, endloc, param);
                break;
            }
        case TOK.semicolon:
            if (!(flags & ParseStatementFlags.semiOk))
            {
                error("use `{ }` for an empty statement, not `;`");
            }
            nextToken();
            s = new AST.ExpStatement(loc, cast(AST.Expression)null);
            break;

        case TOK.do_:
            {
                AST.Statement _body;

                nextToken();
                const lookingForElseSave = lookingForElse;
                lookingForElse = Loc.initial;
                _body = parseStatement(ParseStatementFlags.scope_);
                lookingForElse = lookingForElseSave;
                check(TOK.while_);
                check(TOK.leftParenthesis);
                auto condition = parseExpression();
                closeCondition("do .. while", null, condition);
                if (token.value == TOK.semicolon)
                    nextToken();
                else
                    error("terminating `;` required after do-while statement");
                s = new AST.DoStatement(loc, _body, condition, token.loc);
                break;
            }
        case TOK.for_:
            {
                AST.Statement _init;
                AST.Expression condition;
                AST.Expression increment;

                nextToken();
                check(TOK.leftParenthesis);
                if (token.value == TOK.semicolon)
                {
                    _init = null;
                    nextToken();
                }
                else
                {
                    const lookingForElseSave = lookingForElse;
                    lookingForElse = Loc.initial;
                    _init = parseStatement(0);
                    lookingForElse = lookingForElseSave;
                }
                if (token.value == TOK.semicolon)
                {
                    condition = null;
                    nextToken();
                }
                else
                {
                    condition = parseExpression();
                    check(TOK.semicolon, "`for` condition");
                }
                if (token.value == TOK.rightParenthesis)
                {
                    increment = null;
                    nextToken();
                }
                else
                {
                    increment = parseExpression();
                    check(TOK.rightParenthesis);
                }
                Loc endloc;
                AST.Statement _body = parseStatement(ParseStatementFlags.scope_, null, &endloc);
                s = new AST.ForStatement(loc, _init, condition, increment, _body, endloc);
                break;
            }
        case TOK.foreach_:
        case TOK.foreach_reverse_:
            {
                s = parseForeach!(AST.Statement)(loc, null);
                break;
            }
        case TOK.if_:
            {
                nextToken();
                check(TOK.leftParenthesis);
                auto param = parseAssignCondition();
                auto condition = parseExpression();
                closeCondition("if", param, condition);

                {
                    const lookingForElseSave = lookingForElse;
                    lookingForElse = loc;
                    ifbody = parseStatement(ParseStatementFlags.scope_);
                    lookingForElse = lookingForElseSave;
                }
                if (token.value == TOK.else_)
                {
                    const elseloc = token.loc;
                    nextToken();
                    elsebody = parseStatement(ParseStatementFlags.scope_);
                    checkDanglingElse(elseloc);
                }
                else
                    elsebody = null;
                if (condition && ifbody)
                    s = new AST.IfStatement(loc, param, condition, ifbody, elsebody, token.loc);
                else
                    s = null; // don't propagate parsing errors
                break;
            }

        case TOK.else_:
            error("found `else` without a corresponding `if`, `version` or `debug` statement");
            goto Lerror;

        case TOK.scope_:
            if (peekNext() != TOK.leftParenthesis)
                goto Ldeclaration; // scope used as storage class
            nextToken();
            check(TOK.leftParenthesis);
            if (token.value != TOK.identifier)
            {
                error("scope identifier expected");
                goto Lerror;
            }
            else
            {
                TOK t = TOK.onScopeExit;
                Identifier id = token.ident;
                if (id == Id.exit)
                    t = TOK.onScopeExit;
                else if (id == Id.failure)
                    t = TOK.onScopeFailure;
                else if (id == Id.success)
                    t = TOK.onScopeSuccess;
                else
                    error("valid scope identifiers are `exit`, `failure`, or `success`, not `%s`", id.toChars());
                nextToken();
                check(TOK.rightParenthesis);
                AST.Statement st = parseStatement(ParseStatementFlags.scope_);
                s = new AST.ScopeGuardStatement(loc, t, st);
                break;
            }

        case TOK.debug_:
            nextToken();
            if (token.value == TOK.assign)
            {
                if (auto ds = parseDebugSpecification())
                    eSink.error(ds.loc, "%s `%s` declaration must be at module level", ds.kind, ds.toPrettyChars);

                break;
            }
            cond = parseDebugCondition();
            goto Lcondition;

        case TOK.version_:
            nextToken();
            if (token.value == TOK.assign)
            {
                if (auto vs = parseVersionSpecification())
                    eSink.error(vs.loc, "%s `%s` declaration must be at module level", vs.kind, vs.toPrettyChars);

                break;
            }
            cond = parseVersionCondition();
            goto Lcondition;

        Lcondition:
            {
                const lookingForElseSave = lookingForElse;
                lookingForElse = loc;
                ifbody = parseStatement(0);
                lookingForElse = lookingForElseSave;
            }
            elsebody = null;
            if (token.value == TOK.else_)
            {
                const elseloc = token.loc;
                nextToken();
                elsebody = parseStatement(0);
                checkDanglingElse(elseloc);
            }
            s = new AST.ConditionalStatement(loc, cond, ifbody, elsebody);
            if (flags & ParseStatementFlags.scope_)
                s = new AST.ScopeStatement(loc, s, token.loc);
            break;

        case TOK.pragma_:
            {
                Identifier ident;
                AST.Expressions* args = null;
                AST.Statement _body;

                nextToken();
                check(TOK.leftParenthesis);
                if (token.value != TOK.identifier)
                {
                    error("`pragma(identifier)` expected");
                    goto Lerror;
                }
                ident = token.ident;
                nextToken();
                if (token.value == TOK.comma && peekNext() != TOK.rightParenthesis)
                    args = parseArguments(); // pragma(identifier, args...);
                else
                    check(TOK.rightParenthesis); // pragma(identifier);
                if (token.value == TOK.semicolon)
                {
                    nextToken();
                    _body = null;
                }
                else
                    _body = parseStatement(0);
                s = new AST.PragmaStatement(loc, ident, args, _body);
                break;
            }
        case TOK.switch_:
            isfinal = false;
            goto Lswitch;

        Lswitch:
            {
                nextToken();
                check(TOK.leftParenthesis);
                auto param = parseAssignCondition();
                AST.Expression condition = parseExpression();
                closeCondition("switch", null, condition);
                AST.Statement _body = parseStatement(ParseStatementFlags.scope_);
                s = new AST.SwitchStatement(loc, param, condition, _body, isfinal, token.loc);
                break;
            }
        case TOK.case_:
            {
                AST.Expression exp;
                AST.Expressions cases; // array of Expression's
                AST.Expression last = null;

                nextToken();
                do
                {
                    exp = parseAssignExp();
                    cases.push(exp);
                    if (token.value != TOK.comma)
                        break;
                    nextToken(); //comma
                }
                while (token.value != TOK.colon && token.value != TOK.endOfFile);
                check(TOK.colon);

                /* case exp: .. case last:
                 */
                if (token.value == TOK.slice)
                {
                    if (cases.length > 1)
                        error("only one `case` allowed for start of case range");
                    nextToken();
                    check(TOK.case_);
                    last = parseAssignExp();
                    check(TOK.colon);
                }

                if (flags & ParseStatementFlags.curlyScope)
                {
                    auto statements = new AST.Statements();
                    while (token.value != TOK.case_ && token.value != TOK.default_ && token.value != TOK.endOfFile && token.value != TOK.rightCurly)
                    {
                        auto cur = parseStatement(ParseStatementFlags.curlyScope);
                        statements.push(cur);

                        // https://issues.dlang.org/show_bug.cgi?id=21739
                        // Stop at the last break s.t. the following non-case statements are
                        // not merged into the current case. This can happen for
                        // case 1: ... break;
                        // debug { case 2: ... }
                        if (cur && cur.isBreakStatement())
                            break;
                    }
                    s = new AST.CompoundStatement(loc, statements);
                }
                else
                {
                    s = parseStatement(0);
                }
                s = new AST.ScopeStatement(loc, s, token.loc);

                if (last)
                {
                    s = new AST.CaseRangeStatement(loc, exp, last, s);
                }
                else
                {
                    // Keep cases in order by building the case statements backwards
                    for (size_t i = cases.length; i; i--)
                    {
                        exp = cases[i - 1];
                        s = new AST.CaseStatement(loc, exp, s);
                    }
                }
                break;
            }
        case TOK.default_:
            {
                nextToken();
                check(TOK.colon);

                if (flags & ParseStatementFlags.curlyScope)
                {
                    auto statements = new AST.Statements();
                    while (token.value != TOK.case_ && token.value != TOK.default_ && token.value != TOK.endOfFile && token.value != TOK.rightCurly)
                    {
                        statements.push(parseStatement(ParseStatementFlags.curlyScope));
                    }
                    s = new AST.CompoundStatement(loc, statements);
                }
                else
                    s = parseStatement(0);
                s = new AST.ScopeStatement(loc, s, token.loc);
                s = new AST.DefaultStatement(loc, s);
                break;
            }
        case TOK.return_:
            {
                AST.Expression exp;
                nextToken();
                exp = token.value == TOK.semicolon ? null : parseExpression();
                check(TOK.semicolon, "`return` statement");
                s = new AST.ReturnStatement(loc, exp);
                break;
            }
        case TOK.break_:
            {
                Identifier ident;
                nextToken();
                ident = null;
                if (token.value == TOK.identifier)
                {
                    ident = token.ident;
                    nextToken();
                }
                check(TOK.semicolon, "`break` statement");
                s = new AST.BreakStatement(loc, ident);
                break;
            }
        case TOK.continue_:
            {
                Identifier ident;
                nextToken();
                ident = null;
                if (token.value == TOK.identifier)
                {
                    ident = token.ident;
                    nextToken();
                }
                check(TOK.semicolon, "`continue` statement");
                s = new AST.ContinueStatement(loc, ident);
                break;
            }
        case TOK.goto_:
            {
                Identifier ident;
                nextToken();
                if (token.value == TOK.default_)
                {
                    nextToken();
                    s = new AST.GotoDefaultStatement(loc);
                }
                else if (token.value == TOK.case_)
                {
                    AST.Expression exp = null;
                    nextToken();
                    if (token.value != TOK.semicolon)
                        exp = parseExpression();
                    s = new AST.GotoCaseStatement(loc, exp);
                }
                else
                {
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected following `goto`");
                        ident = null;
                    }
                    else
                    {
                        ident = token.ident;
                        nextToken();
                    }
                    s = new AST.GotoStatement(loc, ident);
                }
                check(TOK.semicolon, "`goto` statement");
                break;
            }
        case TOK.synchronized_:
            {
                AST.Expression exp;
                AST.Statement _body;

                Token* t = peek(&token);
                if (skipAttributes(t, &t) && t.value == TOK.class_)
                    goto Ldeclaration;

                nextToken();
                if (token.value == TOK.leftParenthesis)
                {
                    nextToken();
                    exp = parseExpression();
                    closeCondition("synchronized", null, exp);
                }
                else
                    exp = null;
                _body = parseStatement(ParseStatementFlags.scope_);
                s = new AST.SynchronizedStatement(loc, exp, _body);
                break;
            }
        case TOK.with_:
            {
                AST.Expression exp;
                AST.Statement _body;
                Loc endloc = loc;

                nextToken();
                check(TOK.leftParenthesis);
                exp = parseExpression();
                closeCondition("with", null, exp);
                _body = parseStatement(ParseStatementFlags.scope_, null, &endloc);
                s = new AST.WithStatement(loc, exp, _body, endloc);
                break;
            }
        case TOK.try_:
            {
                AST.Statement _body;
                AST.Catches* catches = null;
                AST.Statement finalbody = null;

                nextToken();
                const lookingForElseSave = lookingForElse;
                lookingForElse = Loc.initial;
                _body = parseStatement(ParseStatementFlags.scope_);
                lookingForElse = lookingForElseSave;
                while (token.value == TOK.catch_)
                {
                    AST.Statement handler;
                    AST.Catch c;
                    AST.Type t;
                    Identifier id;
                    const catchloc = token.loc;

                    nextToken();
                    if (token.value != TOK.leftParenthesis)
                    {
                        deprecation("`catch` statement without an exception specification is deprecated");
                        deprecationSupplemental("use `catch(Throwable)` for old behavior");
                        t = null;
                        id = null;
                    }
                    else
                    {
                        check(TOK.leftParenthesis);
                        id = null;
                        t = parseType(&id);
                        check(TOK.rightParenthesis);
                    }
                    handler = parseStatement(0);
                    c = new AST.Catch(catchloc, t, id, handler);
                    if (!catches)
                        catches = new AST.Catches();
                    catches.push(c);
                }

                if (token.value == TOK.finally_)
                {
                    nextToken();
                    finalbody = parseStatement(ParseStatementFlags.scope_);
                }

                s = _body;
                if (!catches && !finalbody)
                    error("`catch` or `finally` expected following `try`");
                else
                {
                    if (catches)
                        s = new AST.TryCatchStatement(loc, _body, catches);
                    if (finalbody)
                        s = new AST.TryFinallyStatement(loc, s, finalbody);
                }
                break;
            }
        case TOK.throw_:
            {
                AST.Expression exp;
                nextToken();
                exp = parseExpression();
                check(TOK.semicolon, "`throw` statement");
                s = new AST.ThrowStatement(loc, exp);
                break;
            }

        case TOK.asm_:
            s = parseAsm(false);
            break;

        case TOK.import_:
            {
                /* https://issues.dlang.org/show_bug.cgi?id=16088
                 *
                 * At this point it can either be an
                 * https://dlang.org/spec/grammar.html#ImportExpression
                 * or an
                 * https://dlang.org/spec/grammar.html#ImportDeclaration.
                 * See if the next token after `import` is a `(`; if so,
                 * then it is an import expression.
                 */
                if (peekNext() == TOK.leftParenthesis)
                {
                    AST.Expression e = parseExpression();
                    check(TOK.semicolon, "`import` Expression");
                    s = new AST.ExpStatement(loc, e);
                }
                else
                {
                    AST.Dsymbols* imports = parseImport();
                    s = new AST.ImportStatement(loc, imports);
                    if (flags & ParseStatementFlags.scope_)
                        s = new AST.ScopeStatement(loc, s, token.loc);
                }
                break;
            }
        case TOK.template_:
            {
                AST.Dsymbol d = parseTemplateDeclaration();
                s = new AST.ExpStatement(loc, d);
                break;
            }
        default:
            error("found `%s` instead of statement", token.toChars());
            goto Lerror;

        Lerror:
            while (token.value != TOK.rightCurly && token.value != TOK.semicolon && token.value != TOK.endOfFile)
                nextToken();
            if (token.value == TOK.semicolon)
                nextToken();
            s = new AST.ErrorStatement;
            break;
        }
        if (pEndloc)
            *pEndloc = prevloc;
        return s;
    }


    private  AST.ExpInitializer parseExpInitializer(Loc loc)
    {
        auto ae = parseAssignExp();
        return new AST.ExpInitializer(loc, ae);
    }

    private AST.Initializer parseStructInitializer(Loc loc)
    {
        /* Scan ahead to discern between a struct initializer and
         * parameterless function literal.
         *
         * We'll scan the topmost curly bracket level for statement-related
         * tokens, thereby ruling out a struct initializer.  (A struct
         * initializer which itself contains function literals may have
         * statements at nested curly bracket levels.)
         *
         * It's important that this function literal check not be
         * pendantic, otherwise a function having the slightest syntax
         * error would emit confusing errors when we proceed to parse it
         * as a struct initializer.
         *
         * The following two ambiguous cases will be treated as a struct
         * initializer (best we can do without type info):
         *     {}
         *     {{statements...}}  - i.e. it could be struct initializer
         *        with one function literal, or function literal having an
         *        extra level of curly brackets
         * If a function literal is intended in these cases (unlikely),
         * source can use a more explicit function literal syntax
         * (e.g. prefix with "()" for empty parameter list).
         */
        int braces = 1;
        int parens = 0;
        for (auto t = peek(&token); 1; t = peek(t))
        {
            switch (t.value)
            {
                case TOK.leftParenthesis:
                    parens++;
                    continue;
                case TOK.rightParenthesis:
                    parens--;
                    continue;
                // https://issues.dlang.org/show_bug.cgi?id=21163
                // lambda params can have the `scope` storage class, e.g
                // `S s = { (scope Type Id){} }`
                case TOK.scope_:
                    if (!parens) goto case;
                    continue;
                /* Look for a semicolon or keyword of statements which don't
                 * require a semicolon (typically containing BlockStatement).
                 * Tokens like "else", "catch", etc. are omitted where the
                 * leading token of the statement is sufficient.
                 */
                case TOK.asm_:
                case TOK.class_:
                case TOK.debug_:
                case TOK.enum_:
                case TOK.if_:
                case TOK.interface_:
                case TOK.pragma_:
                case TOK.semicolon:
                case TOK.struct_:
                case TOK.switch_:
                case TOK.synchronized_:
                case TOK.try_:
                case TOK.union_:
                case TOK.version_:
                case TOK.while_:
                case TOK.with_:
                    if (braces == 1)
                        return parseExpInitializer(loc);
                    continue;

                case TOK.leftCurly:
                    braces++;
                    continue;

                case TOK.rightCurly:
                    if (--braces == 0)
                        break;
                    continue;

                case TOK.endOfFile:
                    break;

                default:
                    continue;
            }
            break;
        }

        auto _is = new AST.StructInitializer(loc);
        bool commaExpected = false;
        nextToken();
        while (1)
        {
            switch (token.value)
            {
                case TOK.identifier:
                {
                    if (commaExpected)
                        error("comma expected separating field initializers");
                    const t = peek(&token);
                    Identifier id;
                    if (t.value == TOK.colon)
                    {
                        id = token.ident;
                        nextToken();
                        nextToken(); // skip over ':'
                    }
                    auto value = parseInitializer();
                    _is.addInit(id, value);
                    commaExpected = true;
                    continue;
                }
                case TOK.comma:
                    if (!commaExpected)
                        error("expression expected, not `,`");
                    nextToken();
                    commaExpected = false;
                    continue;

                case TOK.rightCurly: // allow trailing comma's
                    nextToken();
                    break;

                case TOK.endOfFile:
                    error("found end of file instead of initializer");
                    break;

                default:
                    if (commaExpected)
                        error("comma expected separating field initializers");
                    const t = peek(&token);
                    if (t.value == TOK.colon)
                    {
                        error("incorrect syntax for associative array, expected `[]`, found `{}`");
                        while (token.value != TOK.rightCurly && token.value != TOK.endOfFile)
                        {
                            nextToken();
                        }
                        if (token.value == TOK.rightCurly)
                        {
                            nextToken();
                        }
                        break;
                    }
                    auto value = parseInitializer();
                    _is.addInit(null, value);
                    commaExpected = true;
                    continue;
            }
            break;
        }
        return _is;

    }

    /*****************************************
     * Parse initializer for variable declaration.
     */
    private AST.Initializer parseInitializer()
    {
        const loc = token.loc;

        switch (token.value)
        {
        case TOK.leftCurly:
            return parseStructInitializer(loc);

        case TOK.leftBracket:
            /* Scan ahead to see if it is an array initializer or
             * an expression.
             * If it ends with a ';' ',' or ']', it is an array initializer.
             */
            int brackets = 1;
            for (auto t = peek(&token); 1; t = peek(t))
            {
                switch (t.value)
                {
                case TOK.leftBracket:
                    brackets++;
                    continue;

                case TOK.rightBracket:
                    if (--brackets == 0)
                    {
                        t = peek(t);
                        if (t.value != TOK.semicolon && t.value != TOK.comma && t.value != TOK.rightBracket && t.value != TOK.rightCurly)
                            return parseExpInitializer(loc);
                        break;
                    }
                    continue;

                case TOK.endOfFile:
                    break;

                default:
                    continue;
                }
                break;
            }

            auto ia = new AST.ArrayInitializer(loc);
            bool commaExpected = false;

            nextToken();
            while (1)
            {
                switch (token.value)
                {
                default:
                    if (commaExpected)
                    {
                        error("comma expected separating array initializers, not `%s`", token.toChars());
                        nextToken();
                        break;
                    }
                    auto e = parseAssignExp();
                    if (!e)
                        break;

                    AST.Initializer value;
                    if (token.value == TOK.colon)
                    {
                        nextToken();
                        value = parseInitializer();
                    }
                    else
                    {
                        value = new AST.ExpInitializer(e.loc, e);
                        e = null;
                    }
                    ia.addInit(e, value);
                    commaExpected = true;
                    continue;

                case TOK.leftCurly:
                case TOK.leftBracket:
                    if (commaExpected)
                        error("comma expected separating array initializers, not `%s`", token.toChars());
                    auto value = parseInitializer();
                    AST.Expression e;

                    if (token.value == TOK.colon)
                    {
                        nextToken();
                        if (auto ei = value.isExpInitializer())
                        {
                            e = ei.exp;
                            value = parseInitializer();
                        }
                        else
                            error("initializer expression expected following colon, not `%s`", token.toChars());
                    }
                    ia.addInit(e, value);
                    commaExpected = true;
                    continue;

                case TOK.comma:
                    if (!commaExpected)
                        error("expression expected, not `,`");
                    nextToken();
                    commaExpected = false;
                    continue;

                case TOK.rightBracket: // allow trailing comma's
                    nextToken();
                    break;

                case TOK.endOfFile:
                    error("found `%s` instead of array initializer", token.toChars());
                    break;
                }
                break;
            }
            return ia;

        case TOK.void_:
            const tv = peekNext();
            if (tv == TOK.semicolon || tv == TOK.comma)
            {
                nextToken();
                return new AST.VoidInitializer(loc);
            }
            return parseExpInitializer(loc);

        default:
            return parseExpInitializer(loc);
        }
    }

    /********************
     * Parse inline assembler block.
     * Enters with token on the `asm`.
     * https://dlang.org/spec/iasm.html
     *
     * AsmStatement:
     *   asm FunctionAttributes(opt) { AsmInstructionListopt }
     * AsmInstructionList:
     *   AsmInstruction ;
     *   AsmInstruction ; AsmInstruction
     *
     * Params:
     *   endOfLine = true if EOL means end of asm statement
     * Returns:
     *   inline assembler block as a Statement
     */
    AST.Statement parseAsm(bool endOfLine)
    {
        // Parse the asm block into a sequence of AsmStatements,
        // each AsmStatement is one instruction.
        // Separate out labels.
        // Defer parsing of AsmStatements until semantic processing.

        const loc = token.loc;
        Loc labelloc;

        nextToken();
        STC stc = parsePostfix(STC.none, null);  // optional FunctionAttributes
        if (stc & (STC.const_ | STC.immutable_ | STC.shared_ | STC.wild))
            error("`const`/`immutable`/`shared`/`inout` attributes are not allowed on `asm` blocks");

        check(TOK.leftCurly);
        Token* toklist = null;
        Token** ptoklist = &toklist;
        Identifier label = null;
        auto statements = new AST.Statements();
        size_t nestlevel = 0;
        while (1)
        {
            if (endOfLine)
                nextDefineLine();
            switch (token.value)
            {
            case TOK.identifier:
                if (!toklist)
                {
                    // Look ahead to see if it is a label
                    if (peekNext() == TOK.colon)
                    {
                        // It's a label
                        label = token.ident;
                        labelloc = token.loc;
                        nextToken();
                        nextToken();
                        continue;
                    }
                }
                goto default;

            case TOK.leftCurly:
                ++nestlevel;
                goto default;

            case TOK.rightCurly:
                if (nestlevel > 0)
                {
                    --nestlevel;
                    goto default;
                }
                if (toklist || label)
                {
                    error("`asm` statements must end in `;`");
                }
                break;

            case TOK.endOfLine:
                nextDefineLine();
                goto case;

            case TOK.semicolon:
                if (nestlevel != 0)
                    error("mismatched number of curly brackets");

                if (toklist || label)
                {
                    // Create AsmStatement from list of tokens we've saved
                    AST.AsmStatement as = new AST.AsmStatement(token.loc, toklist);
                    as.caseSensitive = !endOfLine;
                    AST.Statement s = as;
                    toklist = null;
                    ptoklist = &toklist;
                    if (label)
                    {
                        s = new AST.LabelStatement(labelloc, label, s);
                        label = null;
                    }
                    statements.push(s);
                }
                nextToken();
                continue;

            case TOK.endOfFile:
                /* { */
                error("matching `}` expected, not end of file");
                break;

            case TOK.colonColon:  // treat as two separate : tokens for iasmgcc
                *ptoklist = allocateToken();
                memcpy(*ptoklist, &token, Token.sizeof);
                (*ptoklist).value = TOK.colon;
                ptoklist = &(*ptoklist).next;

                *ptoklist = allocateToken();
                memcpy(*ptoklist, &token, Token.sizeof);
                (*ptoklist).value = TOK.colon;
                ptoklist = &(*ptoklist).next;

                *ptoklist = null;
                nextToken();
                continue;

            default:
                *ptoklist = allocateToken();
                memcpy(*ptoklist, &token, Token.sizeof);
                ptoklist = &(*ptoklist).next;
                *ptoklist = null;
                nextToken();
                continue;
            }
            break;
        }
        nextToken();
        if (token.value == TOK.endOfLine)
            nextToken();
        auto s = new AST.CompoundAsmStatement(loc, statements, stc);
        return s;
    }

    /**********************************
     * Issue error if the current token is not `value`,
     * advance to next token.
     * Params:
     *  loc = location for error message
     *  value = token value to compare with
     */
    void check(Loc loc, TOK value)
    {
        if (token.value != value)
            error(loc, "found `%s` when expecting `%s`", token.toChars(), Token.toChars(value));
        nextToken();
    }

    /**********************************
     * Issue error if the current token is not `value`,
     * advance to next token.
     * Params:
     *  value = token value to compare with
     */
    void check(TOK value)
    {
        check(token.loc, value);
    }

    /**********************************
     * Issue error if the current token is not `value`,
     * advance to next token.
     * Params:
     *  value = token value to compare with
     *  string = for error message
     */
    void check(TOK value, const(char)* string)
    {
        if (token.value != value)
            error(token.loc, "found `%s` when expecting `%s` following %s", token.toChars(), Token.toChars(value), string);
        nextToken();
    }

    private void checkParens(TOK value, AST.Expression e)
    {
        if (precedence[e.op] == PREC.rel && !e.parens)
            error(e.loc, "`%s` must be surrounded by parentheses when next to operator `%s`", e.toChars(), Token.toChars(value));
    }

    ///
    enum NeedDeclaratorId
    {
        no,             // Declarator part must have no identifier
        opt,            // Declarator part identifier is optional
        must,           // Declarator part must have identifier
        mustIfDstyle,   // Declarator part must have identifier, but don't recognize old C-style syntax
    }

    /************************************
     * Determine if the scanner is sitting on the start of a declaration.
     * Params:
     *      t       = current token of the scanner
     *      needId  = flag with additional requirements for a declaration
     *      endtok  = ending token
     *      pt      = will be set ending token (if not null)
     * Output:
     *      true if the token `t` is a declaration, false otherwise
     */
    private bool isDeclaration(Token* t, NeedDeclaratorId needId, TOK endtok, Token** pt)
    {
        //printf("isDeclaration(needId = %d)\n", needId);
        int haveId = 0;
        int haveTpl = 0;

        while (1)
        {
            if ((t.value == TOK.const_ || t.value == TOK.immutable_ || t.value == TOK.inout_ || t.value == TOK.shared_) && peek(t).value != TOK.leftParenthesis)
            {
                /* const type
                 * immutable type
                 * shared type
                 * wild type
                 */
                t = peek(t);
                continue;
            }
            break;
        }

        if (!isBasicType(&t))
        {
            goto Lisnot;
        }
        if (!isDeclarator(&t, &haveId, &haveTpl, endtok, needId != NeedDeclaratorId.mustIfDstyle))
            goto Lisnot;
        // needed for `__traits(compiles, arr[0] = 0)`
        if (!haveId && t.value == TOK.assign)
            goto Lisnot;
        if ((needId == NeedDeclaratorId.no && !haveId) ||
            (needId == NeedDeclaratorId.opt) ||
            (needId == NeedDeclaratorId.must && haveId) ||
            (needId == NeedDeclaratorId.mustIfDstyle && haveId))
        {
            if (pt)
                *pt = t;
            goto Lis;
        }
        goto Lisnot;

    Lis:
        //printf("\tis declaration, t = %s\n", t.toChars());
        return true;

    Lisnot:
        //printf("\tis not declaration\n");
        return false;
    }

    // pt = test token. If found, pt is set to the token after BasicType
    private bool isBasicType(Token** pt)
    {
        // This code parallels parseBasicType()
        Token* t = *pt;
        switch (t.value)
        {
        case TOK.wchar_:
        case TOK.dchar_:
        case TOK.bool_:
        case TOK.char_:
        case TOK.int8:
        case TOK.uns8:
        case TOK.int16:
        case TOK.uns16:
        case TOK.int32:
        case TOK.uns32:
        case TOK.int64:
        case TOK.uns64:
        case TOK.int128:
        case TOK.uns128:
        case TOK.float32:
        case TOK.float64:
        case TOK.float80:
        case TOK.imaginary32:
        case TOK.imaginary64:
        case TOK.imaginary80:
        case TOK.complex32:
        case TOK.complex64:
        case TOK.complex80:
        case TOK.void_:
            t = peek(t);
            break;

        case TOK.identifier:
        L5:
            t = peek(t);
            if (t.value == TOK.not)
            {
                goto L4;
            }
            goto L3;
            while (1)
            {
            L2:
                t = peek(t);
            L3:
                if (t.value == TOK.dot)
                {
                Ldot:
                    t = peek(t);
                    if (t.value != TOK.identifier)
                        goto Lfalse;
                    t = peek(t);
                    if (t.value != TOK.not)
                        goto L3;
                L4:
                    /* Seen a !
                     * Look for:
                     * !( args ), !identifier, etc.
                     */
                    t = peek(t);
                    switch (t.value)
                    {
                    case TOK.identifier:
                        goto L5;

                    case TOK.leftParenthesis:
                        if (!skipParens(t, &t))
                            goto Lfalse;
                        goto L3;

                    case TOK.wchar_:
                    case TOK.dchar_:
                    case TOK.bool_:
                    case TOK.char_:
                    case TOK.int8:
                    case TOK.uns8:
                    case TOK.int16:
                    case TOK.uns16:
                    case TOK.int32:
                    case TOK.uns32:
                    case TOK.int64:
                    case TOK.uns64:
                    case TOK.int128:
                    case TOK.uns128:
                    case TOK.float32:
                    case TOK.float64:
                    case TOK.float80:
                    case TOK.imaginary32:
                    case TOK.imaginary64:
                    case TOK.imaginary80:
                    case TOK.complex32:
                    case TOK.complex64:
                    case TOK.complex80:
                    case TOK.void_:
                    case TOK.int32Literal:
                    case TOK.uns32Literal:
                    case TOK.int64Literal:
                    case TOK.uns64Literal:
                    case TOK.int128Literal:
                    case TOK.uns128Literal:
                    case TOK.float32Literal:
                    case TOK.float64Literal:
                    case TOK.float80Literal:
                    case TOK.imaginary32Literal:
                    case TOK.imaginary64Literal:
                    case TOK.imaginary80Literal:
                    case TOK.null_:
                    case TOK.true_:
                    case TOK.false_:
                    case TOK.charLiteral:
                    case TOK.wcharLiteral:
                    case TOK.dcharLiteral:
                    case TOK.string_:
                    case TOK.interpolated:
                    case TOK.hexadecimalString:
                    case TOK.file:
                    case TOK.fileFullPath:
                    case TOK.line:
                    case TOK.moduleString:
                    case TOK.functionString:
                    case TOK.prettyFunction:
                        goto L2;

                    default:
                        goto Lfalse;
                    }
                }
                break;
            }
            break;

        case TOK.dot:
            goto Ldot;

        case TOK.typeof_:
        case TOK.vector:
        case TOK.mixin_:
            /* typeof(exp).identifier...
             */
            t = peek(t);
            if (!skipParens(t, &t))
                goto Lfalse;
            goto L3;

        case TOK.traits:
            // __traits(getMember
            t = peek(t);
            if (t.value != TOK.leftParenthesis)
                goto Lfalse;
            auto lp = t;
            t = peek(t);
            if (t.value != TOK.identifier || t.ident != Id.getMember)
                goto Lfalse;
            if (!skipParens(lp, &lp))
                goto Lfalse;
            // we are in a lookup for decl VS statement
            // so we expect a declarator following __trait if it's a type.
            // other usages wont be ambiguous (alias, template instance, type qual, etc.)
            if (lp.value != TOK.identifier)
                goto Lfalse;

            break;

        case TOK.const_:
        case TOK.immutable_:
        case TOK.shared_:
        case TOK.inout_:
            // const(type)  or  immutable(type)  or  shared(type)  or  wild(type)
            t = peek(t);
            if (t.value != TOK.leftParenthesis)
                goto Lfalse;
            t = peek(t);
            if (!isDeclaration(t, NeedDeclaratorId.no, TOK.rightParenthesis, &t))
            {
                goto Lfalse;
            }
            t = peek(t);
            break;

        default:
            goto Lfalse;
        }
        *pt = t;
        //printf("is\n");
        return true;

    Lfalse:
        //printf("is not\n");
        return false;
    }

    private bool isDeclarator(Token** pt, int* haveId, int* haveTpl, TOK endtok, bool allowAltSyntax = true)
    {
        // This code parallels parseDeclarator()
        Token* t = *pt;
        bool parens;

        //printf("Parser::isDeclarator() %s\n", t.toChars());
        if (t.value == TOK.assign)
            return false;

        while (1)
        {
            parens = false;
            switch (t.value)
            {
            case TOK.mul:
            //case TOK.and:
                t = peek(t);
                continue;

            case TOK.leftBracket:
                t = peek(t);
                if (t.value == TOK.rightBracket)
                {
                    t = peek(t);
                }
                else if (isDeclaration(t, NeedDeclaratorId.no, TOK.rightBracket, &t))
                {
                    // It's an associative array declaration
                    t = peek(t);

                    // ...[type].ident
                    if (t.value == TOK.dot && peek(t).value == TOK.identifier)
                    {
                        t = peek(t);
                        t = peek(t);
                    }
                }
                else
                {
                    // [ expression ]
                    // [ expression .. expression ]
                    if (!isExpression(&t))
                        return false;
                    if (t.value == TOK.slice)
                    {
                        t = peek(t);
                        if (!isExpression(&t))
                            return false;
                        if (t.value != TOK.rightBracket)
                            return false;
                        t = peek(t);
                    }
                    else
                    {
                        if (t.value != TOK.rightBracket)
                            return false;
                        t = peek(t);
                        // ...[index].ident
                        if (t.value == TOK.dot && peek(t).value == TOK.identifier)
                        {
                            t = peek(t);
                            t = peek(t);
                        }
                    }
                }
                continue;

            case TOK.identifier:
                if (*haveId)
                    return false;
                *haveId = true;
                t = peek(t);
                break;

            case TOK.leftParenthesis:
                if (!allowAltSyntax)
                    return false;   // Do not recognize C-style declarations.

                t = peek(t);
                if (t.value == TOK.rightParenthesis)
                    return false; // () is not a declarator

                /* Regard ( identifier ) as not a declarator
                 * BUG: what about ( *identifier ) in
                 *      f(*p)(x);
                 * where f is a class instance with overloaded () ?
                 * Should we just disallow C-style function pointer declarations?
                 */
                if (t.value == TOK.identifier)
                {
                    Token* t2 = peek(t);
                    if (t2.value == TOK.rightParenthesis)
                        return false;
                }

                if (!isDeclarator(&t, haveId, null, TOK.rightParenthesis))
                    return false;
                t = peek(t);
                parens = true;
                break;

            case TOK.delegate_:
            case TOK.function_:
                t = peek(t);
                if (!isParameters(&t))
                    return false;
                skipAttributes(t, &t);
                continue;

            default:
                break;
            }
            break;
        }

        while (1)
        {
            switch (t.value)
            {
                static if (CARRAYDECL)
                {
                case TOK.leftBracket:
                    parens = false;
                    t = peek(t);
                    if (t.value == TOK.rightBracket)
                    {
                        t = peek(t);
                    }
                    else if (isDeclaration(t, NeedDeclaratorId.no, TOK.rightBracket, &t))
                    {
                        // It's an associative array declaration
                        t = peek(t);
                    }
                    else
                    {
                        // [ expression ]
                        if (!isExpression(&t))
                            return false;
                        if (t.value != TOK.rightBracket)
                            return false;
                        t = peek(t);
                    }
                    continue;
                }

            case TOK.leftParenthesis:
                parens = false;
                if (Token* tk = peekPastParen(t))
                {
                    if (tk.value == TOK.leftParenthesis)
                    {
                        if (!haveTpl)
                            return false;
                        *haveTpl = 1;
                        t = tk;
                    }
                    else if (tk.value == TOK.assign)
                    {
                        if (!haveTpl)
                            return false;
                        *haveTpl = 1;
                        *pt = tk;
                        return true;
                    }
                }
                if (!isParameters(&t))
                    return false;
                while (1)
                {
                    switch (t.value)
                    {
                    case TOK.const_:
                    case TOK.immutable_:
                    case TOK.shared_:
                    case TOK.inout_:
                    case TOK.pure_:
                    case TOK.nothrow_:
                    case TOK.return_:
                    case TOK.scope_:
                        t = peek(t);
                        continue;

                    case TOK.at:
                        t = peek(t); // skip '@'
                        t = peek(t); // skip identifier
                        continue;

                    default:
                        break;
                    }
                    break;
                }
                continue;

            // Valid tokens that follow the start of a declaration
            case TOK.rightParenthesis:
            case TOK.rightBracket:
            case TOK.assign:
            case TOK.comma:
            case TOK.dotDotDot:
            case TOK.semicolon:
            case TOK.leftCurly:
            case TOK.in_:
            case TOK.out_:
            case TOK.do_:
                // The !parens is to disallow unnecessary parentheses
                if (!parens && (endtok == TOK.reserved || endtok == t.value))
                {
                    *pt = t;
                    return true;
                }
                return false;

            // To recognize the shortened function declaration syntax
            case TOK.goesTo:
                /*
                    1. https://issues.dlang.org/show_bug.cgi?id=24088

                    2. We need to make sure the would-be
                       declarator has an identifier otherwise function literals
                       are handled incorrectly. Some special treatment is required
                       here, it turns out that a lot of code in the compiler relies
                       on this mess (in the parser), i.e. having isDeclarator be more
                       precise the parsing of other things go kaboom, so we do it in a
                       separate case.
                */
                if (*haveId)
                    goto case TOK.do_;
                goto default;

            case TOK.identifier:
                if (t.ident == Id._body)
                {
                    usageOfBodyKeyword();
                    goto case TOK.do_;
                }
                goto default;

            case TOK.if_:
                return haveTpl ? true : false;

            // Used for mixin type parsing
            case TOK.endOfFile:
                if (endtok == TOK.endOfFile)
                    goto case TOK.do_;
                return false;

            default:
                return false;
            }
        }
        assert(0);
    }

    private bool isParameters(Token** pt)
    {
        // This code parallels parseParameterList()
        Token* t = *pt;

        //printf("isParameters()\n");
        if (t.value != TOK.leftParenthesis)
            return false;

        t = peek(t);
        for (; 1; t = peek(t))
        {
        L1:
            switch (t.value)
            {
            case TOK.rightParenthesis:
                break;

            case TOK.at:
                Token* pastAttr;
                if (skipAttributes(t, &pastAttr))
                {
                    t = pastAttr;
                    goto default;
                }
                break;

            case TOK.dotDotDot:
                t = peek(t);
                break;

            case TOK.in_:
            case TOK.out_:
            case TOK.ref_:
            case TOK.lazy_:
            case TOK.scope_:
            case TOK.final_:
            case TOK.auto_:
            case TOK.return_:
                continue;

            case TOK.const_:
            case TOK.immutable_:
            case TOK.shared_:
            case TOK.inout_:
                t = peek(t);
                if (t.value == TOK.leftParenthesis)
                {
                    t = peek(t);
                    if (!isDeclaration(t, NeedDeclaratorId.no, TOK.rightParenthesis, &t))
                        return false;
                    t = peek(t); // skip past closing ')'
                    goto L2;
                }
                goto L1;

            default:
                {
                    if (!isBasicType(&t))
                        return false;
                L2:
                    int tmp = false;
                    if (t.value != TOK.dotDotDot && !isDeclarator(&t, &tmp, null, TOK.reserved))
                        return false;
                    if (t.value == TOK.assign)
                    {
                        t = peek(t);
                        if (!isExpression(&t))
                            return false;
                    }
                    if (t.value == TOK.dotDotDot)
                    {
                        t = peek(t);
                        break;
                    }
                }
                if (t.value == TOK.comma)
                {
                    continue;
                }
                break;
            }
            break;
        }
        if (t.value != TOK.rightParenthesis)
            return false;
        t = peek(t);
        *pt = t;
        return true;
    }

    private bool isExpression(Token** pt)
    {
        // This is supposed to determine if something is an expression.
        // What it actually does is scan until a closing right bracket
        // is found.

        Token* t = *pt;
        int brnest = 0;
        int panest = 0;
        int curlynest = 0;

        for (;; t = peek(t))
        {
            switch (t.value)
            {
            case TOK.leftBracket:
                brnest++;
                continue;

            case TOK.rightBracket:
                if (--brnest >= 0)
                    continue;
                break;

            case TOK.leftParenthesis:
                panest++;
                continue;

            case TOK.comma:
                if (brnest || panest)
                    continue;
                break;

            case TOK.rightParenthesis:
                if (--panest >= 0)
                    continue;
                break;

            case TOK.leftCurly:
                curlynest++;
                continue;

            case TOK.rightCurly:
                if (--curlynest >= 0)
                    continue;
                return false;

            case TOK.slice:
                if (brnest)
                    continue;
                break;

            case TOK.semicolon:
                if (curlynest)
                    continue;
                return false;

            case TOK.endOfFile:
                return false;

            default:
                continue;
            }
            break;
        }

        *pt = t;
        return true;
    }

    /*******************************************
     * Skip parentheses.
     * Params:
     *      t = on opening $(LPAREN)
     *      pt = *pt is set to token past '$(RPAREN)' on true
     * Returns:
     *      true    successful
     *      false   some parsing error
     */
    bool skipParens(Token* t, Token** pt)
    {
        if (t.value != TOK.leftParenthesis)
            return false;

        int parens = 0;

        while (1)
        {
            switch (t.value)
            {
            case TOK.leftParenthesis:
                parens++;
                break;

            case TOK.rightParenthesis:
                parens--;
                if (parens < 0)
                    goto Lfalse;
                if (parens == 0)
                    goto Ldone;
                break;

            case TOK.endOfFile:
                goto Lfalse;

            default:
                break;
            }
            t = peek(t);
        }
    Ldone:
        if (pt)
            *pt = peek(t); // skip found rparen
        return true;

    Lfalse:
        return false;
    }

    private bool skipParensIf(Token* t, Token** pt)
    {
        if (t.value != TOK.leftParenthesis)
        {
            if (pt)
                *pt = t;
            return true;
        }
        return skipParens(t, pt);
    }

    //returns true if the next value (after optional matching parens) is expected
    private bool hasOptionalParensThen(Token* t, TOK expected)
    {
        Token* tk;
        if (!skipParensIf(t, &tk))
            return false;
        return tk.value == expected;
    }

    /*******************************************
     * Skip attributes.
     * Input:
     *      t is on a candidate attribute
     * Output:
     *      *pt is set to first non-attribute token on success
     * Returns:
     *      true    successful
     *      false   some parsing error
     */
    private bool skipAttributes(Token* t, Token** pt)
    {
        while (1)
        {
            switch (t.value)
            {
            case TOK.const_:
            case TOK.immutable_:
            case TOK.shared_:
            case TOK.inout_:
            case TOK.final_:
            case TOK.auto_:
            case TOK.scope_:
            case TOK.override_:
            case TOK.abstract_:
            case TOK.synchronized_:
                break;

            case TOK.deprecated_:
                if (peek(t).value == TOK.leftParenthesis)
                {
                    t = peek(t);
                    if (!skipParens(t, &t))
                        goto Lerror;
                    // t is on the next of closing parenthesis
                    continue;
                }
                break;

            case TOK.nothrow_:
            case TOK.pure_:
            case TOK.ref_:
            case TOK.gshared:
            case TOK.return_:
                break;

            case TOK.at:
                t = peek(t);
                if (t.value == TOK.identifier)
                {
                    /* @identifier
                     * @identifier!arg
                     * @identifier!(arglist)
                     * any of the above followed by (arglist)
                     * @predefined_attribute
                     */
                    if (isBuiltinAtAttribute(t.ident))
                        break;
                    t = peek(t);
                    if (t.value == TOK.not)
                    {
                        t = peek(t);
                        if (t.value == TOK.leftParenthesis)
                        {
                            // @identifier!(arglist)
                            if (!skipParens(t, &t))
                                goto Lerror;
                            // t is on the next of closing parenthesis
                        }
                        else
                        {
                            // @identifier!arg
                            // Do low rent skipTemplateArgument
                            if (t.value == TOK.vector)
                            {
                                // identifier!__vector(type)
                                t = peek(t);
                                if (!skipParens(t, &t))
                                    goto Lerror;
                            }
                            else
                                t = peek(t);
                        }
                    }
                    if (t.value == TOK.leftParenthesis)
                    {
                        if (!skipParens(t, &t))
                            goto Lerror;
                        // t is on the next of closing parenthesis
                        continue;
                    }
                    continue;
                }
                if (t.value == TOK.leftParenthesis)
                {
                    // @( ArgumentList )
                    if (!skipParens(t, &t))
                        goto Lerror;
                    // t is on the next of closing parenthesis
                    continue;
                }
                goto Lerror;

            default:
                goto Ldone;
            }
            t = peek(t);
        }
    Ldone:
        if (pt)
            *pt = t;
        return true;

    Lerror:
        return false;
    }

    AST.Expression parseExpression()
    {
        auto loc = token.loc;

        //printf("Parser::parseExpression() loc = %d\n", loc.linnum);
        auto e = parseAssignExp();
        while (token.value == TOK.comma)
        {
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.CommaExp(loc, e, e2, false);
            loc = token.loc;
        }
        return e;
    }

    /********************************* Expression Parser ***************************/

    AST.Expression parsePrimaryExp()
    {
        AST.Expression e;
        AST.Type t;
        Identifier id;
        const loc = token.loc;

        //printf("parsePrimaryExp(): loc = %d\n", loc.linnum);
        switch (token.value)
        {
        case TOK.identifier:
            {
                if (peekNext() == TOK.arrow)
                {
                    // skip `identifier ->`
                    nextToken();
                    nextToken();
                    error("use `.` for member lookup, not `->`");
                    goto Lerr;
                }

                if (peekNext() == TOK.goesTo)
                    goto case_delegate;

                id = token.ident;
                nextToken();
                TOK save;
                if (token.value == TOK.not && (save = peekNext()) != TOK.is_ && save != TOK.in_)
                {
                    // identifier!(template-argument-list)
                    auto tempinst = new AST.TemplateInstance(loc, id, parseTemplateArguments());
                    e = new AST.ScopeExp(loc, tempinst);
                }
                else
                    e = new AST.IdentifierExp(loc, id);
                break;
            }
        case TOK.dollar:
            if (!inBrackets)
                error("`$` is valid only inside [] of index or slice");
            e = new AST.DollarExp(loc);
            nextToken();
            break;

        case TOK.dot:
            // Signal global scope '.' operator with "" identifier
            e = new AST.IdentifierExp(loc, Id.empty);
            break;

        case TOK.this_:
            e = new AST.ThisExp(loc);
            nextToken();
            break;

        case TOK.super_:
            e = new AST.SuperExp(loc);
            nextToken();
            break;

        case TOK.int32Literal:
            e = new AST.IntegerExp(loc, token.intvalue, AST.Type.tint32);
            nextToken();
            break;

        case TOK.uns32Literal:
            e = new AST.IntegerExp(loc, token.unsvalue, AST.Type.tuns32);
            nextToken();
            break;

        case TOK.int64Literal:
            e = new AST.IntegerExp(loc, token.intvalue, AST.Type.tint64);
            nextToken();
            break;

        case TOK.uns64Literal:
            e = new AST.IntegerExp(loc, token.unsvalue, AST.Type.tuns64);
            nextToken();
            break;

        case TOK.float32Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.tfloat32);
            nextToken();
            break;

        case TOK.float64Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.tfloat64);
            nextToken();
            break;

        case TOK.float80Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.tfloat80);
            nextToken();
            break;

        case TOK.imaginary32Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.timaginary32);
            nextToken();
            break;

        case TOK.imaginary64Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.timaginary64);
            nextToken();
            break;

        case TOK.imaginary80Literal:
            e = new AST.RealExp(loc, token.floatvalue, AST.Type.timaginary80);
            nextToken();
            break;

        case TOK.null_:
            e = new AST.NullExp(loc);
            nextToken();
            break;

        case TOK.file:
            e = new AST.FileInitExp(loc, EXP.file);
            nextToken();
            break;
        case TOK.fileFullPath:
            e = new AST.FileInitExp(loc, EXP.fileFullPath);
            nextToken();
            break;

        case TOK.line:
            e = new AST.LineInitExp(loc);
            nextToken();
            break;

        case TOK.moduleString:
            e = new AST.ModuleInitExp(loc);
            nextToken();
            break;
        case TOK.functionString:
            e = new AST.FuncInitExp(loc);
            nextToken();
            break;

        case TOK.prettyFunction:
            e = new AST.PrettyFuncInitExp(loc);
            nextToken();
            break;

        case TOK.true_:
            e = new AST.IntegerExp(loc, 1, AST.Type.tbool);
            nextToken();
            break;

        case TOK.false_:
            e = new AST.IntegerExp(loc, 0, AST.Type.tbool);
            nextToken();
            break;

        case TOK.charLiteral:
            e = new AST.IntegerExp(loc, token.unsvalue, AST.Type.tchar);
            nextToken();
            break;

        case TOK.wcharLiteral:
            e = new AST.IntegerExp(loc, token.unsvalue, AST.Type.twchar);
            nextToken();
            break;

        case TOK.dcharLiteral:
            e = new AST.IntegerExp(loc, token.unsvalue, AST.Type.tdchar);
            nextToken();
            break;

        case TOK.interpolated:
            e = new AST.InterpExp(loc, token.interpolatedSet, token.postfix);
            nextToken();
            break;

        case TOK.string_:
        case TOK.hexadecimalString:
            const bool hexString = token.value == TOK.hexadecimalString;
            {
                // cat adjacent strings
                auto s = token.ustring;
                auto len = token.len;
                auto postfix = token.postfix;
                while (1)
                {
                    const prev = token;
                    nextToken();
                    if (token.value == TOK.string_ || token.value == TOK.hexadecimalString)
                    {
                        if (token.postfix)
                        {
                            if (token.postfix != postfix)
                                error(token.loc, "mismatched string literal postfixes `'%c'` and `'%c'`", postfix, token.postfix);
                            postfix = token.postfix;
                        }

                        error("implicit string concatenation is error-prone and disallowed in D");
                        eSink.errorSupplemental(token.loc, "Use the explicit syntax instead " ~
                             "(concatenating literals is `@nogc`): %s ~ %s",
                             prev.toChars(), token.toChars());

                        const len1 = len;
                        const len2 = token.len;
                        len = len1 + len2;
                        auto s2 = cast(char*)mem.xmalloc_noscan(len * char.sizeof);
                        memcpy(s2, s, len1 * char.sizeof);
                        memcpy(s2 + len1, token.ustring, len2 * char.sizeof);
                        s = s2;
                    }
                    else
                        break;
                }
                e = new AST.StringExp(loc, s[0 .. len], len, 1, postfix);
                e.isStringExp().hexString = hexString;
                break;
            }
        case TOK.void_:
            t = AST.Type.tvoid;
            goto LabelX;

        case TOK.int8:
            t = AST.Type.tint8;
            goto LabelX;

        case TOK.uns8:
            t = AST.Type.tuns8;
            goto LabelX;

        case TOK.int16:
            t = AST.Type.tint16;
            goto LabelX;

        case TOK.uns16:
            t = AST.Type.tuns16;
            goto LabelX;

        case TOK.int32:
            t = AST.Type.tint32;
            goto LabelX;

        case TOK.uns32:
            t = AST.Type.tuns32;
            goto LabelX;

        case TOK.int64:
            t = AST.Type.tint64;
            goto LabelX;

        case TOK.uns64:
            t = AST.Type.tuns64;
            goto LabelX;

        case TOK.int128:
            t = AST.Type.tint128;
            goto LabelX;

        case TOK.uns128:
            t = AST.Type.tuns128;
            goto LabelX;

        case TOK.float32:
            t = AST.Type.tfloat32;
            goto LabelX;

        case TOK.float64:
            t = AST.Type.tfloat64;
            goto LabelX;

        case TOK.float80:
            t = AST.Type.tfloat80;
            goto LabelX;

        case TOK.imaginary32:
            t = AST.Type.timaginary32;
            goto LabelX;

        case TOK.imaginary64:
            t = AST.Type.timaginary64;
            goto LabelX;

        case TOK.imaginary80:
            t = AST.Type.timaginary80;
            goto LabelX;

        case TOK.complex32:
            t = AST.Type.tcomplex32;
            goto LabelX;

        case TOK.complex64:
            t = AST.Type.tcomplex64;
            goto LabelX;

        case TOK.complex80:
            t = AST.Type.tcomplex80;
            goto LabelX;

        case TOK.bool_:
            t = AST.Type.tbool;
            goto LabelX;

        case TOK.char_:
            t = AST.Type.tchar;
            goto LabelX;

        case TOK.wchar_:
            t = AST.Type.twchar;
            goto LabelX;

        case TOK.dchar_:
            t = AST.Type.tdchar;
            goto LabelX;
        LabelX:
            const next = peekNext();
            if (next != TOK.leftParenthesis && next != TOK.dot)
            {
                // defer error for better diagnostics
                e = new AST.TypeExp(loc, parseType);
                break;
            }
            nextToken();
            if (token.value == TOK.leftParenthesis)
            {
                e = new AST.TypeExp(loc, t);
                e = new AST.CallExp(loc, e, parseArguments());
                break;
            }
            check(TOK.dot);
            if (token.value != TOK.identifier)
            {
                error(token.loc, "found `%s` when expecting identifier following `%s`.", token.toChars(), t.toChars());
                goto Lerr;
            }
            e = new AST.DotIdExp(loc, new AST.TypeExp(loc, t), token.ident);
            nextToken();
            break;

        case TOK.typeof_:
            {
                t = parseTypeof();
                e = new AST.TypeExp(loc, t);
                break;
            }
        case TOK.vector:
            {
                t = parseVector();
                e = new AST.TypeExp(loc, t);
                break;
            }
        case TOK.typeid_:
            {
                nextToken();
                check(TOK.leftParenthesis, "`typeid`");
                RootObject o = parseTypeOrAssignExp();
                check(TOK.rightParenthesis);
                e = new AST.TypeidExp(loc, o);
                break;
            }
        case TOK.rvalue:
            {
                nextToken();
                check(TOK.leftParenthesis, "`__rvalue`");
                e = parseAssignExp();
                e.rvalue = true;
                check(TOK.rightParenthesis);
                break;
            }
        case TOK.traits:
            {
                /* __traits(identifier, args...)
                 */
                Identifier ident;
                AST.Objects* args = null;

                nextToken();
                check(TOK.leftParenthesis);
                if (token.value != TOK.identifier)
                {
                    error("`__traits(identifier, args...)` expected");
                    goto Lerr;
                }
                ident = token.ident;
                nextToken();
                if (token.value == TOK.comma)
                    args = parseTemplateArgumentList(); // __traits(identifier, args...)
                else
                    check(TOK.rightParenthesis); // __traits(identifier)

                e = new AST.TraitsExp(loc, ident, args);
                break;
            }
        case TOK.is_:
            {
                AST.Type targ;
                Identifier ident = null;
                AST.Type tspec = null;
                TOK tok = TOK.reserved;
                TOK tok2 = TOK.reserved;
                AST.TemplateParameters* tpl = null;

                nextToken();
                if (token.value != TOK.leftParenthesis)
                {
                    error("expected `(` following `is`, not `%s`", token.toChars());
                    goto Lerr;
                }
                else
                {
                    nextToken();
                    if (token.value == TOK.identifier && peekNext() == TOK.leftParenthesis)
                    {
                        error(loc, "unexpected `(` after `%s`, inside `is` expression. Try enclosing the contents of `is` with a `typeof` expression", token.toChars());
                        nextToken();
                        Token* tempTok = peekPastParen(&token);
                        memcpy(&token, tempTok, Token.sizeof);
                        goto Lerr;
                    }
                    targ = parseType(&ident);
                    if (token.value == TOK.colon || token.value == TOK.equal)
                    {
                        tok = token.value;
                        nextToken();
                        if (tok == TOK.equal && (token.value == TOK.struct_ || token.value == TOK.union_
                            || token.value == TOK.class_ || token.value == TOK.super_ || token.value == TOK.enum_
                            || token.value == TOK.interface_ || token.value == TOK.package_ || token.value == TOK.module_
                            || token.value == TOK.argumentTypes || token.value == TOK.parameters
                            || token.value == TOK.const_ && peekNext() == TOK.rightParenthesis
                            || token.value == TOK.immutable_ && peekNext() == TOK.rightParenthesis
                            || token.value == TOK.shared_ && peekNext() == TOK.rightParenthesis
                            || token.value == TOK.inout_ && peekNext() == TOK.rightParenthesis || token.value == TOK.function_
                            || token.value == TOK.delegate_ || token.value == TOK.return_
                            || (token.value == TOK.vector && peekNext() == TOK.rightParenthesis)))
                        {
                            tok2 = token.value;
                            nextToken();
                        }
                        else
                        {
                            tspec = parseType();
                        }
                    }
                    if (tspec)
                    {
                        if (token.value == TOK.comma)
                            tpl = parseTemplateParameterList(1);
                        else
                        {
                            tpl = new AST.TemplateParameters();
                            check(TOK.rightParenthesis);
                        }
                    }
                    else
                        check(TOK.rightParenthesis);
                }
                e = new AST.IsExp(loc, targ, ident, tok, tspec, tok2, tpl);
                break;
            }
        case TOK.assert_:
            {
                // https://dlang.org/spec/expression.html#assert_expressions
                AST.Expression msg = null;

                nextToken();
                check(TOK.leftParenthesis, "`assert`");
                e = parseAssignExp();
                if (token.value == TOK.comma)
                {
                    nextToken();
                    if (token.value != TOK.rightParenthesis)
                    {
                        msg = parseAssignExp();
                        if (token.value == TOK.comma)
                            nextToken();
                    }
                }
                check(TOK.rightParenthesis);
                e = new AST.AssertExp(loc, e, msg);
                break;
            }
        case TOK.mixin_:
            {
                // https://dlang.org/spec/expression.html#mixin_expressions
                nextToken();
                if (token.value != TOK.leftParenthesis)
                    error(token.loc, "found `%s` when expecting `%s` following `mixin`", token.toChars(), Token.toChars(TOK.leftParenthesis));
                auto exps = parseArguments();
                e = new AST.MixinExp(loc, exps);
                break;
            }
        case TOK.import_:
            {
                nextToken();
                check(TOK.leftParenthesis, "`import`");
                e = parseAssignExp();
                check(TOK.rightParenthesis);
                e = new AST.ImportExp(loc, e);
                break;
            }
        case TOK.new_:
            e = parseNewExp(null);
            break;

        case TOK.auto_:
            {
                if (peekNext() == TOK.ref_ && peekNext2() == TOK.leftParenthesis)
                {
                    Token* tk = peekPastParen(peek(peek(&token)));
                    if (skipAttributes(tk, &tk) && (tk.value == TOK.goesTo || tk.value == TOK.leftCurly))
                    {
                        // auto ref (arguments) => expression
                        // auto ref (arguments) { statements... }
                        goto case_delegate;
                    }
                }
                nextToken();
                error("found `%s` when expecting `ref` and function literal following `auto`", token.toChars());
                goto Lerr;
            }
        case TOK.ref_:
            {
                if (peekNext() == TOK.leftParenthesis)
                {
                    Token* tk = peekPastParen(peek(&token));
                    if (skipAttributes(tk, &tk) && (tk.value == TOK.goesTo || tk.value == TOK.leftCurly))
                    {
                        // ref (arguments) => expression
                        // ref (arguments) { statements... }
                        goto case_delegate;
                    }
                }
                nextToken();
                error("found `%s` when expecting function literal following `ref`", token.toChars());
                goto Lerr;
            }
        case TOK.leftParenthesis:
            {
                Token* tk = peekPastParen(&token);
                if (skipAttributes(tk, &tk) && (tk.value == TOK.goesTo || tk.value == TOK.leftCurly))
                {
                    // (arguments) => expression
                    // (arguments) { statements... }
                    goto case_delegate;
                }

                // ( expression )
                nextToken();
                e = parseExpression();
                e.parens = true;
                check(loc, TOK.rightParenthesis);
                break;
            }
        case TOK.leftBracket:
            {
                /* Parse array literals and associative array literals:
                 *  [ value, value, value ... ]
                 *  [ key:value, key:value, key:value ... ]
                 */
                auto values = new AST.Expressions();
                AST.Expressions* keys = null;

                nextToken();
                while (token.value != TOK.rightBracket && token.value != TOK.endOfFile)
                {
                    e = parseAssignExp();
                    if (token.value == TOK.colon && (keys || values.length == 0))
                    {
                        nextToken();
                        if (!keys)
                            keys = new AST.Expressions();
                        keys.push(e);
                        e = parseAssignExp();
                    }
                    else if (keys)
                    {
                        error("`key:value` expected for associative array literal");
                        keys = null;
                    }
                    values.push(e);
                    if (token.value == TOK.rightBracket)
                        break;
                    check(TOK.comma);
                }
                check(loc, TOK.rightBracket);

                if (keys)
                    e = new AST.AssocArrayLiteralExp(loc, keys, values);
                else
                    e = new AST.ArrayLiteralExp(loc, null, values);
                break;
            }
        case TOK.leftCurly:
        case TOK.function_:
        case TOK.delegate_:
        case_delegate:
            {
                AST.Dsymbol s = parseFunctionLiteral();
                e = new AST.FuncExp(loc, s);
                break;
            }

        default:
            error("expression expected, not `%s`", token.toChars());
        Lerr:
            // Anything for e, as long as it's not NULL
            e = AST.ErrorExp.get();
            nextToken();
            break;
        }
        return e;
    }

    private AST.Expression parseUnaryExp()
    {
        AST.Expression e;
        const loc = token.loc;

        switch (token.value)
        {
        case TOK.and:
            nextToken();
            e = parseUnaryExp();
            e = new AST.AddrExp(loc, e);
            break;

        case TOK.plusPlus:
            nextToken();
            e = parseUnaryExp();
            //e = new AddAssignExp(loc, e, new IntegerExp(loc, 1, Type::tint32));
            e = new AST.PreExp(EXP.prePlusPlus, loc, e);
            break;

        case TOK.minusMinus:
            nextToken();
            e = parseUnaryExp();
            //e = new MinAssignExp(loc, e, new IntegerExp(loc, 1, Type::tint32));
            e = new AST.PreExp(EXP.preMinusMinus, loc, e);
            break;

        case TOK.mul:
            nextToken();
            e = parseUnaryExp();
            e = new AST.PtrExp(loc, e);
            break;

        case TOK.min:
            nextToken();
            e = parseUnaryExp();
            e = new AST.NegExp(loc, e);
            break;

        case TOK.add:
            nextToken();
            e = parseUnaryExp();
            e = new AST.UAddExp(loc, e);
            break;

        case TOK.not:
            nextToken();
            e = parseUnaryExp();
            e = new AST.NotExp(loc, e);
            break;

        case TOK.tilde:
            nextToken();
            e = parseUnaryExp();
            e = new AST.ComExp(loc, e);
            break;

        case TOK.cast_: // cast(type) expression
            {
                nextToken();
                check(TOK.leftParenthesis);
                /* Look for cast(), cast(const), cast(immutable),
                 * cast(shared), cast(shared const), cast(wild), cast(shared wild)
                 */
                ubyte m = 0;
                while (1)
                {
                    switch (token.value)
                    {
                    case TOK.const_:
                        if (peekNext() == TOK.leftParenthesis)
                            break; // const as type constructor
                        m |= MODFlags.const_; // const as storage class
                        nextToken();
                        continue;

                    case TOK.immutable_:
                        if (peekNext() == TOK.leftParenthesis)
                            break;
                        m |= MODFlags.immutable_;
                        nextToken();
                        continue;

                    case TOK.shared_:
                        if (peekNext() == TOK.leftParenthesis)
                            break;
                        m |= MODFlags.shared_;
                        nextToken();
                        continue;

                    case TOK.inout_:
                        if (peekNext() == TOK.leftParenthesis)
                            break;
                        m |= MODFlags.wild;
                        nextToken();
                        continue;

                    default:
                        break;
                    }
                    break;
                }
                if (token.value == TOK.rightParenthesis)
                {
                    nextToken();
                    e = parseUnaryExp();
                    e = new AST.CastExp(loc, e, m);
                }
                else
                {
                    AST.Type t = parseType(); // cast( type )
                    t = t.addSTC(AST.ModToStc(m)); // cast( const type )
                    check(TOK.rightParenthesis);
                    e = parseUnaryExp();
                    e = new AST.CastExp(loc, e, t);
                }
                break;
            }
        case TOK.inout_:
        case TOK.shared_:
        case TOK.const_:
        case TOK.immutable_: // immutable(type)(arguments) / immutable(type).init
            {
                STC stc = parseTypeCtor();

                AST.Type t = parseBasicType();
                t = t.addSTC(stc);

                if (stc == 0 && token.value == TOK.dot)
                {
                    nextToken();
                    if (token.value != TOK.identifier)
                    {
                        error("identifier expected following `%s.`, not `%s`",
                            t.toChars(), token.toChars());
                        return AST.ErrorExp.get();
                    }
                    e = new AST.DotIdExp(loc, new AST.TypeExp(loc, t), token.ident);
                    nextToken();
                    e = parsePostExp(e);
                }
                else
                {
                    e = new AST.TypeExp(loc, t);
                    if (token.value != TOK.leftParenthesis)
                    {
                        error("`(arguments)` expected following `%s`, not `%s`",
                            t.toChars(), token.toChars());
                        return e;
                    }
                    e = new AST.CallExp(loc, e, parseArguments());
                }
                break;
            }
        case TOK.leftParenthesis:
            {
                auto tk = peek(&token);
                static if (CCASTSYNTAX)
                {
                    // If cast
                    if (isDeclaration(tk, NeedDeclaratorId.no, TOK.rightParenthesis, &tk))
                    {
                        tk = peek(tk); // skip over right parenthesis
                        switch (tk.value)
                        {
                        case TOK.not:
                            tk = peek(tk);
                            if (tk.value == TOK.is_ || tk.value == TOK.in_) // !is or !in
                                break;
                            goto case;

                        case TOK.dot:
                        case TOK.plusPlus:
                        case TOK.minusMinus:
                        case TOK.new_:
                        case TOK.leftParenthesis:
                        case TOK.identifier:
                        case TOK.this_:
                        case TOK.super_:
                        case TOK.int32Literal:
                        case TOK.uns32Literal:
                        case TOK.int64Literal:
                        case TOK.uns64Literal:
                        case TOK.int128Literal:
                        case TOK.uns128Literal:
                        case TOK.float32Literal:
                        case TOK.float64Literal:
                        case TOK.float80Literal:
                        case TOK.imaginary32Literal:
                        case TOK.imaginary64Literal:
                        case TOK.imaginary80Literal:
                        case TOK.null_:
                        case TOK.true_:
                        case TOK.false_:
                        case TOK.charLiteral:
                        case TOK.wcharLiteral:
                        case TOK.dcharLiteral:
                        case TOK.string_:
                        case TOK.interpolated:
                        case TOK.function_:
                        case TOK.delegate_:
                        case TOK.typeof_:
                        case TOK.traits:
                        case TOK.vector:
                        case TOK.file:
                        case TOK.fileFullPath:
                        case TOK.line:
                        case TOK.moduleString:
                        case TOK.functionString:
                        case TOK.prettyFunction:
                        case TOK.wchar_:
                        case TOK.dchar_:
                        case TOK.bool_:
                        case TOK.char_:
                        case TOK.int8:
                        case TOK.uns8:
                        case TOK.int16:
                        case TOK.uns16:
                        case TOK.int32:
                        case TOK.uns32:
                        case TOK.int64:
                        case TOK.uns64:
                        case TOK.int128:
                        case TOK.uns128:
                        case TOK.float32:
                        case TOK.float64:
                        case TOK.float80:
                        case TOK.imaginary32:
                        case TOK.imaginary64:
                        case TOK.imaginary80:
                        case TOK.complex32:
                        case TOK.complex64:
                        case TOK.complex80:
                        case TOK.void_:
                            {
                                // (type) una_exp
                                nextToken();
                                // Note: `t` may be an expression that looks like a type
                                auto t = parseType();
                                check(TOK.rightParenthesis);

                                // if .identifier
                                // or .identifier!( ... )
                                if (token.value == TOK.dot)
                                {
                                    if (peekNext() != TOK.identifier && peekNext() != TOK.new_)
                                    {
                                        error("identifier or new keyword expected following `(...)`.");
                                        nextToken();
                                        return AST.ErrorExp.get();
                                    }
                                    e = new AST.TypeExp(loc, t);
                                    e.parens = true;
                                    e = parsePostExp(e);
                                }
                                else if (token.value == TOK.leftParenthesis ||
                                    token.value == TOK.plusPlus || token.value == TOK.minusMinus)
                                {
                                    // (type)(expr)
                                    // (callable)(args)
                                    // (expr)++
                                    auto te = new AST.TypeExp(loc, t);
                                    te.parens = true;
                                    e = parsePostExp(te);
                                }
                                else
                                {
                                    e = parseUnaryExp();
                                    e = new AST.CastExp(loc, e, t);
                                    error(loc, "C style cast illegal, use `%s`", e.toChars());
                                }
                                return e;
                            }
                        default:
                            break;
                        }
                    }
                }
                e = parsePrimaryExp();
                e = parsePostExp(e);
                break;
            }
        case TOK.throw_:
            {
                nextToken();
                // Deviation from the DIP:
                // Parse AssignExpression instead of Expression to avoid conflicts for comma
                // separated lists, e.g. function arguments
                AST.Expression exp = parseAssignExp();
                e = new AST.ThrowExp(loc, exp);
                break;
            }

        default:
            e = parsePrimaryExp();
            e = parsePostExp(e);
            break;
        }
        assert(e);

        // ^^ is right associative and has higher precedence than the unary operators
        while (token.value == TOK.pow)
        {
            nextToken();
            AST.Expression e2 = parseUnaryExp();
            e = new AST.PowExp(loc, e, e2);
        }

        return e;
    }

    private AST.Expression parsePostExp(AST.Expression e)
    {
        while (1)
        {
            const loc = token.loc;
            switch (token.value)
            {
            case TOK.dot:
                nextToken();
                if (token.value == TOK.identifier)
                {
                    Identifier id = token.ident;

                    nextToken();
                    if (token.value == TOK.not && peekNext() != TOK.is_ && peekNext() != TOK.in_)
                    {
                        AST.Objects* tiargs = parseTemplateArguments();
                        e = new AST.DotTemplateInstanceExp(loc, e, id, tiargs);
                    }
                    else
                        e = new AST.DotIdExp(loc, e, id);
                    continue;
                }
                if (token.value == TOK.new_)
                {
                    e = parseNewExp(e);
                    continue;
                }
                error("identifier or `new` expected following `.`, not `%s`", token.toChars());
                break;

            case TOK.plusPlus:
                e = new AST.PostExp(EXP.plusPlus, loc, e);
                break;

            case TOK.minusMinus:
                e = new AST.PostExp(EXP.minusMinus, loc, e);
                break;

            case TOK.leftParenthesis:
                AST.Expressions* args = new AST.Expressions();
                AST.Identifiers* names = new AST.Identifiers();
                parseNamedArguments(args, names);
                e = new AST.CallExp(loc, e, args, names);
                continue;

            case TOK.leftBracket:
                {
                    // array dereferences:
                    //      array[index]
                    //      array[]
                    //      array[lwr .. upr]
                    AST.Expression index;
                    AST.Expression upr;
                    auto arguments = new AST.Expressions();

                    inBrackets++;
                    nextToken();
                    while (token.value != TOK.rightBracket && token.value != TOK.endOfFile)
                    {
                        index = parseAssignExp();
                        if (token.value == TOK.slice)
                        {
                            // array[..., lwr..upr, ...]
                            nextToken();
                            upr = parseAssignExp();
                            arguments.push(new AST.IntervalExp(loc, index, upr));
                        }
                        else
                            arguments.push(index);
                        if (token.value == TOK.rightBracket)
                            break;
                        check(TOK.comma);
                    }
                    check(TOK.rightBracket);
                    inBrackets--;
                    e = new AST.ArrayExp(loc, e, arguments);
                    continue;
                }
            default:
                return e;
            }
            nextToken();
        }
    }

    private AST.Expression parseMulExp()
    {
        const loc = token.loc;
        auto e = parseUnaryExp();

        while (1)
        {
            switch (token.value)
            {
            case TOK.mul:
                nextToken();
                auto e2 = parseUnaryExp();
                e = new AST.MulExp(loc, e, e2);
                continue;

            case TOK.div:
                nextToken();
                auto e2 = parseUnaryExp();
                e = new AST.DivExp(loc, e, e2);
                continue;

            case TOK.mod:
                nextToken();
                auto e2 = parseUnaryExp();
                e = new AST.ModExp(loc, e, e2);
                continue;

            default:
                break;
            }
            break;
        }
        return e;
    }

    private AST.Expression parseAddExp()
    {
        const loc = token.loc;
        auto e = parseMulExp();

        while (1)
        {
            switch (token.value)
            {
            case TOK.add:
                nextToken();
                auto e2 = parseMulExp();
                e = new AST.AddExp(loc, e, e2);
                continue;

            case TOK.min:
                nextToken();
                auto e2 = parseMulExp();
                e = new AST.MinExp(loc, e, e2);
                continue;

            case TOK.tilde:
                nextToken();
                auto e2 = parseMulExp();
                e = new AST.CatExp(loc, e, e2);
                continue;

            default:
                break;
            }
            break;
        }
        return e;
    }

    private AST.Expression parseShiftExp()
    {
        const loc = token.loc;
        auto e = parseAddExp();

        while (1)
        {
            switch (token.value)
            {
            case TOK.leftShift:
                nextToken();
                auto e2 = parseAddExp();
                e = new AST.ShlExp(loc, e, e2);
                continue;

            case TOK.rightShift:
                nextToken();
                auto e2 = parseAddExp();
                e = new AST.ShrExp(loc, e, e2);
                continue;

            case TOK.unsignedRightShift:
                nextToken();
                auto e2 = parseAddExp();
                e = new AST.UshrExp(loc, e, e2);
                continue;

            default:
                break;
            }
            break;
        }
        return e;
    }

    private AST.Expression parseCmpExp()
    {
        const loc = token.loc;

        auto e = parseShiftExp();
        EXP op = EXP.reserved;

        switch (token.value)
        {
        case TOK.equal:         op = EXP.equal; goto Lequal;
        case TOK.notEqual:      op = EXP.notEqual; goto Lequal;
        Lequal:
            nextToken();
            auto e2 = parseShiftExp();
            e = new AST.EqualExp(op, loc, e, e2);
            break;

        case TOK.not:
        {
            // Attempt to identify '!is'
            const tv = peekNext();
            if (tv == TOK.in_)
            {
                nextToken();
                nextToken();
                auto e2 = parseShiftExp();
                e = new AST.InExp(loc, e, e2);
                e = new AST.NotExp(loc, e);
                break;
            }
            if (tv != TOK.is_)
                break;
            nextToken();
            op = EXP.notIdentity;
            goto Lidentity;
        }
        case TOK.is_:           op = EXP.identity; goto Lidentity;
        Lidentity:
            nextToken();
            auto e2 = parseShiftExp();
            e = new AST.IdentityExp(op, loc, e, e2);
            break;

        case TOK.lessThan:       op = EXP.lessThan;       goto Lcmp;
        case TOK.lessOrEqual:    op = EXP.lessOrEqual;    goto Lcmp;
        case TOK.greaterThan:    op = EXP.greaterThan;    goto Lcmp;
        case TOK.greaterOrEqual: op = EXP.greaterOrEqual; goto Lcmp;
        Lcmp:
            nextToken();
            auto e2 = parseShiftExp();
            e = new AST.CmpExp(op, loc, e, e2);
            break;

        case TOK.in_:
            nextToken();
            auto e2 = parseShiftExp();
            e = new AST.InExp(loc, e, e2);
            break;

        default:
            break;
        }
        return e;
    }

    private AST.Expression parseAndExp()
    {
        Loc loc = token.loc;
        auto e = parseCmpExp();
        while (token.value == TOK.and)
        {
            checkParens(TOK.and, e);
            nextToken();
            auto e2 = parseCmpExp();
            checkParens(TOK.and, e2);
            e = new AST.AndExp(loc, e, e2);
            loc = token.loc;
        }
        return e;
    }

    private AST.Expression parseXorExp()
    {
        const loc = token.loc;

        auto e = parseAndExp();
        while (token.value == TOK.xor)
        {
            checkParens(TOK.xor, e);
            nextToken();
            auto e2 = parseAndExp();
            checkParens(TOK.xor, e2);
            e = new AST.XorExp(loc, e, e2);
        }
        return e;
    }

    private AST.Expression parseOrExp()
    {
        const loc = token.loc;

        auto e = parseXorExp();
        while (token.value == TOK.or)
        {
            checkParens(TOK.or, e);
            nextToken();
            auto e2 = parseXorExp();
            checkParens(TOK.or, e2);
            e = new AST.OrExp(loc, e, e2);
        }
        return e;
    }

    private AST.Expression parseAndAndExp()
    {
        const loc = token.loc;

        auto e = parseOrExp();
        while (token.value == TOK.andAnd)
        {
            nextToken();
            auto e2 = parseOrExp();
            e = new AST.LogicalExp(loc, EXP.andAnd, e, e2);
        }
        return e;
    }

    private AST.Expression parseOrOrExp()
    {
        const loc = token.loc;

        auto e = parseAndAndExp();
        while (token.value == TOK.orOr)
        {
            nextToken();
            auto e2 = parseAndAndExp();
            e = new AST.LogicalExp(loc, EXP.orOr, e, e2);
        }
        return e;
    }

    private AST.Expression parseCondExp()
    {
        const loc = token.loc;

        auto e = parseOrOrExp();
        if (token.value == TOK.question)
        {
            nextToken();
            auto e1 = parseExpression();
            check(TOK.colon);
            auto e2 = parseCondExp();
            e = new AST.CondExp(loc, e, e1, e2);
        }
        return e;
    }

    AST.Expression parseAssignExp()
    {
        AST.Expression e;
        e = parseCondExp();
        if (e is null)
            return e;

        // require parens for e.g. `t ? a = 1 : b = 2`
        void checkRequiredParens()
        {
            if (e.op == EXP.question && !e.parens)
                eSink.error(e.loc, "`%s` must be surrounded by parentheses when next to operator `%s`",
                    e.toChars(), Token.toChars(token.value));
        }

        const loc = token.loc;
        switch (token.value)
        {
        case TOK.assign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.AssignExp(loc, e, e2);
            break;

        case TOK.addAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.AddAssignExp(loc, e, e2);
            break;

        case TOK.minAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.MinAssignExp(loc, e, e2);
            break;

        case TOK.mulAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.MulAssignExp(loc, e, e2);
            break;

        case TOK.divAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.DivAssignExp(loc, e, e2);
            break;

        case TOK.modAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.ModAssignExp(loc, e, e2);
            break;

        case TOK.powAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.PowAssignExp(loc, e, e2);
            break;

        case TOK.andAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.AndAssignExp(loc, e, e2);
            break;

        case TOK.orAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.OrAssignExp(loc, e, e2);
            break;

        case TOK.xorAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.XorAssignExp(loc, e, e2);
            break;

        case TOK.leftShiftAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.ShlAssignExp(loc, e, e2);
            break;

        case TOK.rightShiftAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.ShrAssignExp(loc, e, e2);
            break;

        case TOK.unsignedRightShiftAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.UshrAssignExp(loc, e, e2);
            break;

        case TOK.concatenateAssign:
            checkRequiredParens();
            nextToken();
            auto e2 = parseAssignExp();
            e = new AST.CatAssignExp(loc, e, e2);
            break;

        default:
            break;
        }

        return e;
    }

    /*************************
     * Collect argument list.
     * Assume current token is ',', '$(LPAREN)' or '['.
     */
    private AST.Expressions* parseArguments()
    {
        // function call
        AST.Expressions* arguments = new AST.Expressions();
        parseNamedArguments(arguments, null);
        return arguments;
    }

    /*************************
     * Collect argument list.
     * Assume current token is ',', '$(LPAREN)' or '['.
     */
    private void parseNamedArguments(AST.Expressions* arguments, AST.Identifiers* names)
    {
        assert(arguments);

        const endtok = token.value == TOK.leftBracket ? TOK.rightBracket : TOK.rightParenthesis;

        nextToken();

        while (token.value != endtok && token.value != TOK.endOfFile)
        {
            if (peekNext() == TOK.colon)
            {
                // Named argument `name: exp`
                auto loc = token.loc;
                auto ident = token.ident;
                check(TOK.identifier);
                check(TOK.colon);
                if (names)
                    names.push(ident);
                else
                    error(loc, "named arguments not allowed here");
            }
            else
            {
                if (names)
                    names.push(null);
            }

            auto arg = parseAssignExp();
            arguments.push(arg);

            if (token.value != TOK.comma)
                break;

            nextToken(); //comma
        }
        check(endtok);
    }

    /*******************************************
     * Params:
     *    thisexp = If not null, it is the `this` reference for the creation
     *              of an inner class.
     *              https://dlang.org/spec/class.html#nested-explicit
     *              https://dlang.org/spec/expression.html#postfix_expressions
     *              If null, then it is a NewExpression.
     *              https://dlang.org/spec/expression.html#NewExpression
     * Returns:
     *   NewExpression
     */
    private AST.Expression parseNewExp(AST.Expression thisexp)
    {
        const loc = token.loc;

        nextToken();     // skip past `new`

        // parse PlacementExpression if any
        AST.Expression placement;
        if (token.value == TOK.leftParenthesis)
        {
            nextToken();
            placement = parseAssignExp();
            check(TOK.rightParenthesis);
        }

        AST.Expressions* arguments = null;
        AST.Identifiers* names = null;

        // An anonymous nested class starts with "class"
        if (token.value == TOK.class_)
        {
            nextToken();
            if (token.value == TOK.leftParenthesis)
            {
                arguments = new AST.Expressions();
                names = new AST.Identifiers();
                parseNamedArguments(arguments, names);
            }

            AST.BaseClasses* baseclasses = null;
            if (token.value != TOK.leftCurly)
                baseclasses = parseBaseClasses();

            Identifier id = null;
            AST.Dsymbols* members = null;

            if (token.value != TOK.leftCurly)
            {
                error("`{ members }` expected for anonymous class");
            }
            else
            {
                nextToken();
                members = parseDeclDefs(0);
                if (token.value != TOK.rightCurly)
                    error("class member expected");
                nextToken();
            }

            auto cd = new AST.ClassDeclaration(loc, id, baseclasses, members, false);
            auto e = new AST.NewAnonClassExp(loc, placement, thisexp, cd, arguments);
            return e;
        }

        const stc = parseTypeCtor();
        auto t = parseBasicType(true);
        t = parseTypeSuffixes(t);
        t = t.addSTC(stc);
        if (t.ty == Taarray)
        {
            AST.TypeAArray taa = cast(AST.TypeAArray)t;
            AST.Type index = taa.index;
            // `new Type[expr]` is a static array
            auto edim = AST.typeToExpression(index);
            if (edim)
                t = new AST.TypeSArray(taa.next, edim);
        }
        else if (token.value == TOK.leftParenthesis && t.ty != Tsarray)
        {
            arguments = new AST.Expressions();
            names = new AST.Identifiers();
            parseNamedArguments(arguments, names);
        }

        auto e = new AST.NewExp(loc, placement, thisexp, t, arguments, names);
        return e;
    }

    /**********************************************
     */
    private void addComment(AST.Dsymbol s, const(char)* blockComment)
    {
        if (s !is null)
            this.addComment(s, blockComment.toDString());
    }

    private void addComment(AST.Dsymbol s, const(char)[] blockComment)
    {
        if (s !is null)
        {
            s.addComment(combineComments(blockComment, token.lineComment, true));
            token.lineComment = null;
        }
    }

    /**********************************************
     * Recognize builtin @ attributes
     * Params:
     *  ident = identifier
     * Returns:
     *  storage class for attribute, 0 if not
     */
    static STC isBuiltinAtAttribute(Identifier ident)
    {
        return (ident == Id.property) ? STC.property :
               (ident == Id.nogc)     ? STC.nogc     :
               (ident == Id.safe)     ? STC.safe     :
               (ident == Id.trusted)  ? STC.trusted  :
               (ident == Id.system)   ? STC.system   :
               (ident == Id.live)     ? STC.live     :
               (ident == Id.future)   ? STC.future   :
               (ident == Id.disable)  ? STC.disable  :
               STC.none;
    }

    enum STC atAttrGroup =
                STC.property |
                STC.nogc     |
                STC.safe     |
                STC.trusted  |
                STC.system   |
                STC.live     |
                /*STC.future   |*/ // probably should be included
                STC.disable;

    void usageOfBodyKeyword()
    {
        if (mod.edition >= Edition.v2024)
        {
            eSink.error(token.loc, "usage of identifer `body` as a keyword is obsolete. Use `do` instead.");
        }
    }
}

enum PREC : int
{
    zero,
    expr,
    assign,
    cond,
    oror,
    andand,
    or,
    xor,
    and,
    equal,
    rel,
    shift,
    add,
    mul,
    pow,
    unary,
    primary,
}

/**********************************
 * Set operator precedence for each operator.
 *
 * Used by hdrgen
 */
immutable PREC[EXP.max + 1] precedence =
[
    EXP.type : PREC.expr,
    EXP.error : PREC.expr,
    EXP.objcClassReference : PREC.expr, // Objective-C class reference, same as EXP.type

    EXP.mixin_ : PREC.primary,

    EXP.import_ : PREC.primary,
    EXP.dotVariable : PREC.primary,
    EXP.scope_ : PREC.primary,
    EXP.identifier : PREC.primary,
    EXP.this_ : PREC.primary,
    EXP.super_ : PREC.primary,
    EXP.int64 : PREC.primary,
    EXP.float64 : PREC.primary,
    EXP.complex80 : PREC.primary,
    EXP.null_ : PREC.primary,
    EXP.string_ : PREC.primary,
    EXP.arrayLiteral : PREC.primary,
    EXP.assocArrayLiteral : PREC.primary,
    EXP.classReference : PREC.primary,
    EXP.file : PREC.primary,
    EXP.fileFullPath : PREC.primary,
    EXP.line : PREC.primary,
    EXP.moduleString : PREC.primary,
    EXP.functionString : PREC.primary,
    EXP.prettyFunction : PREC.primary,
    EXP.typeid_ : PREC.primary,
    EXP.is_ : PREC.primary,
    EXP.assert_ : PREC.primary,
    EXP.halt : PREC.primary,
    EXP.template_ : PREC.primary,
    EXP.dSymbol : PREC.primary,
    EXP.function_ : PREC.primary,
    EXP.variable : PREC.primary,
    EXP.symbolOffset : PREC.primary,
    EXP.structLiteral : PREC.primary,
    EXP.compoundLiteral : PREC.primary,
    EXP.arrayLength : PREC.primary,
    EXP.delegatePointer : PREC.primary,
    EXP.delegateFunctionPointer : PREC.primary,
    EXP.remove : PREC.primary,
    EXP.tuple : PREC.primary,
    EXP.traits : PREC.primary,
    EXP.overloadSet : PREC.primary,
    EXP.void_ : PREC.primary,
    EXP.vectorArray : PREC.primary,
    EXP._Generic : PREC.primary,

    // post
    EXP.dotTemplateInstance : PREC.primary,
    EXP.dotIdentifier : PREC.primary,
    EXP.dotTemplateDeclaration : PREC.primary,
    EXP.dot : PREC.primary,
    EXP.dotType : PREC.primary,
    EXP.plusPlus : PREC.primary,
    EXP.minusMinus : PREC.primary,
    EXP.prePlusPlus : PREC.primary,
    EXP.preMinusMinus : PREC.primary,
    EXP.call : PREC.primary,
    EXP.slice : PREC.primary,
    EXP.array : PREC.primary,
    EXP.index : PREC.primary,

    EXP.delegate_ : PREC.unary,
    EXP.address : PREC.unary,
    EXP.star : PREC.unary,
    EXP.negate : PREC.unary,
    EXP.uadd : PREC.unary,
    EXP.not : PREC.unary,
    EXP.tilde : PREC.unary,
    EXP.delete_ : PREC.unary,
    EXP.new_ : PREC.unary,
    EXP.newAnonymousClass : PREC.unary,
    EXP.cast_ : PREC.unary,
    EXP.throw_ : PREC.unary,

    EXP.vector : PREC.unary,
    EXP.pow : PREC.pow,

    EXP.mul : PREC.mul,
    EXP.div : PREC.mul,
    EXP.mod : PREC.mul,

    EXP.add : PREC.add,
    EXP.min : PREC.add,
    EXP.concatenate : PREC.add,

    EXP.leftShift : PREC.shift,
    EXP.rightShift : PREC.shift,
    EXP.unsignedRightShift : PREC.shift,

    EXP.lessThan : PREC.rel,
    EXP.lessOrEqual : PREC.rel,
    EXP.greaterThan : PREC.rel,
    EXP.greaterOrEqual : PREC.rel,
    EXP.in_ : PREC.rel,

    /* Note that we changed precedence, so that < and != have the same
     * precedence. This change is in the parser, too.
     */
    EXP.equal : PREC.rel,
    EXP.notEqual : PREC.rel,
    EXP.identity : PREC.rel,
    EXP.notIdentity : PREC.rel,

    EXP.and : PREC.and,
    EXP.xor : PREC.xor,
    EXP.or : PREC.or,

    EXP.andAnd : PREC.andand,
    EXP.orOr : PREC.oror,

    EXP.question : PREC.cond,

    EXP.assign : PREC.assign,
    EXP.construct : PREC.assign,
    EXP.blit : PREC.assign,
    EXP.loweredAssignExp : PREC.assign,
    EXP.addAssign : PREC.assign,
    EXP.minAssign : PREC.assign,
    EXP.concatenateAssign : PREC.assign,
    EXP.concatenateElemAssign : PREC.assign,
    EXP.concatenateDcharAssign : PREC.assign,
    EXP.mulAssign : PREC.assign,
    EXP.divAssign : PREC.assign,
    EXP.modAssign : PREC.assign,
    EXP.powAssign : PREC.assign,
    EXP.leftShiftAssign : PREC.assign,
    EXP.rightShiftAssign : PREC.assign,
    EXP.unsignedRightShiftAssign : PREC.assign,
    EXP.andAssign : PREC.assign,
    EXP.orAssign : PREC.assign,
    EXP.xorAssign : PREC.assign,

    EXP.comma : PREC.expr,
    EXP.declaration : PREC.expr,

    EXP.interval : PREC.assign,
];

enum ParseStatementFlags : int
{
    scope_        = 2,        // start a new scope
    curly         = 4,        // { } statement is required
    curlyScope    = 8,        // { } starts a new scope
    semiOk        = 0x10,     // empty ';' are really ok
}

struct PrefixAttributes(AST)
{
    STC storageClass;
    AST.Expression depmsg;
    LINK link;
    AST.Visibility visibility;
    bool setAlignment;
    AST.Expression ealign;
    AST.Expressions* udas;
    const(char)* comment;
}

/// The result of the `ParseLinkage` function
struct ParsedLinkage(AST)
{
    /// What linkage was specified
    LINK link;
    /// If `extern(C++, class|struct)`, contains the `class|struct`
    CPPMANGLE cppmangle;
    /// If `extern(C++, some.identifier)`, will be the identifiers
    AST.Identifiers* idents;
    /// If `extern(C++, (some_tuple_expression)|"string"), will be the expressions
    AST.Expressions* identExps;
}


/*********************************** Private *************************************/

/***********************
 * How multiple declarations are parsed.
 * If 1, treat as C.
 * If 0, treat:
 *      int *p, i;
 * as:
 *      int* p;
 *      int* i;
 */
private enum CDECLSYNTAX = 0;

/*****
 * Support C cast syntax:
 *      (type)(expression)
 */
private enum CCASTSYNTAX = 1;

/*****
 * Support postfix C array declarations, such as
 *      int a[3][4];
 */
private enum CARRAYDECL = 1;

/*****************************
 * Destructively extract storage class from pAttrs.
 */
private STC getStorageClass(AST)(PrefixAttributes!(AST)* pAttrs)
{
    STC stc = STC.none;
    if (pAttrs)
    {
        stc = pAttrs.storageClass;
        pAttrs.storageClass = STC.none;
    }
    return stc;
}
