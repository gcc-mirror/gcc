/**
 * Does the semantic 1 pass on the AST, which looks at symbol declarations but not initializers
 * or function bodies.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dsymbolsem.d, _dsymbolsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_dsymbolsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dsymbolsem.d
 */

module dmd.dsymbolsem;

import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.attribsem;
import dmd.clone;
import dmd.cond;
import dmd.timetrace;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.deps;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.dversion;
import dmd.enumsem;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.importc;
import dmd.init;
import dmd.initsem;
import dmd.hdrgen;
import dmd.lexer;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.nspace;
import dmd.objc;
import dmd.optimize;
import dmd.parse;
import dmd.root.array;
import dmd.root.string;
import dmd.root.rmem;
import dmd.root.speller;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.safe;
import dmd.semantic2;
import dmd.semantic3;
import dmd.sideeffect;
import dmd.staticassert;
import dmd.tokens;
import dmd.statement;
import dmd.statementsem : ready;
import dmd.target;
import dmd.targetcompiler;
import dmd.templatesem;
import dmd.typesem;
import dmd.visitor;

enum LOG = false;

/***************************************
 * Create a new scope from sc.
 * semantic, semantic2 and semantic3 will use this for aggregate members.
 */
Scope* newScope(AggregateDeclaration _this, Scope* sc)
{
    static Scope* defaultNewScope(AggregateDeclaration _this, Scope* sc)
    {
        auto sc2 = sc.push(_this);
        sc2.stc &= STC.flowThruAggregate;
        sc2.parent = _this;
        sc2.inunion = _this.isUnionDeclaration();
        sc2.visibility = Visibility(Visibility.Kind.public_);
        sc2.explicitVisibility = false;
        sc2.aligndecl = null;
        sc2.userAttribDecl = null;
        sc2.namespace = null;
        return sc2;
    }

    static Scope* classNewScope(ClassDeclaration _this, Scope* sc)
    {
        auto sc2 = defaultNewScope(_this, sc);
        if (_this.isCOMclass())
        {
            /* This enables us to use COM objects under Linux and
             * work with things like XPCOM
             */
            sc2.linkage = target.systemLinkage();
        }
        return sc2;
    }

    static Scope* interfaceNewScope(InterfaceDeclaration _this, Scope* sc)
    {
        auto sc2 = classNewScope(_this, sc);
        if (_this.com)
            sc2.linkage = LINK.windows;
        else if (_this.classKind == ClassKind.cpp)
            sc2.linkage = LINK.cpp;
        else if (_this.classKind == ClassKind.objc)
            sc2.linkage = LINK.objc;
        return sc2;
    }

    if (auto _id = _this.isInterfaceDeclaration())
        return interfaceNewScope(_id, sc);
    else if (auto cd = _this.isClassDeclaration())
        return classNewScope(cd, sc);
    return defaultNewScope(_this, sc);
}

void addObjcSymbols(Dsymbol _this, ClassDeclarations* classes, ClassDeclarations* categories)
{
    if (auto ad = _this.isAttribDeclaration())
        objc.addSymbols(ad, classes, categories);
    else if (auto cd = _this.isClassDeclaration())
        objc.addSymbols(cd, classes, categories);
}

/************************************
 * Maybe `ident` was a C or C++ name. Check for that,
 * and suggest the D equivalent.
 * Params:
 *  ident = unknown identifier
 * Returns:
 *  D identifier string if found, null if not
 */
const(char)* search_correct_C(Identifier ident)
{
    import dmd.astenums : Twchar;
    TOK tok;
    if (ident == Id.NULL)
        tok = TOK.null_;
    else if (ident == Id.TRUE)
        tok = TOK.true_;
    else if (ident == Id.FALSE)
        tok = TOK.false_;
    else if (ident == Id.unsigned)
        tok = TOK.uns32;
    else if (ident == Id.wchar_t)
        tok = target.c.wchar_tsize == 2 ? TOK.wchar_ : TOK.dchar_;
    else
        return null;
    return Token.toChars(tok);
}

/******************************
 * Add symbol s to innermost symbol table.
 * Params:
 *  _this = scope object
 *  s = symbol to insert
 * Returns:
 *  null if already in table, `s` if not
 */
Dsymbol insert(Scope* _this, Dsymbol s)
{
    //printf("insert() %s\n", s.toChars());
    if (VarDeclaration vd = s.isVarDeclaration())
    {
        if (_this.lastVar)
            vd.lastVar = _this.lastVar;
        _this.lastVar = vd;
    }
    else if (WithScopeSymbol ss = s.isWithScopeSymbol())
    {
        if (VarDeclaration vd = ss.withstate.wthis)
        {
            if (_this.lastVar)
                vd.lastVar = _this.lastVar;
            _this.lastVar = vd;
        }
        return null;
    }

    auto scopesym = _this.inner().scopesym;
    //printf("\t\tscopesym = %p\n", scopesym);
    if (!scopesym.symtab)
        scopesym.symtab = new DsymbolTable();
    if (!_this.inCfile)
        return scopesym.symtabInsert(s);

    // ImportC insert
    if (!scopesym.symtabInsert(s)) // if already in table
    {
        Dsymbol s2 = scopesym.symtabLookup(s, s.ident); // s2 is existing entry

        auto svar = s.isVarDeclaration();
        auto s2var = s2.isVarDeclaration();
        if (((svar && svar.storage_class & STC.extern_) &&
                (s2var && s2var.storage_class & STC.extern_) && _this.func) ||
                s.isFuncDeclaration())
        {
            return handleSymbolRedeclarations(*_this, s, s2, scopesym);
        }
        else // aside externs and func decls, we should be free to handle tags
        {
            return handleTagSymbols(*_this, s, s2, scopesym);
        }
    }
    return s; // inserted
}

/*******************************************
 * Look for member of the form:
 *      const(MemberInfo)[] getMembers(string);
 * Returns NULL if not found
 */
FuncDeclaration findGetMembers(ScopeDsymbol dsym)
{
    import dmd.opover : search_function;
    Dsymbol s = search_function(dsym, Id.getmembers);
    FuncDeclaration fdx = s ? s.isFuncDeclaration() : null;
    version (none)
    {
        // Finish
        __gshared TypeFunction tfgetmembers;
        if (!tfgetmembers)
        {
            Scope sc;
            sc.eSink = global.errorSink;
            Parameters* p = new Parameter(STC.in_, Type.tchar.constOf().arrayOf(), null, null);
            auto parameters = new Parameters(p);
            Type tret = null;
            TypeFunction tf = new TypeFunction(parameters, tret, VarArg.none, LINK.d);
            tfgetmembers = tf.dsymbolSemantic(Loc.initial, &sc).isTypeFunction();
        }
        if (fdx)
            fdx = fdx.overloadExactMatch(tfgetmembers);
    }
    if (fdx && fdx.isVirtual())
        fdx = null;
    return fdx;
}

/***********************************
 * Retrieve the .min or .max values.
 * Only valid after semantic analysis.
 * Params:
 *  _this = bit field instance
 *  id = Id.min or Id.max
 * Returns:
 *  the min or max value
 */
ulong getMinMax(BitFieldDeclaration _this, Identifier id)
{
    const width = _this.fieldWidth;
    const uns = _this.type.isUnsigned();
    const min = id == Id.min;
    ulong v;
    assert(width != 0);  // should have been rejected in semantic pass
    if (width == ulong.sizeof * 8)
        v = uns ? (min ? ulong.min : ulong.max)
                : (min ?  long.min :  long.max);
    else
        v = uns ? (min ? 0
                       : (1L << width) - 1)
                : (min ? -(1L << (width - 1))
                       :  (1L << (width - 1)) - 1);
    return v;
}

/* Append vthis field (this.tupleof[$-1]) to make this aggregate type nested.
 */
void makeNested(AggregateDeclaration _this)
{
    if (_this.enclosing) // if already nested
        return;
    if (_this.sizeok == Sizeok.done)
        return;
    if (_this.isUnionDeclaration() || _this.isInterfaceDeclaration())
        return;
    if (_this.storage_class & STC.static_)
        return;

    // If nested struct, add in hidden 'this' pointer to outer scope
    auto s = _this.toParentLocal();
    if (!s)
        s = _this.toParent2();
    if (!s)
        return;
    Type t = null;
    if (auto fd = s.isFuncDeclaration())
    {
        _this.enclosing = fd;

        /* https://issues.dlang.org/show_bug.cgi?id=14422
         * If a nested class parent is a function, its
         * context pointer (== `outer`) should be void* always.
         */
        t = Type.tvoidptr;
    }
    else if (auto ad = s.isAggregateDeclaration())
    {
        if (_this.isClassDeclaration() && ad.isClassDeclaration())
        {
            _this.enclosing = ad;
        }
        else if (_this.isStructDeclaration())
        {
            if (auto ti = ad.parent.isTemplateInstance())
            {
                _this.enclosing = ti.enclosing;
            }
        }
        t = ad.handleType();
    }
    if (_this.enclosing)
    {
        import dmd.typesem : alignment;
        //printf("makeNested %s, enclosing = %s\n", toChars(), enclosing.toChars());
        assert(t);
        if (t.ty == Tstruct)
            t = Type.tvoidptr; // t should not be a ref type

        assert(!_this.vthis);
        _this.vthis = new ThisDeclaration(_this.loc, t);
        //vthis.storage_class |= STC.ref_;

        // Emulate vthis.addMember()
        _this.members.push(_this.vthis);

        // Emulate vthis.dsymbolSemantic()
        _this.vthis.storage_class |= STC.field;
        _this.vthis.parent = _this;
        _this.vthis.visibility = Visibility(Visibility.Kind.public_);
        _this.vthis.alignment = t.alignment();
        _this.vthis.semanticRun = PASS.semanticdone;

        if (_this.sizeok == Sizeok.fwd)
            _this.fields.push(_this.vthis);

        _this.makeNested2();
    }
}

/* Append vthis2 field (this.tupleof[$-1]) to add a second context pointer.
 */
void makeNested2(AggregateDeclaration _this)
{
    import dmd.typesem : alignment;

    if (_this.vthis2)
        return;
    if (!_this.vthis)
        _this.makeNested();   // can't add second before first
    if (!_this.vthis)
        return;
    if (_this.sizeok == Sizeok.done)
        return;
    if (_this.isUnionDeclaration() || _this.isInterfaceDeclaration())
        return;
    if (_this.storage_class & STC.static_)
        return;

    auto s0 = _this.toParentLocal();
    auto s = _this.toParent2();
    if (!s || !s0 || s == s0)
        return;
    auto cd = s.isClassDeclaration();
    Type t = cd ? cd.type : Type.tvoidptr;

    _this.vthis2 = new ThisDeclaration(_this.loc, t);
    //vthis2.storage_class |= STC.ref_;

    // Emulate vthis2.addMember()
    _this.members.push(_this.vthis2);

    // Emulate vthis2.dsymbolSemantic()
    _this.vthis2.storage_class |= STC.field;
    _this.vthis2.parent = _this;
    _this.vthis2.visibility = Visibility(Visibility.Kind.public_);
    _this.vthis2.alignment = t.alignment();
    _this.vthis2.semanticRun = PASS.semanticdone;

    if (_this.sizeok == Sizeok.fwd)
        _this.fields.push(_this.vthis2);
}

/************************************
 * Perform unqualified name lookup by following the chain of scopes up
 * until found.
 *
 * Params:
 *  _this = Scope object
 *  loc = location to use for error messages
 *  ident = name to look up
 *  pscopesym = if supplied and name is found, set to scope that ident was found in, otherwise set to null
 *  flags = modify search based on flags
 *
 * Returns:
 *  symbol if found, null if not
 */
Dsymbol search(Scope* _this, Loc loc, Identifier ident, out Dsymbol pscopesym, SearchOptFlags flags = SearchOpt.all)
{
    version (LOGSEARCH)
    {
        printf("Scope.search(%p, '%s' flags=x%x)\n", _this, ident.toChars(), flags);
        // Print scope chain
        for (Scope* sc = _this; sc; sc = sc.enclosing)
        {
            if (!sc.scopesym)
                continue;
            printf("\tscope %s\n", sc.scopesym.toChars());
        }

        static void printMsg(string txt, Dsymbol s)
        {
            printf("%.*s  %s.%s, kind = '%s'\n", cast(int)txt.length, txt.ptr,
                s.parent ? s.parent.toChars() : "", s.toChars(), s.kind());
        }
    }

    // This function is called only for unqualified lookup
    assert(!(flags & (SearchOpt.localsOnly | SearchOpt.importsOnly)));

    /* If ident is "start at module scope", only look at module scope
     */
    if (ident == Id.empty)
    {
        // Look for module scope
        for (Scope* sc = _this; sc; sc = sc.enclosing)
        {
            assert(sc != sc.enclosing);
            if (!sc.scopesym)
                continue;
            if (Dsymbol s = sc.scopesym.isModule())
            {
                //printMsg("\tfound", s);
                pscopesym = sc.scopesym;
                return s;
            }
        }
        return null;
    }

    Dsymbol checkAliasThis(AggregateDeclaration ad, Identifier ident, SearchOptFlags flags, Expression* exp)
    {
        if (!ad || !ad.aliasthis)
            return null;

        Declaration decl = ad.aliasthis.sym.isDeclaration();
        if (!decl)
            return null;

        Type t = decl.type;
        ScopeDsymbol sds;
        TypeClass tc;
        TypeStruct ts;
        switch(t.ty)
        {
            case Tstruct:
                ts = cast(TypeStruct)t;
                sds = ts.sym;
                break;
            case Tclass:
                tc = cast(TypeClass)t;
                sds = tc.sym;
                break;
            case Tinstance:
                sds = (cast(TypeInstance)t).tempinst;
                break;
            case Tenum:
                sds = (cast(TypeEnum)t).sym;
                break;
            default: break;
        }

        if (!sds)
            return null;

        Dsymbol ret = sds.search(loc, ident, flags);
        if (ret)
        {
            *exp = new DotIdExp(loc, *exp, ad.aliasthis.ident);
            *exp = new DotIdExp(loc, *exp, ident);
            return ret;
        }

        if (!ts && !tc)
            return null;

        Dsymbol s;
        *exp = new DotIdExp(loc, *exp, ad.aliasthis.ident);
        if (ts && !(ts.att & AliasThisRec.tracing))
        {
            ts.att = cast(AliasThisRec)(ts.att | AliasThisRec.tracing);
            s = checkAliasThis(sds.isAggregateDeclaration(), ident, flags, exp);
            ts.att = cast(AliasThisRec)(ts.att & ~AliasThisRec.tracing);
        }
        else if(tc && !(tc.att & AliasThisRec.tracing))
        {
            tc.att = cast(AliasThisRec)(tc.att | AliasThisRec.tracing);
            s = checkAliasThis(sds.isAggregateDeclaration(), ident, flags, exp);
            tc.att = cast(AliasThisRec)(tc.att & ~AliasThisRec.tracing);
        }
        return s;
    }

    Dsymbol searchScopes(SearchOptFlags flags)
    {
        for (Scope* sc = _this; sc; sc = sc.enclosing)
        {
            assert(sc != sc.enclosing);
            if (!sc.scopesym)
                continue;
            //printf("\tlooking in scopesym '%s', kind = '%s', flags = x%x\n", sc.scopesym.toChars(), sc.scopesym.kind(), flags);

            if (sc.scopesym.isModule())
                flags |= SearchOpt.unqualifiedModule;    // tell Module.search() that SearchOpt.localsOnly is to be obeyed
            else if (sc.inCfile && sc.scopesym.isStructDeclaration())
                continue;                                // C doesn't have struct scope

            if (Dsymbol s = sc.scopesym.search(loc, ident, flags))
            {
                if (flags & SearchOpt.tagNameSpace)
                {
                    // ImportC: if symbol is not a tag, look for it in tag table
                    if (!s.isScopeDsymbol())
                    {
                        auto ps = cast(void*)s in sc._module.tagSymTab;
                        if (!ps)
                            goto NotFound;
                        s = *ps;
                    }
                }
                //printMsg("\tfound local", s);
                pscopesym = sc.scopesym;
                return s;
            }

        NotFound:
            if (sc.previews.fixAliasThis)
            {
                Expression exp = new ThisExp(loc);
                if (Dsymbol aliasSym = checkAliasThis(sc.scopesym.isAggregateDeclaration(), ident, flags, &exp))
                {
                    //printf("found aliassym: %s\n", aliasSym.toChars());
                    pscopesym = new ExpressionDsymbol(exp);
                    return aliasSym;
                }
            }

            // Stop when we hit a module, but keep going if that is not just under the global scope
            if (sc.scopesym.isModule() && !(sc.enclosing && !sc.enclosing.enclosing))
                break;
        }
        return null;
    }

    if (_this.ignoresymbolvisibility)
        flags |= SearchOpt.ignoreVisibility;

    // First look in local scopes
    Dsymbol s = searchScopes(flags | SearchOpt.localsOnly);
    version (LOGSEARCH) if (s) printMsg("-Scope.search() found local", s);
    if (!s)
    {
        // Second look in imported modules
        s = searchScopes(flags | SearchOpt.importsOnly);
        version (LOGSEARCH) if (s) printMsg("-Scope.search() found import", s);
    }
    return s;
}

Dsymbol search_correct(Scope* _this, Identifier ident)
{
    if (global.gag)
        return null; // don't do it for speculative compiles; too time consuming

    /************************************************
     * Given the failed search attempt, try to find
     * one with a close spelling.
     * Params:
     *      seed = identifier to search for
     *      cost = set to the cost, which rises with each outer scope
     * Returns:
     *      Dsymbol if found, null if not
     */
    Dsymbol scope_search_fp(const(char)[] seed, out int cost)
    {
        //printf("scope_search_fp('%s')\n", seed);
        /* If not in the lexer's string table, it certainly isn't in the symbol table.
         * Doing this first is a lot faster.
         */
        if (!seed.length)
            return null;
        Identifier id = Identifier.lookup(seed);
        if (!id)
            return null;
        Scope* sc = _this;
        Module.clearCache();
        Dsymbol scopesym;
        Dsymbol s = sc.search(Loc.initial, id, scopesym, SearchOpt.ignoreErrors);
        if (!s)
            return null;

        // Do not show `@disable`d declarations
        if (auto decl = s.isDeclaration())
            if (decl.storage_class & STC.disable)
                return null;
        // Or `deprecated` ones if we're not in a deprecated scope
        if (s.isDeprecated() && !sc.isDeprecated())
            return null;

        for (cost = 0; sc; sc = sc.enclosing, ++cost)
            if (sc.scopesym == scopesym)
                break;
        if (scopesym != s.parent)
        {
            ++cost; // got to the symbol through an import
            if (s.visible().kind == Visibility.Kind.private_)
                return null;
        }
        return s;
    }

    Dsymbol scopesym;
    // search for exact name first
    if (auto s = _this.search(Loc.initial, ident, scopesym, SearchOpt.ignoreErrors))
        return s;
    return speller!scope_search_fp(ident.toString());
}

structalign_t alignment(Scope* _this)
{
    if (_this.aligndecl)
    {
        auto ad = _this.aligndecl.getAlignment(_this);
        return ad.salign;
    }
    else
    {
        structalign_t sa;
        sa.setDefault();
        return sa;
    }
}

Scope* scopeCreateGlobal(Module _module, ErrorSink eSink)
{
    Scope* sc = Scope.alloc();
    *sc = Scope.init;
    sc._module = _module;
    sc.minst = _module;
    sc.scopesym = new ScopeDsymbol();
    sc.scopesym.symtab = new DsymbolTable();
    sc.eSink = eSink;
    assert(eSink);
    // Add top level package as member of this global scope
    Dsymbol m = _module;
    while (m.parent)
        m = m.parent;
    m.addMember(null, sc.scopesym);
    m.parent = null; // got changed by addMember()
    sc.previews.setFromParams(global.params);

    if (_module.filetype == FileType.c)
        sc.inCfile = true;
    // Create the module scope underneath the global scope
    sc = sc.push(_module);
    sc.parent = _module;
    return sc;
}

/*************************************
 * Does semantic analysis on the public face of declarations.
 */
void dsymbolSemantic(Dsymbol dsym, Scope* sc)
{
    scope v = new DsymbolSemanticVisitor(sc);
    dsym.accept(v);
}

// function used to call semantic3 on a module's dependencies
void semantic3OnDependencies(Module m)
{
    if (!m)
        return;

    if (m.semanticRun > PASS.semantic3)
        return;

    m.semantic3(null);

    foreach (i; 1 .. m.aimports.length)
        semantic3OnDependencies(m.aimports[i]);
}

/*******************************************
 * Can't run semantic on s now, try again later.
 */
void addDeferredSemantic(Dsymbol s)
{
    //printf("Module::addDeferredSemantic('%s')\n", s.toChars());
    if (!s.deferred)
    {
        s.deferred = true;
        Module.deferred.push(s);
    }
}

void addDeferredSemantic2(Dsymbol s)
{
    //printf("Module::addDeferredSemantic2('%s')\n", s.toChars());
    if (!s.deferred2)
    {
        s.deferred2 = true;
        Module.deferred2.push(s);
    }
}

void addDeferredSemantic3(Dsymbol s)
{
    //printf("Module::addDeferredSemantic3('%s')\n", s.toChars());
    if (!s.deferred3)
    {
        s.deferred3 = true;
        Module.deferred3.push(s);
    }
}

/******************************************
 * Run semantic() on deferred symbols.
 */
void runDeferredSemantic()
{
    __gshared int nested;
    if (nested)
        return;
    //if (Module.deferred.length) printf("+Module::runDeferredSemantic(), len = %ld\n", deferred.length);
    nested++;

    size_t len;
    do
    {
        len = Module.deferred.length;
        if (!len)
            break;

        Dsymbol* todo;
        Dsymbol* todoalloc = null;
        Dsymbol tmp;
        if (len == 1)
        {
            todo = &tmp;
        }
        else
        {
            todo = cast(Dsymbol*)Mem.check(malloc(len * Dsymbol.sizeof));
            todoalloc = todo;
        }
        memcpy(todo, Module.deferred.tdata(), len * Dsymbol.sizeof);
        foreach (Dsymbol s; Module.deferred[])
            s.deferred = false;
        Module.deferred.setDim(0);

        foreach (i; 0..len)
        {
            Dsymbol s = todo[i];
            s.dsymbolSemantic(null);
            //printf("deferred: %s, parent = %s\n", s.toChars(), s.parent.toChars());
        }
        //printf("\tdeferred.length = %ld, len = %ld\n", deferred.length, len);
        if (todoalloc)
            free(todoalloc);
    }
    while (Module.deferred.length != len); // while making progress
    nested--;
    //printf("-Module::runDeferredSemantic(), len = %ld\n", deferred.length);
}

void runDeferredSemantic2()
{
    runDeferredSemantic();

    Dsymbols* a = &Module.deferred2;
    for (size_t i = 0; i < a.length; i++)
    {
        Dsymbol s = (*a)[i];
        s.deferred2 = false;
        //printf("[%d] %s semantic2a\n", i, s.toPrettyChars());
        s.semantic2(null);

        if (global.errors)
            break;
    }
    a.setDim(0);
}

void runDeferredSemantic3()
{
    runDeferredSemantic2();

    Dsymbols* a = &Module.deferred3;
    for (size_t i = 0; i < a.length; i++)
    {
        Dsymbol s = (*a)[i];
        s.deferred3 = false;
        //printf("[%d] %s semantic3a\n", i, s.toPrettyChars());
        s.semantic3(null);

        if (global.errors)
            break;
    }
    a.setDim(0);
}

bool isOverlappedWith(VarDeclaration _this, VarDeclaration v)
{
    import dmd.typesem : size;
    const vsz = v.type.size();
    const tsz = _this.type.size();
    assert(vsz != SIZE_INVALID && tsz != SIZE_INVALID);

    // Overlap is checked by comparing bit offsets
    auto bitoffset  = _this.offset * 8;
    auto vbitoffset =     v.offset * 8;

    // Bitsize of types are overridden by any bitfield widths.
    ulong tbitsize;
    if (auto bf = _this.isBitFieldDeclaration())
    {
        bitoffset += bf.bitOffset;
        tbitsize = bf.fieldWidth;
    }
    else
        tbitsize = tsz * 8;

    ulong vbitsize;
    if (auto vbf = v.isBitFieldDeclaration())
    {
        vbitoffset += vbf.bitOffset;
        vbitsize = vbf.fieldWidth;
    }
    else
        vbitsize = vsz * 8;

    return   bitoffset < vbitoffset + vbitsize &&
            vbitoffset <  bitoffset + tbitsize;
}

private Type tupleDeclGetType(TupleDeclaration _this)
{
    /* If this tuple represents a type, return that type
     */

    //printf("TupleDeclaration::getType() %s\n", toChars());
    if (_this.isexp || _this.building)
        return null;
    if (_this.tupletype)
        return _this.tupletype;

    /* It's only a type tuple if all the Object's are types
     */
    for (size_t i = 0; i < _this.objects.length; i++)
    {
        RootObject o = (*_this.objects)[i];
        if (!o.isType())
        {
            //printf("\tnot[%d], %p, %d\n", i, o, o.dyncast());
            return null;
        }
    }

    /* We know it's a type tuple, so build the TypeTuple
     */
    Types* types = cast(Types*)_this.objects;
    auto args = new Parameters(_this.objects.length);
    OutBuffer buf;
    int hasdeco = 1;
    for (size_t i = 0; i < types.length; i++)
    {
        Type t = (*types)[i];
        //printf("type = %s\n", t.toChars());
        version (none)
        {
            buf.printf("_%s_%d", _this.ident.toChars(), i);
            auto id = Identifier.idPool(buf.extractSlice());
            auto arg = new Parameter(Loc.initial, STC.in_, t, id, null);
        }
        else
        {
            auto arg = new Parameter(Loc.initial, STC.none, t, null, null, null);
        }
        (*args)[i] = arg;
        if (!t.deco)
            hasdeco = 0;
    }

    _this.tupletype = new TypeTuple(args);
    if (hasdeco)
        return _this.tupletype.typeSemantic(Loc.initial, null);

    return _this.tupletype;
}

private Type aliasDeclGetType(AliasDeclaration _this)
{
    if (_this.type)
        return _this.type;
    return toAlias(_this).getType();
}

private Type aggregateDeclGetType(AggregateDeclaration _this)
{
    /* Apply storage classes to forward references. (Issue 22254)
     * Note: Avoid interfaces for now. Implementing qualifiers on interface
     * definitions exposed some issues in their TypeInfo generation in DMD.
     * Related PR: https://github.com/dlang/dmd/pull/13312
     */
    if (_this.semanticRun == PASS.initial && !_this.isInterfaceDeclaration())
    {
        auto stc = _this.storage_class;
        if (_this._scope)
            stc |= _this._scope.stc;
        _this.type = _this.type.addSTC(stc);
    }
    return _this.type;
}

Type getType(Dsymbol _this)
{
    if (auto td = _this.isTupleDeclaration())
        return tupleDeclGetType(td);
    else if (auto ad = _this.isAliasDeclaration())
        return aliasDeclGetType(ad);
    else if (auto agd = _this.isAggregateDeclaration())
        return aggregateDeclGetType(agd);
    else if (auto ed = _this.isEnumDeclaration())
        return ed.type;

    // is this a type?
    return null;
}

private uinteger_t aggregateDeclSize(AggregateDeclaration _this, Loc loc)
{
    //printf("+AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
    bool ok = determineSize(_this, loc);
    //printf("-AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
    return ok ? _this.structsize : SIZE_INVALID;
}

private uinteger_t declSize(Declaration _this, Loc loc)
{
    import dmd.typesem: size;
    assert(_this.type);
    const sz = _this.type.size();
    if (sz == SIZE_INVALID)
        _this.errors = true;
    return sz;
}

/*********************************
 * Returns:
 *  SIZE_INVALID when the size cannot be determined
 */
uinteger_t size(Dsymbol _this, Loc loc)
{
    if (auto ad = _this.isAggregateDeclaration())
        return aggregateDeclSize(ad, loc);
    else if (auto d = _this.isDeclaration())
        return declSize(d, loc);
    .error(loc, "%s `%s` symbol `%s` has no size", _this.kind, _this.toPrettyChars, _this.toChars());
    return SIZE_INVALID;
}

private bool funcDeclEquals(const FuncDeclaration _this, const Dsymbol s)
{
    auto fd1 = _this;
    auto fd2 = s.isFuncDeclaration();
    if (!fd2)
        return false;

    auto fa1 = fd1.isFuncAliasDeclaration();
    auto faf1 = fa1 ? fa1.toAliasFunc() : fd1;

    auto fa2 = fd2.isFuncAliasDeclaration();
    auto faf2 = fa2 ? fa2.toAliasFunc() : fd2;

    if (fa1 && fa2)
        return faf1.equals(faf2) && fa1.hasOverloads == fa2.hasOverloads;

    bool b1 = fa1 !is null;
    if (b1 && faf1.isUnique() && !fa1.hasOverloads)
        b1 = false;

    bool b2 = fa2 !is null;
    if (b2 && faf2.isUnique() && !fa2.hasOverloads)
        b2 = false;

    if (b1 != b2)
        return false;

    return faf1.toParent().equals(faf2.toParent()) &&
           faf1.ident.equals(faf2.ident) &&
           faf1.type.equals(faf2.type);
}

private bool overDeclEquals(const OverDeclaration _this, const Dsymbol s)
{
    if (auto od2 = s.isOverDeclaration())
        return _this.aliassym.equals(od2.aliassym);
    return _this.aliassym == s;
}

private bool packageEquals(const Package _this, const Dsymbol s)
{
    // custom 'equals' for bug 17441. "package a" and "module a" are not equal
    auto p = cast(Package)s;
    return p && _this.isModule() == p.isModule() && _this.ident.equals(p.ident);
}

bool equals(const Dsymbol _this, const Dsymbol s)
{
    if (_this == s)
        return true;

    if(auto fd = _this.isFuncDeclaration())
        return funcDeclEquals(fd, s);
    else if (auto od = _this.isOverDeclaration())
        return overDeclEquals(od, s);
    else if (auto pkg = _this.isPackage())
        return packageEquals(pkg, s);

    // Overload sets don't have an ident
    // Function-local declarations may have identical names
    // if they are declared in different scopes
    if (s && _this.ident && s.ident && _this.ident.equals(s.ident) && _this.localNum == s.localNum)
        return true;

    return false;
}

private bool aliasOverloadInsert(AliasDeclaration ad, Dsymbol s)
{
    //printf("[%s] AliasDeclaration::overloadInsert('%s') s = %s %s @ [%s]\n",
    //       loc.toChars(), toChars(), s.kind(), s.toChars(), s.loc.toChars());

    /** Aliases aren't overloadable themselves, but if their Aliasee is
     *  overloadable they are converted to an overloadable Alias (either
     *  FuncAliasDeclaration or OverDeclaration).
     *
     *  This is done by moving the Aliasee into such an overloadable alias
     *  which is then used to replace the existing Aliasee. The original
     *  Alias (_this_) remains a useless shell.
     *
     *  This is a horrible mess. It was probably done to avoid replacing
     *  existing AST nodes and references, but it needs a major
     *  simplification b/c it's too complex to maintain.
     *
     *  A simpler approach might be to merge any colliding symbols into a
     *  simple Overload class (an array) and then later have that resolve
     *  all collisions.
     */
    if (ad.semanticRun < PASS.semanticdone)
    {
        /* Don't know yet what the aliased symbol is, so assume it can
         * be overloaded and check later for correctness.
         */
        if (ad.overnext)
            return ad.overnext.overloadInsert(s);
        if (s is ad)
            return true;
        ad.overnext = s;
        return true;
    }
    /* Semantic analysis is already finished, and the aliased entity
     * is not overloadable.
     */
    if (ad.type)
    {
        /*
            If type has been resolved already we could
            still be inserting an alias from an import.

            If we are handling an alias then pretend
            it was inserting and return true, if not then
            false since we didn't even pretend to insert something.
        */
        return ad._import && ad.equals(s);
    }

    // https://issues.dlang.org/show_bug.cgi?id=23865
    // only insert if the symbol can be part of a set
    const s1 = s.toAlias();
    const isInsertCandidate = s1.isFuncDeclaration() || s1.isOverDeclaration() || s1.isTemplateDeclaration();

    /* When s is added in member scope by static if, mixin("code") or others,
     * aliassym is determined already. See the case in: test/compilable/test61.d
     */
    auto sa = ad.aliassym.toAlias();

    if (auto fd = sa.isFuncDeclaration())
    {
        auto fa = new FuncAliasDeclaration(ad.ident, fd);
        fa.visibility = ad.visibility;
        fa.parent = ad.parent;
        ad.aliassym = fa;
        if (isInsertCandidate)
            return ad.aliassym.overloadInsert(s);
    }
    if (auto td = sa.isTemplateDeclaration())
    {
        auto od = new OverDeclaration(ad.ident, td.funcroot ? td.funcroot : td);
        od.visibility = ad.visibility;
        od.parent = ad.parent;
        ad.aliassym = od;
        if (isInsertCandidate)
            return ad.aliassym.overloadInsert(s);
    }
    if (auto od = sa.isOverDeclaration())
    {
        if (sa.ident != ad.ident || sa.parent != ad.parent)
        {
            od = new OverDeclaration(ad.ident, od);
            od.visibility = ad.visibility;
            od.parent = ad.parent;
            ad.aliassym = od;
        }
        if (isInsertCandidate)
            return od.overloadInsert(s);
    }
    if (auto os = sa.isOverloadSet())
    {
        if (sa.ident != ad.ident || sa.parent != ad.parent)
        {
            os = new OverloadSet(ad.ident, os);
            // TODO: visibility is lost here b/c OverloadSets have no visibility attribute
            // Might no be a practical issue, b/c the code below fails to resolve the overload anyhow.
            // ----
            // module os1;
            // import a, b;
            // private alias merged = foo; // private alias to overload set of a.foo and b.foo
            // ----
            // module os2;
            // import a, b;
            // public alias merged = bar; // public alias to overload set of a.bar and b.bar
            // ----
            // module bug;
            // import os1, os2;
            // void test() { merged(123); } // should only look at os2.merged
            //
            // os.visibility = visibility;
            os.parent = ad.parent;
            ad.aliassym = os;
        }
        if (isInsertCandidate)
        {
            os.push(s);
            return true;
        }
    }
    return false;
}

/**********************************
 * Overload existing TemplateDeclaration '_this' with the new one 's'.
 * Params:
 *    s = symbol to be inserted
 * Return: true if successful; i.e. no conflict.
 */
private bool templateOverloadInsert(TemplateDeclaration _this, Dsymbol s){
    static if (LOG)
    {
        printf("TemplateDeclaration.overloadInsert('%s')\n", s.toChars());
    }

    if (FuncDeclaration fd = s.isFuncDeclaration())
    {
        if (_this.funcroot)
            return _this.funcroot.overloadInsert(fd);
        _this.funcroot = fd;
        return _this.funcroot.overloadInsert(_this);
    }

    // https://issues.dlang.org/show_bug.cgi?id=15795
    // if candidate is an alias and its sema is not run then
    // insertion can fail because the thing it alias is not known
    if (AliasDeclaration ad = s.isAliasDeclaration())
    {
        if (s._scope)
            aliasSemantic(ad, s._scope);
        if (ad.aliassym && ad.aliassym is _this)
            return false;
    }

    TemplateDeclaration td = s.toAlias().isTemplateDeclaration();
    if (!td)
        return false;

    TemplateDeclaration pthis = _this;
    TemplateDeclaration* ptd;
    for (ptd = &pthis; *ptd; ptd = &(*ptd).overnext)
    {
    }

    td.overroot = _this;
    *ptd = td;
    static if (LOG)
    {
        printf("\ttrue: no conflict\n");
    }
    return true;
}

private bool importOverloadInsert(Import _this, Dsymbol s){
    /* Allow multiple imports with the same package base, but disallow
     * alias collisions
     * https://issues.dlang.org/show_bug.cgi?id=5412
     */
    assert(_this.ident && _this.ident == s.ident);
    if (_this.aliasId)
        return false;
    const imp = s.isImport();
    return imp && !imp.aliasId;
}

/**
 * Overload _this FuncDeclaration with the new one f.
 * Return true if successful; i.e. no conflict.
*/
private bool funcOverloadInsert(FuncDeclaration _this, Dsymbol s){
    //printf("FuncDeclaration::overloadInsert(s = %s) _this = %s\n", s.toChars(), toChars());
    assert(s != _this);
    if (AliasDeclaration ad = s.isAliasDeclaration())
    {
        if (_this.overnext)
            return _this.overnext.overloadInsert(ad);
        if (!ad.aliassym && ad.type.ty != Tident && ad.type.ty != Tinstance && ad.type.ty != Ttypeof)
        {
            //printf("\tad = '%s'\n", ad.type.toChars());
            return false;
        }
        _this.overnext = ad;
        //printf("\ttrue: no conflict\n");
        return true;
    }
    TemplateDeclaration td = s.isTemplateDeclaration();
    if (td)
    {
        if (!td.funcroot)
            td.funcroot = _this;
        if (_this.overnext)
            return _this.overnext.overloadInsert(td);
        _this.overnext = td;
        return true;
    }
    FuncDeclaration fd = s.isFuncDeclaration();
    if (!fd)
        return false;

    if (_this.overnext)
    {
        td = _this.overnext.isTemplateDeclaration();
        if (td)
            fd.overloadInsert(td);
        else
            return _this.overnext.overloadInsert(fd);
    }
    _this.overnext = fd;
    //printf("\ttrue: no conflict\n");
    return true;
}

private bool overdeclOverloadInsert(OverDeclaration _this, Dsymbol s){
    //printf("OverDeclaration::overloadInsert('%s') _this.aliassym = %p, _this.overnext = %p\n", s.toChars(), _this.aliassym, _this.overnext);
    if (_this.overnext)
        return _this.overnext.overloadInsert(s);
    if (s == _this)
        return true;
    _this.overnext = s;
    return true;
}

bool overloadInsert(Dsymbol _this, Dsymbol s)
{
    // Cannot overload postblits or destructors
    if(_this.isPostBlitDeclaration() || _this.isDtorDeclaration())
        return false;
    else if(OverDeclaration od = _this.isOverDeclaration())
        return overdeclOverloadInsert(od, s);
    else if(FuncDeclaration fd = _this.isFuncDeclaration())
        return funcOverloadInsert(fd, s);
    if(Import imp = _this.isImport())
        return importOverloadInsert(imp, s);
    else if(TemplateDeclaration td = _this.isTemplateDeclaration())
        return templateOverloadInsert(td, s);
    else if (AliasDeclaration ad = _this.isAliasDeclaration())
        return aliasOverloadInsert(ad, s);

    return false;
}

/***************************************************
 * Determine the numerical value of the AlignmentDeclaration
 * Params:
 *      ad = AlignmentDeclaration
 *      sc = context
 * Returns:
 *      ad with alignment value determined
 */
AlignDeclaration getAlignment(AlignDeclaration ad, Scope* sc)
{
    if (!ad.salign.isUnknown())   // UNKNOWN is 0
        return ad;

    if (!ad.exps)
    {
        ad.salign.setDefault();
        return ad;
    }

    dinteger_t strictest = 0;   // strictest alignment
    bool errors;
    foreach (ref exp; (*ad.exps)[])
    {
        sc = sc.startCTFE();
        auto e = exp.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        sc = sc.endCTFE();
        e = e.ctfeInterpret();
        exp = e;                // could be re-evaluated if exps are assigned to more than one AlignDeclaration by CParser.applySpecifier(),
                                // e.g. `_Alignas(8) int a, b;`
        if (e.op == EXP.error)
            errors = true;
        else
        {
            auto n = e.toInteger();
            if (sc.inCfile && n == 0)       // C11 6.7.5-6 allows 0 for alignment
                continue;

            if (n < 1 || n & (n - 1) || ushort.max < n || !e.type.isIntegral())
            {
                error(ad.loc, "alignment must be an integer positive power of 2, not 0x%llx", cast(ulong)n);
                errors = true;
            }
            if (n > strictest)  // C11 6.7.5-6
                strictest = n;
        }
    }

    if (errors || strictest == 0)  // C11 6.7.5-6 says alignment of 0 means no effect
        ad.salign.setDefault();
    else
        ad.salign.set(cast(uint) strictest);

    return ad;
}


/*********************************
 * Resolve recursive tuple expansion in eponymous template.
 */
Dsymbol toAlias2(Dsymbol s)
{
    if (auto ad = s.isAliasDeclaration())
    {
        if (ad.inuse)
        {
            .error(ad.loc, "%s `%s` recursive alias declaration", ad.kind, ad.toPrettyChars);
            return ad;
        }
        ad.inuse = 1;
        Dsymbol ds = ad.aliassym ? ad.aliassym.toAlias2() : ad;
        ad.inuse = 0;
        return ds;
    }
    if (auto td = s.isTupleDeclaration())
    {
        //printf("TupleDeclaration::toAlias2() '%s' objects = %s\n", toChars(), objects.toChars());
        for (size_t i = 0; i < td.objects.length; i++)
        {
            RootObject o = (*td.objects)[i];
            if (Dsymbol ds = isDsymbol(o))
            {
                ds = ds.toAlias2();
                (*td.objects)[i] = ds;
            }
        }
        return td;
    }
    return toAlias(s);
}

private Dsymbol toAliasImpl(AliasDeclaration ad)
{
    static if (0)
    printf("[%s] AliasDeclaration::toAlias('%s', this = %p, aliassym: %s, kind: '%s', inuse = %d)\n",
        ad.loc.toChars(), ad.toChars(), ad, ad.aliassym ? ad.aliassym.toChars() : "", ad.aliassym ? ad.aliassym.kind() : "", ad.inuse);
    assert(ad != ad.aliassym);
    //static int count; if (++count == 10) *(char*)0=0;

    Dsymbol err()
    {
        // Avoid breaking "recursive alias" state during errors gagged
        if (global.gag)
            return ad;
        ad.aliassym = new AliasDeclaration(ad.loc, ad.ident, Type.terror);
        ad.type = Type.terror;
        return ad.aliassym;
    }
    // Reading the AliasDeclaration
    if (!ad.ignoreRead)
        ad.wasRead = true;                 // can never assign to this AliasDeclaration again

    if (ad.inuse == 1 && ad.type && ad._scope)
    {
        ad.inuse = 2;
        const olderrors = global.errors;
        Dsymbol s = ad.type.toDsymbol(ad._scope);
        //printf("[%s] type = %s, s = %p, this = %p\n", loc.toChars(), type.toChars(), s, this);
        if (global.errors != olderrors)
            return err();
        if (s)
        {
            s = s.toAlias();
            if (global.errors != olderrors)
                return err();
            ad.aliassym = s;
            ad.inuse = 0;
        }
        else
        {
            Type t = ad.type.typeSemantic(ad.loc, ad._scope);
            if (t.ty == Terror)
                return err();
            if (global.errors != olderrors)
                return err();
            //printf("t = %s\n", t.toChars());
            ad.inuse = 0;
        }
    }
    if (ad.inuse)
    {
        .error(ad.loc, "%s `%s` recursive alias declaration", ad.kind, ad.toPrettyChars);
        return err();
    }

    if (ad.semanticRun >= PASS.semanticdone)
    {
        // semantic is already done.

        // Do not see aliassym !is null, because of lambda aliases.

        // Do not see type.deco !is null, even so "alias T = const int;` needs
        // semantic analysis to take the storage class `const` as type qualifier.
    }
    else
    {
        // stop AliasAssign tuple building
        if (ad.aliassym)
        {
            if (auto td = ad.aliassym.isTupleDeclaration())
            {
                if (td.building)
                {
                    td.building = false;
                    ad.semanticRun = PASS.semanticdone;
                    return td;
                }
            }
        }
        if (ad._import && ad._import._scope)
        {
            /* If this is an internal alias for selective/renamed import,
             * load the module first.
             */
            ad._import.dsymbolSemantic(null);
        }
        if (ad._scope)
        {
            aliasSemantic(ad, ad._scope);
        }
    }

    ad.inuse = 1;
    Dsymbol s = ad.aliassym ? ad.aliassym.toAlias() : ad;
    ad.inuse = 0;
    return s;
}

/*********************************
 * If this symbol is really an alias for another,
 * return that other.
 * If needed, semantic() is invoked due to resolve forward reference.
 */
Dsymbol toAlias(Dsymbol s)
{
    if (auto ad = s.isAliasDeclaration())
    {
        return ad.toAliasImpl();
    }
    if (auto imp = s.isImport())
    {
        if (imp.aliasId)
            return imp.mod;
        return imp;
    }
    if (auto vd = s.isVarDeclaration())
    {
        //printf("VarDeclaration::toAlias('%s', this = %p, aliassym = %p)\n", toChars(), this, aliassym);
        if ((!vd.type || !vd.type.deco) && vd._scope)
            dsymbolSemantic(vd, vd._scope);

        assert(vd != vd.aliasTuple);
        return vd.aliasTuple ? vd.aliasTuple.toAlias() : vd;
    }
    // resolve real symbol
    if (auto ti = s.isTemplateInstance())
    {
        static if (LOG)
        {
            printf("TemplateInstance.toAlias()\n");
        }
        if (!ti.inst)
        {
            // Maybe we can resolve it
            if (ti._scope)
            {
                dsymbolSemantic(ti, ti._scope);
            }
            if (!ti.inst)
            {
                .error(ti.loc, "%s `%s` cannot resolve forward reference", ti.kind, ti.toPrettyChars);
                ti.errors = true;
                return ti;
            }
        }

        if (ti.inst != ti)
            return ti.inst.toAlias();

        if (ti.aliasdecl)
        {
            return ti.aliasdecl.toAlias();
        }

        return ti.inst;
    }
    return s;
}

const(char)* getMessage(DeprecatedDeclaration dd)
{
    if (auto sc = dd._scope)
    {
        dd._scope = null;

        sc = sc.startCTFE();
        dd.msg = dd.msg.expressionSemantic(sc);
        dd.msg = resolveProperties(sc, dd.msg);
        sc = sc.endCTFE();
        dd.msg = dd.msg.ctfeInterpret();

        if (auto se = dd.msg.toStringExp())
            dd.msgstr = se.toStringz().ptr;
        else
            error(dd.msg.loc, "compile time constant expected, not `%s`", dd.msg.toChars());
    }
    return dd.msgstr;
}

bool checkDeprecated(Dsymbol d, Loc loc, Scope* sc)
{
    if (global.params.useDeprecated == DiagnosticReporting.off)
        return false;
    if (!d.isDeprecated())
        return false;
    // Don't complain if we're inside a deprecated symbol's scope
    if (sc.isDeprecated())
        return false;
    // Don't complain if we're inside a template constraint
    // https://issues.dlang.org/show_bug.cgi?id=21831
    if (sc.inTemplateConstraint)
        return false;

    const(char)* message = null;
    for (Dsymbol p = d; p; p = p.parent)
    {
        message = p.depdecl ? p.depdecl.getMessage() : null;
        if (message)
            break;
    }
    if (message)
        deprecation(loc, "%s `%s` is deprecated - %s", d.kind, d.toPrettyChars, message);
    else
        deprecation(loc, "%s `%s` is deprecated", d.kind, d.toPrettyChars);

    if (auto ti = sc.parent ? sc.parent.isInstantiated() : null)
        ti.printInstantiationTrace(Classification.deprecation);
    else if (auto ti = sc.parent ? sc.parent.isTemplateInstance() : null)
        ti.printInstantiationTrace(Classification.deprecation);

    return true;
}

/*********************************
 * Check type to see if it is based on a deprecated symbol.
 */
private void checkDeprecated(Type type, Loc loc, Scope* sc)
{
    if (Dsymbol s = type.toDsymbol(sc))
    {
        s.checkDeprecated(loc, sc);
    }

    if (auto tn = type.nextOf())
        tn.checkDeprecated(loc, sc);
}

// Returns true if a contract can appear without a function body.
package bool allowsContractWithoutBody(FuncDeclaration funcdecl)
{
    assert(!funcdecl.fbody);

    /* Contracts can only appear without a body when they are virtual
     * interface functions or abstract.
     */
    Dsymbol parent = funcdecl.toParent();
    InterfaceDeclaration id = parent.isInterfaceDeclaration();

    if (!funcdecl.isAbstract() &&
        (funcdecl.fensures || funcdecl.frequires) &&
        !(id && funcdecl.isVirtual()))
    {
        auto cd = parent.isClassDeclaration();
        if (!(cd && cd.isAbstract()))
            return false;
    }
    return true;
}

/***************************************
 * Determine if struct is POD (Plain Old Data).
 *
 * POD is defined as:
 *      $(OL
 *      $(LI not nested)
 *      $(LI no postblits, destructors, or assignment operators)
 *      $(LI no `ref` fields or fields that are themselves non-POD)
 *      )
 * The idea being these are compatible with C structs.
 *
 * Returns:
 *     true if struct is POD
 */
bool isPOD(StructDeclaration sd)
{
    // If we've already determined whether this struct is POD.
    if (sd.ispod != ThreeState.none)
        return (sd.ispod == ThreeState.yes);


    bool hasCpCtorLocal;
    bool hasMoveCtorLocal;
    bool needCopyCtor;
    bool needMoveCtor;
    needCopyOrMoveCtor(sd, hasCpCtorLocal, hasMoveCtorLocal, needCopyCtor, needMoveCtor);

    if (sd.enclosing                    || // is nested
        search(sd, sd.loc, Id.postblit) || // has postblit
        search(sd, sd.loc, Id.dtor)     || // has destructor
        /* This is commented out because otherwise buildkite vibe.d:
           `canCAS!Task` fails to compile
         */
        //hasMoveCtorLocal               || // has move constructor
        hasCpCtorLocal)                   // has copy constructor
    {
        sd.ispod = ThreeState.no;
        return false;
    }

    // Recursively check all fields are POD.
    for (size_t i = 0; i < sd.fields.length; i++)
    {
        VarDeclaration v = sd.fields[i];
        if (v.storage_class & STC.ref_)
        {
            sd.ispod = ThreeState.no;
            return false;
        }

        if (auto ts = v.type.baseElemOf().isTypeStruct())
        {
            if (!ts.sym.isPOD())
            {
                sd.ispod = ThreeState.no;
                return false;
            }
        }
    }

    sd.ispod = ThreeState.yes;
    return true;
}

/****************************************
 * Fill in vtbl[] for base class based on member functions of class cd.
 * Input:
 *      bc              BaseClass
 *      vtbl            if !=NULL, fill it in
 *      newinstance     !=0 means all entries must be filled in by members
 *                      of cd, not members of any base classes of cd.
 * Returns:
 *      true if any entries were filled in by members of cd (not exclusively
 *      by base classes)
 */
bool fillVtbl(BaseClass* bc, ClassDeclaration cd, FuncDeclarations* vtbl, int newinstance)
{
    bool result = false;

    //printf("BaseClass.fillVtbl(this='%s', cd='%s')\n", sym.toChars(), cd.toChars());
    if (vtbl)
        vtbl.setDim(bc.sym.vtbl.length);

    // first entry is ClassInfo reference
    for (size_t j = bc.sym.vtblOffset(); j < bc.sym.vtbl.length; j++)
    {
        FuncDeclaration ifd = bc.sym.vtbl[j].isFuncDeclaration();

        //printf("        vtbl[%d] is '%s'\n", j, ifd ? ifd.toChars() : "null");
        assert(ifd);

        // Find corresponding function in this class
        auto tf = ifd.type.toTypeFunction();
        auto fd = cd.findFunc(ifd.ident, tf);
        if (fd && !fd.isAbstract())
        {
            if (fd.toParent() == cd)
                result = true;
        }
        else
            fd = null;
        if (vtbl)
            (*vtbl)[j] = fd;
    }
    return result;
}

/*
If sd has a copy constructor and ctor is an rvalue constructor,
issue an error.

Params:
    sd = struct declaration that may contain both an rvalue and copy constructor
    ctor = constructor that will be checked if it is an rvalue constructor
    ti = template instance the ctor is part of

Return:
    `true` if sd has a copy constructor and ctor is an rvalue constructor
*/
bool checkHasBothRvalueAndCpCtor(StructDeclaration sd, CtorDeclaration ctor, TemplateInstance ti)
{
    //printf("checkHasBothRvalueAndCpCtor() sd: %s ctor: %s ti: %s\n", sd.toChars(), ctor.toChars(), ti.toChars());
    /* cannot use ctor.isMoveCtor because semantic pass may not have been run yet,
     * so use isRvalueConstructor()
     */
    if (sd && sd.hasCopyCtor && isRvalueConstructor(sd, ctor))
    {
        .error(ctor.loc, "cannot define both an rvalue constructor and a copy constructor for `struct %s`", sd.toChars());
        .errorSupplemental(ti.loc, "Template instance `%s` creates an rvalue constructor for `struct %s`",
                ti.toPrettyChars(), sd.toChars());

        return true;
    }

    return false;
}

/************************************************
 * Check if ctor is an rvalue constructor.
 * A constructor that receives a single parameter of the same type as
 * `Unqual!typeof(this)` is an rvalue constructor.
 * Params:
 *      sd = struct that ctor is a member of
 *      ctor = constructor to test
 * Returns:
 *      true if it is an rvalue constructor
 */
bool isRvalueConstructor(StructDeclaration sd, CtorDeclaration ctor)
{
    // note commonality with setting isMoveCtor in the semantic code for CtorDeclaration
    auto tf = ctor.type.isTypeFunction();
    const dim = tf.parameterList.length;
    if (dim == 1 || (dim > 1 && tf.parameterList[1].defaultArg))
    {
        auto param = tf.parameterList[0];
        if (!(param.storageClass & STC.ref_) && param.type.mutableOf().unSharedOf() == sd.type.mutableOf().unSharedOf())
        {
            return true;
        }
    }
    return false;
}

/*************************************
 * Find the `alias this` symbol of e's type.
 * Params:
 *      sc = context
 *      e = expression forming the `this`
 *      gag = do not print errors, return `null` instead
 *      findOnly = don't do further processing like resolving properties,
 *                 i.e. just return plain dotExp() result.
 * Returns:
 *      Expression that is `e.aliasthis`
 */
Expression resolveAliasThis(Scope* sc, Expression e, bool gag = false, bool findOnly = false)
{
    //printf("resolveAliasThis() %s\n", toChars(e));
    import dmd.typesem : dotExp;
    for (AggregateDeclaration ad = isAggregate(e.type); ad;)
    {
        if (ad.aliasthis)
        {
            Loc loc = e.loc;
            Type tthis = (e.op == EXP.type ? e.type : null);
            const flags = cast(DotExpFlag) (DotExpFlag.noAliasThis | (gag * DotExpFlag.gag));
            const olderrors = gag ? global.startGagging() : 0;
            e = dotExp(ad.type, sc, e, ad.aliasthis.ident, flags);
            if (!e || findOnly)
                return gag && global.endGagging(olderrors) ? null : e;

            if (tthis && ad.aliasthis.sym.needThis())
            {
                if (auto ve = e.isVarExp())
                {
                    if (auto fd = ve.var.isFuncDeclaration())
                    {
                        // https://issues.dlang.org/show_bug.cgi?id=13009
                        // Support better match for the overloaded alias this.
                        bool hasOverloads;
                        if (auto f = fd.overloadModMatch(loc, tthis, hasOverloads))
                        {
                            if (!hasOverloads)
                                fd = f;     // use exact match
                            e = new VarExp(loc, fd, hasOverloads);
                            e.type = f.type;
                            e = new CallExp(loc, e);
                            goto L1;
                        }
                    }
                }
                /* non-@property function is not called inside typeof(),
                 * so resolve it ahead.
                 */
                {
                    ubyte save = sc.intypeof;
                    sc.intypeof = 1; // bypass "need this" error check
                    e = resolveProperties(sc, e);
                    sc.intypeof = save;
                }
            L1:
                e = new TypeExp(loc, new TypeTypeof(loc, e));
                e = e.expressionSemantic(sc);
            }
            e = resolveProperties(sc, e);
            if (!gag)
                ad.aliasthis.checkDeprecatedAliasThis(loc, sc);
            else if (global.endGagging(olderrors))
                e = null;
        }

        import dmd.dclass : ClassDeclaration;
        auto cd = ad.isClassDeclaration();
        if ((!e || !ad.aliasthis) && cd && cd.baseClass && cd.baseClass != ClassDeclaration.object)
        {
            ad = cd.baseClass;
            continue;
        }
        break;
    }
    return e;
}

/**
 * Check if an `alias this` is deprecated
 *
 * Usually one would use `expression.checkDeprecated(scope, aliasthis)` to
 * check if `expression` uses a deprecated `aliasthis`, but this calls
 * `toPrettyChars` which lead to the following message:
 * "Deprecation: alias this `fullyqualified.aggregate.__anonymous` is deprecated"
 *
 * Params:
 *   at  = The `AliasThis` object to check
 *   loc = `Loc` of the expression triggering the access to `at`
 *   sc  = `Scope` of the expression
 *         (deprecations do not trigger in deprecated scopes)
 *
 * Returns:
 *   Whether the alias this was reported as deprecated.
 */
private bool checkDeprecatedAliasThis(AliasThis at, Loc loc, Scope* sc)
{
    if (global.params.useDeprecated != DiagnosticReporting.off
        && at.isDeprecated() && !sc.isDeprecated())
    {
        const(char)* message = null;
        for (Dsymbol p = at; p; p = p.parent)
        {
            message = p.depdecl ? p.depdecl.getMessage() : null;
            if (message)
                break;
        }
        if (message)
            deprecation(loc, "`alias %s this` is deprecated - %s",
                        at.sym.toChars(), message);
        else
            deprecation(loc, "`alias %s this` is deprecated",
                        at.sym.toChars());

        if (auto ti = sc.parent ? sc.parent.isInstantiated() : null)
            ti.printInstantiationTrace(Classification.deprecation);

        return true;
    }
    return false;
}

// Save the scope and defer semantic analysis on the Dsymbol.
void deferDsymbolSemantic(Scope* sc, Dsymbol s, Scope* scx)
{
    s._scope = scx ? scx : sc.copy();
    s._scope.setNoFree();
    addDeferredSemantic(s);
}

struct Ungag
{
    uint oldgag;

    extern (D) this(uint old) nothrow @safe
    {
        this.oldgag = old;
    }

    extern (C++) ~this() nothrow
    {
        global.gag = oldgag;
    }
}

Ungag ungagSpeculative(const Dsymbol s)
{
    const oldgag = global.gag;
    if (global.gag && !s.isSpeculative() && !s.toParent2().isFuncDeclaration())
        global.gag = 0;
    return Ungag(oldgag);
}

/*******************************************
 * Print deprecation warning if we're deprecated, when
 * this module is imported from scope sc.
 *
 * Params:
 *  m = the module
 *  sc = the scope into which we are imported
 *  loc = the location of the import statement
 */
private void checkImportDeprecation(Module m, Loc loc, Scope* sc)
{
    if (!m.md || !m.md.isdeprecated || sc.isDeprecated)
        return;

    Expression msg = m.md.msg;
    if (StringExp se = msg ? msg.toStringExp() : null)
    {
        const slice = se.peekString();
        if (slice.length)
        {
            deprecation(m.loc, "%s `%s` is deprecated - %.*s", m.kind, m.toPrettyChars, cast(int)slice.length, slice.ptr);
            return;
        }
    }
    deprecation(m.loc, "%s `%s` is deprecated", m.kind, m.toPrettyChars);
}

private extern(C++) final class DsymbolSemanticVisitor : Visitor
{
    import dmd.typesem: size;

    alias visit = Visitor.visit;

    Scope* sc;
    this(Scope* sc) scope @safe
    {
        this.sc = sc;
    }

    override void visit(Dsymbol dsym)
    {
        .error(dsym.loc, "%s `%s` %p has no semantic routine", dsym.kind, dsym.toPrettyChars, dsym);
    }

    override void visit(ScopeDsymbol) { }
    override void visit(Declaration) { }

    override void visit(AliasThis dsym)
    {
        if (dsym.semanticRun != PASS.initial)
            return;

        if (dsym._scope)
        {
            sc = dsym._scope;
            dsym._scope = null;
        }

        if (!sc)
            return;

        dsym.semanticRun = PASS.semantic;
        dsym.isDeprecated_ = !!(sc.stc & STC.deprecated_);

        Dsymbol p = sc.parent.pastMixin();
        AggregateDeclaration ad = p.isAggregateDeclaration();
        if (!ad)
        {
            error(dsym.loc, "alias this can only be a member of aggregate, not %s `%s`", p.kind(), p.toChars());
            return;
        }

        assert(ad.members);
        Dsymbol s = ad.search(dsym.loc, dsym.ident);
        if (!s)
        {
            Dsymbol pscopesym;
            s = sc.search(dsym.loc, dsym.ident, pscopesym);
            if (s)
                error(dsym.loc, "`%s` is not a member of `%s`", s.toChars(), ad.toChars());
            else
                error(dsym.loc, "undefined identifier `%s`", dsym.ident.toChars());
            return;
        }
        if (ad.aliasthis && s != ad.aliasthis)
        {
            error(dsym.loc, "there can be only one alias this");
            return;
        }

        /* disable the alias this conversion so the implicit conversion check
         * doesn't use it.
         */
        ad.aliasthis = null;

        Dsymbol sx = s;
        if (sx.isAliasDeclaration())
            sx = sx.toAlias();
        Declaration d = sx.isDeclaration();
        if (d && !d.isTupleDeclaration())
        {
            /* https://issues.dlang.org/show_bug.cgi?id=18429
             *
             * If the identifier in the AliasThis declaration
             * is defined later and is a voldemort type, we must
             * perform semantic on the declaration to deduce the type.
             */
            if (!d.type)
                d.dsymbolSemantic(sc);

            Type t = d.type;
            assert(t);
            if (ad.type.implicitConvTo(t) > MATCH.nomatch)
            {
                error(dsym.loc, "alias this is not reachable as `%s` already converts to `%s`", ad.toChars(), t.toChars());
            }
        }

        dsym.sym = s;
        // Restore alias this
        ad.aliasthis = dsym;
        dsym.semanticRun = PASS.semanticdone;
    }

    override void visit(AliasDeclaration dsym)
    {
        if (dsym.semanticRun >= PASS.semanticdone)
            return;
        assert(dsym.semanticRun <= PASS.semantic);

        if (!sc)
            return;

        dsym.semanticRun = PASS.semantic;

        dsym.storage_class |= sc.stc & STC.deprecated_;
        dsym.visibility = sc.visibility;
        dsym.userAttribDecl = sc.userAttribDecl;

        if (!sc.func && dsym.inNonRoot())
            return;

        aliasSemantic(dsym, sc);
    }

    override void visit(AliasAssign dsym)
    {
        //printf("visit(AliasAssign)\n");
        if (dsym.semanticRun >= PASS.semanticdone)
            return;
        assert(dsym.semanticRun <= PASS.semantic);

        if (!sc.func && dsym.inNonRoot())
            return;

        aliasAssignSemantic(dsym, sc);
    }

    override void visit(VarDeclaration dsym)
    {
        version (none)
        {
            printf("VarDeclaration::semantic('%s', parent = '%s') sem = %d\n",
                   dsym.toChars(), sc.parent ? sc.parent.toChars() : null, dsym.semanticRun);
            printf(" type = %s\n", dsym.type ? dsym.type.toChars() : "null");
            printf(" stc = x%llx\n", dsym.storage_class);
            printf(" storage_class = x%llx\n", dsym.storage_class);
            printf("linkage = %d\n", dsym._linkage);
            //if (strcmp(toChars(), "mul") == 0) assert(0);
        }
        //if (semanticRun > PASS.initial)
        //    return;
        //semanticRun = PSSsemantic;

        if (dsym.semanticRun >= PASS.semanticdone)
            return;

        if (sc && sc.inunion && sc.inunion.isAnonDeclaration())
            dsym.overlapped = true;

        dsym.sequenceNumber = global.varSequenceNumber++;
        if (!dsym.isScope())
            dsym.maybeScope = true;

        Scope* scx = null;
        if (dsym._scope)
        {
            sc = dsym._scope;
            scx = sc;
            dsym._scope = null;
        }

        if (!sc)
            return;

        dsym.semanticRun = PASS.semantic;

        // 'static foreach' variables should not inherit scope properties
        // https://issues.dlang.org/show_bug.cgi?id=19482
        if ((dsym.storage_class & (STC.foreach_ | STC.local)) == (STC.foreach_ | STC.local))
        {
            dsym._linkage = LINK.d;
            dsym.visibility = Visibility(Visibility.Kind.public_);
            dsym.overlapped = false; // unset because it is modified early on this function
            dsym.userAttribDecl = null; // unset because it is set by Dsymbol.setScope()
        }
        else
        {
            /* Pick up storage classes from context, but except synchronized,
             * override, abstract, and final.
             */
            dsym.storage_class |= (sc.stc & ~(STC.synchronized_ | STC.override_ | STC.abstract_ | STC.final_));
            dsym.userAttribDecl = sc.userAttribDecl;
            dsym.cppnamespace = sc.namespace;
            dsym._linkage = sc.linkage;
            dsym.visibility = sc.visibility;
            dsym.alignment = sc.alignment();
        }

        if (dsym.storage_class & STC.extern_ && dsym._init)
        {
            if (sc.inCfile)
            {
                // https://issues.dlang.org/show_bug.cgi?id=24447
                // extern int x = 3; is allowed in C
                dsym.storage_class &= ~STC.extern_;
            }
            else
                .error(dsym.loc, "%s `%s` extern symbols cannot have initializers", dsym.kind, dsym.toPrettyChars);

        }

        AggregateDeclaration ad = dsym.isThis();
        if (ad)
            dsym.storage_class |= ad.storage_class & STC.TYPECTOR;

        if ((dsym.storage_class & STC.auto_) && (dsym.storage_class & STC.ref_))
        {
            if (!(dsym.storage_class & STC.autoref))
            {
                .error(dsym.loc, "%s `%s` - `auto ref` variable must have `auto` and `ref` adjacent", dsym.kind, dsym.toChars());
                dsym.storage_class |= STC.autoref;
            }
        }

        /* If auto type inference, do the inference
         */
        int inferred = 0;
        if (!dsym.type)
        {
            dsym.inuse++;

            // Infering the type requires running semantic,
            // so mark the scope as ctfe if required
            bool needctfe = (dsym.storage_class & (STC.manifest | STC.static_)) != 0 || !sc.func;
            if (needctfe)
            {
                sc.condition = true;
                sc = sc.startCTFE();
            }
            //printf("inferring type for %s with init %s\n", dsym.toChars(), dsym._init.toChars());
            dsym._init = dsym._init.inferType(sc);
            dsym.type = dsym._init.initializerToExpression(null, sc.inCfile).type;
            if (needctfe)
                sc = sc.endCTFE();

            dsym.inuse--;
            inferred = 1;

            /* This is a kludge to support the existing syntax for RAII
             * declarations.
             */
            dsym.storage_class &= ~STC.auto_;
            dsym.originalType = dsym.type.syntaxCopy();
        }
        else
        {
            if (!dsym.originalType)
                dsym.originalType = dsym.type.syntaxCopy();

            /* Prefix function attributes of variable declaration can affect
             * its type:
             *      pure nothrow void function() fp;
             *      static assert(is(typeof(fp) == void function() pure nothrow));
             */
            Scope* sc2 = sc.push();
            sc2.stc |= (dsym.storage_class & STC.FUNCATTR);
            dsym.inuse++;
            dsym.type = dsym.type.typeSemantic(dsym.loc, sc2);
            dsym.inuse--;
            sc2.pop();
        }
        //printf(" semantic type = %s\n", dsym.type ? dsym.type.toChars() : "null");
        if (dsym.type.ty == Terror)
            dsym.errors = true;

        dsym.type.checkDeprecated(dsym.loc, sc);
        dsym.parent = sc.parent;
        //printf("this = %p, parent = %p, '%s'\n", dsym, dsym.parent, dsym.parent.toChars());

        /* If scope's alignment is the default, use the type's alignment,
         * otherwise the scope overrrides.
         */
        import dmd.typesem : alignment;
        if (dsym.alignment.isDefault())
            dsym.alignment = dsym.type.alignment(); // use type's alignment

        if (sc.inCfile && !dsym.alignment.isDefault())
        {
            /* C11 6.7.5-4 alignment declaration cannot be less strict than the
             * type alignment of the object or member being declared.
             */
            if (dsym.alignment.get() < dsym.type.alignsize())
            {
                if (dsym.alignment.fromAlignas())
                {
                    error(dsym.loc, "`_Alignas` specifier cannot be less strict than alignment of `%s`",
                          dsym.toChars());
                }
                if (!dsym.alignment.isPack())
                    dsym.alignment.setDefault();
            }
        }

        //printf("sc.stc = %x\n", sc.stc);
        //printf("storage_class = x%x\n", storage_class);

        dsym.type.checkComplexTransition(dsym.loc, sc);

        // Calculate type size + safety checks
        if (dsym.storage_class & STC.gshared && !dsym.isMember())
        {
            sc.setUnsafe(false, dsym.loc, "using `__gshared` instead of `shared`");
        }

        Dsymbol parent = dsym.toParent();

        Type tb = dsym.type.toBasetype();
        Type tbn = tb.baseElemOf();
        if (tb.ty == Tvoid && !(dsym.storage_class & STC.lazy_))
        {
            if (inferred)
            {
                .error(dsym.loc, "%s `%s` - type `%s` is inferred from initializer `%s`, and variables cannot be of type `void`",
                    dsym.kind, dsym.toPrettyChars, dsym.type.toChars(), toChars(dsym._init));
            }
            else
                .error(dsym.loc, "%s `%s` - variables cannot be of type `void`", dsym.kind, dsym.toPrettyChars);
            dsym.type = Type.terror;
            tb = dsym.type;
        }
        if (tb.ty == Tfunction)
        {
            .error(dsym.loc, "%s `%s` cannot be declared to be a function", dsym.kind, dsym.toPrettyChars);
            dsym.type = Type.terror;
            tb = dsym.type;
        }
        if (auto ts = tb.isTypeStruct())
        {
            // Require declarations, except when it's just a reference (as done for pointers)
            // or when the variable is defined externally
            if (!ts.sym.members && !(dsym.storage_class & (STC.ref_ | STC.extern_)))
            {
                .error(dsym.loc, "%s `%s` - no definition of struct `%s`", dsym.kind, dsym.toPrettyChars, ts.toChars());

                // Explain why the definition is required when it's part of another type
                if (!dsym.type.isTypeStruct())
                {
                    // Prefer Loc of the dependant type
                    const s = dsym.type.toDsymbol(sc);
                    const loc = (s ? s : dsym).loc;
                    loc.errorSupplemental("required by type `%s`", dsym.type.toChars());
                }
                errorSupplemental(dsym.loc, "see https://dlang.org/spec/struct.html#opaque_struct_unions");
                errorSupplemental(dsym.loc, "perhaps declare a variable with pointer type `%s*` instead", dsym.type.toChars());

                // Flag variable as error to avoid invalid error messages due to unknown size
                dsym.type = Type.terror;
            }
        }
        if ((dsym.storage_class & STC.auto_) && !inferred && !(dsym.storage_class & STC.autoref))
            .error(dsym.loc, "%s `%s` - storage class `auto` has no effect if type is not inferred, did you mean `scope`?", dsym.kind, dsym.toPrettyChars);

        if (auto tt = tb.isTypeTuple())
        {
            /* Instead, declare variables for each of the tuple elements
             * and add those.
             */
            size_t nelems = Parameter.dim(tt.arguments);
            Expression ie = (dsym._init && !dsym._init.isVoidInitializer()) ? dsym._init.initializerToExpression(null, sc.inCfile) : null;
            if (ie)
                ie = ie.expressionSemantic(sc);
            if (nelems > 0 && ie)
            {
                auto iexps = new Expressions(ie);
                auto exps = new Expressions();
                for (size_t pos = 0; pos < iexps.length; pos++)
                {
                Lexpand1:
                    Expression e = (*iexps)[pos];
                    Parameter arg = Parameter.getNth(tt.arguments, pos);
                    arg.type = arg.type.typeSemantic(dsym.loc, sc);
                    //printf("[%d] iexps.length = %d, ", pos, iexps.length);
                    //printf("e = (%s %s, %s), ", Token.tochars[e.op], e.toChars(), e.type.toChars());
                    //printf("arg = (%s, %s)\n", arg.toChars(), arg.type.toChars());

                    if (e != ie)
                    {
                        if (iexps.length > nelems)
                            goto Lnomatch;
                        if (e.type.implicitConvTo(arg.type))
                            continue;
                    }

                    if (auto te = e.isTupleExp())
                    {
                        if (iexps.length - 1 + te.exps.length > nelems)
                            goto Lnomatch;

                        iexps.remove(pos);
                        iexps.insert(pos, te.exps);
                        (*iexps)[pos] = Expression.combine(te.e0, (*iexps)[pos]);
                        goto Lexpand1;
                    }
                    else if (isAliasThisTuple(e))
                    {
                        auto v = copyToTemp(STC.none, "__tup", e);
                        v.dsymbolSemantic(sc);
                        auto ve = new VarExp(dsym.loc, v);
                        ve.type = e.type;

                        exps.setDim(1);
                        (*exps)[0] = ve;
                        expandAliasThisTuples(exps, 0);

                        for (size_t u = 0; u < exps.length; u++)
                        {
                        Lexpand2:
                            Expression ee = (*exps)[u];
                            arg = Parameter.getNth(tt.arguments, pos + u);
                            arg.type = arg.type.typeSemantic(dsym.loc, sc);
                            //printf("[%d+%d] exps.length = %d, ", pos, u, exps.length);
                            //printf("ee = (%s %s, %s), ", Token.tochars[ee.op], ee.toChars(), ee.type.toChars());
                            //printf("arg = (%s, %s)\n", arg.toChars(), arg.type.toChars());

                            size_t iexps_dim = iexps.length - 1 + exps.length;
                            if (iexps_dim > nelems)
                                goto Lnomatch;
                            if (ee.type.implicitConvTo(arg.type))
                                continue;

                            if (expandAliasThisTuples(exps, u) != -1)
                                goto Lexpand2;
                        }

                        if ((*exps)[0] != ve)
                        {
                            Expression e0 = (*exps)[0];
                            (*exps)[0] = new CommaExp(dsym.loc, new DeclarationExp(dsym.loc, v), e0);
                            (*exps)[0].type = e0.type;

                            iexps.remove(pos);
                            iexps.insert(pos, exps);
                            goto Lexpand1;
                        }
                    }
                }
                if (iexps.length < nelems)
                    goto Lnomatch;

                ie = new TupleExp(dsym._init.loc, iexps);
            }
        Lnomatch:

            if (ie && ie.op == EXP.tuple)
            {
                auto te = ie.isTupleExp();
                size_t tedim = te.exps.length;
                if (tedim != nelems)
                {
                    error(dsym.loc, "sequence of %d elements cannot be assigned to sequence of %d elements", cast(int)tedim, cast(int)nelems);
                    for (size_t u = tedim; u < nelems; u++) // fill dummy expression
                        te.exps.push(ErrorExp.get());
                }
            }

            auto exps = new Objects(nelems);
            for (size_t i = 0; i < nelems; i++)
            {
                Parameter arg = Parameter.getNth(tt.arguments, i);

                OutBuffer buf;
                buf.printf("__%s_field_%llu", dsym.ident.toChars(), cast(ulong)i);
                auto id = Identifier.idPool(buf[]);

                Initializer ti;
                if (ie)
                {
                    Expression einit = ie;
                    if (auto te = ie.isTupleExp())
                    {
                        einit = (*te.exps)[i];
                        if (i == 0)
                            einit = Expression.combine(te.e0, einit);
                    }
                    ti = new ExpInitializer(einit.loc, einit);
                }
                else
                    ti = dsym._init ? dsym._init.syntaxCopy() : null;

                STC storage_class = STC.temp | dsym.storage_class;
                if ((dsym.storage_class & STC.parameter) && (arg.storageClass & STC.parameter))
                    storage_class |= arg.storageClass;
                auto v = new VarDeclaration(dsym.loc, arg.type, id, ti, storage_class);
                //printf("declaring field %s of type %s\n", v.toChars(), v.type.toChars());
                v.overlapped = dsym.overlapped;

                v.dsymbolSemantic(sc);

                Expression e = new VarExp(dsym.loc, v);
                (*exps)[i] = e;
            }
            auto v2 = new TupleDeclaration(dsym.loc, dsym.ident, exps);
            v2.parent = dsym.parent;
            v2.isexp = true;
            dsym.aliasTuple = v2;
            dsym.semanticRun = PASS.semanticdone;
            return;
        }

        /* Storage class can modify the type
         */
        dsym.type = dsym.type.addStorageClass(dsym.storage_class);

        /* Adjust storage class to reflect type
         */
        if (dsym.type.isConst())
        {
            dsym.storage_class |= STC.const_;
            if (dsym.type.isShared())
                dsym.storage_class |= STC.shared_;
        }
        else if (dsym.type.isImmutable())
            dsym.storage_class |= STC.immutable_;
        else if (dsym.type.isShared())
            dsym.storage_class |= STC.shared_;
        else if (dsym.type.isWild())
            dsym.storage_class |= STC.wild;

        if (STC stc = dsym.storage_class & (STC.synchronized_ | STC.override_ | STC.abstract_ | STC.final_))
        {
            if (stc == STC.final_)
                .error(dsym.loc, "%s `%s` cannot be `final`, perhaps you meant `const`?", dsym.kind, dsym.toPrettyChars);
            else
            {
                OutBuffer buf;
                stcToBuffer(buf, stc);
                .error(dsym.loc, "%s `%s` cannot be `%s`", dsym.kind, dsym.toPrettyChars, buf.peekChars());
            }
            dsym.storage_class &= ~stc; // strip off
        }

        // At this point we can add `scope` to the STC instead of `in`,
        // because we are never going to use this variable's STC for user messages
        if (dsym.storage_class & STC.constscoperef)
            dsym.storage_class |= STC.scope_;

        import dmd.typesem : hasPointers;

        if (dsym.storage_class & STC.scope_)
        {
            STC stc = dsym.storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.gshared);
            if (stc)
            {
                OutBuffer buf;
                stcToBuffer(buf, stc);
                .error(dsym.loc, "%s `%s` cannot be `scope` and `%s`", dsym.kind, dsym.toPrettyChars, buf.peekChars());
            }
            else if (dsym.isMember())
            {
                error(dsym.loc, "field `%s` cannot be `scope`", dsym.toChars());
            }
            else if (!dsym.type.hasPointers())
            {
                dsym.storage_class &= ~STC.scope_;     // silently ignore; may occur in generic code
                // https://issues.dlang.org/show_bug.cgi?id=23168
                if (dsym.storage_class & STC.returnScope)
                {
                    dsym.storage_class &= ~(STC.return_ | STC.returnScope);
                }
            }
        }

        if (dsym.storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.templateparameter | STC.gshared | STC.ctfe))
        {
        }
        else
        {
            AggregateDeclaration aad = parent.isAggregateDeclaration();
            if (aad)
            {
                if (global.params.v.field && dsym.storage_class & (STC.const_ | STC.immutable_) && dsym._init && !dsym._init.isVoidInitializer())
                {
                    const(char)* s = (dsym.storage_class & STC.immutable_) ? "immutable" : "const";
                    message(dsym.loc, "`%s.%s` is `%s` field", ad.toPrettyChars(), dsym.toChars(), s);
                }
                dsym.storage_class |= STC.field;
                if (auto ts = tbn.isTypeStruct())
                    if (ts.sym.noDefaultCtor)
                    {
                        if (!dsym.isThisDeclaration() && !dsym._init)
                            aad.noDefaultCtor = true;
                    }
            }

            InterfaceDeclaration id = parent.isInterfaceDeclaration();
            if (id)
            {
                error(dsym.loc, "field `%s` not allowed in interface", dsym.toChars());
            }
            else if (aad && aad.sizeok == Sizeok.done)
            {
                error(dsym.loc, "cannot declare field `%s` because it will change the determined size of `%s`", dsym.toChars(), aad.toChars());
            }

            /* Templates cannot add fields to aggregates
             */
            TemplateInstance ti = parent.isTemplateInstance();
            if (ti)
            {
                // Take care of nested templates
                while (1)
                {
                    TemplateInstance ti2 = ti.tempdecl.parent.isTemplateInstance();
                    if (!ti2)
                        break;
                    ti = ti2;
                }
                // If it's a member template
                AggregateDeclaration ad2 = ti.tempdecl.isMember();
                if (ad2 && dsym.storage_class != STC.none)
                {
                    .error(dsym.loc, "%s `%s` - cannot use template to add field to aggregate `%s`", dsym.kind, dsym.toPrettyChars, ad2.toChars());
                }
            }
        }

        mixin alignSectionVarsExtra; doAlign(); // align section variables

        if ((dsym.storage_class & (STC.ref_ | STC.field)) == (STC.ref_ | STC.field) && dsym.ident != Id.This)
        {
            .error(dsym.loc, "%s `%s` - field declarations cannot be `ref`", dsym.kind, dsym.toPrettyChars);
        }

        if (dsym.type.hasWild())
        {
            if (dsym.storage_class & (STC.static_ | STC.extern_ | STC.gshared | STC.manifest | STC.field) || dsym.isDataseg())
            {
                .error(dsym.loc, "%s `%s` - only parameters or stack-based variables can be `inout`", dsym.kind, dsym.toPrettyChars);
            }
            FuncDeclaration func = sc.func;
            if (func)
            {
                if (func.fes)
                    func = func.fes.func;
                bool isWild = false;
                for (FuncDeclaration fd = func; fd; fd = fd.toParentDecl().isFuncDeclaration())
                {
                    if (fd.type.isTypeFunction().iswild)
                    {
                        isWild = true;
                        break;
                    }
                }
                if (!isWild)
                {
                    .error(dsym.loc, "%s `%s` - `inout` variables can only be declared inside `inout` functions", dsym.kind, dsym.toPrettyChars);
                }
            }
        }

        if (!(dsym.storage_class & (STC.ctfe | STC.extern_ | STC.ref_ | STC.result)) &&
            tbn.ty == Tstruct && tbn.isTypeStruct().sym.noDefaultCtor)
        {
            if (!dsym._init)
            {
                if (dsym.isField())
                {
                    /* For fields, we'll check the constructor later to make sure it is initialized
                     */
                    dsym.storage_class |= STC.nodefaultctor;
                }
                else if (dsym.storage_class & STC.parameter)
                {
                }
                else
                    .error(dsym.loc, "%s `%s` - default construction is disabled for type `%s`", dsym.kind, dsym.toPrettyChars, dsym.type.toChars());
            }
        }

        bool dsymIsRef = (dsym.storage_class & (STC.ref_ | STC.field | STC.parameter | STC.temp | STC.foreach_)) == STC.ref_;
        if (dsymIsRef)
        {
            if (!dsym._init && dsym.ident != Id.This)
            {
                if (dsym.storage_class & STC.autoref)
                {
                    dsymIsRef = false;
                    dsym.storage_class &= ~STC.ref_;
                }
                else
                    .error(dsym.loc, "%s `%s` - initializer is required for `ref` variable", dsym.kind, dsym.toPrettyChars);
            }
            else if (dsym._init.isVoidInitializer())
            {
                .error(dsym.loc, "%s `%s` - void initializer not allowed for `ref` variable", dsym.kind, dsym.toPrettyChars);
            }
        }

        FuncDeclaration fd = parent.isFuncDeclaration();
        if (dsym.type.isScopeClass() && !(dsym.storage_class & STC.nodtor))
        {
            if (dsym.storage_class & (STC.field | STC.out_ | STC.ref_ | STC.static_ | STC.manifest | STC.gshared) || !fd)
            {
                .error(dsym.loc, "%s `%s` globals, statics, fields, manifest constants, ref and out parameters cannot be `scope`", dsym.kind, dsym.toPrettyChars);
            }

            // @@@DEPRECATED_2.097@@@  https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
            // Deprecated in 2.087
            // Remove this when the feature is removed from the language
            if (!(dsym.storage_class & STC.scope_))
            {
                if (!(dsym.storage_class & STC.parameter) && dsym.ident != Id.withSym)
                    .error(dsym.loc, "%s `%s` reference to `scope class` must be `scope`", dsym.kind, dsym.toPrettyChars);
            }
        }

        // Calculate type size + safety checks
        if (sc && sc.func)
        {
            if (dsym._init && dsym._init.isVoidInitializer() && !(dsym.storage_class & STC.temp))
            {
                // Don't do these checks for STC.temp vars because the generated `opAssign`
                // for a struct with postblit and destructor void initializes a temporary
                // __swap variable, which can be trusted

                if (dsym.type.hasPointers()) // also computes type size
                    sc.setUnsafe(false, dsym.loc,
                        "`void` initializing a pointer");
                else if (dsym.type.hasInvariant())
                    sc.setUnsafe(false, dsym.loc,
                        "`void` initializing a struct with an invariant");
                else if (dsym.type.toBasetype().ty == Tbool)
                    sc.setUnsafePreview(global.params.systemVariables, false, dsym.loc,
                        "`void` initializing a `bool` (which must always be 0 or 1)");
                else if (dsym.type.hasUnsafeBitpatterns())
                    sc.setUnsafePreview(global.params.systemVariables, false, dsym.loc,
                        "`void` initializing a type with unsafe bit patterns");
            }
            else if (!dsym._init &&
                     !(dsym.storage_class & (STC.static_ | STC.extern_ | STC.gshared | STC.manifest | STC.field | STC.parameter)) &&
                     dsym.type.hasVoidInitPointers())
            {
                sc.setUnsafe(false, dsym.loc, "`void` initializers for pointers");
            }
        }

        if ((!dsym._init || dsym._init.isVoidInitializer) && !fd)
        {
            // If not mutable, initializable by constructor only
            dsym.setInCtorOnly = true;
        }

        if (dsym._init)
        { } // remember we had an explicit initializer
        else if (dsym.storage_class & STC.manifest)
            .error(dsym.loc, "%s `%s` - manifest constants must have initializers", dsym.kind, dsym.toPrettyChars);

        // Don't allow non-extern, non-__gshared variables to be interfaced with C++
        if (dsym._linkage == LINK.cpp && !(dsym.storage_class & (STC.ctfe | STC.extern_ | STC.gshared)) && dsym.isDataseg())
        {
            const char* p = (dsym.storage_class & STC.shared_) ? "shared" : "static";
            .error(dsym.loc, "%s `%s` cannot have `extern(C++)` linkage because it is `%s`", dsym.kind, dsym.toPrettyChars, p);
            errorSupplemental(dsym.loc, "perhaps declare it as `__gshared` instead");
            dsym.errors = true;
        }

        bool isBlit = false;
        uinteger_t sz;
        if (sc.inCfile && !dsym._init)
        {
            addDefaultCInitializer(dsym);
        }
        if (!dsym._init &&
            !(dsym.storage_class & (STC.static_ | STC.gshared | STC.extern_)) &&
            fd &&
            (!(dsym.storage_class & (STC.field | STC.in_ | STC.foreach_ | STC.parameter | STC.result)) ||
             (dsym.storage_class & STC.out_)) &&
            (sz = dsym.type.size()) != 0)
        {
            // Provide a default initializer

            //printf("Providing default initializer for '%s'\n", dsym.toChars());
            if (sz == SIZE_INVALID && dsym.type.ty != Terror)
                .error(dsym.loc, "%s `%s` - size of type `%s` is invalid", dsym.kind, dsym.toPrettyChars, dsym.type.toChars());

            Type tv = dsym.type;
            while (tv.ty == Tsarray)    // Don't skip Tenum
                tv = tv.nextOf();
            if (tv.needsNested())
            {
                /* Nested struct requires valid enclosing frame pointer.
                 * In StructLiteralExp::toElem(), it's calculated.
                 */
                assert(tbn.ty == Tstruct);
                checkFrameAccess(dsym.loc, sc, tbn.isTypeStruct().sym);

                Expression e = tv.defaultInitLiteral(dsym.loc);
                e = new BlitExp(dsym.loc, new VarExp(dsym.loc, dsym), e);
                e = e.expressionSemantic(sc);
                dsym._init = new ExpInitializer(dsym.loc, e);
                goto Ldtor;
            }
            if (tv.ty == Tstruct && tv.isTypeStruct().sym.zeroInit)
            {
                /* If a struct is all zeros, as a special case
                 * set its initializer to the integer 0.
                 * In AssignExp::toElem(), we check for this and issue
                 * a memset() to initialize the struct.
                 * Must do same check in interpreter.
                 */
                Expression e = IntegerExp.literal!0;
                e = new BlitExp(dsym.loc, new VarExp(dsym.loc, dsym), e);
                e.type = dsym.type;      // don't type check this, it would fail
                dsym._init = new ExpInitializer(dsym.loc, e);
                goto Ldtor;
            }
            if (dsym.type.baseElemOf().ty == Tvoid)
            {
                .error(dsym.loc, "%s `%s` of type `%s` does not have a default initializer", dsym.kind, dsym.toPrettyChars, dsym.type.toChars());
            }
            else if (auto e = dsym.type.defaultInit(dsym.loc))
            {
                dsym._init = new ExpInitializer(dsym.loc, e);
            }

            // Default initializer is always a blit
            isBlit = true;
        }
        if (dsym._init)
        {
            sc = sc.push();
            sc.stc &= ~(STC.TYPECTOR | STC.pure_ | STC.nothrow_ | STC.nogc | STC.ref_ | STC.disable);

            if (sc.inCfile &&
                dsym.type.isTypeSArray() &&
                dsym.type.isTypeSArray().isIncomplete() &&
                dsym._init.isVoidInitializer() &&
                !(dsym.storage_class & STC.field))
            {
                .error(dsym.loc, "%s `%s` - incomplete array type must have initializer", dsym.kind, dsym.toPrettyChars);
            }

            ExpInitializer ei = dsym._init.isExpInitializer();

            if (ei) // https://issues.dlang.org/show_bug.cgi?id=13424
                    // Preset the required type to fail in FuncLiteralDeclaration::semantic3
                ei.exp = inferType(ei.exp, dsym.type);

            /*
             * https://issues.dlang.org/show_bug.cgi?id=24474
             * at function scope, the compiler thinks the type of the variable is not known yet
             * (semantically complete) so looks like typeof gets locked in a cyclic situation
             * semantics is actually done. just set it for importc
             */
            if (sc.func && dsym.type && dsym.type.deco && sc.inCfile)
                dsym.semanticRun = PASS.semanticdone;

            // If inside function, there is no semantic3() call
            if (sc.func || sc.intypeof == 1)
            {
                // If local variable, use AssignExp to handle all the various
                // possibilities.
                if (fd && !(dsym.storage_class & (STC.manifest | STC.static_ | STC.gshared | STC.extern_)) && !dsym._init.isVoidInitializer())
                {
                    //printf("fd = '%s', var = '%s'\n", fd.toChars(), dsym.toChars());
                    if (!ei)
                    {
                        ArrayInitializer ai = dsym._init.isArrayInitializer();
                        Expression e;
                        if (ai && tb.ty == Taarray)
                            e = ai.toAssocArrayLiteral();
                        else
                            e = dsym._init.initializerToExpression(dsym.type, sc.inCfile);
                        if (!e)
                        {
                            // Run semantic, but don't need to interpret
                            dsym._init = dsym._init.initializerSemantic(sc, dsym.type, INITnointerpret);
                            e = dsym._init.initializerToExpression(null, sc.inCfile);
                            if (!e)
                            {
                                .error(dsym.loc, "%s `%s` is not a static and cannot have static initializer", dsym.kind, dsym.toPrettyChars);
                                e = ErrorExp.get();
                            }
                        }
                        ei = new ExpInitializer(dsym._init.loc, e);
                        dsym._init = ei;
                    }
                    else if (sc.inCfile && dsym.type.isTypeSArray() &&
                             dsym.type.isTypeSArray().isIncomplete())
                    {
                        // C11 6.7.9-22 determine the size of the incomplete array,
                        // or issue an error that the initializer is invalid.
                        dsym._init = dsym._init.initializerSemantic(sc, dsym.type, INITinterpret);
                    }

                    if (ei && dsym.isScope())
                    {
                        Expression ex = ei.exp.lastComma();
                        if (ex.op == EXP.blit || ex.op == EXP.construct)
                            ex = (cast(AssignExp)ex).e2;
                        if (auto ne = ex.isNewExp())
                        {
                            if (ne.placement)
                            {
                            }
                            /* See if initializer is a NewExp that can be allocated on the stack.
                             */
                            else if (dsym.type.toBasetype().ty == Tclass)
                            {
                                /* Unsafe to allocate on stack if constructor is not `scope` because the `this` can leak.
                                 * https://issues.dlang.org/show_bug.cgi?id=23145
                                 */
                                if (ne.member && !(ne.member.storage_class & STC.scope_))
                                {
                                    import dmd.escape : setUnsafeDIP1000;
                                    const inSafeFunc = sc.func && sc.func.isSafeBypassingInference();   // isSafeBypassingInference may call setUnsafe().
                                    if (setUnsafeDIP1000(*sc, false, dsym.loc, "`scope` allocation of `%s` with a non-`scope` constructor", dsym))
                                        errorSupplemental(ne.member.loc, "is the location of the constructor");
                                }
                                ne.onstack = 1;
                                dsym.onstack = true;
                            }
                        }
                        else if (auto fe = ex.isFuncExp())
                        {
                            // or a delegate that doesn't escape a reference to the function
                            FuncDeclaration f = fe.fd;
                            if (f.tookAddressOf)
                                f.tookAddressOf--;
                        }
                        else if (auto ale = ex.isArrayLiteralExp())
                        {
                            // or an array literal assigned to a `scope` variable
                            if (sc.useDIP1000 == FeatureState.enabled
                                && !dsym.type.nextOf().needsDestruction())
                                ale.onstack = true;
                        }
                    }

                    Expression exp = ei.exp;
                    Expression e1 = new VarExp(dsym.loc, dsym);

                    void constructInit(bool isBlit)
                    {
                        if (isBlit)
                            exp = new BlitExp(dsym.loc, e1, exp);
                        else
                            exp = new ConstructExp(dsym.loc, e1, exp);
                        dsym.canassign++;
                        exp = exp.expressionSemantic(sc);
                        dsym.canassign--;
                    }

                    if (dsymIsRef) // follow logic similar to typesem.argumentMatchParameter() and statementsem.visitForeach()
                    {
                        dsym.storage_class |= STC.nodtor;
                        exp = exp.expressionSemantic(sc);
                        Type tp = dsym.type;
                        Type ta = exp.type;
                        if (!exp.isLvalue())
                        {
                            if (dsym.storage_class & STC.autoref)
                            {
                                dsym.storage_class &= ~STC.ref_;
                                constructInit(isBlit);
                            }
                            else
                            {
                                .error(dsym.loc, "rvalue `%s` cannot be assigned to `ref %s`", exp.toChars(), dsym.toChars());
                                exp = ErrorExp.get();
                            }
                        }
                        else if (!ta.constConv(tp))
                        {
                            if (dsym.storage_class & STC.autoref)
                            {
                                dsym.storage_class &= ~STC.ref_;
                                constructInit(false);
                            }
                            else
                            {
                                .error(dsym.loc, "type `%s` cannot be assigned to `ref %s %s`", ta.toChars(), tp.toChars(), dsym.toChars());
                                exp = ErrorExp.get();
                            }
                        }
                        else if (exp.isBitField())
                        {
                            if (dsym.storage_class & STC.autoref)
                            {
                                dsym.storage_class &= ~STC.ref_;
                                constructInit(false);
                            }
                            else
                            {
                                .error(dsym.loc, "bitfield `%s` cannot be assigned to `ref %s`", exp.toChars(), dsym.toChars());
                                exp = ErrorExp.get();
                            }
                        }
                        else
                        {
                            constructInit(false);
                        }
                    }
                    else
                    {
                        constructInit(isBlit);
                    }

                    if (exp.op == EXP.error)
                    {
                        dsym._init = new ErrorInitializer();
                        ei = null;
                    }
                    else
                        ei.exp = exp.optimize(WANTvalue);
                }
                else
                {
                    // https://issues.dlang.org/show_bug.cgi?id=14166
                    // Don't run CTFE for the temporary variables inside typeof
                    dsym._init = dsym._init.initializerSemantic(sc, dsym.type, sc.intypeof == 1 ? INITnointerpret : INITinterpret);
                    import dmd.semantic2 : lowerStaticAAs;
                    lowerStaticAAs(dsym, sc);
                    auto init_err = dsym._init.isExpInitializer();
                    if (init_err && init_err.exp.op == EXP.showCtfeContext)
                    {
                        init_err.exp = ErrorExp.get();
                        errorSupplemental(dsym.loc, "compile time context created here");
                    }
                }
            }
            else if (parent.isAggregateDeclaration())
            {
                dsym._scope = scx ? scx : sc.copy();
                dsym._scope.setNoFree();
            }
            else if (dsym.storage_class & (STC.const_ | STC.immutable_ | STC.manifest) ||
                     dsym.type.isConst() || dsym.type.isImmutable() ||
                     sc.inCfile)
            {
                /* Because we may need the results of a const declaration in a
                 * subsequent type, such as an array dimension, before semantic2()
                 * gets ordinarily run, try to run semantic2() now.
                 * If a C array is of unknown size, the initializer can provide the size. Do this
                 * eagerly because C does it eagerly.
                 * Ignore failure.
                 */
                if (!inferred)
                {
                    const errors = global.errors;
                    dsym.inuse++;
                    // Bug 20549. Don't try this on modules or packages, syntaxCopy
                    // could crash (inf. recursion) on a mod/pkg referencing itself
                    if (ei && (ei.exp.op != EXP.scope_ ? true : !ei.exp.isScopeExp().sds.isPackage()))
                    {
                        if (ei.exp.type)
                        {
                            // If exp is already resolved we are done, our original init exp
                            // could have a type painting that we need to respect
                            // e.g.  ['a'] typed as string, or [['z'], ""] as string[]
                            // See https://issues.dlang.org/show_bug.cgi?id=15711
                        }
                        else
                        {
                            Expression exp = ei.exp.syntaxCopy();

                            bool needctfe = dsym.isDataseg() || (dsym.storage_class & STC.manifest);
                            if (needctfe)
                                sc = sc.startCTFE();
                            sc = sc.push();
                            sc.varDecl = dsym; // https://issues.dlang.org/show_bug.cgi?id=24051
                            exp = exp.expressionSemantic(sc);
                            exp = resolveProperties(sc, exp);
                            sc = sc.pop();
                            if (needctfe)
                                sc = sc.endCTFE();
                            ei.exp = exp;
                        }

                        Type tb2 = dsym.type.toBasetype();
                        Type ti = ei.exp.type.toBasetype();

                        /* The problem is the following code:
                         *  struct CopyTest {
                         *     double x;
                         *     this(double a) { x = a * 10.0;}
                         *     this(this) { x += 2.0; }
                         *  }
                         *  const CopyTest z = CopyTest(5.3);  // ok
                         *  const CopyTest w = z;              // not ok, postblit not run
                         *  static assert(w.x == 55.0);
                         * because the postblit doesn't get run on the initialization of w.
                         */
                        if (auto ts = ti.isTypeStruct())
                        {
                            StructDeclaration sd = ts.sym;
                            /* Look to see if initializer involves a copy constructor
                             * (which implies a postblit)
                             */
                            // there is a copy constructor
                            // and exp is the same struct
                            if (sd.postblit && tb2.toDsymbol(null) == sd)
                            {
                                // The only allowable initializer is a (non-copy) constructor
                                if (ei.exp.isLvalue())
                                    .error(dsym.loc, "%s `%s` of type struct `%s` uses `this(this)`, which is not allowed in static initialization", dsym.kind, dsym.toPrettyChars, tb2.toChars());
                            }
                        }
                    }

                    dsym._init = dsym._init.initializerSemantic(sc, dsym.type, INITinterpret);
                    dsym.inuse--;
                    if (global.errors > errors)
                    {
                        dsym._init = new ErrorInitializer();
                        dsym.type = Type.terror;
                    }
                }
                else
                {
                    dsym._scope = scx ? scx : sc.copy();
                    dsym._scope.setNoFree();
                }
            }
            sc = sc.pop();
        }

    Ldtor:
        /* Build code to execute destruction, if necessary
         */
        dsym.edtor = dsym.callScopeDtor(sc);
        if (dsym.edtor)
        {
            if (sc.func && dsym.storage_class & (STC.static_ | STC.gshared))
                dsym.edtor = dsym.edtor.expressionSemantic(sc._module._scope);
            else
                dsym.edtor = dsym.edtor.expressionSemantic(sc);

            version (none)
            {
                // currently disabled because of std.stdio.stdin, stdout and stderr
                if (dsym.isDataseg() && !(dsym.storage_class & STC.extern_))
                    .error(dsym.loc, "%s `%s` static storage variables cannot have destructors", dsym.kind, dsym.toPrettyChars);
            }
        }

        dsym.semanticRun = PASS.semanticdone;

        if (dsym.type.toBasetype().ty == Terror)
            dsym.errors = true;

        if(sc.scopesym && !sc.scopesym.isAggregateDeclaration())
        {
            for (ScopeDsymbol sym = sc.scopesym; sym && dsym.endlinnum == 0;
                 sym = sym.parent ? sym.parent.isScopeDsymbol() : null)
                dsym.endlinnum = sym.endlinnum;
        }
    }

    override void visit(TypeInfoDeclaration dsym)
    {
        assert(dsym._linkage == LINK.c);
    }

    override void visit(CAsmDeclaration dsym)
    {
        if (dsym.semanticRun >= PASS.semanticdone)
            return;
        import dmd.iasm : asmSemantic;
        asmSemantic(dsym, sc);
        dsym.semanticRun = PASS.semanticdone;
    }

    override void visit(BitFieldDeclaration dsym)
    {
        //printf("BitField::semantic('%s')\n", dsym.toChars());
        if (dsym.semanticRun >= PASS.semanticdone)
            return;

        visit(cast(VarDeclaration)dsym);
        if (dsym.errors)
            return;

        if (!(sc.previews.bitfields || sc.inCfile))
        {
            .error(dsym.loc, "%s `%s` use `-%spreview=bitfields` for bitfield support", dsym.kind, dsym.toPrettyChars, SwitchPrefix.ptr);
        }

        if (!dsym.parent.isStructDeclaration() && !dsym.parent.isClassDeclaration())
        {
            .error(dsym.loc, "%s `%s` - bitfield must be member of struct, union, or class", dsym.kind, dsym.toPrettyChars);
        }

        sc = sc.startCTFE();
        auto width = dsym.width.expressionSemantic(sc);
        sc = sc.endCTFE();
        width = width.ctfeInterpret();
        if (!dsym.type.isIntegral())
        {
            // C11 6.7.2.1-5
            error(width.loc, "bitfield type `%s` is not an integer type", dsym.type.toChars());
            dsym.errors = true;
        }
        if (!width.isIntegerExp())
        {
            error(width.loc, "bitfield width `%s` is not an integer constant", dsym.width.toChars());
            dsym.errors = true;
        }
        const uwidth = width.toInteger(); // uwidth is unsigned
        if (uwidth == 0 && !dsym.isAnonymous())
        {
            error(width.loc, "bitfield `%s` has zero width", dsym.toChars());
            dsym.errors = true;
        }
        const sz = dsym.type.size();
        if (sz == SIZE_INVALID)
            dsym.errors = true;
        const max_width = sz * 8;
        if (uwidth > max_width)
        {
            error(width.loc, "width `%lld` of bitfield `%s` does not fit in type `%s`", cast(long)uwidth, dsym.toChars(), dsym.type.toChars());
            dsym.errors = true;
        }
        dsym.fieldWidth = cast(uint)uwidth;
    }

    override void visit(Import imp)
    {
        timeTraceBeginEvent(TimeTraceEventType.sema1Import);
        scope (exit) timeTraceEndEvent(TimeTraceEventType.sema1Import, imp);
        static if (LOG)
        {
            printf("Import::semantic('%s') %s\n", imp.toPrettyChars(), imp.id.toChars());
            scope(exit)
                printf("-Import::semantic('%s'), pkg = %p\n", imp.toChars(), imp.pkg);
        }
        if (imp.semanticRun > PASS.initial)
            return;

        if (imp._scope)
        {
            sc = imp._scope;
            imp._scope = null;
        }
        if (!sc)
            return;

        imp.parent = sc.parent;

        imp.semanticRun = PASS.semantic;

        // Load if not already done so
        if (!imp.mod)
        {
            // https://issues.dlang.org/show_bug.cgi?id=22857
            // if parser errors occur when loading a module
            // we should just stop compilation
            if (imp.load(sc))
            {
                for (size_t i = 0; i < imp.aliasdecls.length; i++)
                    imp.aliasdecls[i].type = Type.terror;
                return;
            }

            if (imp.mod)
            {
                imp.mod.importAll(null);
                imp.mod.checkImportDeprecation(imp.loc, sc);
            }
        }
        if (!imp.mod)
        {
            imp.semanticRun = PASS.semanticdone;
            addImportDep(global.params.moduleDeps, imp, sc._module);
        }

        // Modules need a list of each imported module

        // if inside a template instantiation, the instantianting
        // module gets the import.
        // https://issues.dlang.org/show_bug.cgi?id=17181
        Module importer = sc._module;
        if (sc.minst && sc.tinst)
        {
            importer = sc.minst;
            if (!sc.tinst.importedModules.contains(imp.mod))
                sc.tinst.importedModules.push(imp.mod);
        }
        //printf("%s imports %s\n", importer.toChars(), imp.mod.toChars());
        if (!importer.aimports.contains(imp.mod))
            importer.aimports.push(imp.mod);

        if (sc.explicitVisibility)
            imp.visibility = sc.visibility;

        if (!imp.aliasId && !imp.names.length) // neither a selective nor a renamed import
        {
            ScopeDsymbol scopesym = sc.getScopesym();

            if (!imp.isstatic)
            {
                scopesym.importScope(imp.mod, imp.visibility);
            }


            imp.addPackageAccess(scopesym);
        }

        // if a module has errors it means that parsing has failed.
        if (!imp.mod.errors)
            imp.mod.dsymbolSemantic(null);

        if (imp.mod.needmoduleinfo)
        {
            //printf("module4 %s because of %s\n", importer.toChars(), imp.mod.toChars());
            importer.needmoduleinfo = 1;
        }

        sc = sc.push(imp.mod);
        sc.visibility = imp.visibility;
        for (size_t i = 0; i < imp.aliasdecls.length; i++)
        {
            AliasDeclaration ad = imp.aliasdecls[i];
            //printf("\tImport %s alias %s = %s, scope = %p\n", toPrettyChars(), aliases[i].toChars(), names[i].toChars(), ad._scope);
            Dsymbol sym = imp.mod.search(imp.loc, imp.names[i], SearchOpt.ignorePrivateImports);
            if (sym)
            {
                import dmd.access : symbolIsVisible;
                if (!symbolIsVisible(sc, sym) && !sym.errors)
                {
                    .error(imp.loc, "%s `%s` member `%s` is not visible from module `%s`", imp.mod.kind, imp.mod.toPrettyChars,
                        imp.names[i].toChars(), sc._module.toChars());
                    sym.errors = true;
                }
                ad.dsymbolSemantic(sc);
                // If the import declaration is in non-root module,
                // analysis of the aliased symbol is deferred.
                // Therefore, don't see the ad.aliassym or ad.type here.
            }
            else
            {
                Dsymbol s = imp.mod.search_correct(imp.names[i]);
                // https://issues.dlang.org/show_bug.cgi?id=23908
                // Don't suggest symbols from the importer's module
                if (s && s.parent != importer)
                    .error(imp.loc, "%s `%s` import `%s` not found, did you mean %s `%s`?", imp.mod.kind, imp.mod.toPrettyChars, imp.names[i].toChars(), s.kind(), s.toPrettyChars());
                else
                    .error(imp.loc, "%s `%s` import `%s` not found", imp.mod.kind, imp.mod.toPrettyChars, imp.names[i].toChars());
                ad.type = Type.terror;
            }
        }
        sc = sc.pop();

        imp.semanticRun = PASS.semanticdone;
        addImportDep(global.params.moduleDeps, imp, sc._module);
    }

    void attribSemantic(AttribDeclaration ad)
    {
        if (ad.semanticRun != PASS.initial)
            return;
        ad.semanticRun = PASS.semantic;
        Dsymbols* d = ad.include(sc);
        //printf("\tAttribDeclaration::semantic '%s', d = %p\n",toChars(), d);
        if (!d)
        {
            ad.semanticRun = PASS.semanticdone;
            return;
        }

        Scope* sc2 = ad.newScope(sc);
        bool errors;
        for (size_t i = 0; i < d.length; i++)
        {
            Dsymbol s = (*d)[i];
            s.dsymbolSemantic(sc2);
            errors |= s.errors;
        }
        if (errors)
            ad.errors = true;
        if (sc2 != sc)
            sc2.pop();

        ad.semanticRun = PASS.semanticdone;
    }

    override void visit(AttribDeclaration atd)
    {
        attribSemantic(atd);
    }

    override void visit(AnonDeclaration scd)
    {
        //printf("\tAnonDeclaration::semantic isunion:%d ptr:%p\n", scd.isunion, scd);
        assert(sc.parent);
        auto p = sc.parent.pastMixin();
        auto ad = p.isAggregateDeclaration();
        if (!ad)
        {
            error(scd.loc, "%s can only be a part of an aggregate, not %s `%s`", scd.kind(), p.kind(), p.toChars());
            scd.errors = true;
            return;
        }

        if (!scd.decl)
            return;

        sc = sc.push();
        sc.stc &= ~(STC.auto_ | STC.scope_ | STC.static_ | STC.gshared);
        sc.inunion = scd.isunion ? scd : null;
        sc.resetAllFlags();
        for (size_t i = 0; i < scd.decl.length; i++)
        {
            Dsymbol s = (*scd.decl)[i];
            if (auto var = s.isVarDeclaration)
            {
                if (scd.isunion)
                    var.overlapped = true;
            }
            s.dsymbolSemantic(sc);
        }
        sc = sc.pop();
    }

    override void visit(PragmaDeclaration pd)
    {
        import dmd.pragmasem : pragmaDeclSemantic;
        pragmaDeclSemantic(pd, sc);
    }

    override void visit(StaticIfDeclaration sid)
    {
        attribSemantic(sid);
    }

    override void visit(StaticForeachDeclaration sfd)
    {
        attribSemantic(sfd);
    }

    private Dsymbols* compileIt(MixinDeclaration cd)
    {
        //printf("MixinDeclaration::compileIt(loc = %d) %s\n", cd.loc.linnum, cd.exp.toChars());
        OutBuffer buf;
        if (expressionsToString(buf, sc, cd.exps, cd.loc, null, true))
            return null;

        const errors = global.errors;
        const len = buf.length;
        buf.writeByte(0);
        const str = buf.extractSlice()[0 .. len];
        const bool doUnittests = global.params.parsingUnittestsRequired();
        scope p = new Parser!ASTCodegen(sc._module, str, false, global.errorSink, &global.compileEnv, doUnittests);
        adjustLocForMixin(str, cd.loc, *p.baseLoc, global.params.mixinOut);
        p.linnum = p.baseLoc.startLine;
        p.nextToken();

        auto d = p.parseDeclDefs(0);
        if (global.errors != errors)
            return null;

        if (p.token.value != TOK.endOfFile)
        {
            .error(cd.loc, "%s `%s` incomplete mixin declaration `%s`", cd.kind, cd.toPrettyChars, str.ptr);
            return null;
        }
        return d;
    }

    /***********************************************************
     * https://dlang.org/spec/module.html#mixin-declaration
     */
    override void visit(MixinDeclaration cd)
    {
        //printf("MixinDeclaration::semantic()\n");
        if (!cd.compiled)
        {
            cd.decl = compileIt(cd);
            attribAddMember(cd, sc, cd.scopesym);
            cd.compiled = true;

            if (cd._scope && cd.decl)
            {
                for (size_t i = 0; i < cd.decl.length; i++)
                {
                    Dsymbol s = (*cd.decl)[i];
                    s.setScope(cd._scope);
                }
            }
        }
        attribSemantic(cd);
    }

    override void visit(CPPNamespaceDeclaration ns)
    {
        Identifier identFromSE (StringExp se)
        {
            const sident = se.toStringz();
            if (!sident.length || !Identifier.isValidIdentifier(sident))
            {
                error(ns.exp.loc, "expected valid identifier for C++ namespace but got `%s`", se.toErrMsg());
                return null;
            }
            else
                return Identifier.idPool(sident);
        }

        if (ns.ident !is null)
            return attribSemantic(ns);

        ns.cppnamespace = sc.namespace;
        sc = sc.startCTFE();
        ns.exp = ns.exp.expressionSemantic(sc);
        ns.exp = resolveProperties(sc, ns.exp);
        sc = sc.endCTFE();
        ns.exp = ns.exp.ctfeInterpret();
        // Can be either a tuple of strings or a string itself
        if (auto te = ns.exp.isTupleExp())
        {
            expandTuples(te.exps);
            CPPNamespaceDeclaration current = ns.cppnamespace;
            for (size_t d = 0; d < te.exps.length; ++d)
            {
                auto exp = (*te.exps)[d];
                auto prev = d ? current : ns.cppnamespace;
                current = (d + 1) != te.exps.length
                    ? new CPPNamespaceDeclaration(ns.loc, exp, null)
                    : ns;
                current.exp = exp;
                current.cppnamespace = prev;
                if (auto se = exp.toStringExp())
                {
                    current.ident = identFromSE(se);
                    if (current.ident is null)
                        return; // An error happened in `identFromSE`
                }
                else
                    error(ns.exp.loc, "`%s`: index %llu is not a string constant, it is a `%s`",
                                 ns.exp.toChars(), cast(ulong) d, ns.exp.type.toChars());
            }
        }
        else if (auto se = ns.exp.toStringExp())
            ns.ident = identFromSE(se);
        // Empty Tuple
        else if (ns.exp.isTypeExp() && ns.exp.isTypeExp().type.toBasetype().isTypeTuple())
        {
        }
        else if (!ns.exp.type.isTypeError())
            error(ns.exp.loc, "compile time string constant (or sequence) expected, not `%s`",
                         ns.exp.toChars());
        attribSemantic(ns);
    }

    override void visit(UserAttributeDeclaration uad)
    {
        //printf("UserAttributeDeclaration::semantic() %p\n", this);
        if (uad.decl && !uad._scope)
            uad.Dsymbol.setScope(sc); // for function local symbols
        arrayExpressionSemantic(uad.atts.peekSlice(), sc, true);
        return attribSemantic(uad);
    }

    override void visit(StaticAssert sa)
    {
        if (sa.semanticRun < PASS.semanticdone)
            sa.semanticRun = PASS.semanticdone;
        else
            return;

        // https://issues.dlang.org/show_bug.cgi?id=24645
        // This is a short-circuit. Usually, static assert conditions are evaluated
        // in semantic2, but it's not uncommon to use this pattern:
        // ---
        // version(X)
        // {}
        // else
        //   static assert(false, "unsupported platform");
        // ---
        // However, without this short-circuit, the static assert error may get drowned
        // out by subsequent semantic1 (import) errors. Only short-circuit at module scope though,
        // inside mixin templates you want an instantiation trace (which you don't get here).
        if (sc.parent && sc.parent.isModule())
            if (auto i = sa.exp.isIntegerExp())
                if (i.toInteger() == 0)
                    staticAssertFail(sa, sc);
    }

    override void visit(DebugSymbol ds)
    {
        //printf("DebugSymbol::semantic() %s\n", toChars());
        if (ds.semanticRun < PASS.semanticdone)
            ds.semanticRun = PASS.semanticdone;
    }

    override void visit(VersionSymbol vs)
    {
        if (vs.semanticRun < PASS.semanticdone)
            vs.semanticRun = PASS.semanticdone;
    }

    override void visit(Package pkg)
    {
        if (pkg.semanticRun < PASS.semanticdone)
            pkg.semanticRun = PASS.semanticdone;
    }

    override void visit(Module m)
    {
        if (m.semanticRun != PASS.initial)
            return;

        timeTraceBeginEvent(TimeTraceEventType.sema1Module);
        scope (exit) timeTraceEndEvent(TimeTraceEventType.sema1Module, m);

        //printf("+Module::semantic(this = %p, '%s'): parent = %p\n", this, toChars(), parent);
        m.semanticRun = PASS.semantic;
        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope* sc = m._scope; // see if already got one from importAll()
        if (!sc)
        {
            sc = scopeCreateGlobal(m, global.errorSink); // create root scope
        }

        //printf("Module = %p, linkage = %d\n", sc.scopesym, sc.linkage);
        // Pass 1 semantic routines: do public side of the definition
        m.members.foreachDsymbol( (s)
        {
            //printf("\tModule('%s'): '%s'.dsymbolSemantic()\n", toChars(), s.toChars());
            s.dsymbolSemantic(sc);
            runDeferredSemantic();
        });

        if (m.userAttribDecl)
        {
            m.userAttribDecl.dsymbolSemantic(sc);
        }
        if (!m._scope)
        {
            sc = sc.pop();
            sc.pop(); // 2 pops because scopeCreateGlobal() created 2
        }
        m.semanticRun = PASS.semanticdone;
        //printf("-Module::semantic(this = %p, '%s'): parent = %p\n", this, toChars(), parent);
    }

    override void visit(EnumDeclaration ed)
    {
        enumSemantic(sc, ed);
    }

    override void visit(EnumMember em)
    {
        enumMemberSemantic(sc, em);
    }

    override void visit(TemplateDeclaration tempdecl)
    {
        templateDeclarationSemantic(sc, tempdecl);
    }

    override void visit(TemplateInstance ti)
    {
        templateInstanceSemantic(ti, sc, ArgumentList());
    }

    override void visit(TemplateMixin tm)
    {
        static if (LOG)
        {
            printf("+TemplateMixin.dsymbolSemantic('%s', this=%p)\n", tm.toChars(), tm);
            fflush(stdout);
        }
        if (tm.semanticRun != PASS.initial)
        {
            // When a class/struct contains mixin members, and is done over
            // because of forward references, never reach here so semanticRun
            // has been reset to PASS.initial.
            static if (LOG)
            {
                printf("\tsemantic done\n");
            }
            return;
        }
        tm.semanticRun = PASS.semantic;
        static if (LOG)
        {
            printf("\tdo semantic\n");
        }

        Scope* scx = null;
        if (tm._scope)
        {
            sc = tm._scope;
            scx = tm._scope; // save so we don't make redundant copies
            tm._scope = null;
        }

        /* Run semantic on each argument, place results in tiargs[],
         * then find best match template with tiargs
         */
        if (!tm.findMixinTempDecl(sc) || !tm.semanticTiargs(sc) || !tm.findBestMatch(sc, ArgumentList()))
        {
            if (tm.semanticRun == PASS.initial) // forward reference had occurred
            {
                //printf("forward reference - deferring\n");
                return deferDsymbolSemantic(sc, tm, scx);
            }

            tm.inst = tm;
            tm.errors = true;
            return; // error recovery
        }

        auto tempdecl = tm.tempdecl.isTemplateDeclaration();
        assert(tempdecl);

        if (!tm.ident)
        {
            /* Assign scope local unique identifier, as same as lambdas.
             */
            const(char)[] s = "__mixin";

            if (FuncDeclaration func = sc.parent.isFuncDeclaration())
            {
                tm.symtab = func.localsymtab;
                if (tm.symtab)
                {
                    // Inside template constraint, symtab is not set yet.
                    goto L1;
                }
            }
            else
            {
                tm.symtab = sc.parent.isScopeDsymbol().symtab;
            L1:
                assert(tm.symtab);
                tm.ident = Identifier.generateId(s, tm.symtab.length + 1);
                tm.symtab.insert(tm);
            }
        }

        tm.inst = tm;
        tm.parent = sc.parent;

        /* Detect recursive mixin instantiations.
         */
        for (Dsymbol s = tm.parent; s; s = s.parent)
        {
            //printf("\ts = '%s'\n", s.toChars());
            TemplateMixin tmix = s.isTemplateMixin();
            if (!tmix || tempdecl != tmix.tempdecl)
                continue;

            /* Different argument list lengths happen with variadic args
             */
            if (tm.tiargs.length != tmix.tiargs.length)
                continue;

            for (size_t i = 0; i < tm.tiargs.length; i++)
            {
                RootObject o = (*tm.tiargs)[i];
                Type ta = isType(o);
                Expression ea = isExpression(o);
                Dsymbol sa = isDsymbol(o);
                RootObject tmo = (*tmix.tiargs)[i];
                if (ta)
                {
                    Type tmta = isType(tmo);
                    if (!tmta)
                        goto Lcontinue;
                    if (!ta.equals(tmta))
                        goto Lcontinue;
                }
                else if (ea)
                {
                    Expression tme = isExpression(tmo);
                    if (!tme || !dmd.expressionsem.equals(ea, tme))
                        goto Lcontinue;
                }
                else if (sa)
                {
                    Dsymbol tmsa = isDsymbol(tmo);
                    if (sa != tmsa)
                        goto Lcontinue;
                }
                else
                    assert(0);
            }
            .error(tm.loc, "%s `%s` recursive mixin instantiation", tm.kind, tm.toPrettyChars);
            return;

        Lcontinue:
            continue;
        }

        // Copy the syntax trees from the TemplateDeclaration
        tm.members = Dsymbol.arraySyntaxCopy(tempdecl.members);
        if (!tm.members)
            return;

        tm.symtab = new DsymbolTable();

        sc.getScopesym().importScope(tm, Visibility(Visibility.Kind.public_));

        static if (LOG)
        {
            printf("\tcreate scope for template parameters '%s'\n", tm.toChars());
        }
        Scope* scy = sc.push(tm);
        scy.parent = tm;

        /* https://issues.dlang.org/show_bug.cgi?id=930
         *
         * If the template that is to be mixed in is in the scope of a template
         * instance, we have to also declare the type aliases in the new mixin scope.
         */
        auto parentInstance = tempdecl.parent ? tempdecl.parent.isTemplateInstance() : null;
        if (parentInstance)
            parentInstance.declareParameters(scy);

        tm.argsym = new ScopeDsymbol();
        tm.argsym.parent = scy.parent;
        Scope* argscope = scy.push(tm.argsym);

        const errorsave = global.errors;

        // Declare each template parameter as an alias for the argument type
        tm.declareParameters(argscope);

        // Add members to enclosing scope, as well as this scope
        tm.members.foreachDsymbol(s => s.addMember(argscope, tm));

        // Do semantic() analysis on template instance members
        static if (LOG)
        {
            printf("\tdo semantic() on template instance members '%s'\n", tm.toChars());
        }
        Scope* sc2 = argscope.push(tm);
        //size_t deferred_dim = Module.deferred.length;

        __gshared int nest;
        //printf("%d\n", nest);
        if (++nest > global.recursionLimit)
        {
            global.gag = 0; // ensure error message gets printed
            .error(tm.loc, "%s `%s` recursive expansion", tm.kind, tm.toPrettyChars);
            fatal();
        }

        tm.members.foreachDsymbol( s => s.setScope(sc2) );

        tm.members.foreachDsymbol( s => s.importAll(sc2) );

        tm.members.foreachDsymbol( s => s.dsymbolSemantic(sc2) );

        nest--;

        /* In DeclDefs scope, TemplateMixin does not have to handle deferred symbols.
         * Because the members would already call addDeferredSemantic() for themselves.
         * See Struct, Class, Interface, and EnumDeclaration.dsymbolSemantic().
         */
        //if (!sc.func && Module.deferred.length > deferred_dim) {}

        AggregateDeclaration ad = tm.isMember();
        if (sc.func && !ad)
        {
            tm.semantic2(sc2);
            tm.semantic3(sc2);
        }

        // Give additional context info if error occurred during instantiation
        if (global.errors != errorsave)
        {
            .error(tm.loc, "%s `%s` error instantiating", tm.kind, tm.toPrettyChars);
            tm.errors = true;
        }

        sc2.pop();
        argscope.pop();
        scy.pop();

        static if (LOG)
        {
            printf("-TemplateMixin.dsymbolSemantic('%s', this=%p)\n", tm.toChars(), tm);
        }
    }

    override void visit(Nspace ns)
    {
        if (ns.semanticRun != PASS.initial)
            return;
        static if (LOG)
        {
            printf("+Nspace::semantic('%s')\n", ns.toChars());
            scope(exit) printf("-Nspace::semantic('%s')\n", ns.toChars());
        }
        if (ns._scope)
        {
            sc = ns._scope;
            ns._scope = null;
        }
        if (!sc)
            return;

        bool repopulateMembers = false;
        if (ns.identExp)
        {
            // resolve the namespace identifier
            sc = sc.startCTFE();
            Expression resolved = ns.identExp.expressionSemantic(sc);
            resolved = resolveProperties(sc, resolved);
            sc = sc.endCTFE();
            resolved = resolved.ctfeInterpret();
            StringExp name = resolved.toStringExp();
            TupleExp tup = name ? null : resolved.isTupleExp();
            if (!tup && !name)
            {
                error(ns.loc, "expected string expression for namespace name, got `%s`", ns.identExp.toChars());
                return;
            }
            ns.identExp = resolved; // we don't need to keep the old AST around
            if (name)
            {
                const(char)[] ident = name.toStringz();
                if (ident.length == 0 || !Identifier.isValidIdentifier(ident))
                {
                    error(ns.loc, "expected valid identifier for C++ namespace but got `%.*s`", cast(int)ident.length, ident.ptr);
                    return;
                }
                ns.ident = Identifier.idPool(ident);
            }
            else
            {
                // create namespace stack from the tuple
                Nspace parentns = ns;
                foreach (i, exp; *tup.exps)
                {
                    name = exp.toStringExp();
                    if (!name)
                    {
                        error(ns.loc, "expected string expression for namespace name, got `%s`", exp.toChars());
                        return;
                    }
                    const(char)[] ident = name.toStringz();
                    if (ident.length == 0 || !Identifier.isValidIdentifier(ident))
                    {
                        error(ns.loc, "expected valid identifier for C++ namespace but got `%.*s`", cast(int)ident.length, ident.ptr);
                        return;
                    }
                    if (i == 0)
                    {
                        ns.ident = Identifier.idPool(ident);
                    }
                    else
                    {
                        // insert the new namespace
                        Nspace childns = new Nspace(ns.loc, Identifier.idPool(ident), null, parentns.members);
                        parentns.members = new Dsymbols;
                        parentns.members.push(childns);
                        parentns = childns;
                        repopulateMembers = true;
                    }
                }
            }
        }

        ns.semanticRun = PASS.semantic;
        ns.parent = sc.parent;
        // Link does not matter here, if the UDA is present it will error
        checkGNUABITag(ns, LINK.cpp);

        if (!ns.members)
        {
            ns.semanticRun = PASS.semanticdone;
            return;
        }
        assert(sc);
        sc = sc.push(ns);
        sc.linkage = LINK.cpp; // note that namespaces imply C++ linkage
        sc.parent = ns;
        foreach (s; *ns.members)
        {
            if (repopulateMembers)
            {
                s.addMember(sc, sc.scopesym);
                s.setScope(sc);
            }
            s.importAll(sc);
        }
        foreach (s; *ns.members)
        {
            static if (LOG)
            {
                printf("\tmember '%s', kind = '%s'\n", s.toChars(), s.kind());
            }
            s.dsymbolSemantic(sc);
        }
        sc.pop();
        ns.semanticRun = PASS.semanticdone;
    }

     /// Do the semantic analysis on the external interface to the function.
    override void visit(FuncDeclaration funcdecl)
    {
        funcDeclarationSemantic(sc, funcdecl);
    }

    override void visit(CtorDeclaration ctd)
    {
        //printf("CtorDeclaration::semantic() %p %s\n", ctd, ctd.toChars());
        if (ctd.semanticRun >= PASS.semanticdone)
            return;
        if (ctd._scope)
        {
            sc = ctd._scope;
            ctd._scope = null;
        }

        ctd.parent = sc.parent;
        Dsymbol p = ctd.toParentDecl();
        AggregateDeclaration ad = p.isAggregateDeclaration();
        if (!ad)
        {
            error(ctd.loc, "constructor can only be a member of aggregate, not %s `%s`", p.kind(), p.toChars());
            ctd.type = Type.terror;
            ctd.errors = true;
            return;
        }

        sc = sc.push();

        if (sc.stc & STC.static_)
        {
            if (sc.stc & STC.shared_)
                error(ctd.loc, "`shared static` has no effect on a constructor inside a `shared static` block. Use `shared static this()`");
            else
                error(ctd.loc, "`static` has no effect on a constructor inside a `static` block. Use `static this()`");
        }

        sc.stc &= ~STC.static_; // not a static constructor

        funcDeclarationSemantic(sc, ctd);

        sc.pop();

        if (ctd.errors)
            return;

        TypeFunction tf = ctd.type.toTypeFunction();
        immutable dim = tf.parameterList.length;
        auto sd = ad.isStructDeclaration();

        /* See if it's the default constructor
         * But, template constructor should not become a default constructor.
         */
        if (ad && (!ctd.parent.isTemplateInstance() || ctd.parent.isTemplateMixin()))
        {
            if (!sd)
            {
                if (dim == 0 && tf.parameterList.varargs == VarArg.none)
                    ad.defaultCtor = ctd;
                return;
            }

            if (dim == 0 && tf.parameterList.varargs == VarArg.none) // empty default ctor w/o any varargs
            {
                if (ctd.fbody || !(ctd.storage_class & STC.disable))
                {
                    .error(ctd.loc, "%s `%s` default constructor for structs only allowed " ~
                        "with `@disable`, no body, and no parameters", ctd.kind, ctd.toPrettyChars);
                    ctd.storage_class |= STC.disable;
                    ctd.fbody = null;
                }
                sd.noDefaultCtor = true;
            }
            else if (dim == 0 && tf.parameterList.varargs != VarArg.none) // allow varargs only ctor
            {
            }
            else if (dim && !tf.parameterList.hasArgsWithoutDefault)
            {
                if (ctd.storage_class & STC.disable)
                {
                    .error(ctd.loc, "%s `%s` is marked `@disable`, so it cannot have default "~
                              "arguments for all parameters.", ctd.kind, ctd.toPrettyChars);
                    errorSupplemental(ctd.loc, "Use `@disable this();` if you want to disable default initialization.");
                }
                else
                    .error(ctd.loc, "%s `%s` all parameters have default arguments, "~
                              "but structs cannot have default constructors.", ctd.kind, ctd.toPrettyChars);
            }
            else if ((dim == 1 || (dim > 1 && tf.parameterList[1].defaultArg)))
            {
                //printf("tf: %s\n", toChars(tf));
                auto param = tf.parameterList[0];
                if (param.type.mutableOf().unSharedOf() == sd.type.mutableOf().unSharedOf())
                {
                    //printf("copy constructor %p\n", ctd);
                    assert(!ctd.isCpCtor && !ctd.isMoveCtor);
                    if (param.storageClass & STC.ref_)
                        ctd.isCpCtor = true;            // copy constructor
                    else
                        ctd.isMoveCtor = true;          // move constructor
                    assert(!(ctd.isCpCtor && ctd.isMoveCtor));
                }
            }
        }
        // https://issues.dlang.org/show_bug.cgi?id=22593
        else if (auto ti = ctd.parent.isTemplateInstance())
        {
            checkHasBothRvalueAndCpCtor(sd, ctd, ti);
        }
    }

    override void visit(PostBlitDeclaration pbd)
    {
        //printf("PostBlitDeclaration::semantic() %s\n", toChars());
        //printf("ident: %s, %s, %p, %p\n", ident.toChars(), Id.dtor.toChars(), ident, Id.dtor);
        //printf("stc = x%llx\n", sc.stc);
        if (pbd.semanticRun >= PASS.semanticdone)
            return;
        if (pbd._scope)
        {
            sc = pbd._scope;
            pbd._scope = null;
        }

        pbd.parent = sc.parent;
        Dsymbol p = pbd.toParent2();
        StructDeclaration ad = p.isStructDeclaration();
        if (!ad)
        {
            error(pbd.loc, "postblit can only be a member of struct, not %s `%s`", p.kind(), p.toChars());
            pbd.type = Type.terror;
            pbd.errors = true;
            return;
        }
        if (pbd.ident == Id.postblit && pbd.semanticRun < PASS.semantic)
            ad.postblits.push(pbd);
        if (!pbd.type)
            pbd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, pbd.storage_class);

        sc = sc.push();
        sc.stc &= ~STC.static_; // not static
        sc.linkage = LINK.d;

        funcDeclarationSemantic(sc, pbd);

        sc.pop();
    }

    override void visit(DtorDeclaration dd)
    {
        //printf("DtorDeclaration::semantic() %s\n", dd.toChars());
        //printf("ident: %s, %s, %p, %p\n", dd.ident.toChars(), Id.dtor.toChars(), dd.ident, Id.dtor);
        if (dd.semanticRun >= PASS.semanticdone)
            return;
        if (dd._scope)
        {
            sc = dd._scope;
            dd._scope = null;
        }

        dd.parent = sc.parent;
        Dsymbol p = dd.toParent2();
        AggregateDeclaration ad = p.isAggregateDeclaration();
        if (!ad)
        {
            error(dd.loc, "destructor can only be a member of aggregate, not %s `%s`", p.kind(), p.toChars());
            dd.type = Type.terror;
            dd.errors = true;
            return;
        }

        if (ad.isClassDeclaration() && ad.classKind == ClassKind.d)
        {
            // Class destructors are implicitly `scope`
            dd.storage_class |= STC.scope_;
        }

        if (dd.ident == Id.dtor && dd.semanticRun < PASS.semantic)
            ad.userDtors.push(dd);
        if (!dd.type)
        {
            dd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, dd.storage_class);
            if (ad.classKind == ClassKind.cpp && dd.ident == Id.dtor)
            {
                if (auto cldec = ad.isClassDeclaration())
                {
                    assert (cldec.cppDtorVtblIndex == -1); // double-call check already by dd.type
                    if (cldec.baseClass && cldec.baseClass.cppDtorVtblIndex != -1)
                    {
                        // override the base virtual
                        cldec.cppDtorVtblIndex = cldec.baseClass.cppDtorVtblIndex;
                    }
                    else if (!dd.isFinal())
                    {
                        // reserve the dtor slot for the destructor (which we'll create later)
                        cldec.cppDtorVtblIndex = cast(int)cldec.vtbl.length;
                        cldec.vtbl.push(dd);
                        if (target.cpp.twoDtorInVtable)
                            cldec.vtbl.push(dd); // deleting destructor uses a second slot
                    }
                }
            }
        }

        sc = sc.push();
        sc.stc &= ~STC.static_; // not a static destructor
        if (sc.linkage != LINK.cpp)
            sc.linkage = LINK.d;

        funcDeclarationSemantic(sc, dd);

        sc.pop();
    }

    void visitStaticCDtorDeclaration(FuncDeclaration sd, bool isDestructor)
    {
        if (sd.semanticRun >= PASS.semanticdone)
            return;
        if (sd._scope)
        {
            sc = sd._scope;
            sd._scope = null;
        }
        sd.parent = sc.parent;
        Dsymbol p = sd.parent.pastMixin();
        const bool isShared = !!(sd.isSharedStaticDtorDeclaration() || sd.isSharedStaticCtorDeclaration());
        const(char)* what = isDestructor ? "destructor" : "constructor";
        if (!p.isScopeDsymbol())
        {
            const(char)* s = isShared ? "shared " : "";
            error(sd.loc, "`%sstatic` %s can only be member of module/aggregate/template, not %s `%s`", s, what, p.kind(), p.toChars());
            sd.type = Type.terror;
            sd.errors = true;
            return;
        }

        if (!sd.type)
            sd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, sd.storage_class);

        /* If the static [dc]tor appears within a template instantiation,
         * it could get called multiple times by the module constructors
         * for different modules. Thus, protect it with a gate.
         */
        if (sd.isInstantiated() && sd.semanticRun < PASS.semantic)
        {
            /* Add this prefix to the constructor:
             * ```
             * static int gate;
             * if ([--|++]gate != [0|1]) return; // dependant on ctor/dtor
             * ```
             * or, for shared constructor:
             * ```
             * shared int gate;
             * enum op  = isDestructor ? "-=" : "+=";
             * enum cmp = isDestructor ? 0 : 1;
             * if (core.atomic.atomicOp!op(gate, 1) != cmp) return;
             * ```
             */

            auto v = new VarDeclaration(Loc.initial, Type.tint32, Id.gate, null);
            v.storage_class = STC.temp | STC.static_ | (isShared ? STC.shared_ : STC.none);

            Statement s = new ExpStatement(Loc.initial, v);
            auto sa = new Statements(s);

            Expression e;
            if (isShared)
            {
                e = doAtomicOp(isDestructor ? "-=" : "+=", v.ident, IntegerExp.literal!(1));
                if (e is null)
                {
                    .error(sd.loc, "%s `%s` shared static %s within a template require `core.atomic : atomicOp` to be present", sd.kind, sd.toPrettyChars, what);
                    return;
                }
            }
            else
            {
                IntegerExp one = isDestructor ? IntegerExp.literal!(-1) : IntegerExp.literal!(1);
                e = new AddAssignExp(
                    Loc.initial, new IdentifierExp(Loc.initial, v.ident), one);
            }
            IntegerExp cmp = isDestructor ? IntegerExp.literal!0 : IntegerExp.literal!1;
            e = new EqualExp(EXP.notEqual, Loc.initial, e, cmp);
            s = new IfStatement(Loc.initial, null, e, new ReturnStatement(Loc.initial, null), null, Loc.initial);

            sa.push(s);
            if (sd.fbody)
                sa.push(sd.fbody);

            sd.fbody = new CompoundStatement(Loc.initial, sa);
            if (isDestructor)
                (cast(StaticDtorDeclaration)sd).vgate = v;
        }
        const LINK save = sc.linkage;
        if (save != LINK.d)
        {
            const(char)* s = isShared ? "shared " : "";
            deprecation(sd.loc, "`%sstatic` %s can only be of D linkage", s, what);
            // Just correct it
            sc.linkage = LINK.d;
        }
        funcDeclarationSemantic(sc, sd);
        sc.linkage = save;

        // We're going to need ModuleInfo
        Module m = sd.getModule();
        if (!m)
            m = sc._module;
        if (m)
        {
            m.needmoduleinfo = 1;
            //printf("module2 %s needs moduleinfo\n", m.toChars());
        }
    }
    override void visit(StaticCtorDeclaration scd)
    {
        //printf("StaticCtorDeclaration::semantic()\n");
        visitStaticCDtorDeclaration(scd, false);

        foreachUda(scd, sc, (Expression e) {
            import dmd.attrib : isEnumAttribute;
            if (!isEnumAttribute(e, Id.udaStandalone))
                return 0;

            if (auto sharedCtor = scd.isSharedStaticCtorDeclaration())
            {
                auto trust = sharedCtor.type.isTypeFunction().trust;
                if (trust != TRUST.system && trust != TRUST.trusted)
                    error(e.loc, "a module constructor using `@%s` must be `@system` or `@trusted`", Id.udaStandalone.toChars());
                sharedCtor.standalone = true;
            }
            else
                .error(e.loc, "`@%s` can only be used on shared static constructors", Id.udaStandalone.toChars());

            return 1;
        });
    }

    override void visit(StaticDtorDeclaration sdd)
    {
        visitStaticCDtorDeclaration(sdd, true);
    }

    override void visit(InvariantDeclaration invd)
    {
        if (invd.semanticRun >= PASS.semanticdone)
            return;
        if (invd._scope)
        {
            sc = invd._scope;
            invd._scope = null;
        }

        invd.parent = sc.parent;
        Dsymbol p = invd.parent.pastMixin();
        AggregateDeclaration ad = p.isAggregateDeclaration();
        if (!ad)
        {
            error(invd.loc, "`invariant` can only be a member of aggregate, not %s `%s`", p.kind(), p.toChars());
            invd.type = Type.terror;
            invd.errors = true;
            return;
        }
        if (invd.ident != Id.classInvariant &&
             invd.semanticRun < PASS.semantic &&
             !ad.isUnionDeclaration()           // users are on their own with union fields
           )
        {
            invd.fixupInvariantIdent(ad.invs.length);
            ad.invs.push(invd);
        }
        if (!invd.type)
            invd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, invd.storage_class);

        sc = sc.push();
        sc.stc &= ~STC.static_; // not a static invariant
        sc.stc |= STC.const_; // invariant() is always const
        sc.contract = Contract.invariant_;
        sc.linkage = LINK.d;

        funcDeclarationSemantic(sc, invd);

        sc.pop();
    }

    override void visit(UnitTestDeclaration utd)
    {
        if (utd.semanticRun >= PASS.semanticdone)
            return;
        if (utd._scope)
        {
            sc = utd._scope;
            utd._scope = null;
        }

        utd.visibility = sc.visibility;

        utd.parent = sc.parent;
        Dsymbol p = utd.parent.pastMixin();
        if (!p.isScopeDsymbol())
        {
            error(utd.loc, "`unittest` can only be a member of module/aggregate/template, not %s `%s`", p.kind(), p.toChars());
            utd.type = Type.terror;
            utd.errors = true;
            return;
        }

        if (global.params.useUnitTests)
        {
            if (!utd.type)
                utd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, utd.storage_class);
            Scope* sc2 = sc.push();
            sc2.linkage = LINK.d;
            funcDeclarationSemantic(sc, utd);
            sc2.pop();
        }

        version (none)
        {
            // We're going to need ModuleInfo even if the unit tests are not
            // compiled in, because other modules may import this module and refer
            // to this ModuleInfo.
            // (This doesn't make sense to me?)
            Module m = utd.getModule();
            if (!m)
                m = sc._module;
            if (m)
            {
                //printf("module3 %s needs moduleinfo\n", m.toChars());
                m.needmoduleinfo = 1;
            }
        }
    }

    override void visit(NewDeclaration nd)
    {
        //printf("NewDeclaration::semantic()\n");
        if (nd.semanticRun >= PASS.semanticdone)
            return;
        if (!nd.type)
            nd.type = new TypeFunction(ParameterList(), Type.tvoid.pointerTo(), LINK.d, nd.storage_class);

        funcDeclarationSemantic(sc, nd);
    }

    override void visit(StructDeclaration sd)
    {
        enum log = false;
        if (log) printf("+StructDeclaration::semantic(this=%p, '%s', sizeok = %d)\n", sd, sd.toPrettyChars(), sd.sizeok);

        //static int count; if (++count == 20) assert(0);

        if (sd.semanticRun >= PASS.semanticdone)
            return;
        const errors = global.errors;

        //printf("+StructDeclaration::semantic(this=%p, '%s', sizeok = %d)\n", sd, sd.toPrettyChars(), sd.sizeok);
        Scope* scx = null;
        if (sd._scope)
        {
            sc = sd._scope;
            scx = sd._scope; // save so we don't make redundant copies
            sd._scope = null;
        }

        if (!sd.parent)
        {
            assert(sc.parent && sc.func);
            sd.parent = sc.parent;
        }
        assert(sd.parent && !sd.isAnonymous());

        if (sd.errors)
            sd.type = Type.terror;
        if (sd.semanticRun == PASS.initial)
            sd.type = sd.type.addSTC(sc.stc | sd.storage_class);
        sd.type = sd.type.typeSemantic(sd.loc, sc);
        auto ts = sd.type.isTypeStruct();
        if (ts)
        {
            if (ts.sym != sd)
            {
                TemplateInstance ti = ts.sym.isInstantiated();
                if (ti && isError(ti))
                    ts.sym = sd;
                /* For C modules, if module A contains `struct S;` and
                 * module B contains `struct S { members...}` then replace
                 * the former with the latter
                 */
                else if (!ts.sym.members && sd.members)
                    ts.sym = sd;
            }
        }

        // Ungag errors when not speculative
        Ungag ungag = sd.ungagSpeculative();

        if (sd.semanticRun == PASS.initial)
        {
            sd.visibility = sc.visibility;

            if (sd.alignment.isUnknown())       // can be set already by `struct __declspec(align(N)) Tag { ... }`
                sd.alignment = sc.alignment();

            sd.storage_class |= sc.stc;
            if (sd.storage_class & STC.abstract_)
                .error(sd.loc, "%s `%s` structs, unions cannot be `abstract`", sd.kind, sd.toPrettyChars);

            sd.userAttribDecl = sc.userAttribDecl;

            if (sc.linkage == LINK.cpp)
                sd.classKind = ClassKind.cpp;
            else if (sc.linkage == LINK.c)
                sd.classKind = ClassKind.c;
            sd.cppnamespace = sc.namespace;
            sd.cppmangle = sc.cppmangle;
        }
        else if (sd.symtab && !scx)
            return;

        sd.semanticRun = PASS.semantic;
        checkGNUABITag(sd, sc.linkage);

        if (!sd.members) // if opaque declaration
        {
            if (log) printf("\topaque declaration %s\n", sd.toChars());
            sd.semanticRun = PASS.semanticdone;
            return;
        }
        if (!sd.symtab)
        {
            sd.symtab = new DsymbolTable();

            sd.members.foreachDsymbol( s => s.addMember(sc, sd) );
        }

        auto sc2 = sd.newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        sd.members.foreachDsymbol( s => s.setScope(sc2) );
        sd.members.foreachDsymbol( s => s.importAll(sc2) );
        sd.members.foreachDsymbol( (s) { s.dsymbolSemantic(sc2); if (sd.errors) s.errors = true; } );

        if (sd.errors)
            sd.type = Type.terror;

        if (!sd.determineFields())
        {
            if (sd.type.ty != Terror)
            {
                .error(sd.loc, "%s `%s` circular or forward reference", sd.kind, sd.toPrettyChars);
                sd.errors = true;
                sd.type = Type.terror;
            }

            sc2.pop();
            sd.semanticRun = PASS.semanticdone;
            return;
        }
        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types. For example, buildDtor
         * needs to check existence of elaborate dtor in type of each fields.
         * See the case in compilable/test14838.d
         */
        foreach (v; sd.fields)
        {
            Type tb = v.type.baseElemOf();
            if (tb.ty != Tstruct)
                continue;
            auto sdec = (cast(TypeStruct)tb).sym;
            if (sdec.semanticRun >= PASS.semanticdone)
                continue;

            sc2.pop();

            if (log) printf("\tdeferring %s\n", sd.toChars());
            return deferDsymbolSemantic(sc, sd, scx);
        }

        /* Look for special member functions.
         */
        sd.disableNew = sd.search(Loc.initial, Id.classNew) !is null;

        // Look for the constructor
        sd.ctor = sd.searchCtor();

        buildDtors(sd, sc2);

        bool hasCopyCtor;
        bool hasMoveCtor;
        bool needCopyCtor;
        bool needMoveCtor;
        needCopyOrMoveCtor(sd, hasCopyCtor, hasMoveCtor, needCopyCtor, needMoveCtor);
        //printf("%s hasCopy %d hasMove %d needCopy %d needMove %d\n", sd.toChars(), hasCopyCtor, hasMoveCtor, needCopyCtor, needMoveCtor);

        /* When generating a move ctor, generate a copy ctor too, otherwise
         *  https://github.com/s-ludwig/taggedalgebraic/issues/75
         */
        if (0 && needMoveCtor && !hasCopyCtor)
        {
            needCopyCtor = true;
        }

        if (needCopyCtor)
        {
            assert(hasCopyCtor == false);
            buildCopyOrMoveCtor(sd, sc2, false); // build copy constructor
            hasCopyCtor = true;
        }
        if (needMoveCtor)
        {
            assert(hasMoveCtor == false);
            buildCopyOrMoveCtor(sd, sc2, true); // build move constructor
            hasMoveCtor = true;
        }
        sd.hasCopyCtor = hasCopyCtor;
        sd.hasMoveCtor = hasMoveCtor;

        sd.postblit = buildPostBlit(sd, sc2);

        buildOpAssign(sd, sc2);
        buildOpEquals(sd, sc2);

        if (!sc2.inCfile &&
            global.params.useTypeInfo && Type.dtypeinfo)  // these functions are used for TypeInfo
        {
            sd.xeq = buildXopEquals(sd, sc2);
            sd.xcmp = buildXopCmp(sd, sc2);
            sd.xhash = buildXtoHash(sd, sc2);
        }

        sd.inv = buildInv(sd, sc2);

        sd.rtInfoScope = sc2;
        sc2.setNoFree();

        sd.semanticRun = PASS.semanticdone;
        if (log) printf("-StructDeclaration::semantic(this=%p, '%s', sizeok = %d)\n", sd, sd.toPrettyChars(), sd.sizeok);

        sc2.pop();

        if (sd.ctor)
        {
            Dsymbol scall = sd.search(Loc.initial, Id.opCall);
            if (scall)
            {
                const xerrors = global.startGagging();
                sc = sc.push();
                sc.tinst = null;
                sc.minst = null;
                auto fcall = resolveFuncCall(sd.loc, sc, scall, null, null, ArgumentList(), FuncResolveFlag.quiet);
                sc = sc.pop();
                global.endGagging(xerrors);

                if (fcall && fcall.isStatic())
                {
                    .error(fcall.loc, "%s `%s` `static opCall` is hidden by constructors and can never be called", sd.kind, sd.toPrettyChars);
                    errorSupplemental(fcall.loc, "Please use a factory method instead, or replace all constructors with `static opCall`.");
                }
            }
        }

        if (ts && ts.sym != sd)
        {
            StructDeclaration sym = ts.sym;
            if (sd.isCsymbol() && sym.isCsymbol())
            {

                if (!isCCompatible(sd, sym))
                {
                    // Already issued an error.
                    errorSupplemental(sd.loc, "C %ss with the same name from different imports are merged", sd.kind);
                }
                else {
                    /* This is two structs imported from different C files.
                     * Just ignore sd, the second one. The first one will always
                     * be found when going through the type.
                     */
                }
            }
            else
            {
                version (none)
                {
                    printf("this = %p %s\n", sd, sd.toChars());
                    printf("type = %d sym = %p, %s\n", sd.type.ty, sym, sym.toPrettyChars());
                }
                // https://issues.dlang.org/show_bug.cgi?id=19024
                .error(sd.loc, "%s `%s` already exists at %s. Perhaps in another function with the same name?", sd.kind, sd.toPrettyChars, sym.loc.toChars());
            }
        }

        if (global.errors != errors)
        {
            // The type is no good.
            sd.type = Type.terror;
            sd.errors = true;
            if (sd.deferred)
                sd.deferred.errors = true;
        }

        if (sd.deferred && !global.gag)
        {
            sd.deferred.semantic2(sc);
            sd.deferred.semantic3(sc);
        }

        version (none)
        {
            // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
            // Deprecated in 2.100
            // Make an error in 2.110
            if (sd.storage_class & STC.scope_)
                deprecation(sd.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");
        }
        //printf("-StructDeclaration::semantic(this=%p, '%s', sizeok = %d)\n", sd, sd.toPrettyChars(), sd.sizeok);
    }

    //
    // Checks if two structs are compatible
    // Implements the rules according to C23 6.2.7
    //
    static bool isCCompatible(StructDeclaration a, StructDeclaration b)
    {
        // Get the name of a type, while avoiding exposing "__tagXXX" anonymous structs
        static const(char)* typeName(Type t)
        {
            if (TypeStruct ts = t.isTypeStruct())
            {
                if (ts.sym.ident.toString().startsWith("__tag"))
                    return ts.sym.isUnionDeclaration() ? "(anonymous union)".ptr: "(anonymous struct)".ptr;
            }
            return t.toChars();
        }

        void incompatError()
        {
            .error(a.loc, "%s `%s` already exists with an incompatible definition.",
                    a.kind, typeName(a.type));
            errorSupplemental(b.loc, "previously declared here");
        }


        // For recursive calls into unnamed structs (so Type.equals() doesn't work).
        static bool isCCompatibleUnnamedStruct(Type a, Type b)
        {
            TypeStruct ats = a.isTypeStruct();
            if (!ats) return false;
            TypeStruct bts = b.isTypeStruct();
            if (!bts) return false;
            // Hack, anonymous structs within a struct are given
            // an anonymous id starting with __tag.
            if (!ats.sym.ident.toString().startsWith("__tag"))
                return false;
            if (!bts.sym.ident.toString().startsWith("__tag"))
                return false;
            return isCCompatible(ats.sym, bts.sym);
        }

        if (a.fields.length != b.fields.length)
        {
            incompatError();
            errorSupplemental(a.loc, "`%s` has %zu field(s) while `%s` has %zu field(s)",
                    a.toPrettyChars(), a.fields.length, b.toPrettyChars(), b.fields.length);
            return false;
        }
        // both are structs or both are unions
        if ((a.isUnionDeclaration() is null) != (b.isUnionDeclaration() is null))
        {
            incompatError();
            errorSupplemental(a.loc, "`%s` is a %s while `%s` is a %s",
                    a.toPrettyChars(), a.kind, b.toPrettyChars(), b.kind);
            return false;
        }
        if (a.alignment != b.alignment)
        {
            incompatError();
            errorSupplemental(a.loc, "`%s` has different alignment or packing", a.toPrettyChars());
            if (a.alignment.isDefault() && ! b.alignment.isDefault())
            {
                errorSupplemental(a.loc, "`%s` alignment: default", a.toPrettyChars());
                errorSupplemental(b.loc, "`%s` alignment: %u",
                        b.toPrettyChars(), cast(uint)b.alignment.get());
            }
            else if (!a.alignment.isDefault() && b.alignment.isDefault())
            {
                errorSupplemental(a.loc, "`%s` alignment: %u",
                        a.toPrettyChars(), cast(uint)a.alignment.get());
                errorSupplemental(b.loc, "`%s` alignment: default",
                        b.toPrettyChars());
            }
            else if (a.alignment.get() != b.alignment.get())
            {
                errorSupplemental(a.loc, "`%s` alignment: %u",
                        a.toPrettyChars(), cast(uint)a.alignment.get());
                errorSupplemental(b.loc, "`%s` alignment: %u",
                        b.toPrettyChars(), cast(uint)b.alignment.get());
            }
            if (a.alignment.isPack() != b.alignment.isPack())
            {
                errorSupplemental(a.loc, "`%s` packed: %s",
                        a.toPrettyChars(), a.alignment.isPack()?"true".ptr:"false".ptr);
                errorSupplemental(b.loc, "`%s` packed: %s",
                        b.toPrettyChars(), b.alignment.isPack()?"true".ptr:"false".ptr);
            }
            return false;
        }
        foreach (size_t i, VarDeclaration a_field; a.fields[])
        {
            VarDeclaration b_field = b.fields[i];
            //
            // — there shall be a one-to-one correspondence between
            //   their members such that each pair of corresponding
            //   members are declared with compatible types;
            //
            if (!a_field.type.equals(b_field.type) && !isCCompatibleUnnamedStruct(a_field.type, b_field.type))
            {
                // Already errored, just bail
                incompatError();
                if (a_field.type.isTypeError()) return false;
                if (b_field.type.isTypeError()) return false;

                errorSupplemental(a_field.loc, "Field %zu differs in type", i);
                errorSupplemental(a_field.loc, "typeof(%s): %s",
                        a_field.toChars(), typeName(a_field.type));
                errorSupplemental(b_field.loc, "typeof(%s): %s",
                        b_field.toChars(), typeName(b_field.type));
                return false;
            }
            //
            // — if one member of the pair is declared with an
            //   alignment specifier, the second is declared with an
            //   equivalent alignment specifier;
            //
            if (a_field.alignment != b_field.alignment)
            {
                incompatError();
                errorSupplemental(a_field.loc, "Field %zu differs in alignment or packing", i);
                if (a_field.alignment.isDefault() && ! b_field.alignment.isDefault())
                {
                    errorSupplemental(a_field.loc, "`%s.%s` alignment: default",
                            a.toPrettyChars(),a_field.toChars());
                    errorSupplemental(b_field.loc, "`%s.%s` alignment: %u",
                            b.toPrettyChars(), b_field.toChars(), cast(uint)b_field.alignment.get());
                }
                else if (!a_field.alignment.isDefault() && b_field.alignment.isDefault())
                {
                    errorSupplemental(a_field.loc, "`%s.%s` alignment: %u",
                            a.toPrettyChars(), a_field.toChars(), cast(uint)a_field.alignment.get());
                    errorSupplemental(b_field.loc, "`%s.%s` alignment: default",
                            b.toPrettyChars(), b_field.toChars());
                }
                else if (a_field.alignment.get() != b_field.alignment.get())
                {
                    errorSupplemental(a_field.loc, "`%s.%s` alignment: %u",
                            a.toPrettyChars(), a_field.toChars(),
                            cast(uint)a_field.alignment.get());
                    errorSupplemental(b_field.loc, "`%s.%s` alignment: %u",
                            b.toPrettyChars(), b_field.toChars(),
                            cast(uint)b_field.alignment.get());
                }
                if (a_field.alignment.isPack() != b_field.alignment.isPack())
                {
                    errorSupplemental(a_field.loc, "`%s.%s` packed: %s",
                            a.toPrettyChars(), a_field.toChars(),
                            a_field.alignment.isPack()?"true".ptr:"false".ptr);
                    errorSupplemental(b_field.loc, "`%s.%s` packed: %s",
                            b.toPrettyChars(), b_field.toChars(),
                            b_field.alignment.isPack()?"true".ptr:"false".ptr);
                }
                return false;
            }
            //
            // - and, if one member of the pair is declared with a
            //   name, the second is declared with the same name.
            //
            if (a_field.ident.isAnonymous())
            {
                if (!b_field.ident.isAnonymous())
                {
                    incompatError();
                    errorSupplemental(a_field.loc, "Field %zu differs in name", i);
                    errorSupplemental(a_field.loc, "(anonymous)", a_field.ident.toChars());
                    errorSupplemental(b_field.loc, "%s", b_field.ident.toChars());
                    return false;
                }
            }
            else if (b_field.ident.isAnonymous())
            {
                incompatError();
                errorSupplemental(a_field.loc, "Field %zu differs in name", i);
                errorSupplemental(a_field.loc, "%s", a_field.ident.toChars());
                errorSupplemental(b_field.loc, "(anonymous)");
                return false;
            }
            else if (a_field.ident != b_field.ident)
            {
                incompatError();
                errorSupplemental(a_field.loc, "Field %zu differs in name", i);
                errorSupplemental(a_field.loc, "%s", a_field.ident.toChars());
                errorSupplemental(b_field.loc, "%s", b_field.ident.toChars());
                return false;
            }

            //
            // For two structures or unions, corresponding bitfields shall have the same widths.
            //
            BitFieldDeclaration bfa = a_field.isBitFieldDeclaration();
            BitFieldDeclaration bfb = b_field.isBitFieldDeclaration();
            if ((bfa is null) != (bfb is null))
            {
                incompatError();
                errorSupplemental(a_field.loc, "Field %zu differs in being a bitfield", i);
                if (bfa is null)
                {
                    errorSupplemental(a_field.loc, "`%s.%s` is not a bitfield",
                            a.toPrettyChars(), a_field.toChars());
                    errorSupplemental(b_field.loc, "`%s.%s` is a bitfield",
                            b.toPrettyChars(), b_field.toChars());
                }
                else if (bfb is null)
                {
                    errorSupplemental(a_field.loc, "`%s.%s` *is a bitfield",
                            a.toPrettyChars(), a_field.toChars());
                    errorSupplemental(b_field.loc, "`%s.%s` is not a bitfield",
                            b.toPrettyChars(), b_field.toChars());
                }
                return false;
            }
            if (bfa !is null && bfb !is null)
            {
                if (bfa.fieldWidth != bfb.fieldWidth)
                {
                    incompatError();
                    errorSupplemental(a_field.loc, "Field %zu differs in bitfield width", i);
                    errorSupplemental(a_field.loc, "`%s.%s`: %u",
                            a.toPrettyChars(), a_field.toChars(), bfa.fieldWidth);
                    errorSupplemental(b_field.loc, "`%s.%s`: %u",
                            b.toPrettyChars(), b_field.toChars(), bfb.fieldWidth);
                    return false;
                }
            }
        }
        return true;
    }

    void interfaceSemantic(ClassDeclaration cd)
    {
        cd.vtblInterfaces = new BaseClasses();
        cd.vtblInterfaces.reserve(cd.interfaces.length);
        foreach (b; cd.interfaces)
        {
            cd.vtblInterfaces.push(b);
            b.copyBaseInterfaces(cd.vtblInterfaces);
        }
    }

    override void visit(ClassDeclaration cldec)
    {
        //printf("ClassDeclaration.dsymbolSemantic(%s), type = %p, sizeok = %d, this = %p\n", cldec.toChars(), cldec.type, cldec.sizeok, this);
        //printf("\tparent = %p, '%s'\n", sc.parent, sc.parent ? sc.parent.toChars() : "");
        //printf("sc.stc = %x\n", sc.stc);

        //{ static int n;  if (++n == 20) *(char*)0=0; }

        if (cldec.semanticRun >= PASS.semanticdone)
            return;
        const errors = global.errors;

        //printf("+ClassDeclaration.dsymbolSemantic(%s), type = %p, sizeok = %d, this = %p\n", toChars(), type, sizeok, this);

        Scope* scx = null;
        if (cldec._scope)
        {
            sc = cldec._scope;
            scx = cldec._scope; // save so we don't make redundant copies
            cldec._scope = null;
        }

        if (!cldec.parent)
        {
            assert(sc.parent);
            cldec.parent = sc.parent;
        }

        if (cldec.errors)
            cldec.type = Type.terror;
        if (cldec.semanticRun == PASS.initial)
            cldec.type = cldec.type.addSTC(sc.stc | cldec.storage_class);
        cldec.type = cldec.type.typeSemantic(cldec.loc, sc);
        if (auto tc = cldec.type.isTypeClass())
            if (tc.sym != cldec)
            {
                auto ti = tc.sym.isInstantiated();
                if (ti && isError(ti))
                    tc.sym = cldec;
            }

        // Ungag errors when not speculative
        Ungag ungag = cldec.ungagSpeculative();

        if (cldec.semanticRun == PASS.initial)
        {
            cldec.visibility = sc.visibility;

            cldec.storage_class |= sc.stc;
            if (cldec.storage_class & STC.auto_)
                .error(cldec.loc, "%s `%s` storage class `auto` is invalid when declaring a class, did you mean to use `scope`?", cldec.kind, cldec.toPrettyChars);
            if (cldec.storage_class & STC.scope_)
                cldec.stack = true;
            if (cldec.storage_class & STC.abstract_)
                cldec.isabstract = ThreeState.yes;

            cldec.userAttribDecl = sc.userAttribDecl;

            if (sc.linkage == LINK.cpp)
                cldec.classKind = ClassKind.cpp;
            cldec.cppnamespace = sc.namespace;
            cldec.cppmangle = sc.cppmangle;
            if (sc.linkage == LINK.objc)
                objc.setObjc(cldec);
        }
        else if (cldec.symtab && !scx)
        {
            return;
        }
        cldec.rtInfoScope = sc;
        sc.setNoFree();

        cldec.semanticRun = PASS.semantic;
        checkGNUABITag(cldec, sc.linkage);
        checkMustUseReserved(cldec);

        if (cldec.baseok < Baseok.done)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=12078
             * https://issues.dlang.org/show_bug.cgi?id=12143
             * https://issues.dlang.org/show_bug.cgi?id=15733
             * While resolving base classes and interfaces, a base may refer
             * the member of this derived class. In that time, if all bases of
             * this class can  be determined, we can go forward the semantc process
             * beyond the Lancestorsdone. To do the recursive semantic analysis,
             * temporarily set and unset `_scope` around exp().
             */
            T resolveBase(T)(lazy T exp)
            {
                if (!scx)
                {
                    scx = sc.copy();
                    scx.setNoFree();
                }
                static if (!is(T == void))
                {
                    cldec._scope = scx;
                    auto r = exp();
                    cldec._scope = null;
                    return r;
                }
                else
                {
                    cldec._scope = scx;
                    exp();
                    cldec._scope = null;
                }
            }

            cldec.baseok = Baseok.start;

            // Expand any tuples in baseclasses[]
            for (size_t i = 0; i < cldec.baseclasses.length;)
            {
                auto b = (*cldec.baseclasses)[i];
                b.type = resolveBase(b.type.typeSemantic(cldec.loc, sc));

                Type tb = b.type.toBasetype();
                if (auto tup = tb.isTypeTuple())
                {
                    cldec.baseclasses.remove(i);
                    size_t dim = Parameter.dim(tup.arguments);
                    for (size_t j = 0; j < dim; j++)
                    {
                        Parameter arg = Parameter.getNth(tup.arguments, j);
                        b = new BaseClass(arg.type);
                        cldec.baseclasses.insert(i + j, b);
                    }
                }
                else
                    i++;
            }

            if (cldec.baseok >= Baseok.done)
            {
                //printf("%s already semantic analyzed, semanticRun = %d\n", toChars(), semanticRun);
                if (cldec.semanticRun >= PASS.semanticdone)
                    return;
                goto Lancestorsdone;
            }

            // See if there's a base class as first in baseclasses[]
            if (cldec.baseclasses.length)
            {
                BaseClass* b = (*cldec.baseclasses)[0];
                Type tb = b.type.toBasetype();
                TypeClass tc = tb.isTypeClass();
                if (!tc)
                {
                    if (b.type != Type.terror)
                        .error(cldec.loc, "%s `%s` base type must be `class` or `interface`, not `%s`", cldec.kind, cldec.toPrettyChars, b.type.toChars());
                    cldec.baseclasses.remove(0);
                    goto L7;
                }
                if (tc.sym.isDeprecated())
                {
                    if (!cldec.isDeprecated())
                    {
                        // Deriving from deprecated class makes this one deprecated too
                        cldec.setDeprecated();
                        tc.checkDeprecated(cldec.loc, sc);
                    }
                }
                if (tc.sym.isInterfaceDeclaration())
                    goto L7;

                for (ClassDeclaration cdb = tc.sym; cdb; cdb = cdb.baseClass)
                {
                    if (cdb == cldec)
                    {
                        .error(cldec.loc, "%s `%s` circular inheritance", cldec.kind, cldec.toPrettyChars);
                        cldec.baseclasses.remove(0);
                        goto L7;
                    }
                }

                /* https://issues.dlang.org/show_bug.cgi?id=11034
                 * Class inheritance hierarchy
                 * and instance size of each classes are orthogonal information.
                 * Therefore, even if tc.sym.sizeof == Sizeok.none,
                 * we need to set baseClass field for class covariance check.
                 */
                cldec.baseClass = tc.sym;
                b.sym = cldec.baseClass;

                if (tc.sym.baseok < Baseok.done)
                    resolveBase(tc.sym.dsymbolSemantic(null)); // Try to resolve forward reference
                if (tc.sym.baseok < Baseok.done)
                {
                    //printf("\ttry later, forward reference of base class %s\n", tc.sym.toChars());
                    if (tc.sym._scope)
                        addDeferredSemantic(tc.sym);
                    cldec.baseok = Baseok.none;
                }
            L7:
            }

            // Treat the remaining entries in baseclasses as interfaces
            // Check for errors, handle forward references
            int multiClassError = cldec.baseClass is null ? 0 : 1;

            BCLoop:
            for (size_t i = (cldec.baseClass ? 1 : 0); i < cldec.baseclasses.length;)
            {
                BaseClass* b = (*cldec.baseclasses)[i];
                Type tb = b.type.toBasetype();
                TypeClass tc = tb.isTypeClass();
                if (!tc || !tc.sym.isInterfaceDeclaration())
                {
                    // It's a class
                    if (tc)
                    {
                        if (multiClassError == 0)
                        {
                            .error(cldec.loc,"`%s`: base class must be specified first, " ~
                                  "before any interfaces.", cldec.toPrettyChars());
                            multiClassError += 1;
                        }
                        else if (multiClassError >= 1)
                        {
                                if(multiClassError == 1)
                                    .error(cldec.loc, "`%s`: multiple class inheritance is not supported." ~
                                          " Use multiple interface inheritance and/or composition.", cldec.toPrettyChars());
                                multiClassError += 1;

                                if (tc.sym.fields.length)
                                    errorSupplemental(cldec.loc,"`%s` has fields, consider making it a member of `%s`",
                                                      b.type.toChars(), cldec.type.toChars());
                                else
                                    errorSupplemental(cldec.loc,"`%s` has no fields, consider making it an `interface`",
                                                      b.type.toChars());
                        }
                    }
                    // It's something else: e.g. `int` in `class Foo : Bar, int { ... }`
                    else if (b.type != Type.terror)
                    {
                        error(cldec.loc,"`%s`: base type must be `interface`, not `%s`",
                              cldec.toPrettyChars(), b.type.toChars());
                    }
                    cldec.baseclasses.remove(i);
                    continue;
                }

                // Check for duplicate interfaces
                for (size_t j = (cldec.baseClass ? 1 : 0); j < i; j++)
                {
                    BaseClass* b2 = (*cldec.baseclasses)[j];
                    if (b2.sym == tc.sym)
                    {
                        .error(cldec.loc, "%s `%s` inherits from duplicate interface `%s`", cldec.kind, cldec.toPrettyChars, b2.sym.toChars());
                        cldec.baseclasses.remove(i);
                        continue BCLoop;
                    }
                }
                if (tc.sym.isDeprecated())
                {
                    if (!cldec.isDeprecated())
                    {
                        // Deriving from deprecated class makes this one deprecated too
                        cldec.setDeprecated();
                        tc.checkDeprecated(cldec.loc, sc);
                    }
                }

                b.sym = tc.sym;

                if (tc.sym.baseok < Baseok.done)
                    resolveBase(tc.sym.dsymbolSemantic(null)); // Try to resolve forward reference
                if (tc.sym.baseok < Baseok.done)
                {
                    //printf("\ttry later, forward reference of base %s\n", tc.sym.toChars());
                    if (tc.sym._scope)
                        addDeferredSemantic(tc.sym);
                    cldec.baseok = Baseok.none;
                }
                i++;
            }
            if (cldec.baseok == Baseok.none)
            {
                // Forward referencee of one or more bases, try again later
                //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, toChars());
                return deferDsymbolSemantic(sc, cldec, scx);
            }
            cldec.baseok = Baseok.done;

            if (cldec.classKind == ClassKind.objc || (cldec.baseClass && cldec.baseClass.classKind == ClassKind.objc))
                cldec.classKind = ClassKind.objc; // Objective-C classes do not inherit from Object

            // If no base class, and this is not an Object, use Object as base class
            if (!cldec.baseClass && cldec.ident != Id.Object && cldec.object && cldec.classKind == ClassKind.d)
            {
                void badObjectDotD()
                {
                    ObjectNotFound(cldec.loc, cldec.ident);
                }

                if (!cldec.object || cldec.object.errors)
                    badObjectDotD();

                Type t = cldec.object.type;
                t = t.typeSemantic(cldec.loc, sc).toBasetype();
                if (t.ty == Terror)
                    badObjectDotD();
                TypeClass tc = t.isTypeClass();
                assert(tc);

                auto b = new BaseClass(tc);
                cldec.baseclasses.shift(b);

                cldec.baseClass = tc.sym;
                assert(!cldec.baseClass.isInterfaceDeclaration());
                b.sym = cldec.baseClass;
            }
            if (cldec.baseClass)
            {
                if (cldec.baseClass.storage_class & STC.final_)
                    .error(cldec.loc, "%s `%s` cannot inherit from class `%s` because it is `final`", cldec.kind, cldec.toPrettyChars, cldec.baseClass.toChars());

                // Inherit properties from base class
                if (cldec.baseClass.isCOMclass())
                    cldec.com = true;
                if (cldec.baseClass.isCPPclass())
                    cldec.classKind = ClassKind.cpp;
                if (cldec.classKind != cldec.baseClass.classKind)
                    .error(cldec.loc, "%s `%s` with %s linkage cannot inherit from class `%s` with %s linkage", cldec.kind, cldec.toPrettyChars,
                        ClassKindToChars(cldec.classKind), cldec.baseClass.toChars(), ClassKindToChars(cldec.baseClass.classKind));

                if (cldec.baseClass.stack)
                    cldec.stack = true;
                cldec.enclosing = cldec.baseClass.enclosing;
                cldec.storage_class |= cldec.baseClass.storage_class & STC.TYPECTOR;
            }

            cldec.interfaces = cldec.baseclasses.tdata()[(cldec.baseClass ? 1 : 0) .. cldec.baseclasses.length];
            foreach (b; cldec.interfaces)
            {
                // If this is an interface, and it derives from a COM interface,
                // then this is a COM interface too.
                if (b.sym.isCOMinterface())
                    cldec.com = true;
                if (cldec.classKind == ClassKind.cpp && !b.sym.isCPPinterface())
                {
                    .error(cldec.loc, "C++ class `%s` cannot implement D interface `%s`",
                        cldec.toPrettyChars(), b.sym.toPrettyChars());
                }
            }
            interfaceSemantic(cldec);
        }
    Lancestorsdone:
        //printf("\tClassDeclaration.dsymbolSemantic(%s) baseok = %d\n", toChars(), baseok);

        if (!cldec.members) // if opaque declaration
        {
            cldec.semanticRun = PASS.semanticdone;
            return;
        }
        if (!cldec.symtab)
        {
            cldec.symtab = new DsymbolTable();

            /* https://issues.dlang.org/show_bug.cgi?id=12152
             * The semantic analysis of base classes should be finished
             * before the members semantic analysis of this class, in order to determine
             * vtbl in this class. However if a base class refers the member of this class,
             * it can be resolved as a normal forward reference.
             * Call addMember() and setScope() to make this class members visible from the base classes.
             */
            cldec.members.foreachDsymbol( s => s.addMember(sc, cldec) );

            auto sc2 = cldec.newScope(sc);

            /* Set scope so if there are forward references, we still might be able to
             * resolve individual members like enums.
             */
            cldec.members.foreachDsymbol( s => s.setScope(sc2) );

            sc2.pop();
        }

        for (size_t i = 0; i < cldec.baseclasses.length; i++)
        {
            BaseClass* b = (*cldec.baseclasses)[i];
            Type tb = b.type.toBasetype();
            TypeClass tc = tb.isTypeClass();
            if (tc.sym.semanticRun < PASS.semanticdone)
            {
                // Forward referencee of one or more bases, try again later
                if (tc.sym._scope)
                    addDeferredSemantic(tc.sym);
                //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, toChars());
                return deferDsymbolSemantic(sc, cldec, scx);
            }
        }

        if (cldec.baseok == Baseok.done)
        {
            cldec.baseok = Baseok.semanticdone;
            objc.setMetaclass(cldec, sc);

            // initialize vtbl
            if (cldec.baseClass)
            {
                if (cldec.classKind == ClassKind.cpp && cldec.baseClass.vtbl.length == 0)
                {
                    .error(cldec.loc, "%s `%s` C++ base class `%s` needs at least one virtual function", cldec.kind, cldec.toPrettyChars, cldec.baseClass.toChars());
                }

                // Copy vtbl[] from base class
                assert(cldec.vtbl.length == 0);
                cldec.vtbl.setDim(cldec.baseClass.vtbl.length);
                memcpy(cldec.vtbl.tdata(), cldec.baseClass.vtbl.tdata(), (void*).sizeof * cldec.vtbl.length);

                cldec.vthis = cldec.baseClass.vthis;
                cldec.vthis2 = cldec.baseClass.vthis2;
            }
            else
            {
                // No base class, so this is the root of the class hierarchy
                cldec.vtbl.setDim(0);
                if (cldec.vtblOffset())
                    cldec.vtbl.push(cldec); // leave room for classinfo as first member
            }

            /* If this is a nested class, add the hidden 'this'
             * member which is a pointer to the enclosing scope.
             */
            if (cldec.vthis) // if inheriting from nested class
            {
                // Use the base class's 'this' member
                if (cldec.storage_class & STC.static_)
                    .error(cldec.loc, "%s `%s` static class cannot inherit from nested class `%s`", cldec.kind, cldec.toPrettyChars, cldec.baseClass.toChars());
                if (cldec.toParentLocal() != cldec.baseClass.toParentLocal() &&
                    (!cldec.toParentLocal() ||
                     !cldec.baseClass.toParentLocal().getType() ||
                     !cldec.baseClass.toParentLocal().getType().isBaseOf(cldec.toParentLocal().getType(), null)))
                {
                    if (cldec.toParentLocal())
                    {
                        .error(cldec.loc, "%s `%s` is nested within `%s`, but super class `%s` is nested within `%s`", cldec.kind, cldec.toPrettyChars,
                            cldec.toParentLocal().toChars(),
                            cldec.baseClass.toChars(),
                            cldec.baseClass.toParentLocal().toChars());
                    }
                    else
                    {
                        .error(cldec.loc, "%s `%s` is not nested, but super class `%s` is nested within `%s`", cldec.kind, cldec.toPrettyChars,
                            cldec.baseClass.toChars(),
                            cldec.baseClass.toParentLocal().toChars());
                    }
                }
                if (cldec.vthis2)
                {
                    if (cldec.toParent2() != cldec.baseClass.toParent2() &&
                        (!cldec.toParent2() ||
                         !cldec.baseClass.toParent2().getType() ||
                         !cldec.baseClass.toParent2().getType().isBaseOf(cldec.toParent2().getType(), null)))
                    {
                        if (cldec.toParent2() && cldec.toParent2() != cldec.toParentLocal())
                        {
                            .error(cldec.loc, "%s `%s` needs the frame pointer of `%s`, but super class `%s` needs the frame pointer of `%s`", cldec.kind, cldec.toPrettyChars,
                                cldec.toParent2().toChars(),
                                cldec.baseClass.toChars(),
                                cldec.baseClass.toParent2().toChars());
                        }
                        else
                        {
                            .error(cldec.loc, "%s `%s` doesn't need a frame pointer, but super class `%s` needs the frame pointer of `%s`", cldec.kind, cldec.toPrettyChars,
                                cldec.baseClass.toChars(),
                                cldec.baseClass.toParent2().toChars());
                        }
                    }
                }
                else
                    cldec.makeNested2();
            }
            else
                cldec.makeNested();
        }

        auto sc2 = cldec.newScope(sc);

        cldec.members.foreachDsymbol( s => s.importAll(sc2) );

        // Note that members.length can grow due to tuple expansion during semantic()
        cldec.members.foreachDsymbol( s => s.dsymbolSemantic(sc2) );

        if (!cldec.determineFields())
        {
            assert(cldec.type == Type.terror);
            sc2.pop();
            return;
        }
        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types.
         */
        foreach (v; cldec.fields)
        {
            Type tb = v.type.baseElemOf();
            if (tb.ty != Tstruct)
                continue;
            auto sd = (cast(TypeStruct)tb).sym;
            if (sd.semanticRun >= PASS.semanticdone)
                continue;

            sc2.pop();

            //printf("\tdeferring %s\n", toChars());
            return deferDsymbolSemantic(sc, cldec, scx);
        }

        /* Look for special member functions.
         * They must be in this class, not in a base class.
         */
        // Can be in base class
        cldec.disableNew = cldec.search(Loc.initial, Id.classNew) !is null;

        // Look for the constructor
        cldec.ctor = cldec.searchCtor();

        if (!cldec.ctor && cldec.noDefaultCtor)
        {
            // A class object is always created by constructor, so this check is legitimate.
            foreach (v; cldec.fields)
            {
                if (v.storage_class & STC.nodefaultctor)
                    error(v.loc, "field `%s` must be initialized in constructor", v.toChars());
            }
        }

        // If this class has no constructor, but base class has a default
        // ctor, create a constructor:
        //    this() { }
        if (!cldec.ctor && cldec.baseClass && cldec.baseClass.ctor)
        {
            auto fd = resolveFuncCall(cldec.loc, sc2, cldec.baseClass.ctor, null, cldec.type, ArgumentList(), FuncResolveFlag.quiet);
            if (!fd) // try shared base ctor instead
                fd = resolveFuncCall(cldec.loc, sc2, cldec.baseClass.ctor, null, cldec.type.sharedOf, ArgumentList(), FuncResolveFlag.quiet);
            if (fd && !fd.errors)
            {
                //printf("Creating default this(){} for class %s\n", toChars());
                auto btf = fd.type.toTypeFunction();
                auto tf = new TypeFunction(ParameterList(), null, LINK.d, fd.storage_class);
                tf.mod = btf.mod;
                // Don't copy @safe, ... from the base class constructor and let it be inferred instead
                // This is required if other lowerings add code to the generated constructor which
                // is less strict (e.g. `preview=dtorfields` might introduce a call to a less qualified dtor)

                auto ctor = new CtorDeclaration(cldec.loc, Loc.initial, STC.none, tf);
                ctor.storage_class |= STC.inference | (fd.storage_class & STC.scope_);
                ctor.isGenerated = true;
                ctor.fbody = new CompoundStatement(Loc.initial, new Statements());

                cldec.members.push(ctor);
                ctor.addMember(sc, cldec);
                ctor.dsymbolSemantic(sc2);

                cldec.ctor = ctor;
                cldec.defaultCtor = ctor;
            }
            else
            {
                .error(cldec.loc, "%s `%s` cannot implicitly generate a default constructor when base class `%s` is missing a default constructor", cldec.kind, cldec.toPrettyChars,
                    cldec.baseClass.toPrettyChars());
            }
        }

        buildDtors(cldec, sc2);

        if (cldec.classKind == ClassKind.cpp && cldec.cppDtorVtblIndex != -1)
        {
            // now we've built the aggregate destructor, we'll make it virtual and assign it to the reserved vtable slot
            cldec.dtor.vtblIndex = cldec.cppDtorVtblIndex;
            cldec.vtbl[cldec.cppDtorVtblIndex] = cldec.dtor;

            if (target.cpp.twoDtorInVtable)
            {
                // TODO: create a C++ compatible deleting destructor (call out to `operator delete`)
                //       for the moment, we'll call the non-deleting destructor and leak
                cldec.vtbl[cldec.cppDtorVtblIndex + 1] = cldec.dtor;
            }
        }

        if (auto f = hasIdentityOpAssign(cldec, sc2))
        {
            if (!(f.storage_class & STC.disable))
                .error(f.loc, "%s `%s` identity assignment operator overload is illegal", cldec.kind, cldec.toPrettyChars);
        }

        cldec.inv = buildInv(cldec, sc2);

        cldec.semanticRun = PASS.semanticdone;
        //printf("-ClassDeclaration.dsymbolSemantic(%s), type = %p\n", toChars(), type);

        sc2.pop();

        /* isAbstract() is undecidable in some cases because of circular dependencies.
         * Now that semantic is finished, get a definitive result, and error if it is not the same.
         */
        if (cldec.isabstract != ThreeState.none)    // if evaluated it before completion
        {
            const isabstractsave = cldec.isabstract;
            cldec.isabstract = ThreeState.none;
            cldec.isAbstract();               // recalculate
            if (cldec.isabstract != isabstractsave)
            {
                .error(cldec.loc, "%s `%s` cannot infer `abstract` attribute due to circular dependencies", cldec.kind, cldec.toPrettyChars);
            }
        }

        if (cldec.type.ty == Tclass && (cast(TypeClass)cldec.type).sym != cldec)
        {
            // https://issues.dlang.org/show_bug.cgi?id=17492
            ClassDeclaration cd = (cast(TypeClass)cldec.type).sym;
            version (none)
            {
                printf("this = %p %s\n", cldec, cldec.toPrettyChars());
                printf("type = %d sym = %p, %s\n", cldec.type.ty, cd, cd.toPrettyChars());
            }
            .error(cldec.loc, "%s `%s` already exists at %s. Perhaps in another function with the same name?", cldec.kind, cldec.toPrettyChars, cd.loc.toChars());
        }

        if (global.errors != errors || (cldec.baseClass && cldec.baseClass.errors))
        {
            // The type is no good, but we should keep the
            // the type so that we have more accurate error messages
            // See: https://issues.dlang.org/show_bug.cgi?id=23552
            cldec.errors = true;
            if (cldec.deferred)
                cldec.deferred.errors = true;
        }

        // Verify fields of a synchronized class are not public
        if (cldec.storage_class & STC.synchronized_)
        {
            foreach (vd; cldec.fields)
            {
                if (!vd.isThisDeclaration() &&
                    vd.visible() >= Visibility(Visibility.Kind.public_))
                {
                    .error(vd.loc, "%s `%s` Field members of a `synchronized` class cannot be `%s`", vd.kind, vd.toPrettyChars,
                        visibilityToChars(vd.visible().kind));
                }
            }
        }

        if (cldec.deferred && !global.gag)
        {
            cldec.deferred.semantic2(sc);
            cldec.deferred.semantic3(sc);
        }
        //printf("-ClassDeclaration.dsymbolSemantic(%s), type = %p, sizeok = %d, this = %p\n", toChars(), type, sizeok, this);

        version (none)
        {
            // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
            // Deprecated in 2.100
            // Make an error in 2.110
            // Don't forget to remove code at https://github.com/dlang/dmd/blob/b2f8274ba76358607fc3297a1e9f361480f9bcf9/src/dmd/dsymbolsem.d#L1032-L1036
            if (cldec.storage_class & STC.scope_)
                deprecation(cldec.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");
        }
    }

    override void visit(InterfaceDeclaration idec)
    {
        /// Returns: `true` is this is an anonymous Objective-C metaclass
        static bool isAnonymousMetaclass(InterfaceDeclaration idec)
        {
            return idec.classKind == ClassKind.objc &&
                idec.objc.isMeta &&
                idec.isAnonymous;
        }

        //printf("InterfaceDeclaration.dsymbolSemantic(%s), type = %p\n", toChars(), type);
        if (idec.semanticRun >= PASS.semanticdone)
            return;
        const errors = global.errors;

        //printf("+InterfaceDeclaration.dsymbolSemantic(%s), type = %p\n", toChars(), type);

        Scope* scx = null;
        if (idec._scope)
        {
            sc = idec._scope;
            scx = idec._scope; // save so we don't make redundant copies
            idec._scope = null;
        }

        if (!idec.parent)
        {
            assert(sc.parent && sc.func);
            idec.parent = sc.parent;
        }
        // Objective-C metaclasses are anonymous
        assert(idec.parent && !idec.isAnonymous || isAnonymousMetaclass(idec));

        if (idec.errors)
            idec.type = Type.terror;
        idec.type = idec.type.typeSemantic(idec.loc, sc);
        if (idec.type.ty == Tclass && (cast(TypeClass)idec.type).sym != idec)
        {
            auto ti = (cast(TypeClass)idec.type).sym.isInstantiated();
            if (ti && isError(ti))
                (cast(TypeClass)idec.type).sym = idec;
        }

        // Ungag errors when not speculative
        Ungag ungag = idec.ungagSpeculative();

        if (idec.semanticRun == PASS.initial)
        {
            idec.visibility = sc.visibility;

            idec.storage_class |= sc.stc;
            idec.userAttribDecl = sc.userAttribDecl;
        }
        else if (idec.symtab)
        {
            if (idec.sizeok == Sizeok.done || !scx)
            {
                idec.semanticRun = PASS.semanticdone;
                return;
            }
        }
        idec.semanticRun = PASS.semantic;

        if (idec.baseok < Baseok.done)
        {
            T resolveBase(T)(lazy T exp)
            {
                if (!scx)
                {
                    scx = sc.copy();
                    scx.setNoFree();
                }
                static if (!is(T == void))
                {
                    idec._scope = scx;
                    auto r = exp();
                    idec._scope = null;
                    return r;
                }
                else
                {
                    idec._scope = scx;
                    exp();
                    idec._scope = null;
                }
            }

            idec.baseok = Baseok.start;

            // Expand any tuples in baseclasses[]
            for (size_t i = 0; i < idec.baseclasses.length;)
            {
                auto b = (*idec.baseclasses)[i];
                b.type = resolveBase(b.type.typeSemantic(idec.loc, sc));

                Type tb = b.type.toBasetype();
                if (auto tup = tb.isTypeTuple())
                {
                    idec.baseclasses.remove(i);
                    size_t dim = Parameter.dim(tup.arguments);
                    for (size_t j = 0; j < dim; j++)
                    {
                        Parameter arg = Parameter.getNth(tup.arguments, j);
                        b = new BaseClass(arg.type);
                        idec.baseclasses.insert(i + j, b);
                    }
                }
                else
                    i++;
            }

            if (idec.baseok >= Baseok.done)
            {
                //printf("%s already semantic analyzed, semanticRun = %d\n", toChars(), semanticRun);
                if (idec.semanticRun >= PASS.semanticdone)
                    return;
                goto Lancestorsdone;
            }

            if (!idec.baseclasses.length && sc.linkage == LINK.cpp)
                idec.classKind = ClassKind.cpp;
            idec.cppnamespace = sc.namespace;
            checkGNUABITag(idec, sc.linkage);
            checkMustUseReserved(idec);

            if (sc.linkage == LINK.objc)
                objc.setObjc(idec);

            // Check for errors, handle forward references
            BCLoop:
            for (size_t i = 0; i < idec.baseclasses.length;)
            {
                BaseClass* b = (*idec.baseclasses)[i];
                Type tb = b.type.toBasetype();
                TypeClass tc = (tb.ty == Tclass) ? cast(TypeClass)tb : null;
                if (!tc || !tc.sym.isInterfaceDeclaration())
                {
                    if (b.type != Type.terror)
                        .error(idec.loc, "%s `%s` base type must be `interface`, not `%s`", idec.kind, idec.toPrettyChars, b.type.toChars());
                    idec.baseclasses.remove(i);
                    continue;
                }

                // Check for duplicate interfaces
                for (size_t j = 0; j < i; j++)
                {
                    BaseClass* b2 = (*idec.baseclasses)[j];
                    if (b2.sym == tc.sym)
                    {
                        .error(idec.loc, "%s `%s` inherits from duplicate interface `%s`", idec.kind, idec.toPrettyChars, b2.sym.toChars());
                        idec.baseclasses.remove(i);
                        continue BCLoop;
                    }
                }
                if (tc.sym == idec || idec.isBaseOf2(tc.sym))
                {
                    .error(idec.loc, "%s `%s` circular inheritance of interface", idec.kind, idec.toPrettyChars);
                    idec.baseclasses.remove(i);
                    continue;
                }
                if (tc.sym.isDeprecated())
                {
                    if (!idec.isDeprecated())
                    {
                        // Deriving from deprecated interface makes this one deprecated too
                        idec.setDeprecated();
                        tc.checkDeprecated(idec.loc, sc);
                    }
                }

                b.sym = tc.sym;

                if (tc.sym.baseok < Baseok.done)
                    resolveBase(tc.sym.dsymbolSemantic(null)); // Try to resolve forward reference
                if (tc.sym.baseok < Baseok.done)
                {
                    //printf("\ttry later, forward reference of base %s\n", tc.sym.toChars());
                    if (tc.sym._scope)
                        addDeferredSemantic(tc.sym);
                    idec.baseok = Baseok.none;
                }
                i++;
            }
            if (idec.baseok == Baseok.none)
            {
                // Forward referencee of one or more bases, try again later
                return deferDsymbolSemantic(sc, idec, scx);
            }
            idec.baseok = Baseok.done;

            idec.interfaces = idec.baseclasses.tdata()[0 .. idec.baseclasses.length];
            foreach (b; idec.interfaces)
            {
                // If this is an interface, and it derives from a COM interface,
                // then this is a COM interface too.
                if (b.sym.isCOMinterface())
                    idec.com = true;
                if (b.sym.isCPPinterface())
                    idec.classKind = ClassKind.cpp;
            }

            interfaceSemantic(idec);
        }
    Lancestorsdone:

        if (!idec.members) // if opaque declaration
        {
            idec.semanticRun = PASS.semanticdone;
            return;
        }
        if (!idec.symtab)
            idec.symtab = new DsymbolTable();

        for (size_t i = 0; i < idec.baseclasses.length; i++)
        {
            BaseClass* b = (*idec.baseclasses)[i];
            Type tb = b.type.toBasetype();
            TypeClass tc = tb.isTypeClass();
            if (tc.sym.semanticRun < PASS.semanticdone)
            {
                // Forward referencee of one or more bases, try again later
                if (tc.sym._scope)
                    addDeferredSemantic(tc.sym);
                return deferDsymbolSemantic(sc, idec, scx);
            }
        }

        if (idec.baseok == Baseok.done)
        {
            idec.baseok = Baseok.semanticdone;
            objc.setMetaclass(idec, sc);

            // initialize vtbl
            if (idec.vtblOffset())
                idec.vtbl.push(idec); // leave room at vtbl[0] for classinfo

            // Cat together the vtbl[]'s from base interfaces
            foreach (i, b; idec.interfaces)
            {
                // Skip if b has already appeared
                for (size_t k = 0; k < i; k++)
                {
                    if (b == idec.interfaces[k])
                        goto Lcontinue;
                }

                // Copy vtbl[] from base class
                if (b.sym.vtblOffset())
                {
                    size_t d = b.sym.vtbl.length;
                    if (d > 1)
                    {
                        idec.vtbl.pushSlice(b.sym.vtbl[1 .. d]);
                    }
                }
                else
                {
                    idec.vtbl.append(&b.sym.vtbl);
                }

            Lcontinue:
            }
        }

        idec.members.foreachDsymbol( s => s.addMember(sc, idec) );

        auto sc2 = idec.newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        idec.members.foreachDsymbol( s => s.setScope(sc2) );

        idec.members.foreachDsymbol( s => s.importAll(sc2) );

        idec.members.foreachDsymbol( s => s.dsymbolSemantic(sc2) );

        idec.semanticRun = PASS.semanticdone;
        //printf("-InterfaceDeclaration.dsymbolSemantic(%s), type = %p\n", toChars(), type);

        sc2.pop();

        if (global.errors != errors)
        {
            // The type is no good.
            idec.type = Type.terror;
        }

        version (none)
        {
            if (type.ty == Tclass && (cast(TypeClass)idec.type).sym != idec)
            {
                printf("this = %p %s\n", idec, idec.toChars());
                printf("type = %d sym = %p\n", idec.type.ty, (cast(TypeClass)idec.type).sym);
            }
        }
        assert(idec.type.ty != Tclass || (cast(TypeClass)idec.type).sym == idec);

        version (none)
        {
            // @@@DEPRECATED_2.120@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
            // Deprecated in 2.087
            // Made an error in 2.100, but removal depends on `scope class` being removed too
            // Don't forget to remove code at https://github.com/dlang/dmd/blob/b2f8274ba76358607fc3297a1e9f361480f9bcf9/src/dmd/dsymbolsem.d#L1032-L1036
            if (idec.storage_class & STC.scope_)
                error(idec.loc, "`scope` as a type constraint is obsolete.  Use `scope` at the usage site.");
        }
    }
}

/*
Adds dsym as a member of scope sds.

Params:
    dsym = dsymbol to inserted
    sc = scope where the dsymbol is declared
    sds = ScopeDsymbol where dsym is inserted
*/
void addMember(Dsymbol dsym, Scope* sc, ScopeDsymbol sds)
{
    auto addMemberVisitor = new AddMemberVisitor(sc, sds);
    dsym.accept(addMemberVisitor);
}

private void attribAddMember(AttribDeclaration atb, Scope* sc, ScopeDsymbol sds)
{
    Dsymbols* d = atb.include(sc);
    if (d)
    {
        Scope* sc2 = atb.newScope(sc);
        d.foreachDsymbol( s => s.addMember(sc2, sds) );
        if (sc2 != sc)
            sc2.pop();
    }
}

private extern(C++) class AddMemberVisitor : Visitor
{
    alias visit = Visitor.visit;

    Scope* sc;
    ScopeDsymbol sds;

    this(Scope* sc, ScopeDsymbol sds) @safe
    {
        this.sc = sc;
        this.sds = sds;
    }

    override void visit(Dsymbol dsym)
    {
        //printf("Dsymbol::addMember('%s')\n", toChars());
        //printf("Dsymbol::addMember(this = %p, '%s' scopesym = '%s')\n", this, toChars(), sds.toChars());
        //printf("Dsymbol::addMember(this = %p, '%s' sds = %p, sds.symtab = %p)\n", this, toChars(), sds, sds.symtab);
        dsym.parent = sds;
        if (dsym.isAnonymous()) // no name, so can't add it to symbol table
            return;

        if (!sds.symtabInsert(dsym)) // if name is already defined
        {
            if (dsym.isAliasDeclaration() && !dsym._scope)
                dsym.setScope(sc);
            Dsymbol s2 = sds.symtabLookup(dsym, dsym.ident);
            /* https://issues.dlang.org/show_bug.cgi?id=17434
             *
             * If we are trying to add an import to the symbol table
             * that has already been introduced, then keep the one with
             * larger visibility. This is fine for imports because if
             * we have multiple imports of the same file, if a single one
             * is public then the symbol is reachable.
             */
            if (auto i1 = dsym.isImport())
            {
                if (auto i2 = s2.isImport())
                {
                    if (sc.explicitVisibility && sc.visibility > i2.visibility)
                        sds.symtab.update(dsym);
                }
            }

            // If using C tag/prototype/forward declaration rules
            if (sc && sc.inCfile && !dsym.isImport())
            // When merging master, replace with: if (sc && sc.inCfile && !dsym.isImport())
            {
                if (handleTagSymbols(*sc, dsym, s2, sds))
                    return;
                if (handleSymbolRedeclarations(*sc, dsym, s2, sds))
                    return;

                sds.multiplyDefined(Loc.initial, dsym, s2);  // ImportC doesn't allow overloading
                dsym.errors = true;
                return;
            }

            if (!s2.overloadInsert(dsym))
            {
                if (auto _td = s2.isTemplateDeclaration())
                    _td.computeOneMember();
                sds.multiplyDefined(Loc.initial, dsym, s2);
                dsym.errors = true;
            }
        }
        if (sds.isAggregateDeclaration() || sds.isEnumDeclaration())
        {
            if (dsym.ident == Id.__sizeof ||
                !(sc && sc.inCfile) && (dsym.ident == Id.__xalignof || dsym.ident == Id._mangleof))
            {
                .error(dsym.loc, "%s `%s` `.%s` property cannot be redefined", dsym.kind, dsym.toPrettyChars, dsym.ident.toChars());
                dsym.errors = true;
            }
        }
    }


    override void visit(StaticAssert _)
    {
        // we didn't add anything
    }

    /*****************************
     * Add import to sd's symbol table.
     */
    override void visit(Import imp)
    {
        //printf("Import.addMember(this=%s, sds=%s, sc=%p)\n", imp.toChars(), sds.toChars(), sc);
        if (imp.names.length == 0)
            return visit(cast(Dsymbol)imp);
        if (imp.aliasId)
            visit(cast(Dsymbol)imp);

        /* Instead of adding the import to sds's symbol table,
         * add each of the alias=name pairs
         */
        for (size_t i = 0; i < imp.names.length; i++)
        {
            Identifier name = imp.names[i];
            Identifier _alias = imp.aliases[i];
            if (!_alias)
                _alias = name;
            auto tname = new TypeIdentifier(imp.loc, name);
            auto ad = new AliasDeclaration(imp.loc, _alias, tname);
            ad._import = imp;
            addMember(ad, sc, sds);
            imp.aliasdecls.push(ad);
        }
    }

    override void visit(AttribDeclaration atb)
    {
       attribAddMember(atb, sc, sds);
    }

    override void visit(StorageClassDeclaration stcd)
    {
        Dsymbols* d = stcd.include(sc);
        if (d)
        {
            Scope* sc2 = stcd.newScope(sc);

            d.foreachDsymbol( (s)
            {
                //printf("\taddMember %s to %s\n", s.toChars(), sds.toChars());
                // STC.local needs to be attached before the member is added to the scope (because it influences the parent symbol)
                if (auto decl = s.isDeclaration())
                {
                    decl.storage_class |= stcd.stc & STC.local;
                    if (auto sdecl = s.isStorageClassDeclaration()) // TODO: why is this not enough to deal with the nested case?
                    {
                        sdecl.stc |= stcd.stc & STC.local;
                    }
                }
                s.addMember(sc2, sds);
            });

            if (sc2 != sc)
                sc2.pop();
        }
    }

    override void visit(VisibilityDeclaration visd)
    {
        if (visd.pkg_identifiers)
        {
            Dsymbol tmp;
            Package.resolve(visd.pkg_identifiers, &tmp, null);
            visd.visibility.pkg = tmp ? tmp.isPackage() : null;
            visd.pkg_identifiers = null;
        }
        if (visd.visibility.kind == Visibility.Kind.package_ && visd.visibility.pkg && sc._module)
        {
            Module m = sc._module;

            // https://issues.dlang.org/show_bug.cgi?id=17441
            // While isAncestorPackageOf does an equality check, the fix for the issue adds a check to see if
            // each package's .isModule() properites are equal.
            //
            // Properties generated from `package(foo)` i.e. visibility.pkg have .isModule() == null.
            // This breaks package declarations of the package in question if they are declared in
            // the same package.d file, which _do_ have a module associated with them, and hence a non-null
            // isModule()
            if (!m.isPackage() || !visd.visibility.pkg.ident.equals(m.isPackage().ident))
            {
                Package pkg = m.parent ? m.parent.isPackage() : null;
                if (!pkg || !visd.visibility.pkg.isAncestorPackageOf(pkg))
                    .error(visd.loc, "%s `%s` does not bind to one of ancestor packages of module `%s`", visd.kind(), visd.toPrettyChars(false), m.toPrettyChars(true));
            }
        }
        attribAddMember(visd, sc, sds);
    }

    override void visit(StaticIfDeclaration sid)
    {
        //printf("StaticIfDeclaration::addMember() '%s'\n", sid.toChars());
        /* This is deferred until the condition evaluated later (by the include() call),
         * so that expressions in the condition can refer to declarations
         * in the same scope, such as:
         *
         * template Foo(int i)
         * {
         *     const int j = i + 1;
         *     static if (j == 3)
         *         const int k;
         * }
         */
        sid.scopesym = sds;
    }


    override void visit(StaticForeachDeclaration sfd)
    {
        // used only for caching the enclosing symbol
        sfd.scopesym = sds;
    }

    /***************************************
     * Lazily initializes the scope to forward to.
     */
    override void visit(ForwardingAttribDeclaration fad)
    {
        fad.sym.parent = sds;
        sds = fad.sym;
        attribAddMember(fad, sc, fad.sym);
    }

    override void visit(MixinDeclaration md)
    {
        //printf("MixinDeclaration::addMember(sc = %p, sds = %p, memnum = %d)\n", sc, sds, md.memnum);
        md.scopesym = sds;
    }

    override void visit(DebugSymbol ds)
    {
        //printf("DebugSymbol::addMember('%s') %s\n", sds.toChars(), ds.toChars());
        Module m = sds.isModule();
        // Do not add the member to the symbol table,
        // just make sure subsequent debug declarations work.
        if (!m)
        {
            .error(ds.loc, "%s `%s` declaration must be at module level", ds.kind, ds.toPrettyChars);
            ds.errors = true;
        }
        else
        {
            if (m.debugidsNot && findCondition(*m.debugidsNot, ds.ident))
            {
                .error(ds.loc, "%s `%s` defined after use", ds.kind, ds.toPrettyChars);
                ds.errors = true;
            }
            if (!m.debugids)
                m.debugids = new Identifiers();
            m.debugids.push(ds.ident);
        }
    }

    override void visit(VersionSymbol vs)
    {
        //printf("VersionSymbol::addMember('%s') %s\n", sds.toChars(), vs.toChars());
        Module m = sds.isModule();
        // Do not add the member to the symbol table,
        // just make sure subsequent debug declarations work.
        VersionCondition.checkReserved(vs.loc, vs.ident.toString());
        if (!m)
        {
            .error(vs.loc, "%s `%s` declaration must be at module level", vs.kind, vs.toPrettyChars);
            vs.errors = true;
        }
        else
        {
            if (m.versionidsNot && findCondition(*m.versionidsNot, vs.ident))
            {
                .error(vs.loc, "%s `%s` defined after use", vs.kind, vs.toPrettyChars);
                vs.errors = true;
            }
            if (!m.versionids)
                m.versionids = new Identifiers();
            m.versionids.push(vs.ident);
        }

    }

    override void visit(Nspace ns)
    {
        visit(cast(Dsymbol)ns);

        if (ns.members)
        {
            if (!ns.symtab)
                ns.symtab = new DsymbolTable();
            // The namespace becomes 'imported' into the enclosing scope
            for (Scope* sce = sc; 1; sce = sce.enclosing)
            {
                ScopeDsymbol sds2 = sce.scopesym;
                if (sds2)
                {
                    sds2.importScope(ns, Visibility(Visibility.Kind.public_));
                    break;
                }
            }
            assert(sc);
            sc = sc.push(ns);
            sc.linkage = LINK.cpp; // namespaces default to C++ linkage
            sc.parent = ns;
            ns.members.foreachDsymbol(s => s.addMember(sc, ns));
            sc.pop();
        }
    }

    override void visit(EnumDeclaration ed)
    {
        version (none)
        {
            printf("EnumDeclaration::addMember() %s\n", ed.toChars());
            for (size_t i = 0; i < ed.members.length; i++)
            {
                EnumMember em = (*ed.members)[i].isEnumMember();
                printf("    member %s\n", em.toChars());
            }
        }
        if (!ed.isAnonymous())
        {
            visit(cast(Dsymbol)ed);
        }

        addEnumMembersToSymtab(ed, sc, sds);
    }
}

/*******************************************
 * Add members of EnumDeclaration to the symbol table(s).
 * Params:
 *      ed = EnumDeclaration
 *      sc = context of `ed`
 *      sds = symbol table that `ed` resides in
 */
void addEnumMembersToSymtab(EnumDeclaration ed, Scope* sc, ScopeDsymbol sds)
{
    const bool isCEnum = sc.inCfile; // it's an ImportC enum
    //printf("addEnumMembersToSymtab(ed: %s added: %d Cfile: %d)\n", ed.toChars(), ed.added, isCEnum);
    if (ed.added)
        return;
    ed.added = true;

    if (!ed.members)
        return;

    const bool isAnon = ed.isAnonymous();

    if ((isCEnum || isAnon) && !sds.symtab)
        sds.symtab = new DsymbolTable();

    if ((isCEnum || !isAnon) && !ed.symtab)
        ed.symtab = new DsymbolTable();

    ed.members.foreachDsymbol( (s)
    {
        if (EnumMember em = s.isEnumMember())
        {
            //printf("adding EnumMember %s to %s %d\n", em.toChars(), ed.toChars(), isCEnum);
            em.ed = ed;
            if (isCEnum)
            {
                /* C doesn't add the enum member to the symbol table of the enum tag, it adds
                 * it to the symbol table that the tag is in. This is in contrast to D, where enum
                 * members become members of the enum tag. To accommodate this, we add
                 * the enum members to both symbol tables.
                 */
                em.addMember(sc, ed);   // add em to ed's symbol table
                if (em.errors)
                    return;
                em.addMember(sc, sds);  // add em to symbol table that ed is in
                em.parent = ed; // restore it after previous addMember() changed it
            }
            else
            {
                em.addMember(sc, isAnon ? sds : ed);
            }
        }
    });
}

private OverloadSet mergeOverloadSet(ScopeDsymbol sds, Identifier ident, OverloadSet os, Dsymbol s)
{
    if (!os)
    {
        os = new OverloadSet(ident);
        os.parent = sds;
    }
    if (OverloadSet os2 = s.isOverloadSet())
    {
        // Merge the cross-module overload set 'os2' into 'os'
        if (os.a.length == 0)
        {
            os.a.setDim(os2.a.length);
            memcpy(os.a.tdata(), os2.a.tdata(), (os.a[0]).sizeof * os2.a.length);
        }
        else
        {
            for (size_t i = 0; i < os2.a.length; i++)
            {
                os = mergeOverloadSet(sds, ident, os, os2.a[i]);
            }
        }
        return os;
    }

    assert(s.isOverloadable());
    /* Don't add to os[] if s is alias of previous sym
     */
    for (size_t j = 0; j < os.a.length; j++)
    {
        Dsymbol s2 = os.a[j];
        if (s.toAlias() == s2.toAlias())
        {
            if (s2.isDeprecated() || (s2.visible() < s.visible() && s.visible().kind != Visibility.Kind.none))
            {
                os.a[j] = s;
            }
            return os;
        }
    }
    os.push(s);
    return os;
}

/***
 * Returns true if any of the symbols `p1` or `p2` resides in the enclosing
 * instantiation scope of `this`.
 */
bool followInstantiationContext(Dsymbol d,Dsymbol p1, Dsymbol p2 = null)
{
    static bool has2This(Dsymbol s)
    {
        if (auto f = s.isFuncDeclaration())
            return f.hasDualContext;
        if (auto ad = s.isAggregateDeclaration())
            return ad.vthis2 !is null;
        return false;
    }

    if (!has2This(d))
        return false;

    assert(p1);
    auto outer = d.toParent();
    while (outer)
    {
        auto ti = outer.isTemplateInstance();
        if (!ti)
            return false;
        foreach (oarg; *ti.tiargs)
        {
            auto sa = getDsymbol(oarg);
            if (!sa)
                continue;
            sa = sa.toAlias().toParent2();
            if (!sa)
                continue;
            if (sa == p1)
                return true;
            if (p2 && sa == p2)
                return true;
        }
        outer = ti.tempdecl.toParent();
    }
    return false;
}

/**
 * Returns the declaration scope scope of `this` unless any of the symbols
 * `p1` or `p2` resides in its enclosing instantiation scope then the
 * latter is returned.
 */
Dsymbol toParentP(Dsymbol d, Dsymbol p1, Dsymbol p2 = null)
{
    return followInstantiationContext(d, p1, p2) ? d.toParent2() : d.toParentLocal();
}

// function used to perform semantic on AliasDeclaration
void aliasSemantic(AliasDeclaration ds, Scope* sc)
{
    //printf("AliasDeclaration::semantic() %s %p\n", ds.toChars(), ds.aliassym);

    // as DsymbolSemanticVisitor::visit(AliasDeclaration), in case we're called first.
    // see https://issues.dlang.org/show_bug.cgi?id=21001
    ds.storage_class |= sc.stc & STC.deprecated_;
    ds.visibility = sc.visibility;
    ds.userAttribDecl = sc.userAttribDecl;

    void normalRet()
    {
        ds.inuse = 0;
        ds.semanticRun = PASS.semanticdone;

        if (auto sx = ds.overnext)
        {
            ds.overnext = null;
            if (!ds.overloadInsert(sx))
                ScopeDsymbol.multiplyDefined(Loc.initial, sx, ds);
        }
    }

    void errorRet()
    {
        ds.aliassym = null;
        ds.type = Type.terror;
        ds.inuse = 0;
        normalRet();
    }

    // preserve the original type
    if (!ds.originalType && ds.type)
        ds.originalType = ds.type.syntaxCopy();

    if (ds.aliassym)
    {
        auto fd = ds.aliassym.isFuncLiteralDeclaration();
        auto td = ds.aliassym.isTemplateDeclaration();
        if (fd || td && td.literal)
        {
            if (fd && fd.semanticRun >= PASS.semanticdone)
                return normalRet();

            Expression e = new FuncExp(ds.loc, ds.aliassym);
            e = e.expressionSemantic(sc);
            if (auto fe = e.isFuncExp())
            {
                ds.aliassym = fe.td ? cast(Dsymbol)fe.td : fe.fd;
                return normalRet();
            }
            else
                return errorRet();
        }

        if (ds.aliassym.isTemplateInstance())
            ds.aliassym.dsymbolSemantic(sc);
        return normalRet();
    }
    ds.inuse = 1;

    // Given:
    //  alias foo.bar.abc def;
    // it is not knowable from the syntax whether `def` is an alias
    // for type `foo.bar.abc` or an alias for symbol `foo.bar.abc`. It is up to the semantic()
    // pass to distinguish.
    // If it is a type, then `.type` is set and getType() will return that
    // type. If it is a symbol, then `.aliassym` is set and type is `null` -
    // toAlias() will return `.aliassym`

    const errors = global.errors;
    Type oldtype = ds.type;

    // Ungag errors when not instantiated DeclDefs scope alias
    auto ungag = Ungag(global.gag);
    //printf("%s parent = %s, gag = %d, instantiated = %d\n", ds.toChars(), ds.parent.toChars(), global.gag, ds.isInstantiated() !is null);
    if (ds.parent && global.gag && !ds.isInstantiated() && !ds.toParent2().isFuncDeclaration() && (sc.minst || sc.tinst))
    {
        //printf("%s type = %s\n", ds.toPrettyChars(), ds.type.toChars());
        global.gag = 0;
    }

    // https://issues.dlang.org/show_bug.cgi?id=18480
    // Detect `alias sym = sym;` to prevent creating loops in overload overnext lists.
    if (auto tident = ds.type.isTypeIdentifier())
    {
        if (sc.hasEdition(Edition.v2024) && tident.idents.length)
        {
            alias mt = tident;
            Dsymbol pscopesym;
            Dsymbol s = sc.search(ds.loc, mt.ident, pscopesym);
            // detect `alias a = var1.member_var;` which confusingly resolves to
            // `typeof(var1).member_var`, which can be valid inside the aggregate type
            if (s && s.isVarDeclaration() &&
                mt.ident != Id.This && mt.ident != Id._super)
            {
                s = tident.toDsymbol(sc);
                // don't error for `var1.static_symbol`
                if (s && s.needThis())
                {
                    error(ds.loc, "cannot alias %s member `%s` of variable `%s`",
                        s.kind(), s.toChars(), mt.ident.toChars());
                    errorSupplemental(ds.loc, "Use `typeof(%s)` instead to preserve behaviour",
                        mt.ident.toChars());
                }
            }
        }
        // Selective imports are allowed to alias to the same name `import mod : sym=sym`.
        if (!ds._import)
        {
            if (tident.ident is ds.ident && !tident.idents.length)
            {
                error(ds.loc, "`alias %s = %s;` cannot alias itself, use a qualified name to create an overload set",
                    ds.ident.toChars(), tident.ident.toChars());
                ds.type = Type.terror;
            }
        }
    }
    /* This section is needed because Type.resolve() will:
     *   const x = 3;
     *   alias y = x;
     * try to convert identifier x to 3.
     */
    auto s = ds.type.toDsymbol(sc);
    if (errors != global.errors)
        return errorRet();
    if (s == ds)
    {
        .error(ds.loc, "%s `%s` cannot resolve", ds.kind, ds.toPrettyChars);
        return errorRet();
    }
    if (!s || !s.isEnumMember())
    {
        Type t;
        Expression e;
        Scope* sc2 = sc;
        if (ds.storage_class & (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.disable))
        {
            // For 'ref' to be attached to function types, and picked
            // up by Type.resolve(), it has to go into sc.
            sc2 = sc.push();
            sc2.stc |= ds.storage_class & (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.shared_ | STC.disable);
        }
        ds.type = ds.type.addSTC(ds.storage_class);
        ds.type.resolve(ds.loc, sc2, e, t, s);
        if (sc2 != sc)
            sc2.pop();

        if (e)  // Try to convert Expression to Dsymbol
        {
            // TupleExp is naturally converted to a TupleDeclaration
            if (auto te = e.isTupleExp())
                s = new TupleDeclaration(te.loc, ds.ident, cast(Objects*)te.exps);
            else
            {
                s = getDsymbol(e);
                if (!s)
                {
                    if (e.op != EXP.error)
                        .error(ds.loc, "%s `%s` cannot alias an expression `%s`", ds.kind, ds.toPrettyChars, e.toChars());
                    return errorRet();
                }
            }
        }
        ds.type = t;
    }
    if (s == ds)
    {
        assert(global.errors);
        return errorRet();
    }
    if (s) // it's a symbolic alias
    {
        //printf("alias %s resolved to %s %s\n", ds.toChars(), s.kind(), s.toChars());
        ds.type = null;
        ds.aliassym = s;
    }
    else    // it's a type alias
    {
        //printf("alias %s resolved to type %s\n", ds.toChars(), ds.type.toChars());
        ds.type = ds.type.typeSemantic(ds.loc, sc);
        ds.aliassym = null;
    }

    if (global.gag && errors != global.errors)
        return errorRet();

    normalRet();
}

/********************
 * Perform semantic on AliasAssignment.
 * Has a lot of similarities to aliasSemantic(). Perhaps they should share code.
 */
private void aliasAssignSemantic(AliasAssign ds, Scope* sc)
{
    //printf("AliasAssign::semantic() %p,  %s\n", ds, ds.ident.toChars());

    void errorRet()
    {
        ds.errors = true;
        ds.type = Type.terror;
        ds.semanticRun = PASS.semanticdone;
        return;
    }

    /* Find the AliasDeclaration corresponding to ds.
     * Returns: AliasDeclaration if found, null if error
     */
    AliasDeclaration findAliasDeclaration(AliasAssign ds, Scope* sc)
    {
        Dsymbol scopesym;
        Dsymbol as = sc.search(ds.loc, ds.ident, scopesym);
        if (!as)
        {
            .error(ds.loc, "%s `%s` undefined identifier `%s`", ds.kind, ds.toPrettyChars, ds.ident.toChars());
            return null;
        }
        if (as.errors)
            return null;

        auto ad = as.isAliasDeclaration();
        if (!ad)
        {
            .error(ds.loc, "%s `%s` identifier `%s` must be an alias declaration", ds.kind, ds.toPrettyChars, as.toChars());
            return null;
        }

        if (ad.overnext)
        {
            error(ds.loc, "%s `%s` cannot reassign overloaded alias", ds.kind, ds.toPrettyChars);
            return null;
        }

        // Check constraints on the parent
        auto adParent = ad.toParent();
        if (adParent != ds.toParent())
        {
            if (!adParent)
                adParent = ds.toParent();
            .error(ds.loc, "`%s` must have same parent `%s` as alias `%s`", ds.ident.toChars(), adParent.toChars(), ad.toChars());
            return null;
        }
        if (!adParent.isTemplateInstance())
        {
            .error(ds.loc, "%s `%s` must be a member of a template", ds.kind, ds.toPrettyChars);
            return null;
        }

        return ad;
    }

    auto aliassym = findAliasDeclaration(ds, sc);
    if (!aliassym)
        return errorRet();

    if (aliassym.wasRead)
    {
        if (!aliassym.errors)
            error(ds.loc, "%s was read, so cannot reassign", aliassym.toChars());
        aliassym.errors = true;
        return errorRet();
    }

    aliassym.ignoreRead = true; // temporarilly allow reads of aliassym

    const storage_class = sc.stc & (STC.deprecated_ | STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.shared_ | STC.disable);

    if (ds.aliassym)
    {
        auto fd = ds.aliassym.isFuncLiteralDeclaration();
        auto td = ds.aliassym.isTemplateDeclaration();
        if (fd && fd.semanticRun >= PASS.semanticdone)
        {
        }
        else if (fd || td && td.literal)
        {

            Expression e = new FuncExp(ds.loc, ds.aliassym);
            e = e.expressionSemantic(sc);
            auto fe = e.isFuncExp();
            if (!fe)
                return errorRet();
            ds.aliassym = fe.td ? cast(Dsymbol)fe.td : fe.fd;
        }
        else if (ds.aliassym.isTemplateInstance())
            ds.aliassym.dsymbolSemantic(sc);

        aliassym.type = null;
        aliassym.aliassym = ds.aliassym;
        return;
    }

    /* Given:
     *    abc = def;
     * it is not knownable from the syntax whether `def` is a type or a symbol.
     * It appears here as `ds.type`. Do semantic analysis on `def` to disambiguate.
     */

    const errors = global.errors;
    Dsymbol s;

    // Try AliasSeq optimization
    if (auto ti = ds.type.isTypeInstance())
    {
        import dmd.templatesem : findTempDecl;
        if (!findTempDecl(ti.tempinst, sc, null))
            return errorRet();
        if (auto tempinst = isAliasSeq(sc, ti))
        {
            s = aliasAssignInPlace(sc, tempinst, aliassym);
            if (!s)
                return errorRet();
            goto Lsymdone;
        }
    }

    /* This section is needed because Type.resolve() will:
     *   const x = 3;
     *   alias y = x;
     * try to convert identifier x to 3.
     */
    s = ds.type.toDsymbol(sc);
    if (errors != global.errors)
        return errorRet();
    if (s == aliassym)
    {
        .error(ds.loc, "%s `%s` cannot resolve", ds.kind, ds.toPrettyChars);
        return errorRet();
    }

    if (!s || !s.isEnumMember())
    {
        Type t;
        Expression e;
        Scope* sc2 = sc;
        if (storage_class & (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.shared_ | STC.disable))
        {
            // For 'ref' to be attached to function types, and picked
            // up by Type.resolve(), it has to go into sc.
            sc2 = sc.push();
            sc2.stc |= storage_class & (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.shared_ | STC.disable);
        }
        ds.type = ds.type.addSTC(storage_class);
        ds.type.resolve(ds.loc, sc2, e, t, s);
        if (sc2 != sc)
            sc2.pop();

        if (e)  // Try to convert Expression to Dsymbol
        {
            // TupleExp is naturally converted to a TupleDeclaration
            if (auto te = e.isTupleExp())
                s = new TupleDeclaration(te.loc, ds.ident, cast(Objects*)te.exps);
            else
            {
                s = getDsymbol(e);
                if (!s)
                {
                    if (e.op != EXP.error)
                        .error(ds.loc, "%s `%s` cannot alias an expression `%s`", ds.kind, ds.toPrettyChars, e.toChars());
                    return errorRet();
                }
            }
        }
        ds.type = t;
    }
    if (s == aliassym)
    {
        assert(global.errors);
        return errorRet();
    }

    if (s) // it's a symbolic alias
    {
    Lsymdone:
        //printf("alias %s resolved to %s %s\n", toChars(), s.kind(), s.toChars());
        aliassym.type = null;
        aliassym.aliassym = s;
        aliassym.storage_class |= sc.stc & STC.deprecated_;
        aliassym.visibility = sc.visibility;
        aliassym.userAttribDecl = sc.userAttribDecl;
    }
    else    // it's a type alias
    {
        //printf("alias %s resolved to type %s\n", toChars(), type.toChars());
        aliassym.type = ds.type.typeSemantic(ds.loc, sc);
        aliassym.aliassym = null;
    }

    aliassym.ignoreRead = false;

    if (aliassym.type && aliassym.type.ty == Terror ||
        global.gag && errors != global.errors)
    {
        aliassym.type = Type.terror;
        aliassym.aliassym = null;
        return errorRet();
    }

    ds.semanticRun = PASS.semanticdone;
}

/***************************************
 * Expands template instance arguments inside 'alias assign' target declaration (aliassym),
 * instead of inside 'tempinst.tiargs' every time.
 * Params:
 *      tempinst = AliasSeq instance
 *      aliassym = the AliasDeclaration corresponding to AliasAssign
 * Returns:
 *       null.
 */
private TupleDeclaration aliasAssignInPlace(Scope* sc, TemplateInstance tempinst,
                                            AliasDeclaration aliassym)
{
    // Mark instance with semantic done, not needed but just in case.
    tempinst.inst = tempinst;
    tempinst.semanticRun = PASS.semanticdone;
    TupleDeclaration td;
    if (aliassym.type)
    {
        // Convert TypeTuple to TupleDeclaration to avoid back and forth allocations
        // in the assignment process
        if (auto tt = aliassym.type.isTypeTuple())
        {
            auto objs = new Objects(tt.arguments.length);
            foreach (i, p; *tt.arguments)
                (*objs)[i] = p.type;
            td = new TupleDeclaration(tempinst.loc, aliassym.ident, objs);
            td.storage_class |= STC.templateparameter;
            td.building = true;
            aliassym.type = null;
        }
        else if (aliassym.type.isTypeError())
            return null;

    }
    else if (auto otd = aliassym.aliassym.isTupleDeclaration())
    {
        if (otd.building)
            td = otd;
        else
        {
            td = new TupleDeclaration(tempinst.loc, aliassym.ident, otd.objects.copy());
            td.storage_class |= STC.templateparameter;
            td.building = true;
        }
    }
    // If starting from single element in aliassym (td == null) we need to build the tuple
    // after semanticTiargs to keep same semantics (for example a FuncLiteraldeclaration
    // template argument is converted to FuncExp)
    if (td)
        aliassym.aliassym = td;
    aliassym.semanticRun = PASS.semanticdone;
    if (!TemplateInstance_semanticTiargs(tempinst.loc, sc, tempinst.tiargs, 0, td))
    {
        tempinst.errors = true;
        return null;
    }
    // The alias will stop tuple 'building' mode when used (in AliasDeclaration.toAlias(),
    // then TupleDeclaration.getType() will work again)
    aliassym.semanticRun = PASS.initial;
    if (!td)
    {
        td = new TupleDeclaration(tempinst.loc, aliassym.ident, tempinst.tiargs);
        td.storage_class |= STC.templateparameter;
        td.building = true;
        return td;
    }

    auto tiargs = tempinst.tiargs;
    size_t oldlen = td.objects.length;
    size_t origstart;
    size_t insertidx;
    size_t insertlen;
    foreach (i, o; *tiargs)
    {
        if (o !is td)
        {
            ++insertlen;
            continue;
        }
        // tuple contains itself (tuple = AliasSeq!(..., tuple, ...))
        if (insertlen) // insert any left element before
        {
            td.objects.insert(insertidx, (*tiargs)[i - insertlen .. i]);
            if (insertidx == 0) // reset original tuple start point
                origstart = insertlen;
            insertlen = 0;
        }
        if (insertidx) // insert tuple if found more than one time
        {
            td.objects.reserve(oldlen); // reserve first to assert a valid slice
            td.objects.pushSlice((*td.objects)[origstart .. origstart + oldlen]);
        }
        insertidx = td.objects.length;
    }
    if (insertlen)
    {
        if (insertlen != tiargs.length) // insert any left element
            td.objects.pushSlice((*tiargs)[$ - insertlen .. $]);
        else
            // just assign tiargs if tuple = AliasSeq!(nottuple, nottuple...)
            td.objects = tempinst.tiargs;
    }
    return td;
}

/*********************************
 * Iterate this dsymbol or members of this scoped dsymbol, then
 * call `fp` with the found symbol and `params`.
 * Params:
 *  symbol = the dsymbol or parent of members to call fp on
 *  fp = function pointer to process the iterated symbol.
 *       If it returns nonzero, the iteration will be aborted.
 *  ctx = context parameter passed to fp.
 * Returns:
 *  nonzero if the iteration is aborted by the return value of fp,
 *  or 0 if it's completed.
 */
int apply(Dsymbol symbol, int function(Dsymbol, void*) fp, void* ctx)
{
    if (auto nd = symbol.isNspace())
    {
        return nd.members.foreachDsymbol( (s) { return s && s.apply(fp, ctx); } );
    }
    if (auto ad = symbol.isAttribDeclaration())
    {
        return ad.include(ad._scope).foreachDsymbol( (s) { return s && s.apply(fp, ctx); } );
    }
    if (auto tm = symbol.isTemplateMixin())
    {
        if (tm._scope) // if fwd reference
            dsymbolSemantic(tm, null); // try to resolve it

        return tm.members.foreachDsymbol( (s) { return s && s.apply(fp, ctx); } );
    }

    return fp(symbol, ctx);
}

/***************************************
 * Check if a template instance is a trivial AliasSeq but without other overloads.
 * We can only be 100% sure of being AliasSeq after running semanticTiargs()
 * and findBestMatch() but this optimization must happen before that.
 */
private TemplateInstance isAliasSeq(Scope* sc, TypeInstance ti)
{
    auto tovers = ti.tempinst.tempdecl.isOverloadSet();
    foreach (size_t oi; 0 .. tovers ? tovers.a.length : 1)
    {
        Dsymbol dstart = tovers ? tovers.a[oi] : ti.tempinst.tempdecl;
        int r = overloadApply(dstart, (Dsymbol s)
        {
            auto td = s.isTemplateDeclaration();
            return (td && td.isTrivialAliasSeq) ? 0 : 1;
        });
        if (r)
            return null;
    }
    return ti.tempinst;
}

/***************************************
 * Find all instance fields in `ad`, then push them into `fields`.
 *
 * Runs semantic() for all instance field variables, but also
 * the field types can remain yet not resolved forward references,
 * except direct recursive definitions.
 * After the process sizeok is set to Sizeok.fwd.
 *
 * Params:
 *      ad = the AggregateDeclaration to examine
 * Returns:
 *      false if any errors occur.
 */
bool determineFields(AggregateDeclaration ad)
{
    if (ad._scope)
        dsymbolSemantic(ad, null);
    if (ad.sizeok != Sizeok.none)
        return true;

    //printf("determineFields() %s, fields.length = %d\n", toChars(), fields.length);
    // determineFields can be called recursively from one of the fields's v.semantic
    ad.fields.setDim(0);

    static int func(Dsymbol s, void* ctx)
    {
        auto ad = cast(AggregateDeclaration)ctx;
        auto v = s.isVarDeclaration();
        if (!v)
            return 0;
        if (v.storage_class & STC.manifest)
            return 0;

        if (v.semanticRun < PASS.semanticdone)
            v.dsymbolSemantic(null);
        // Return in case a recursive determineFields triggered by v.semantic already finished
        if (ad.sizeok != Sizeok.none)
            return 1;

        if (v.aliasTuple)
        {
            // If this variable was really a tuple, process each element.
            return v.aliasTuple.foreachVar(tv => tv.apply(&func, cast(void*) ad));
        }

        if (v.storage_class & (STC.static_ | STC.extern_ | STC.tls | STC.gshared | STC.manifest | STC.ctfe | STC.templateparameter))
            return 0;
        if (!v.isField() || v.semanticRun < PASS.semanticdone)
            return 1;   // unresolvable forward reference

        ad.fields.push(v);

        if (v.storage_class & STC.ref_)
            return 0;
        auto tv = v.type.baseElemOf();
        if (auto tvs = tv.isTypeStruct())
        {
            if (ad == tvs.sym)
            {
                const(char)* psz = (v.type.toBasetype().ty == Tsarray) ? "static array of " : "";
                .error(ad.loc, "%s `%s` cannot have field `%s` with %ssame struct type", ad.kind, ad.toPrettyChars, v.toChars(), psz);
                ad.type = Type.terror;
                ad.errors = true;
                return 1;
            }
        }
        return 0;
    }

    if (ad.members)
    {
        for (size_t i = 0; i < ad.members.length; i++)
        {
            auto s = (*ad.members)[i];
            if (s.apply(&func, cast(void *)ad))
            {
                if (ad.sizeok != Sizeok.none)
                {
                    // recursive determineFields already finished
                    return true;
                }
                return false;
            }
        }
    }

    if (ad.sizeok != Sizeok.done)
        ad.sizeok = Sizeok.fwd;

    return true;
}

/****************************
 * A Singleton that loads core.stdc.config
 * Returns:
 *  Module of core.stdc.config, null if couldn't find it
 */
Module loadCoreStdcConfig()
{
    __gshared Module core_stdc_config;
    auto pkgids = new Identifier[2];
    pkgids[0] = Id.core;
    pkgids[1] = Id.stdc;
    return loadModuleFromLibrary(core_stdc_config, pkgids, Id.config);
}

/****************************
 * A Singleton that loads core.atomic
 * Returns:
 *  Module of core.atomic, null if couldn't find it
 */
private Module loadCoreAtomic()
{
    __gshared Module core_atomic;
    auto pkgids = new Identifier[1];
    pkgids[0] = Id.core;
    return loadModuleFromLibrary(core_atomic, pkgids, Id.atomic);
}

/****************************
 * A Singleton that loads std.math
 * Returns:
 *  Module of std.math, null if couldn't find it
 */
Module loadStdMath()
{
    __gshared Module std_math;
    auto pkgids = new Identifier[1];
    pkgids[0] = Id.std;
    return loadModuleFromLibrary(std_math, pkgids, Id.math);
}

/**********************************
 * Load a Module from the library.
 * Params:
 *  mod = cached return value of this call
 *  pkgids = package identifiers
 *  modid = module id
 * Returns:
 *  Module loaded, null if cannot load it
 */
extern (D) private static Module loadModuleFromLibrary(ref Module mod, Identifier[] pkgids, Identifier modid)
{
    if (mod)
        return mod;

    auto imp = new Import(Loc.initial, pkgids[], modid, null, true);
    // Module.load will call fatal() if there's no module available.
    // Gag the error here, pushing the error handling to the caller.
    const errors = global.startGagging();
    imp.load(null);
    if (imp.mod)
    {
        imp.mod.importAll(null);
        imp.mod.dsymbolSemantic(null);
    }
    global.endGagging(errors);
    mod = imp.mod;
    return mod;
}

/// Do an atomic operation (currently tailored to [shared] static ctors|dtors) needs
private CallExp doAtomicOp (string op, Identifier var, Expression arg)
{
    assert(op == "-=" || op == "+=");

    Module mod = loadCoreAtomic();
    if (!mod)
        return null;    // core.atomic couldn't be loaded

    const loc = Loc.initial;

    Objects* tiargs = new Objects(1);
    (*tiargs)[0] = new StringExp(loc, op);

    Expressions* args = new Expressions(2);
    (*args)[0] = new IdentifierExp(loc, var);
    (*args)[1] = arg;

    auto sc = new ScopeExp(loc, mod);
    auto dti = new DotTemplateInstanceExp(
        loc, sc, Id.atomicOp, tiargs);

    return CallExp.create(loc, dti, args);
}

/***************************************************
 * Set up loc for a parse of a mixin. Append the input text to the mixin.
 * Params:
 *      input = mixin text
 *      loc = location of expansion
 *      baseLoc = location to adjust
 *      mixinOut = sink for mixin text data
 * Returns:
 *      adjusted loc suitable for Parser
 */

void adjustLocForMixin(const(char)[] input, Loc loc, ref BaseLoc baseLoc, ref Output mixinOut)
{
    if (mixinOut.doOutput)
    {
        const lines = mixinOut.bufferLines;
        writeMixin(input, loc, mixinOut.bufferLines, *mixinOut.buffer);
        baseLoc.startLine = lines + 2;
        baseLoc.filename = mixinOut.name;
        return;
    }

    SourceLoc sl = SourceLoc(loc);
    if (sl.filename.length == 0)
    {
        // Rare case of compiler-generated mixin exp, e.g. __xtoHash
        baseLoc.filename = "";
        return;
    }

    /* Create a pseudo-filename for the mixin string, as it may not even exist
     * in the source file.
     */
    auto len = sl.filename.length + 7 + (sl.linnum).sizeof * 3 + 1;
    char* filename = cast(char*) mem.xmalloc(len);
    snprintf(filename, len, "%.*s-mixin-%d", cast(int) sl.filename.length, sl.filename.ptr, cast(int) sl.linnum);
    baseLoc.startLine = sl.line;
    baseLoc.filename = filename.toDString;
}

/**************************************
 * Append source code text to output for better debugging.
 * Canonicalize line endings.
 * Params:
 *      s = source code text
 *      loc = location of source code text
 *      lines = line count to update
 *      output = sink for output
 */
private void writeMixin(const(char)[] s, Loc loc, ref int lines, ref OutBuffer buf)
{
    buf.writestring("// expansion at ");
    buf.writestring(loc.toChars());
    buf.writenl();

    ++lines;

    // write by line to create consistent line endings
    size_t lastpos = 0;
    for (size_t i = 0; i < s.length; ++i)
    {
        // detect LF and CRLF
        const c = s[i];
        if (c == '\n' || (c == '\r' && i+1 < s.length && s[i+1] == '\n'))
        {
            buf.writestring(s[lastpos .. i]);
            buf.writenl();
            ++lines;
            if (c == '\r')
                ++i;
            lastpos = i + 1;
        }
    }

    if(lastpos < s.length)
        buf.writestring(s[lastpos .. $]);

    if (s.length == 0 || s[$-1] != '\n')
    {
        buf.writenl(); // ensure empty line after expansion
        ++lines;
    }
    buf.writenl();
    ++lines;
}

/*********************************************
 * Search for ident as member of d.
 * Params:
 *  d = dsymbol where ident is searched for
 *  loc = location to print for error messages
 *  ident = identifier to search for
 *  flags = search options
 * Returns:
 *  null if not found
 */
Dsymbol search(Dsymbol d, Loc loc, Identifier ident, SearchOptFlags flags = SearchOpt.all)
{
    scope v = new SearchVisitor(loc, ident, flags);
    d.accept(v);
    return v.result;
}

Dsymbol search_correct(Dsymbol d, Identifier ident)
{
    /***************************************************
     * Search for symbol with correct spelling.
     */
    Dsymbol symbol_search_fp(const(char)[] seed, out int cost)
    {
        /* If not in the lexer's string table, it certainly isn't in the symbol table.
         * Doing this first is a lot faster.
         */
        if (!seed.length)
            return null;
        Identifier id = Identifier.lookup(seed);
        if (!id)
            return null;
        cost = 0;   // all the same cost
        Dsymbol s = d;
        Module.clearCache();
        return s.search(Loc.initial, id, SearchOpt.ignoreErrors);
    }

    if (global.gag)
        return null; // don't do it for speculative compiles; too time consuming
    // search for exact name first
    if (auto s = d.search(Loc.initial, ident, SearchOpt.ignoreErrors))
        return s;

    import dmd.root.speller : speller;
    return speller!symbol_search_fp(ident.toString());
}

private extern(C++) class SearchVisitor : Visitor
{
    alias visit = Visitor.visit;

    const Loc loc;
    Identifier ident;
    SearchOptFlags flags;
    Dsymbol result;

    this(Loc loc, Identifier ident, SearchOptFlags flags) @safe
    {
        this.loc = loc;
        this.ident = ident;
        this.flags = flags;
    }

    void setResult(Dsymbol d)
    {
        result = d;
    }

    override void visit(Dsymbol d)
    {
        //printf("Dsymbol::search(this=%p,%s, ident='%s')\n", d, d.toChars(), ident.toChars());
        return setResult(null);
    }

    override void visit(ScopeDsymbol sds)
    {
        //printf("%s.ScopeDsymbol::search(ident='%s', flags=x%x)\n", sds.toChars(), ident.toChars(), flags);
        //if (strcmp(ident.toChars(),"c") == 0) *(char*)0=0;

        // Look in symbols declared in this module
        if (sds.symtab && !(flags & SearchOpt.importsOnly))
        {
            //printf(" look in locals\n");
            auto s1 = sds.symtab.lookup(ident);
            if (s1)
            {
                //printf("\tfound in locals = '%s.%s'\n",toChars(),s1.toChars());
                return setResult(s1);
            }
        }
        //printf(" not found in locals\n");

        // Look in imported scopes
        if (!sds.importedScopes)
            return setResult(null);

        //printf(" look in imports\n");
        Dsymbol s = null;
        OverloadSet a = null;
        // Look in imported modules
        for (size_t i = 0; i < sds.importedScopes.length; i++)
        {
            // If private import, don't search it
            if ((flags & SearchOpt.ignorePrivateImports) && sds.visibilities[i] == Visibility.Kind.private_)
                continue;
            SearchOptFlags sflags = flags & (SearchOpt.ignoreErrors | SearchOpt.ignoreAmbiguous); // remember these in recursive searches
            Dsymbol ss = (*sds.importedScopes)[i];
            //printf("\tscanning import '%s', visibilities = %d, isModule = %p, isImport = %p\n", ss.toChars(), visibilities[i], ss.isModule(), ss.isImport());

            if (ss.isModule())
            {
                if (flags & SearchOpt.localsOnly)
                    continue;
            }
            else // mixin template
            {
                if (flags & SearchOpt.importsOnly)
                    continue;

                sflags |= SearchOpt.localsOnly;
            }

            /* Don't find private members if ss is a module
             */
            Dsymbol s2 = ss.search(loc, ident, sflags | (ss.isModule() ? SearchOpt.ignorePrivateImports : SearchOpt.all));
            import dmd.access : symbolIsVisible;
            if (!s2 || !(flags & SearchOpt.ignoreVisibility) && !symbolIsVisible(sds, s2))
                continue;
            if (!s)
            {
                s = s2;
                if (s && s.isOverloadSet())
                    a = sds.mergeOverloadSet(ident, a, s);
            }
            else if (s2 && s != s2)
            {
                if (s.toAlias() == s2.toAlias() || s.getType() == s2.getType() && s.getType())
                {
                    /* After following aliases, we found the same
                     * symbol, so it's not an ambiguity.  But if one
                     * alias is deprecated or less accessible, prefer
                     * the other.
                     */
                    if (s.isDeprecated() || s.visible() < s2.visible() && s2.visible().kind != Visibility.Kind.none)
                        s = s2;
                }
                else
                {
                    /* Two imports of the same module should be regarded as
                     * the same.
                     */
                    Import i1 = s.isImport();
                    Import i2 = s2.isImport();
                    if (!(i1 && i2 && (i1.mod == i2.mod || (!i1.parent.isImport() && !i2.parent.isImport() && i1.ident.equals(i2.ident)))))
                    {
                        /* https://issues.dlang.org/show_bug.cgi?id=8668
                         * Public selective import adds AliasDeclaration in module.
                         * To make an overload set, resolve aliases in here and
                         * get actual overload roots which accessible via s and s2.
                         */
                        s = s.toAlias();
                        s2 = s2.toAlias();
                        /* If both s2 and s are overloadable (though we only
                         * need to check s once)
                         */

                        auto so2 = s2.isOverloadSet();
                        if ((so2 || s2.isOverloadable()) && (a || s.isOverloadable()))
                        {
                            if (symbolIsVisible(sds, s2))
                            {
                                a = sds.mergeOverloadSet(ident, a, s2);
                            }
                            if (!symbolIsVisible(sds, s))
                                s = s2;
                            continue;
                        }

                        /* Two different overflow sets can have the same members
                         * https://issues.dlang.org/show_bug.cgi?id=16709
                         */
                        auto so = s.isOverloadSet();
                        if (so && so2)
                        {
                            if (so.a.length == so2.a.length)
                            {
                                foreach (j; 0 .. so.a.length)
                                {
                                    if (so.a[j] !is so2.a[j])
                                        goto L1;
                                }
                                continue;  // the same
                              L1:
                                {   } // different
                            }
                        }

                        if (flags & SearchOpt.ignoreAmbiguous) // if return NULL on ambiguity
                            return setResult(null);

                        /* If two imports from C import files, pick first one, as C has global name space
                         */
                        if (s.isCsymbol() && s2.isCsymbol())
                            continue;

                        if (!(flags & SearchOpt.ignoreErrors))
                            ScopeDsymbol.multiplyDefined(loc, s, s2);
                        break;
                    }
                }
            }
        }
        if (s)
        {
            /* Build special symbol if we had multiple finds
             */
            if (a)
            {
                if (!s.isOverloadSet())
                    a = sds.mergeOverloadSet(ident, a, s);
                s = a;
            }
            //printf("\tfound in imports %s.%s\n", toChars(), s.toChars());
            return setResult(s);
        }
        //printf(" not found in imports\n");
        return setResult(null);
    }

    override void visit(WithScopeSymbol ws)
    {
        //printf("WithScopeSymbol.search(%s)\n", ident.toChars());
        if (flags & SearchOpt.importsOnly)
            return setResult(null);
        // Acts as proxy to the with class declaration
        Dsymbol s = null;
        Expression eold = null;
        for (Expression e = ws.withstate.exp; e && e != eold; e = resolveAliasThis(ws._scope, e, true))
        {
            if (auto se = e.isScopeExp())
            {
                s = se.sds;
            }
            else if (e.isTypeExp())
            {
                s = e.type.toDsymbol(null);
            }
            else
            {
                Type t = e.type.toBasetype();
                s = t.toDsymbol(null);
            }
            if (s)
            {
                s = s.search(loc, ident, flags);
                if (s)
                    return setResult(s);
            }
            eold = e;
        }
        return setResult(null);
    }

    override void visit(ArrayScopeSymbol ass)
    {
        //printf("ArrayScopeSymbol::search('%s', flags = %d)\n", ident.toChars(), flags);
        if (ident != Id.dollar)
            return visit(cast(ScopeDsymbol)ass);

        VarDeclaration* pvar;
        Expression ce;

        static Dsymbol dollarFromTypeTuple(Loc loc, TypeTuple tt, Scope* sc)
        {

            /* $ gives the number of type entries in the type tuple
             */
            auto v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, null);
            Expression e = new IntegerExp(Loc.initial, tt.arguments.length, Type.tsize_t);
            v._init = new ExpInitializer(Loc.initial, e);
            v.storage_class |= STC.temp | STC.static_ | STC.const_;
            v.dsymbolSemantic(sc);
            return v;
        }

        const DYNCAST kind = ass.arrayContent.dyncast();
        switch (kind) with (DYNCAST)
        {
        case dsymbol:
            TupleDeclaration td = cast(TupleDeclaration) ass.arrayContent;
            /* $ gives the number of elements in the tuple
             */
            auto v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, null);
            Expression e = new IntegerExp(Loc.initial, td.objects.length, Type.tsize_t);
            v._init = new ExpInitializer(Loc.initial, e);
            v.storage_class |= STC.temp | STC.static_ | STC.const_;
            v.dsymbolSemantic(ass._scope);
            return setResult(v);
        case type:
            return setResult(dollarFromTypeTuple(loc, cast(TypeTuple) ass.arrayContent, ass._scope));
        default:
            break;
        }
        Expression exp = cast(Expression) ass.arrayContent;
        if (auto ie = exp.isIndexExp())
        {
            /* array[index] where index is some function of $
             */
            pvar = &ie.lengthVar;
            ce = ie.e1;
        }
        else if (auto se = exp.isSliceExp())
        {
            /* array[lwr .. upr] where lwr or upr is some function of $
             */
            pvar = &se.lengthVar;
            ce = se.e1;
        }
        else if (auto ae = exp.isArrayExp())
        {
            /* array[e0, e1, e2, e3] where e0, e1, e2 are some function of $
             * $ is a opDollar!(dim)() where dim is the dimension(0,1,2,...)
             */
            pvar = &ae.lengthVar;
            ce = ae.e1;
        }
        else
        {
            /* Didn't find $, look in enclosing scope(s).
             */
            return setResult(null);
        }
        ce = ce.lastComma();
        /* If we are indexing into an array that is really a type
         * tuple, rewrite this as an index into a type tuple and
         * try again.
         */
        if (auto te = ce.isTypeExp())
        {
            if (auto ttp = te.type.isTypeTuple())
                return setResult(dollarFromTypeTuple(loc, ttp, ass._scope));
        }
        /* *pvar is lazily initialized, so if we refer to $
         * multiple times, it gets set only once.
         */
        if (!*pvar) // if not already initialized
        {
            /* Create variable v and set it to the value of $
             */
            VarDeclaration v;
            Type t;
            if (auto tupexp = ce.isTupleExp())
            {
                /* It is for an expression tuple, so the
                 * length will be a const.
                 */
                Expression e = new IntegerExp(Loc.initial, tupexp.exps.length, Type.tsize_t);
                v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, new ExpInitializer(Loc.initial, e));
                v.storage_class |= STC.temp | STC.static_ | STC.const_;
            }
            else if (ce.type && (t = ce.type.toBasetype()) !is null && (t.ty == Tstruct || t.ty == Tclass))
            {
                // Look for opDollar
                assert(exp.op == EXP.array || exp.op == EXP.slice);
                AggregateDeclaration ad = isAggregate(t);
                assert(ad);
                Dsymbol s = ad.search(loc, Id.opDollar);
                if (!s) // no dollar exists -- search in higher scope
                    return setResult(null);
                s = s.toAlias();
                Expression e = null;
                // Check for multi-dimensional opDollar(dim) template.
                if (TemplateDeclaration td = s.isTemplateDeclaration())
                {
                    dinteger_t dim = 0;
                    if (auto ae = exp.isArrayExp())
                    {
                        dim = ae.currentDimension;
                    }
                    else if (exp.isSliceExp())
                    {
                        dim = 0; // slices are currently always one-dimensional
                    }
                    else
                    {
                        assert(0);
                    }
                    Expression edim = new IntegerExp(Loc.initial, dim, Type.tsize_t);
                    edim = edim.expressionSemantic(ass._scope);
                    auto tiargs = new Objects(edim);
                    e = new DotTemplateInstanceExp(loc, ce, td.ident, tiargs);
                }
                else
                {
                    /* opDollar exists, but it's not a template.
                     * This is acceptable ONLY for single-dimension indexing.
                     * Note that it's impossible to have both template & function opDollar,
                     * because both take no arguments.
                     */
                    auto ae = exp.isArrayExp();
                    if (ae && ae.arguments.length != 1)
                    {
                        error(exp.loc, "`%s` only defines opDollar for one dimension", ad.toChars());
                        return setResult(null);
                    }
                    Declaration d = s.isDeclaration();
                    assert(d);
                    e = new DotVarExp(loc, ce, d);
                }
                e = e.expressionSemantic(ass._scope);
                if (!e.type)
                    error(exp.loc, "`%s` has no value", e.toChars());
                t = e.type.toBasetype();
                if (t && t.ty == Tfunction)
                    e = new CallExp(e.loc, e);
                v = new VarDeclaration(loc, null, Id.dollar, new ExpInitializer(Loc.initial, e));
                v.storage_class |= STC.temp | STC.ctfe | STC.rvalue;
            }
            else
            {
                /* For arrays, $ will either be a compile-time constant
                 * (in which case its value in set during constant-folding),
                 * or a variable (in which case an expression is created in
                 * toir.c).
                 */

                // https://issues.dlang.org/show_bug.cgi?id=16213
                // For static arrays $ is known at compile time,
                // so declare it as a manifest constant.
                auto tsa = ce.type ? ce.type.isTypeSArray() : null;
                if (tsa)
                {
                    auto e = new ExpInitializer(loc, tsa.dim);
                    v = new VarDeclaration(loc, tsa.dim.type, Id.dollar, e, STC.manifest);
                }
                else
                {
                    auto e = new VoidInitializer(Loc.initial);
                    e.type = Type.tsize_t;
                    v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, e);
                    v.storage_class |= STC.temp | STC.ctfe; // it's never a true static variable
                }
            }
            *pvar = v;
        }
        (*pvar).dsymbolSemantic(ass._scope);
        return setResult((*pvar));

    }

    override void visit(Import imp)
    {
        //printf("%s.Import.search(ident = '%s', flags = x%x)\n", imp.toChars(), ident.toChars(), flags);
        if (!imp.pkg)
        {
            imp.load(null);
            imp.mod.importAll(null);
            imp.mod.dsymbolSemantic(null);
        }
        // Forward it to the package/module
        return setResult(imp.pkg.search(loc, ident, flags));

    }

    override void visit(Nspace ns)
    {
        //printf("%s.Nspace.search('%s')\n", toChars(), ident.toChars());
        if (ns._scope && !ns.symtab)
            dsymbolSemantic(ns, ns._scope);

        if (!ns.members || !ns.symtab) // opaque or semantic() is not yet called
        {
            if (!(flags & SearchOpt.ignoreErrors))
                .error(loc, "%s `%s` is forward referenced when looking for `%s`", ns.kind, ns.toPrettyChars, ident.toChars());
            return setResult(null);
        }

        visit(cast(ScopeDsymbol)ns);
    }

    override void visit(EnumDeclaration em)
    {
        //printf("%s.EnumDeclaration::search('%s')\n", em.toChars(), ident.toChars());
        if (em._scope)
        {
            // Try one last time to resolve this enum
            dsymbolSemantic(em, em._scope);
        }

        visit(cast(ScopeDsymbol)em);
    }

    override void visit(Package pkg)
    {
        //printf("%s Package.search('%s', flags = x%x)\n", pkg.toChars(), ident.toChars(), flags);
        flags &= ~cast(int)SearchOpt.localsOnly;  // searching an import is always transitive
        if (!pkg.isModule() && pkg.mod)
        {
            // Prefer full package name.
            Dsymbol s = pkg.symtab ? pkg.symtab.lookup(ident) : null;
            if (s)
                return setResult(s);
            //printf("[%s] through pkdmod: %s\n", loc.toChars(), toChars());
            return setResult(pkg.mod.search(loc, ident, flags));
        }

        visit(cast(ScopeDsymbol)pkg);
    }

    override void visit(Module m)
    {
        /* Since modules can be circularly referenced,
         * need to stop infinite recursive searches.
         * This is done with the cache.
         */
        //printf("%s Module.search('%s', flags = x%x) insearch = %d\n", m.toChars(), ident.toChars(), flags, m.insearch);
        if (m.insearch)
            return setResult(null);

        /* Qualified module searches always search their imports,
         * even if SearchLocalsOnly
         */
        if (!(flags & SearchOpt.unqualifiedModule))
            flags &= ~(SearchOpt.unqualifiedModule | SearchOpt.localsOnly);

        if (m.searchCacheIdent == ident && m.searchCacheFlags == flags)
        {
            //printf("%s Module::search('%s', flags = %d) insearch = %d searchCacheSymbol = %s\n",
            //        toChars(), ident.toChars(), flags, insearch, searchCacheSymbol ? searchCacheSymbol.toChars() : "null");
            return setResult(m.searchCacheSymbol);
        }

        const errors = global.errors;

        m.insearch = true;
        visit(cast(ScopeDsymbol)m);
        Dsymbol s = result;
        m.insearch = false;

        if (errors == global.errors)
        {
            // https://issues.dlang.org/show_bug.cgi?id=10752
            // Can cache the result only when it does not cause
            // access error so the side-effect should be reproduced in later search.
            m.searchCacheIdent = ident;
            m.searchCacheSymbol = s;
            m.searchCacheFlags = flags;
        }
        return setResult(s);
    }

    override void visit(Declaration decl)
    {
        Dsymbol s = null;
        if (decl.type)
        {
            s = decl.type.toDsymbol(decl._scope);
            if (s)
                s = s.search(loc, ident, flags);
        }
        return setResult(s);
    }

    override void visit(StructDeclaration sd)
    {
        //printf("%s.StructDeclaration::search('%s', flags = x%x)\n", sd.toChars(), ident.toChars(), flags);
        if (sd._scope && !sd.symtab)
            dsymbolSemantic(sd, sd._scope);

        if (!sd.members || !sd.symtab) // opaque or semantic() is not yet called
        {
            // .stringof is always defined (but may be hidden by some other symbol)
            if(ident != Id.stringof && !(flags & SearchOpt.ignoreErrors) && sd.semanticRun < PASS.semanticdone)
                .error(loc, "%s `%s` is forward referenced when looking for `%s`", sd.kind, sd.toPrettyChars, ident.toChars());
            return setResult(null);
        }

        visit(cast(ScopeDsymbol)sd);
    }

    override void visit(ClassDeclaration cd)
    {
        //printf("%s.ClassDeclaration.search('%s', flags=x%x)\n", cd.toChars(), ident.toChars(), flags);
        //if (_scope) printf("%s baseok = %d\n", toChars(), baseok);
        if (cd._scope && cd.baseok < Baseok.semanticdone)
        {
            if (!cd.inuse)
            {
                // must semantic on base class/interfaces
                cd.inuse = true;
                dsymbolSemantic(cd, null);
                cd.inuse = false;
            }
        }

        if (!cd.members || !cd.symtab) // opaque or addMember is not yet done
        {
            // .stringof is always defined (but may be hidden by some other symbol)
            if (ident != Id.stringof && !(flags & SearchOpt.ignoreErrors) && cd.semanticRun < PASS.semanticdone)
                cd.classError("%s `%s` is forward referenced when looking for `%s`", ident.toChars());
            //*(char*)0=0;
            return setResult(null);
        }

        visit(cast(ScopeDsymbol)cd);
        auto s = result;

        // don't search imports of base classes
        if (flags & SearchOpt.importsOnly)
            return setResult(s);

        if (s)
            return setResult(s);

        // Search bases classes in depth-first, left to right order
        foreach (b; (*cd.baseclasses)[])
        {
            if (!b.sym)
                continue;

            if (!b.sym.symtab)
            {
                cd.classError("%s `%s` base `%s` is forward referenced", b.sym.ident.toChars());
                continue;
            }

            import dmd.access : symbolIsVisible;

            s = b.sym.search(loc, ident, flags);
            if (!s)
                continue;
            if (s == cd) // happens if s is nested in this and derives from this
                s = null;
            else if (!(flags & SearchOpt.ignoreVisibility) && !(s.visible().kind == Visibility.Kind.protected_) && !symbolIsVisible(cd, s))
                s = null;
            else
                break;
        }

        return setResult(s);
    }
}
/*************************************
 * Set scope for future semantic analysis so we can
 * deal better with forward references.
 *
 * Params:
 *   d = dsymbol for which the scope is set
 *   sc = scope that is used to set the value
 */
void setScope(Dsymbol d, Scope* sc)
{
    scope setScopeVisitor = new SetScopeVisitor(sc);
    d.accept(setScopeVisitor);
}

private extern(C++) class SetScopeVisitor : Visitor
{
    alias visit = typeof(super).visit;
    Scope* sc;

    this(Scope* sc) @safe
    {
        this.sc = sc;
    }

    override void visit(Dsymbol d)
    {
        //printf("Dsymbol::setScope() %p %s, %p stc = %llx\n", d, d.toChars(), sc, sc.stc);
        if (!sc.nofree)
            sc.setNoFree(); // may need it even after semantic() finishes
        d._scope = sc;
        if (sc.depdecl)
            d.depdecl = sc.depdecl;
        if (!d.userAttribDecl)
            d.userAttribDecl = sc.userAttribDecl;
    }

    override void visit(Import i)
    {
        visit(cast(Dsymbol)i);
        if (i.aliasdecls.length)
        {
            if (!i.mod)
                i.importAll(sc);

            sc = sc.push(i.mod);
            sc.visibility = i.visibility;
            foreach (ad; i.aliasdecls)
                ad.setScope(sc);
            sc = sc.pop();
        }
    }

    override void visit(Nspace ns)
    {
        visit(cast(Dsymbol)ns);
        if (ns.members)
        {
            assert(sc);
            sc = sc.push(ns);
            sc.linkage = LINK.cpp; // namespaces default to C++ linkage
            sc.parent = ns;
            ns.members.foreachDsymbol(s => s.setScope(sc));
            sc.pop();
        }
    }

    override void visit(EnumDeclaration ed)
    {
        if (ed.semanticRun > PASS.initial)
            return;
        visit(cast(Dsymbol)ed);
    }

    override void visit(AggregateDeclaration ad)
    {
        // Might need a scope to resolve forward references. The check for
        // semanticRun prevents unnecessary setting of _scope during deferred
        // setScope phases for aggregates which already finished semantic().
        // See https://issues.dlang.org/show_bug.cgi?id=16607
        if (ad.semanticRun < PASS.semanticdone)
            visit(cast(Dsymbol)ad);
    }

    override void visit(AttribDeclaration atr)
    {
        Dsymbols* d = atr.include(sc);
        //printf("\tAttribDeclaration::setScope '%s', d = %p\n",toChars(), d);
        if (d)
        {
            Scope* sc2 = atr.newScope(sc);
            d.foreachDsymbol( s => s.setScope(sc2) );
            if (sc2 != sc)
                sc2.pop();
        }
    }

    override void visit(DeprecatedDeclaration dd)
    {
        //printf("DeprecatedDeclaration::setScope() %p\n", this);
        if (dd.decl)
            visit(cast(Dsymbol)dd); // for forward reference
        visit(cast(AttribDeclaration)dd);
    }

    override void visit(CPPMangleDeclaration cppmd)
    {
        if (cppmd.decl)
            visit(cast(Dsymbol)cppmd); // for forward reference
        visit(cast(AttribDeclaration)cppmd);
    }

    override void visit(AnonDeclaration anond)
    {
        if (anond.decl)
            visit(cast(Dsymbol)anond); // for forward reference
        visit(cast(AttribDeclaration)anond);
    }

    override void visit(ConditionalDeclaration condd)
    {
        condd.include(sc).foreachDsymbol( s => s.setScope(sc) );
    }

    override void visit(StaticIfDeclaration sid)
    {
        // do not evaluate condition before semantic pass
        // But do set the scope, in case we need it for forward referencing
        visit(cast(Dsymbol)sid); // for forward reference
    }

    override void visit(StaticForeachDeclaration sfd)
    {
        // do not evaluate condition before semantic pass
        // But do set the scope, in case we need it for forward referencing
        visit(cast(Dsymbol)sfd); // for forward reference
    }

    override void visit(MixinDeclaration md)
    {
        visit(cast(Dsymbol)md);
    }

    override void visit(UserAttributeDeclaration uad)
    {
        //printf("UserAttributeDeclaration::setScope() %p\n", this);
        if (uad.decl)
            visit(cast(Dsymbol)uad);
        visit(cast(AttribDeclaration)uad);
    }
}

void importAll(Dsymbol d, Scope* sc)
{
    scope iav = new ImportAllVisitor(sc);
    d.accept(iav);
}

extern(C++) class ImportAllVisitor : Visitor
{
    alias visit = typeof(super).visit;
    Scope* sc;

    this(Scope* sc) @safe
    {
        this.sc = sc;
    }

    override void visit(Dsymbol d) {}

    override void visit(Import imp)
    {
        if (imp.mod) return; // Already done

        /*
         * https://issues.dlang.org/show_bug.cgi?id=15525
         *
         * Loading the import has failed,
         * most likely because of parsing errors.
         * Therefore we cannot trust the resulting AST.
         */
        if (imp.load(sc))
        {
            // https://issues.dlang.org/show_bug.cgi?id=23873
            // For imports that are not at module or function level,
            // e.g. aggregate level, the import symbol is added to the
            // symbol table and later semantic is performed on it.
            // This leads to semantic analysis on an malformed AST
            // which causes all kinds of segfaults.
            // The fix is to note that the module has errors and avoid
            // semantic analysis on it.
            if(imp.mod)
                imp.mod.errors = true;
            return;
        }

        if (!imp.mod) return; // Failed

        if (sc.stc & STC.static_)
            imp.isstatic = true;
        imp.mod.importAll(null);
        imp.mod.checkImportDeprecation(imp.loc, sc);
        if (sc.explicitVisibility)
            imp.visibility = sc.visibility;
        if (!imp.isstatic && !imp.aliasId && !imp.names.length)
            sc.scopesym.importScope(imp.mod, imp.visibility);
        // Enable access to pkgs/mod as soon as posible, because compiler
        // can traverse them before the import gets semantic (Issue: 21501)
        if (!imp.aliasId && !imp.names.length)
            imp.addPackageAccess(sc.scopesym);
    }

    override void visit(Module m)
    {
        //printf("+Module::importAll(this = %p, '%s'): parent = %p\n", m, m.toChars(), m.parent);
        if (m._scope)
            return; // already done
        if (m.filetype == FileType.ddoc)
        {
            error(m.loc, "%s `%s` is a Ddoc file, cannot import it", m.kind, m.toPrettyChars);
            return;
        }

        /* Note that modules get their own scope, from scratch.
         * This is so regardless of where in the syntax a module
         * gets imported, it is unaffected by context.
         * Ignore prevsc.
         */
        Scope* sc = scopeCreateGlobal(m, global.errorSink); // create root scope

        if (m.md && m.md.msg)
            m.md.msg = semanticString(sc, m.md.msg, "deprecation message");

        // Add import of "object", even for the "object" module.
        // If it isn't there, some compiler rewrites, like
        //    classinst == classinst -> .object.opEquals(classinst, classinst)
        // would fail inside object.d.
        if (m.filetype != FileType.c &&
            (m.members.length == 0 ||
             (*m.members)[0].ident != Id.object ||
             (*m.members)[0].isImport() is null))
        {
            auto im = new Import(m.loc, null, Id.object, null, 0);
            m.members.shift(im);
        }
        if (!m.symtab)
        {
            // Add all symbols into module's symbol table
            m.symtab = new DsymbolTable();
            for (size_t i = 0; i < m.members.length; i++)
            {
                Dsymbol s = (*m.members)[i];
                s.addMember(sc, sc.scopesym);
            }
        }
        // anything else should be run after addMember, so version/debug symbols are defined
        /* Set scope for the symbols so that if we forward reference
         * a symbol, it can possibly be resolved on the spot.
         * If this works out well, it can be extended to all modules
         * before any semantic() on any of them.
         */
        m.setScope(sc); // remember module scope for semantic
        for (size_t i = 0; i < m.members.length; i++)
        {
            Dsymbol s = (*m.members)[i];
            s.setScope(sc);
        }
        for (size_t i = 0; i < m.members.length; i++)
        {
            Dsymbol s = (*m.members)[i];
            s.importAll(sc);
        }
        sc = sc.pop();
        sc.pop(); // 2 pops because scopeCreateGlobal() created 2
    }

    override void visit(AttribDeclaration atb)
    {
        Dsymbols* d = atb.include(sc);
        //printf("\tAttribDeclaration::importAll '%s', d = %p\n", toChars(), d);
        if (d)
        {
            Scope* sc2 = atb.newScope(sc);
            d.foreachDsymbol( s => s.importAll(sc2) );
            if (sc2 != sc)
                sc2.pop();
        }
    }

    // do not evaluate condition before semantic pass
    override void visit(StaticIfDeclaration _) {}
    // do not evaluate aggregate before semantic pass
    override void visit(StaticForeachDeclaration _) {}
}

/*******************************
    * Load module.
    * Returns:
    *  true for errors, false for success
    */
extern (D) bool load(Import imp, Scope* sc)
{
    // See if existing module
    const errors = global.errors;
    DsymbolTable dst = Package.resolve(imp.packages, null, &imp.pkg);
    version (none)
    {
        if (pkg && pkg.isModule())
        {
            .error(loc, "can only import from a module, not from a member of module `%s`. Did you mean `import %s : %s`?", pkg.toChars(), pkg.toPrettyChars(), id.toChars());
            mod = pkg.isModule(); // Error recovery - treat as import of that module
            return true;
        }
    }
    Dsymbol s = dst.lookup(imp.id);
    if (s)
    {
        if (s.isModule())
            imp.mod = cast(Module)s;
        else
        {
            if (s.isAliasDeclaration())
            {
                .error(imp.loc, "%s `%s` conflicts with `%s`", s.kind(), s.toPrettyChars(), imp.id.toChars());
            }
            else if (Package p = s.isPackage())
            {
                if (p.isPkgMod == PKG.unknown)
                {
                    const preverrors = global.errors;
                    imp.mod = Module.load(imp.loc, imp.packages, imp.id);
                    if (!imp.mod)
                        p.isPkgMod = PKG.package_;
                    else
                    {
                        // imp.mod is a package.d, or a normal module which conflicts with the package name.
                        if (imp.mod.isPackageFile)
                            imp.mod.tag = p.tag; // reuse the same package tag
                        else
                        {
                            // show error if Module.load does not
                            if (preverrors == global.errors)
                                .error(imp.loc, "%s `%s` from file %s conflicts with %s `%s`", imp.mod.kind(), imp.mod.toPrettyChars(), imp.mod.srcfile.toChars, p.kind(), p.toPrettyChars());
                            return true;
                        }
                    }
                }
                else
                {
                    imp.mod = p.isPackageMod();
                }
                if (!imp.mod)
                {
                    .error(imp.loc, "can only import from a module, not from package `%s.%s`", p.toPrettyChars(), imp.id.toChars());
                }
            }
            else if (imp.pkg)
            {
                .error(imp.loc, "can only import from a module, not from package `%s.%s`", imp.pkg.toPrettyChars(), imp.id.toChars());
            }
            else
            {
                .error(imp.loc, "can only import from a module, not from package `%s`", imp.id.toChars());
            }
        }
    }
    if (!imp.mod)
    {
        // Load module
        imp.mod = Module.load(imp.loc, imp.packages, imp.id);
        if (imp.mod)
        {
            // imp.id may be different from mod.ident, if so then insert alias
            dst.insert(imp.id, imp.mod);
        }
    }
    if (imp.mod && !imp.mod.importedFrom)
        imp.mod.importedFrom = sc ? sc._module.importedFrom : Module.rootModule;
    if (!imp.pkg)
    {
        if (imp.mod && imp.mod.isPackageFile)
        {
            // one level depth package.d file (import pkg; ./pkg/package.d)
            // it's necessary to use the wrapping Package already created
            imp.pkg = imp.mod.pkg;
        }
        else
            imp.pkg = imp.mod;
    }
    return global.errors != errors;
}

void setFieldOffset(Dsymbol d, AggregateDeclaration ad, FieldState* fieldState, bool isunion)
{
    scope v = new SetFieldOffsetVisitor(ad, fieldState, isunion);
    d.accept(v);
}

private extern(C++) class SetFieldOffsetVisitor : Visitor
{
    import dmd.typesem: size;

    alias visit = Visitor.visit;

    AggregateDeclaration ad;
    FieldState* fieldState;
    bool isunion;

    this(AggregateDeclaration ad, FieldState* fieldState, bool isunion) @safe
    {
        this.ad = ad;
        this.fieldState = fieldState;
        this.isunion = isunion;
    }

    override void visit(Dsymbol d) {}

    override void visit(Nspace ns)
    {
        //printf("Nspace::setFieldOffset() %s\n", toChars());
        if (ns._scope) // if fwd reference
            dsymbolSemantic(ns, null); // try to resolve it
        ns.members.foreachDsymbol( s => s.setFieldOffset(ad, fieldState, isunion) );
    }

    override void visit(VarDeclaration vd)
    {
        //printf("VarDeclaration::setFieldOffset(ad = %s) %s\n", ad.toChars(), vd.toChars());

        if (vd.aliasTuple)
        {
            // If this variable was really a tuple, set the offsets for the tuple fields
            vd.aliasTuple.foreachVar((s) { s.setFieldOffset(ad, fieldState, isunion); });
            return;
        }

        if (!vd.isField())
            return;
        assert(!(vd.storage_class & (STC.static_ | STC.extern_ | STC.parameter)));

        //printf("+VarDeclaration::setFieldOffset(ad = %s) %s\n", ad.toChars(), toChars());

        /* Fields that are tuples appear both as part of TupleDeclarations and
         * as members. That means ignore them if they are already a field.
         */
        if (vd.offset)
        {
            // already a field
            fieldState.offset = ad.structsize; // https://issues.dlang.org/show_bug.cgi?id=13613
            return;
        }
        for (size_t i = 0; i < ad.fields.length; i++)
        {
            if (ad.fields[i] == vd)
            {
                // already a field
                fieldState.offset = ad.structsize; // https://issues.dlang.org/show_bug.cgi?id=13613
                return;
            }
        }

        // Check for forward referenced types which will fail the size() call
        Type t = vd.type.toBasetype();
        if (vd.storage_class & STC.ref_)
        {
            // References are the size of a pointer
            t = Type.tvoidptr;
        }
        Type tv = t.baseElemOf();
        if (tv.ty == Tstruct)
        {
            auto ts = cast(TypeStruct)tv;
            assert(ts.sym != ad);   // already checked in ad.determineFields()
            if (!ts.sym.determineSize(vd.loc))
            {
                vd.type = Type.terror;
                vd.errors = true;
                return;
            }
        }

        // List in ad.fields. Even if the type is error, it's necessary to avoid
        // pointless error diagnostic "more initializers than fields" on struct literal.
        ad.fields.push(vd);

        if (t.ty == Terror)
            return;

        /* If coming after a bit field in progress,
         * advance past the field
         */
        fieldState.inFlight = false;

        const sz = t.size(vd.loc);
        assert(sz != SIZE_INVALID && sz < uint.max);
        uint memsize = cast(uint)sz;                // size of member
        uint memalignsize = target.fieldalign(t);   // size of member for alignment purposes
        vd.offset = placeField(vd.loc,
            fieldState.offset,
            memsize, memalignsize, vd.alignment,
            ad.structsize, ad.alignsize,
            isunion);

        //printf("\t%s: memalignsize = %d\n", toChars(), memalignsize);
        //printf(" addField '%s' to '%s' at offset %d, size = %d\n", toChars(), ad.toChars(), offset, memsize);
    }

    override void visit(BitFieldDeclaration bfd)
    {
        enum log = false;
        static if (log)
        {
            printf("BitFieldDeclaration::setFieldOffset(ad: %s, field: %s)\n", ad.toChars(), bfd.toChars());
            void print(const FieldState* fieldState)
            {
                fieldState.print();
                printf("          fieldWidth   = %d bits\n",    bfd.fieldWidth);
            }
            print(fieldState);
        }

        Type t = bfd.type.toBasetype();
        const bool anon = bfd.isAnonymous();

        // List in ad.fields. Even if the type is error, it's necessary to avoid
        // pointless error diagnostic "more initializers than fields" on struct literal.
        if (!anon)
            ad.fields.push(bfd);

        if (t.ty == Terror)
            return;

        const sz = t.size(bfd.loc);
        assert(sz != SIZE_INVALID && sz < uint.max);
        uint memsize = cast(uint)sz;                // size of member
        uint memalignsize = target.fieldalign(t);   // size of member for alignment purposes
        if (log) printf("          memsize: %u memalignsize: %u\n", memsize, memalignsize);

        if (bfd.fieldWidth == 0 && !anon)
            error(bfd.loc, "named bit fields cannot have 0 width");
        if (bfd.fieldWidth > memsize * 8)
            error(bfd.loc, "bit field width %d is larger than type", bfd.fieldWidth);

        const style = target.c.bitFieldStyle;
        if (style != TargetC.BitFieldStyle.MS && style != TargetC.BitFieldStyle.Gcc_Clang)
            assert(0, "unsupported bitfield style");

        const isMicrosoftStyle = style == TargetC.BitFieldStyle.MS;
        const contributesToAggregateAlignment = target.c.contributesToAggregateAlignment(bfd);

        void startNewField()
        {
            if (log) printf("startNewField()\n");
            uint alignsize;
            if (isMicrosoftStyle)
               alignsize = memsize; // not memalignsize
            else
            {
                if (bfd.fieldWidth > 32)
                    alignsize = memalignsize;
                else if (bfd.fieldWidth > 16)
                    alignsize = 4;
                else if (bfd.fieldWidth > 8)
                    alignsize = 2;
                else
                    alignsize = 1;
            }

            uint dummy;
            bfd.offset = placeField(bfd.loc,
                fieldState.offset,
                memsize, alignsize, bfd.alignment,
                ad.structsize,
                contributesToAggregateAlignment ? ad.alignsize : dummy,
                isunion);

            fieldState.inFlight = true;
            fieldState.fieldOffset = bfd.offset;
            fieldState.bitOffset = 0;
            fieldState.fieldSize = memsize;
        }

        if (ad.alignsize == 0)
            ad.alignsize = 1;
        if (!isMicrosoftStyle && contributesToAggregateAlignment && ad.alignsize < memalignsize)
            ad.alignsize = memalignsize;

        if (bfd.fieldWidth == 0)
        {
            if (!isMicrosoftStyle && !isunion)
            {
                // Use type of zero width field to align to next field
                fieldState.offset = (fieldState.offset + memalignsize - 1) & ~(memalignsize - 1);
                ad.structsize = fieldState.offset;
            }
            else if (isMicrosoftStyle && fieldState.inFlight && !isunion)
            {
                // documentation says align to next int
                //const alsz = cast(uint)Type.tint32.size();
                const alsz = memsize; // but it really does this
                fieldState.offset = (fieldState.offset + alsz - 1) & ~(alsz - 1);
                ad.structsize = fieldState.offset;
            }

            fieldState.inFlight = false;
            return;
        }

        if (!fieldState.inFlight)
        {
            //printf("not in flight\n");
            startNewField();
        }
        else if (!isMicrosoftStyle)
        {
            // If the bitfield spans more units of alignment than its type
            // and is at the alignment boundary, start a new field at the
            // next alignment boundary. This affects when offsetof reports
            // a higher number and bitoffsetof starts at zero again.
            if (fieldState.bitOffset % (memalignsize * 8) == 0 &&
                fieldState.bitOffset + bfd.fieldWidth > memsize * 8)
            {
                if (log) printf("more units of alignment than its type\n");
                startNewField();        // the bit field is full
            }
            else
            {
                // if alignment boundary is crossed
                uint start = (fieldState.fieldOffset * 8 + fieldState.bitOffset) % (memalignsize * 8);
                uint end   = start + bfd.fieldWidth;
                //printf("%s start: %d end: %d memalignsize: %d\n", ad.toChars(), start, end, memalignsize);
                if (start / (memsize * 8) != (end - 1) / (memsize * 8))
                {
                    if (log) printf("alignment is crossed\n");
                    startNewField();
                }
            }
        }
        else
        {
            if (memsize != fieldState.fieldSize ||
                fieldState.bitOffset + bfd.fieldWidth > fieldState.fieldSize * 8)
            {
                //printf("new field\n");
                startNewField();
            }
        }

        bfd.offset = fieldState.fieldOffset;
        bfd.bitOffset = fieldState.bitOffset;

        const pastField = bfd.bitOffset + bfd.fieldWidth;
        if (isMicrosoftStyle)
            fieldState.fieldSize = memsize;
        else
        {
            const size = (pastField + 7) / 8;
            fieldState.fieldSize = size;
            //printf(" offset: %d, size: %d\n", offset, size);
            if (isunion)
            {
                const newstructsize = bfd.offset + size;
                if (newstructsize > ad.structsize)
                    ad.structsize = newstructsize;
            }
            else
                ad.structsize = bfd.offset + size;
        }
        //printf("at end: ad.structsize = %d\n", cast(int)ad.structsize);
        //print(fieldState);

        if (!isunion)
        {
            fieldState.offset = bfd.offset + fieldState.fieldSize;
            fieldState.bitOffset = pastField;
        }

        //printf("\t%s: offset = %d bitOffset = %d fieldWidth = %d memsize = %d\n", toChars(), offset, bitOffset, fieldWidth, memsize);
        //printf("\t%s: memalignsize = %d\n", toChars(), memalignsize);
        //printf(" addField '%s' to '%s' at offset %d, size = %d\n", toChars(), ad.toChars(), offset, memsize);
    }

    override void visit(TemplateMixin tm)
    {
        //printf("TemplateMixin.setFieldOffset() %s\n", tm.toChars());
        if (tm._scope) // if fwd reference
            dsymbolSemantic(tm, null); // try to resolve it

        tm.members.foreachDsymbol( (s) { s.setFieldOffset(ad, fieldState, isunion); } );
    }

    override void visit(AttribDeclaration atd)
    {
        atd.include(null).foreachDsymbol( s => s.setFieldOffset(ad, fieldState, isunion) );
    }

    override void visit(AnonDeclaration anond)
    {
        //printf("\tAnonDeclaration::setFieldOffset %s %p\n", isunion ? "union" : "struct", anond);
        if (anond.decl)
        {
            /* This works by treating an AnonDeclaration as an aggregate 'member',
             * so in order to place that member we need to compute the member's
             * size and alignment.
             */
            size_t fieldstart = ad.fields.length;

            /* Hackishly hijack ad's structsize and alignsize fields
             * for use in our fake anon aggregate member.
             */
            uint savestructsize = ad.structsize;
            uint savealignsize = ad.alignsize;
            ad.structsize = 0;
            ad.alignsize = 0;

            FieldState fs;
            anond.decl.foreachDsymbol( (s)
            {
                s.setFieldOffset(ad, &fs, anond.isunion);
                if (anond.isunion)
                    fs.offset = 0;
            });

            /* https://issues.dlang.org/show_bug.cgi?id=13613
             * If the fields in this.members had been already
             * added in ad.fields, just update *poffset for the subsequent
             * field offset calculation.
             */
            if (fieldstart == ad.fields.length)
            {
                ad.structsize = savestructsize;
                ad.alignsize = savealignsize;
                fieldState.offset = ad.structsize;
                return;
            }

            anond.anonstructsize = ad.structsize;
            anond.anonalignsize = ad.alignsize;
            ad.structsize = savestructsize;
            ad.alignsize = savealignsize;

            // 0 sized structs are set to 1 byte
            if (anond.anonstructsize == 0)
            {
                anond.anonstructsize = 1;
                anond.anonalignsize = 1;
            }

            assert(anond._scope);
            auto alignment = anond._scope.alignment();

            /* Given the anon 'member's size and alignment,
             * go ahead and place it.
             */
            anond.anonoffset = placeField(anond.loc,
                fieldState.offset,
                anond.anonstructsize, anond.anonalignsize, alignment,
                ad.structsize, ad.alignsize,
                isunion);

            // Add to the anon fields the base offset of this anonymous aggregate
            //printf("anon fields, anonoffset = %d\n", anond.anonoffset);
            if (anond.anonoffset)
                anond.decl.foreachDsymbol( (s) => s.adjustBaseOffset(anond.anonoffset) );
        }
    }
}

// Adds `offset` as the new base offset of all field members in `d`.
private void adjustBaseOffset(Dsymbol d, uint offset)
{
    switch (d.dsym)
    {
        case DSYM.nspace:
            auto ns = cast(Nspace)d;
            ns.members.foreachDsymbol( s => s.adjustBaseOffset(offset) );
            break;

        case DSYM.templateMixin:
            auto tm = cast(TemplateMixin)d;
            tm.members.foreachDsymbol( s => s.adjustBaseOffset(offset) );
            break;

        case DSYM.anonDeclaration:
            auto ad = cast(AnonDeclaration)d;
            if (ad.decl)
                ad.decl.foreachDsymbol( s => s.adjustBaseOffset(offset) );
            break;

        case DSYM.bitFieldDeclaration:
            auto bfd = cast(BitFieldDeclaration)d;
            bfd.offset += offset;
            //printf("\t%s %d : %d\n", bfd.toChars(), bfd.offset, bfd.bitOffset);
            break;

        default:
            if (auto vd = d.isVarDeclaration())
            {
                if (vd.aliasTuple)
                    vd.aliasTuple.foreachVar( s => s.adjustBaseOffset(offset) );
                else if (vd.isField())
                {
                    vd.offset += offset;
                    //printf("\t%s %d\n", vd.toChars(), vd.offset);
                }
            }
            else if (auto atd = d.isAttribDeclaration())
            {
                atd.include(null).foreachDsymbol( s => s.adjustBaseOffset(offset) );
            }
            break;
    }
}

extern(D) Scope* newScope(Dsymbol d, Scope* sc)
{
    scope nsv = new NewScopeVisitor(sc);
    d.accept(nsv);
    return nsv.sc;
}

private extern(C++) class NewScopeVisitor : Visitor
{
    alias visit = typeof(super).visit;
    Scope* sc;
    this(Scope* sc)
    {
        this.sc = sc;
    }

    /****************************************
     * A hook point to supply scope for members.
     * addMember, setScope, importAll, semantic, semantic2 and semantic3 will use this.
     */
    override void visit(AttribDeclaration dc){}

    override void visit(StorageClassDeclaration swt)
    {
        STC scstc = sc.stc;
        /* These sets of storage classes are mutually exclusive,
         * so choose the innermost or most recent one.
         */
        if (swt.stc & (STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.manifest))
            scstc &= ~(STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.manifest);
        if (swt.stc & (STC.auto_ | STC.scope_ | STC.static_ | STC.manifest | STC.gshared))
            scstc &= ~(STC.auto_ | STC.scope_ | STC.static_ | STC.manifest | STC.gshared);
        if (swt.stc & (STC.const_ | STC.immutable_ | STC.manifest))
            scstc &= ~(STC.const_ | STC.immutable_ | STC.manifest);
        if (swt.stc & (STC.gshared | STC.shared_))
            scstc &= ~(STC.gshared | STC.shared_);
        if (swt.stc & (STC.safe | STC.trusted | STC.system))
            scstc &= ~(STC.safe | STC.trusted | STC.system);
        scstc |= swt.stc;
        //printf("scstc = x%llx\n", scstc);
        sc = swt.createNewScope(sc, scstc, sc.linkage, sc.cppmangle,
        sc.visibility, sc.explicitVisibility, sc.aligndecl, sc.inlining);
    }

    /**
     * Provides a new scope with `STC.deprecated_` and `Scope.depdecl` set
     *
     * Calls `StorageClassDeclaration.newScope` (as it must be called or copied
     * in any function overriding `newScope`), then set the `Scope`'s depdecl.
     *
     * Returns:
     *   Always a new scope, to use for this `DeprecatedDeclaration`'s members.
     */
    override void visit(DeprecatedDeclaration dpd)
    {
        auto oldsc = sc;
        visit((cast(StorageClassDeclaration)dpd));
        auto scx = sc;
        sc = oldsc;
        // The enclosing scope is deprecated as well
        if (scx == sc)
            scx = sc.push();
        scx.depdecl = dpd;
        sc = scx;
    }

    override void visit(LinkDeclaration  lid)
    {
        sc= lid.createNewScope(sc, sc.stc, lid.linkage, sc.cppmangle, sc.visibility, sc.explicitVisibility,
        sc.aligndecl, sc.inlining);
    }

    override void visit(CPPMangleDeclaration cpmd)
    {
        sc = cpmd.createNewScope(sc, sc.stc, LINK.cpp, cpmd.cppmangle, sc.visibility, sc.explicitVisibility,
        sc.aligndecl, sc.inlining);
    }

    /**
     * Returns:
     *   A copy of the parent scope, with `this` as `namespace` and C++ linkage
     *///override Scope* visit(Scope* sc)
    override void visit(CPPNamespaceDeclaration scd)
    {
        auto scx = sc.copy();
        scx.linkage = LINK.cpp;
        scx.namespace = scd;
        sc = scx;
    }

    override void visit(VisibilityDeclaration atbd)
    {
        if (atbd.pkg_identifiers)
            dsymbolSemantic(atbd, sc);

       sc = atbd.createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, atbd.visibility, 1, sc.aligndecl, sc.inlining);
    }

    override void visit(AlignDeclaration visd)
    {
        sc = visd.createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, sc.visibility,
        sc.explicitVisibility, visd, sc.inlining);
    }

    override void visit(PragmaDeclaration prd)
    {
        if (prd.ident == Id.Pinline)
        {
            // We keep track of this pragma inside scopes,
            // then it's evaluated on demand in function semantic
            sc = prd.createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, sc.visibility, sc.explicitVisibility, sc.aligndecl, prd); // @suppress(dscanner.style.long_line)
        }
    }

    /**************************************
     * Use the ForwardingScopeDsymbol as the parent symbol for members.
     */
    override void visit(ForwardingAttribDeclaration  fad)
    {
        sc = sc.push(fad.sym);
    }

    override void visit(UserAttributeDeclaration uac)
    {
        Scope* sc2 = sc;
        if (uac.atts && uac.atts.length)
        {
            // create new one for changes
            sc2 = sc.copy();
            sc2.userAttribDecl = uac;
        }
        sc = sc2;
    }
}


Dsymbols* include(Dsymbol d, Scope* sc)
{
    scope icv = new ConditionIncludeVisitor(sc);
    d.accept(icv);
    return icv.symbols;
}

extern(C++) class ConditionIncludeVisitor : Visitor
{
    alias visit = typeof(super).visit;
    Scope* sc;
    Dsymbols* symbols;
    this(Scope* sc)
    {
        this.sc = sc;
    }

    override void visit(AttribDeclaration ad)
    {
        if (ad.errors)
        {
            symbols = null;
            return;
        }
        symbols = ad.decl;
        return;
    }

// Decide if 'then' or 'else' code should be included
    override void visit(ConditionalDeclaration cdc)
    {
        //printf("ConditionalDeclaration::include(sc = %p) scope = %p\n", sc, _scope);

        if (cdc.errors)
        {
            symbols = null;
            return;
        }
        assert(cdc.condition);
        symbols = dmd.expressionsem.include(cdc.condition, cdc._scope ? cdc._scope : sc) ? cdc.decl : cdc.elsedecl;
    }

    override void visit(StaticIfDeclaration sif)
    {
    /****************************************
     * Different from other AttribDeclaration subclasses, include() call requires
     * the completion of addMember and setScope phases.
     */
        //printf("StaticIfDeclaration::include(sc = %p) scope = %p\n", sc, _scope);
        if (sif.errors || sif.onStack)
        {
            symbols = null;
            return;
        }
        sif.onStack = true;
        scope(exit) sif.onStack = false;

        if (sc && sif.condition.inc == Include.notComputed)
        {
            assert(sif.scopesym); // addMember is already done
            assert(sif._scope); // setScope is already done

            Scope* saved_scope = sc;
            sc = sif._scope;
            visit(cast(ConditionalDeclaration) sif);
            Dsymbols* d = symbols;
            sc = saved_scope;

            if (d && !sif.addisdone)
            {
                // Add members lazily.
                d.foreachDsymbol( s => s.addMember(sif._scope, sif.scopesym) );

                // Set the member scopes lazily.
                d.foreachDsymbol( s => s.setScope(sif._scope) );

                sif.addisdone = true;
            }
            symbols = d;
            return;
        }
        else
        {
            visit(cast(ConditionalDeclaration)sif);
        }
    }

    override void visit(StaticForeachDeclaration sfd)
    {
        if (sfd.errors || sfd.onStack)
        {
            symbols = null;
            return;
        }
        if (sfd.cached)
        {
            assert(!sfd.onStack);
            symbols = sfd.cache;
            return;
        }
        sfd.onStack = true;
        scope(exit) sfd.onStack = false;

        if (sfd._scope)
        {
            sfd.sfe.prepare(sfd._scope); // lower static foreach aggregate
        }
        if (!sfd.sfe.ready())
        {
            symbols = null;// TODO: ok?
            return;
        }

        // expand static foreach
        import dmd.statementsem: makeTupleForeach;
        Dsymbols* d = makeTupleForeach(sfd._scope, true, true, sfd.sfe.aggrfe, sfd.decl, sfd.sfe.needExpansion).decl;
        if (d) // process generated declarations
        {
            // Add members lazily.
            d.foreachDsymbol( s => s.addMember(sfd._scope, sfd.scopesym) );

            // Set the member scopes lazily.
            d.foreachDsymbol( s => s.setScope(sfd._scope) );
        }
        sfd.cached = true;
        sfd.cache = d;
        symbols = d;
    }
}

/**
 * Called from a symbol's semantic to check if `gnuAbiTag` UDA
 * can be applied to them
 *
 * Directly emits an error if the UDA doesn't work with this symbol
 *
 * Params:
 *   sym = symbol to check for `gnuAbiTag`
 *   linkage = Linkage of the symbol (Declaration.link or sc.link)
 */
void checkGNUABITag(Dsymbol sym, LINK linkage)
{
    if (global.params.cplusplus < CppStdRevision.cpp11)
        return;

    foreachUdaNoSemantic(sym, (exp) {
        if (!isGNUABITag(exp))
            return 0; // continue
        if (sym.isCPPNamespaceDeclaration() || sym.isNspace())
        {
            .error(exp.loc, "`@%s` cannot be applied to namespaces", Id.udaGNUAbiTag.toChars());
            sym.errors = true;
        }
        else if (linkage != LINK.cpp)
        {
            .error(exp.loc, "`@%s` can only apply to C++ symbols", Id.udaGNUAbiTag.toChars());
            sym.errors = true;
        }
        // Only one `@gnuAbiTag` is allowed by semantic2
        return 1; // break
    });
}

/**
 * Check if the provided expression references `core.attribute.gnuAbiTag`
 *
 * This should be called after semantic has been run on the expression.
 * Semantic on UDA happens in semantic2 (see `dmd.semantic2`).
 *
 * Params:
 *   e = Expression to check (usually from `UserAttributeDeclaration.atts`)
 *
 * Returns:
 *   `true` if the expression references the compiler-recognized `gnuAbiTag`
 */
bool isGNUABITag(Expression e)
{
    if (global.params.cplusplus < CppStdRevision.cpp11)
        return false;

    auto ts = e.type ? e.type.isTypeStruct() : null;
    if (!ts)
        return false;
    if (ts.sym.ident != Id.udaGNUAbiTag || !ts.sym.parent)
        return false;
    // Can only be defined in druntime
    Module m = ts.sym.parent.isModule();
    if (!m || !m.isCoreModule(Id.attribute))
        return false;
    return true;
}

/******************************************
 * If a variable has a scope destructor call, return call for it.
 * Otherwise, return NULL.
 */
private Expression callScopeDtor(VarDeclaration vd, Scope* sc)
{
    import dmd.typesem: size;

    //printf("VarDeclaration::callScopeDtor() %s\n", toChars());

    // Destruction of STC.field's is handled by buildDtor()
    if (vd.storage_class & (STC.nodtor | STC.ref_ | STC.out_ | STC.field))
    {
        return null;
    }

    if (vd.iscatchvar)
        return null;    // destructor is built by `void semantic(Catch c, Scope* sc)`, not here

    Expression e = null;
    // Destructors for structs and arrays of structs
    Type tv = vd.type.baseElemOf();
    if (tv.ty == Tstruct)
    {
        StructDeclaration sd = (cast(TypeStruct)tv).sym;
        if (!sd.dtor || sd.errors)
            return null;

        const sz = vd.type.size();
        assert(sz != SIZE_INVALID);

        if (vd.type.toBasetype().ty == Tstruct)
        {
            // v.__xdtor()
            e = new VarExp(vd.loc, vd);

            /* This is a hack so we can call destructors on const/immutable objects.
             * Need to add things like "const ~this()" and "immutable ~this()" to
             * fix properly.
             */
            e.type = e.type.mutableOf();

            // Enable calling destructors on shared objects.
            // The destructor is always a single, non-overloaded function,
            // and must serve both shared and non-shared objects.
            e.type = e.type.unSharedOf;

            e = new DotVarExp(vd.loc, e, sd.dtor, false);
            e = new CallExp(vd.loc, e);
        }
        else
        {
            // __ArrayDtor(v[0 .. n])
            e = new VarExp(vd.loc, vd);

            const sdsz = sd.type.size();
            assert(sdsz != SIZE_INVALID && sdsz != 0);
            const n = sz / sdsz;
            SliceExp se = new SliceExp(vd.loc, e, new IntegerExp(vd.loc, 0, Type.tsize_t),
                new IntegerExp(vd.loc, n, Type.tsize_t));

            // Prevent redundant bounds check
            se.upperIsInBounds = true;
            se.lowerIsLessThanUpper = true;

            // This is a hack so we can call destructors on const/immutable objects.
            se.type = sd.type.arrayOf();

            e = new CallExp(vd.loc, new IdentifierExp(vd.loc, Id.__ArrayDtor), se);
        }
        return e;
    }
    // Destructors for classes
    if (!(vd.storage_class & (STC.auto_ | STC.scope_) && !(vd.storage_class & STC.parameter)))
        return null;

    for (ClassDeclaration cd = vd.type.isClassHandle(); cd; cd = cd.baseClass)
    {
        /* We can do better if there's a way with onstack
         * classes to determine if there's no way the monitor
         * could be set.
         */
        //if (cd.isInterfaceDeclaration())
        //    error("interface `%s` cannot be scope", cd.toChars());

        if (!vd.onstack) // if any destructors
            continue;
        // delete'ing C++ classes crashes (and delete is deprecated anyway)
        if (cd.classKind == ClassKind.cpp)
        {
            // Don't call non-existant dtor
            if (!cd.dtor)
                break;

            e = new VarExp(vd.loc, vd);
            e.type = e.type.mutableOf().unSharedOf(); // Hack for mutable ctor on immutable instances
            e = new DotVarExp(vd.loc, e, cd.dtor, false);
            e = new CallExp(vd.loc, e);
            break;
        }

        // delete this;
        Expression ec;
        ec = new VarExp(vd.loc, vd);
        e = new DeleteExp(vd.loc, ec, true);
        break;
    }
    return e;
}

/***************************************
 * Collect all instance fields, then determine instance size.
 * Returns:
 *      false if failed to determine the size.
 */
bool determineSize(AggregateDeclaration ad, Loc loc)
{
    //printf("AggregateDeclaration::determineSize() %s, sizeok = %d\n", toChars(), sizeok);

    // The previous instance size finalizing had:
    if (ad.type.ty == Terror || ad.errors)
        return false;   // failed already
    if (ad.sizeok == Sizeok.done)
        return true;    // succeeded

    if (!ad.members)
    {
        .error(loc, "%s `%s` unknown size", ad.kind, ad.toPrettyChars);
        return false;
    }

    if (ad._scope)
        dsymbolSemantic(ad, null);

    // Determine the instance size of base class first.
    if (auto cd = ad.isClassDeclaration())
    {
        cd = cd.baseClass;
        if (cd && !cd.determineSize(loc))
            goto Lfail;
    }

    // Determine instance fields when sizeok == Sizeok.none
    if (!ad.determineFields())
        goto Lfail;
    if (ad.sizeok != Sizeok.done)
        ad.finalizeSize();

    // this aggregate type has:
    if (ad.type.ty == Terror)
        return false;   // marked as invalid during the finalizing.
    if (ad.sizeok == Sizeok.done)
        return true;    // succeeded to calculate instance size.

Lfail:
    // There's unresolvable forward reference.
    if (ad.type != Type.terror)
        error(loc, "%s `%s` no size because of forward reference", ad.kind, ad.toPrettyChars);
    // Don't cache errors from speculative semantic, might be resolvable later.
    // https://issues.dlang.org/show_bug.cgi?id=16574
    if (!global.gag)
    {
        ad.type = Type.terror;
        ad.errors = true;
    }
    return false;
}

void addComment(Dsymbol d, const(char)* comment)
{
    scope v = new AddCommentVisitor(comment);
    d.accept(v);
}

extern (C++) class AddCommentVisitor: Visitor
{
    alias visit = Visitor.visit;

    const(char)* comment;

    this(const(char)* comment)
    {
        this.comment = comment;
    }

    override void visit(Dsymbol d)
    {
        if (!comment || !*comment)
            return;

        //printf("addComment '%s' to Dsymbol %p '%s'\n", comment, this, toChars());
        void* h = cast(void*)d;      // just the pointer is the key
        auto p = h in d.commentHashTable;
        if (!p)
        {
            d.commentHashTable[h] = comment;
            return;
        }
        if (strcmp(*p, comment) != 0)
        {
            // Concatenate the two
            *p = Lexer.combineComments((*p).toDString(), comment.toDString(), true);
        }
    }
    override void visit(AttribDeclaration atd)
    {
        if (comment)
        {
            atd.include(null).foreachDsymbol( s => s.addComment(comment) );
        }
    }
    override void visit(ConditionalDeclaration cd)
    {
        if (comment)
        {
            cd.decl    .foreachDsymbol( s => s.addComment(comment) );
            cd.elsedecl.foreachDsymbol( s => s.addComment(comment) );
        }
    }
    override void visit(StaticForeachDeclaration sfd) {}
}

void checkCtorConstInit(Dsymbol d)
{
    scope v = new CheckCtorConstInitVisitor();
    d.accept(v);
}

private extern(C++) class CheckCtorConstInitVisitor : Visitor
{
    alias visit = Visitor.visit;

    override void visit(AttribDeclaration ad)
    {
        ad.include(null).foreachDsymbol( s => s.checkCtorConstInit() );
    }

    override void visit(VarDeclaration vd)
    {
        version (none)
        {
            /* doesn't work if more than one static ctor */
            if (vd.ctorinit == 0 && vd.isCtorinit() && !vd.isField())
                error("missing initializer in static constructor for const variable");
        }
    }

    override void visit(Dsymbol d){}
}

/**************************************
* Determine if this symbol is only one.
* Returns:
*      false, ps = null: There are 2 or more symbols
*      true,  ps = null: There are zero symbols
*      true,  ps = symbol: The one and only one symbol
*/
bool oneMember(Dsymbol d, out Dsymbol ps, Identifier ident)
{
    scope v = new OneMemberVisitor(ps, ident);
    d.accept(v);
    return v.result;
}

private extern(C++) class OneMemberVisitor : Visitor
{
    alias visit = Visitor.visit;

    Dsymbol* ps;
    Identifier ident;
    bool result;

    this(out Dsymbol ps, Identifier ident)
    {
        this.ps = &ps;
        this.ident = ident;
    }

    override void visit(AttribDeclaration atb)
    {
        Dsymbols* d = atb.include(null);
        result = oneMembers(d, *ps, ident);
    }

    override void visit(StaticForeachDeclaration sfd)
    {
        // Required to support IFTI on a template that contains a
        // `static foreach` declaration.  `super.oneMember` calls
        // include with a `null` scope.  As `static foreach` requires
        // the scope for expansion, `oneMember` can only return a
        // precise result once `static foreach` has been expanded.
        if (sfd.cached)
        {
            this.visit(cast(AttribDeclaration) sfd);
        }
        else
        {
            *ps = null; // a `static foreach` declaration may in general expand to multiple symbols
            result = false;
        }
    }

    override void visit(StorageClassDeclaration scd)
    {
        bool t = oneMembers(scd.decl, *ps, ident);
        if (t && *ps)
        {
            /* This is to deal with the following case:
             * struct Tick {
             *   template to(T) { const T to() { ... } }
             * }
             * For eponymous function templates, the 'const' needs to get attached to 'to'
             * before the semantic analysis of 'to', so that template overloading based on the
             * 'this' pointer can be successful.
             */
            if (FuncDeclaration fd = (*ps).isFuncDeclaration())
            {
                /* Use storage_class2 instead of storage_class otherwise when we do .di generation
                 * we'll wind up with 'const const' rather than 'const'.
                 */
                /* Don't think we need to worry about mutually exclusive storage classes here
                 */
                fd.storage_class2 |= scd.stc;
            }
        }
        result = t;
    }

    override void visit(ConditionalDeclaration cd)
    {
        //printf("ConditionalDeclaration::oneMember(), inc = %d\n", condition.inc);
        if (cd.condition.inc != Include.notComputed)
        {
            Dsymbols* d = dmd.expressionsem.include(cd.condition, null) ? cd.decl : cd.elsedecl;
            result = oneMembers(d, *ps, ident);
        }
        else
        {
            bool res = (oneMembers(cd.decl, *ps, ident) && *ps is null && oneMembers(cd.elsedecl, *ps, ident) && *ps is null);
            *ps = null;
            result = res;
        }
    }

    override void visit(ScopeDsymbol sd)
    {
        if (sd.isAnonymous())
            result = oneMembers(sd.members, *ps, ident);
        else {
            // visit(Dsymbol dsym)
            *ps = sd;
            result = true;
        }
    }

    override void visit(StaticAssert sa)
    {
        //printf("StaticAssert::oneMember())\n");
        *ps = null;
        result = true;
    }

    override void visit(TemplateInstance ti)
    {
        *ps = null;
        result = true;
    }

    override void visit(TemplateMixin tm)
    {
        *ps = tm;
        result = true;
    }

    override void visit(Dsymbol dsym)
    {
        *ps = dsym;
        result = true;
    }
}

/****************************************
* Return true if any of the members are static ctors or static dtors, or if
* any members have members that are.
*/
bool hasStaticCtorOrDtor(Dsymbol d)
{
    scope v = new HasStaticCtorOrDtor();
    d.accept(v);
    return v.result;
}

private extern(C++) class HasStaticCtorOrDtor : Visitor
{
    import dmd.mtype : Type;

    alias visit = Visitor.visit;
    bool result;

    // attrib.d
    override void visit(AttribDeclaration ad){
        result = ad.include(null).foreachDsymbol( (s) { return s.hasStaticCtorOrDtor(); } ) != 0;
    }

    // dsymbol.d
    override void visit(Dsymbol d){
        //printf("Dsymbol::hasStaticCtorOrDtor() %s\n", toChars());
        result = false;
    }

    override void visit(ScopeDsymbol sd) {
        if (sd.members)
        {
            for (size_t i = 0; i < sd.members.length; i++)
            {
                Dsymbol member = (*(sd.members))[i];
                if (member.hasStaticCtorOrDtor())
                    result = true;
                    return;
            }
        }
        result = false;
    }

    // dtemplate.d
    override void visit(TemplateDeclaration td) {
        result = false; // don't scan uninstantiated templates
    }

    // func.d
    override void visit(StaticCtorDeclaration scd) {
        result = true;
    }

    override void visit(StaticDtorDeclaration sdd) @nogc nothrow pure @safe {
        result = true;
    }
}

bool isFuncHidden(ClassDeclaration cd, FuncDeclaration fd)
{
    import dmd.funcsem : overloadApply;
    //printf("ClassDeclaration.isFuncHidden(class = %s, fd = %s)\n", toChars(), fd.toPrettyChars());
    Dsymbol s = cd.search(Loc.initial, fd.ident, SearchOpt.ignoreAmbiguous | SearchOpt.ignoreErrors);
    if (!s)
    {
        //printf("not found\n");
        /* Because, due to a hack, if there are multiple definitions
            * of fd.ident, NULL is returned.
            */
        return false;
    }
    s = s.toAlias();
    if (auto os = s.isOverloadSet())
    {
        foreach (sm; os.a)
        {
            auto fm = sm.isFuncDeclaration();
            if (overloadApply(fm, s => fd == s.isFuncDeclaration()))
                return false;
        }
        return true;
    }
    else
    {
        auto f = s.isFuncDeclaration();
        //printf("%s fdstart = %p\n", s.kind(), fdstart);
        if (overloadApply(f, s => fd == s.isFuncDeclaration()))
            return false;
        return !fd.parent.isTemplateMixin();
    }
}

Dsymbol vtblSymbol(ClassDeclaration cd)
{
    if (!cd.vtblsym)
    {
        auto vtype = Type.tvoidptr.immutableOf().sarrayOf(cd.vtbl.length);
        auto var = new VarDeclaration(cd.loc, vtype, Identifier.idPool("__vtbl"), null, STC.immutable_ | STC.static_);
        var.addMember(null, cd);
        var.isdataseg = 1;
        var._linkage = LINK.d;
        var.semanticRun = PASS.semanticdone; // no more semantic wanted
        cd.vtblsym = var;
    }
    return cd.vtblsym;
}

bool isAbstract(ClassDeclaration cd)
{
    enum log = false;
    if (cd.isabstract != ThreeState.none)
        return cd.isabstract == ThreeState.yes;

    if (log) printf("isAbstract(%s)\n", cd.toChars());

    bool no()  { if (log) printf("no\n");  cd.isabstract = ThreeState.no;  return false; }
    bool yes() { if (log) printf("yes\n"); cd.isabstract = ThreeState.yes; return true;  }

    if (cd.storage_class & STC.abstract_ || cd._scope && cd._scope.stc & STC.abstract_)
        return yes();

    if (cd.errors)
        return no();

    /* https://issues.dlang.org/show_bug.cgi?id=11169
        * Resolve forward references to all class member functions,
        * and determine whether this class is abstract.
        */
    static int func(Dsymbol s, void*)
    {
        auto fd = s.isFuncDeclaration();
        if (!fd)
            return 0;
        if (fd.storage_class & STC.static_)
            return 0;

        if (fd.isAbstract())
            return 1;
        return 0;
    }

    // opaque class is not abstract if it is not declared abstract
    if (!(cd.members))
        return no();

    for (size_t i = 0; i < cd.members.length; i++)
    {
        auto s = (*(cd.members))[i];
        if (s.apply(&func, null))
        {
            return yes();
        }
    }

    /* If the base class is not abstract, then this class cannot
        * be abstract.
        */
    if (!cd.isInterfaceDeclaration() && (!cd.baseClass || !cd.baseClass.isAbstract()))
        return no();

    /* If any abstract functions are inherited, but not overridden,
        * then the class is abstract. Do this by checking the vtbl[].
        * Need to do semantic() on class to fill the vtbl[].
        */
    cd.dsymbolSemantic(null);

    /* The next line should work, but does not because when ClassDeclaration.dsymbolSemantic()
        * is called recursively it can set PASS.semanticdone without finishing it.
        */
    //if (semanticRun < PASS.semanticdone)
    {
        /* Could not complete semantic(). Try running semantic() on
            * each of the virtual functions,
            * which will fill in the vtbl[] overrides.
            */
        static int virtualSemantic(Dsymbol s, void*)
        {
            auto fd = s.isFuncDeclaration();
            if (fd && !(fd.storage_class & STC.static_) && !fd.isUnitTestDeclaration() && !fd.isCtorDeclaration())
                fd.dsymbolSemantic(null);
            return 0;
        }

        for (size_t i = 0; i < cd.members.length; i++)
        {
            auto s = (*(cd.members))[i];
            s.apply(&virtualSemantic,null);
        }
    }

    /* Finally, check the vtbl[]
        */
    foreach (i; 1 .. cd.vtbl.length)
    {
        auto fd = cd.vtbl[i].isFuncDeclaration();
        //if (fd) printf("\tvtbl[%d] = [%s] %s\n", i, fd.loc.toChars(), fd.toPrettyChars());
        if (!fd || fd.isAbstract())
        {
            return yes();
        }
    }

    return no();
}

void finalizeSize(AggregateDeclaration ad)
{
    scope v = new FinalizeSizeVisitor();
    ad.accept(v);
}

/**********************************
 * Determine if exp is all binary zeros.
 * Params:
 *      exp = expression to check
 * Returns:
 *      true if it's all binary 0
 */
bool _isZeroInit(Expression exp)
{
    import dmd.typesem: size;

    switch (exp.op)
    {
        case EXP.int64:
            return exp.toInteger() == 0;

        case EXP.null_:
            return true;

        case EXP.structLiteral:
        {
            auto sle = exp.isStructLiteralExp();
            if (sle.sd.isNested())
                return false;
            const isCstruct = sle.sd.isCsymbol();  // C structs are default initialized to all zeros
            foreach (i; 0 .. sle.sd.fields.length)
            {
                auto field = sle.sd.fields[i];
                if (field.type.size(field.loc))
                {
                    auto e = sle.elements && i < sle.elements.length ? (*sle.elements)[i] : null;
                    if (e ? !_isZeroInit(e)
                          : !isCstruct && !field.type.isZeroInit(field.loc))
                        return false;
                }
            }
            return true;
        }

        case EXP.arrayLiteral:
        {
            auto ale = cast(ArrayLiteralExp)exp;

            const dim = ale.elements ? ale.elements.length : 0;

            if (ale.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return dim == 0;

            foreach (i; 0 .. dim)
            {
                if (!_isZeroInit(ale[i]))
                    return false;
            }

            /* Note that true is returned for all T[0]
             */
            return true;
        }

        case EXP.string_:
        {
            auto se = cast(StringExp)exp;

            if (se.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return se.len == 0;

            foreach (i; 0 .. se.len)
            {
                if (se.getIndex(i) != 0)
                    return false;
            }
            return true;
        }

        case EXP.vector:
        {
            auto ve = cast(VectorExp) exp;
            return _isZeroInit(ve.e1);
        }

        case EXP.float64:
        case EXP.complex80:
        {
            import dmd.root.ctfloat : CTFloat;
            return (exp.toReal()      is CTFloat.zero) &&
                   (exp.toImaginary() is CTFloat.zero);
        }

        default:
            return false;
    }
}

/***************************************
 * Calculate `ad.field[i].overlapped` and `overlapUnsafe`, and check that all of explicit
 * field initializers have unique memory space on instance.
 * Returns:
 *      true if any errors happen.
 */
private bool checkOverlappedFields(AggregateDeclaration ad)
{
    //printf("AggregateDeclaration::checkOverlappedFields() %s\n", toChars());
    assert(ad.sizeok == Sizeok.done);
    size_t nfields = ad.fields.length;
    if (ad.isNested())
    {
        auto cd = ad.isClassDeclaration();
        if (!cd || !cd.baseClass || !cd.baseClass.isNested())
            nfields--;
        if (ad.vthis2 && !(cd && cd.baseClass && cd.baseClass.vthis2))
            nfields--;
    }
    bool errors = false;

    // Fill in missing any elements with default initializers
    foreach (i; 0 .. nfields)
    {
        auto vd = ad.fields[i];
        if (vd.errors)
        {
            errors = true;
            continue;
        }

        const vdIsVoidInit = vd._init && vd._init.isVoidInitializer();

        // Find overlapped fields with the hole [vd.offset .. vd.offset.size()].
        foreach (j; 0 .. nfields)
        {
            if (i == j)
                continue;
            auto v2 = ad.fields[j];
            if (v2.errors)
            {
                errors = true;
                continue;
            }
            if (!vd.isOverlappedWith(v2))
                continue;

            // vd and v2 are overlapping.
            vd.overlapped = true;
            v2.overlapped = true;

            if (!MODimplicitConv(vd.type.mod, v2.type.mod))
                v2.overlapUnsafe = true;
            if (!MODimplicitConv(v2.type.mod, vd.type.mod))
                vd.overlapUnsafe = true;

            if (i > j)
                continue;

            if (!v2._init)
                continue;

            if (v2._init.isVoidInitializer())
                continue;

            if (vd._init && !vdIsVoidInit && v2._init)
            {
                .error(ad.loc, "overlapping default initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                errors = true;
            }
            else if (v2._init && i < j)
            {
                .error(v2.loc, "union field `%s` with default initialization `%s` must be before field `%s`",
                    v2.toChars(), dmd.hdrgen.toChars(v2._init), vd.toChars());
                errors = true;
            }
        }
    }
    return errors;
}

private extern(C++) class FinalizeSizeVisitor : Visitor
{
    import dmd.typesem: size;

    alias visit = Visitor.visit;

    override void visit(ClassDeclaration outerCd)
    {
        assert(outerCd.sizeok != Sizeok.done);

        // Set the offsets of the fields and determine the size of the class
        if (outerCd.baseClass)
        {
            assert(outerCd.baseClass.sizeok == Sizeok.done);

            outerCd.alignsize = outerCd.baseClass.alignsize;
            if (outerCd.classKind == ClassKind.cpp)
                outerCd.structsize = target.cpp.derivedClassOffset(outerCd.baseClass);
            else
                outerCd.structsize = outerCd.baseClass.structsize;
        }
        else if (outerCd.classKind == ClassKind.objc)
            outerCd.structsize = 0; // no hidden member for an Objective-C class
        else if (outerCd.isInterfaceDeclaration())
        {
            if (outerCd.interfaces.length == 0)
            {
                outerCd.alignsize = target.ptrsize;
                outerCd.structsize = target.ptrsize;      // allow room for __vptr
            }
        }
        else
        {
            outerCd.alignsize = target.ptrsize;
            outerCd.structsize = target.ptrsize;      // allow room for __vptr
            if (outerCd.hasMonitor())
                outerCd.structsize += target.ptrsize; // allow room for __monitor
        }

        //printf("finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
        size_t bi = 0;                  // index into vtblInterfaces[]

        /****
            * Runs through the inheritance graph to set the BaseClass.offset fields.
            * Recursive in order to account for the size of the interface classes, if they are
            * more than just interfaces.
            * Params:
            *      cd = interface to look at
            *      baseOffset = offset of where cd will be placed
            * Returns:
            *      subset of instantiated size used by cd for interfaces
            */
        uint membersPlace(ClassDeclaration cd, uint baseOffset)
        {
            //printf("    membersPlace(%s, %d)\n", cd.toChars(), baseOffset);
            uint offset = baseOffset;

            foreach (BaseClass* b; cd.interfaces)
            {
                if (b.sym.sizeok != Sizeok.done)
                    b.sym.finalizeSize();
                assert(b.sym.sizeok == Sizeok.done);

                if (!b.sym.alignsize)
                    b.sym.alignsize = target.ptrsize;
                offset = alignmember(structalign_t(cast(ushort)b.sym.alignsize), b.sym.alignsize, offset);
                assert(bi < outerCd.vtblInterfaces.length);

                BaseClass* bv = (*(outerCd.vtblInterfaces))[bi];
                if (b.sym.interfaces.length == 0)
                {
                    //printf("\tvtblInterfaces[%d] b=%p b.sym = %s, offset = %d\n", bi, bv, bv.sym.toChars(), offset);
                    bv.offset = offset;
                    ++bi;
                    // All the base interfaces down the left side share the same offset
                    for (BaseClass* b2 = bv; b2.baseInterfaces.length; )
                    {
                        b2 = &b2.baseInterfaces[0];
                        b2.offset = offset;
                        //printf("\tvtblInterfaces[%d] b=%p   sym = %s, offset = %d\n", bi, b2, b2.sym.toChars(), b2.offset);
                    }
                }
                membersPlace(b.sym, offset);
                //printf(" %s size = %d\n", b.sym.toChars(), b.sym.structsize);
                offset += b.sym.structsize;
                if (outerCd.alignsize < b.sym.alignsize)
                    outerCd.alignsize = b.sym.alignsize;
            }
            return offset - baseOffset;
        }

        outerCd.structsize += membersPlace(outerCd, outerCd.structsize);

        if (outerCd.isInterfaceDeclaration())
        {
            outerCd.sizeok = Sizeok.done;
            return;
        }

        // FIXME: Currently setFieldOffset functions need to increase fields
        // to calculate each variable offsets. It can be improved later.
        outerCd.fields.setDim(0);

        FieldState fieldState;
        fieldState.offset = outerCd.structsize;
        foreach (s; *(outerCd.members))
        {
            s.setFieldOffset(outerCd, &fieldState, false);
        }

        outerCd.sizeok = Sizeok.done;

        // Calculate fields[i].overlapped
        outerCd.checkOverlappedFields();
    }

    override void visit(StructDeclaration sd)
    {
        //printf("StructDeclaration::finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
        assert(sd.sizeok != Sizeok.done);

        if (sd.sizeok == Sizeok.inProcess)
        {
            return;
        }
        sd.sizeok = Sizeok.inProcess;

        //printf("+StructDeclaration::finalizeSize() %s, fields.length = %d, sizeok = %d\n", toChars(), fields.length, sizeok);

        sd.fields.setDim(0);   // workaround

        // Set the offsets of the fields and determine the size of the struct
        FieldState fieldState;
        bool isunion = sd.isUnionDeclaration() !is null;
        for (size_t i = 0; i < sd.members.length; i++)
        {
            Dsymbol s = (*sd.members)[i];
            s.setFieldOffset(sd, &fieldState, isunion);
            if (sd.type.ty == Terror)
            {
                errorSupplemental(s.loc, "error on member `%s`", s.toPrettyChars);
                sd.errors = true;
                return;
            }
        }

        if (sd.structsize == 0)
        {
            sd.hasNoFields = true;
            sd.alignsize = 1;

            // A fine mess of what size a zero sized struct should be
            final switch (sd.classKind)
            {
                case ClassKind.d:
                case ClassKind.cpp:
                    sd.structsize = 1;
                    break;

                case ClassKind.c:
                case ClassKind.objc:
                    if (target.c.bitFieldStyle == TargetC.BitFieldStyle.MS)
                    {
                        /* Undocumented MS behavior for:
                         *   struct S { int :0; };
                         */
                        sd.structsize = 4;
                    }
                    else
                        sd.structsize = 0;
                    break;
            }
        }

        // Round struct size up to next alignsize boundary.
        // This will ensure that arrays of structs will get their internals
        // aligned properly.
        if (sd.alignment.isDefault() || sd.alignment.isPack())
            sd.structsize = (sd.structsize + sd.alignsize - 1) & ~(sd.alignsize - 1);
        else
            sd.structsize = (sd.structsize + sd.alignment.get() - 1) & ~(sd.alignment.get() - 1);

        sd.sizeok = Sizeok.done;

        //printf("-StructDeclaration::finalizeSize() %s, fields.length = %d, structsize = %d\n", toChars(), cast(int)fields.length, cast(int)structsize);

        if (sd.errors)
            return;

        // Calculate fields[i].overlapped
        if (sd.checkOverlappedFields())
        {
            sd.errors = true;
            return;
        }

        // Determine if struct is all zeros or not
        sd.zeroInit = true;
        auto lastOffset = -1;
        foreach (vd; sd.fields)
        {
            // First skip zero sized fields
            if (vd.type.size(vd.loc) == 0)
                continue;

            // only consider first sized member of an (anonymous) union
            if (vd.overlapped && vd.offset == lastOffset)
                continue;
            lastOffset = vd.offset;

            if (vd._init)
            {
                if (vd._init.isVoidInitializer())
                    /* Treat as 0 for the purposes of putting the initializer
                     * in the BSS segment, or doing a mass set to 0
                     */
                    continue;

                // Examine init to see if it is all 0s.
                auto exp = vd.getConstInitializer();
                if (!exp || !_isZeroInit(exp))
                {
                    sd.zeroInit = false;
                    break;
                }
            }
            else if (!vd.type.isZeroInit(sd.loc))
            {
                sd.zeroInit = false;
                break;
            }
        }


        sd.argTypes = target.toArgTypes(sd.type);
    }
}

/*****************************************
* Is Dsymbol a variable that contains pointers?
*/
bool hasPointers(Dsymbol d)
{
    scope v = new HasPointersVisitor();
    d.accept(v);
    return v.result;
}

private extern(C++) class HasPointersVisitor : Visitor
{
    import dmd.mtype : Type;

    alias visit = Visitor.visit;
    bool result;

    override void visit(AttribDeclaration ad)
    {
        result = ad.include(null).foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }

    override void visit(VarDeclaration vd)
    {
        import dmd.typesem : hasPointers;
        result = (!vd.isDataseg() && vd.type.hasPointers());
    }

    override void visit(Dsymbol d)
    {
        //printf("Dsymbol::hasPointers() %s\n", toChars());
        result = false;
    }

    override void visit(TemplateMixin tm)
    {
        //printf("TemplateMixin.hasPointers() %s\n", toChars());
        result = tm.members.foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }

    override void visit(Nspace ns)
    {
        //printf("Nspace::hasPointers() %s\n", toChars());
        result = ns.members.foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }
}

/***************************************
 * Expands attribute declarations in members in depth first
 * order. Calls dg(size_t symidx, Dsymbol *sym) for each
 * member.
 * If dg returns !=0, stops and returns that value else returns 0.
 * Use this function to avoid the O(N + N^2/2) complexity of
 * calculating dim and calling N times getNth.
 * Returns:
 *  last value returned by dg()
 */
int _foreach(Scope* sc, Dsymbols* members, scope ForeachDg dg, size_t* pn = null)
{
    assert(dg);
    if (!members)
        return 0;
    size_t n = pn ? *pn : 0; // take over index
    int result = 0;
    foreach (size_t i; 0 .. members.length)
    {
        import dmd.attrib : AttribDeclaration;
        import dmd.dtemplate : TemplateMixin;

        Dsymbol s = (*members)[i];
        if (AttribDeclaration a = s.isAttribDeclaration())
            result = _foreach(sc, a.include(sc), dg, &n);
        else if (TemplateMixin tm = s.isTemplateMixin())
            result = _foreach(sc, tm.members, dg, &n);
        else if (s.isTemplateInstance())
        {
        }
        else if (s.isUnitTestDeclaration())
        {
        }
        else
            result = dg(n++, s);
        if (result)
            break;
    }
    if (pn)
        *pn = n; // update index
    return result;
}

/****************************************
 * Create array of the local classes in the Module, suitable
 * for inclusion in ModuleInfo
 * Params:
 *      mod = the Module
 *      aclasses = array to fill in
 * Returns: array of local classes
 */
void getLocalClasses(Module mod, ref ClassDeclarations aclasses)
{
    //printf("members.length = %d\n", mod.members.length);
    int pushAddClassDg(size_t n, Dsymbol sm)
    {
        if (!sm)
            return 0;

        if (auto cd = sm.isClassDeclaration())
        {
            // compatibility with previous algorithm
            if (cd.parent && cd.parent.isTemplateMixin())
                return 0;

            if (cd.classKind != ClassKind.objc)
                aclasses.push(cd);
        }
        return 0;
    }

    _foreach(null, mod.members, &pushAddClassDg);
}

/*****************************************
* Same as Dsymbol::oneMember(), but look at an array of Dsymbols.
*/
extern (D) bool oneMembers(Dsymbols* members, out Dsymbol ps, Identifier ident)
{
    //printf("Dsymbol::oneMembers() %d\n", members ? members.length : 0);
    Dsymbol s = null;
    if (!members)
    {
        ps = null;
        return true;
    }

    for (size_t i = 0; i < members.length; i++)
    {
        Dsymbol sx = (*members)[i];
        bool x = sx.oneMember(ps, ident);
        //printf("\t[%d] kind %s = %d, s = %p\n", i, sx.kind(), x, *ps);
        if (!x)
        {
            //printf("\tfalse 1\n");
            assert(ps is null);
            return false;
        }
        if (!ps)
            continue;

        assert(ident);
        if (!ps.ident || !ps.ident.equals(ident))
            continue;
        if (!s)
            s = ps;
        else if (s.isOverloadable() && ps.isOverloadable())
        {
            // keep head of overload set
            FuncDeclaration f1 = s.isFuncDeclaration();
            FuncDeclaration f2 = ps.isFuncDeclaration();
            if (f1 && f2)
            {
                assert(!f1.isFuncAliasDeclaration());
                assert(!f2.isFuncAliasDeclaration());
                for (; f1 != f2; f1 = f1.overnext0)
                {
                    if (f1.overnext0 is null)
                    {
                        f1.overnext0 = f2;
                        break;
                    }
                }
            }
        }
        else // more than one symbol
        {
            ps = null;
            //printf("\tfalse 2\n");
            return false;
        }
    }
    ps = s; // s is the one symbol, null if none
    //printf("\ttrue\n");
    return true;
}
