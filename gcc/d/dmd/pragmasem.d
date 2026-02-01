/**
 * Does semantic analysis for pragmas.
 *
 * Specification: $(LINK2 https://dlang.org/spec/pragma.html, Pragmas)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/pragmasem.d, _pragmasem.d)
 * Documentation:  https://dlang.org/phobos/dmd_pragmasem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/pragmasem.d
 */

module dmd.pragmasem;

import core.stdc.stdio;
import core.stdc.string;

import dmd.astenums;
import dmd.arraytypes;
import dmd.attrib;
import dmd.dinterpret;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem : include;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.globals;
import dmd.location;
import dmd.id;
import dmd.statement;

/**
 * Run semantic on `pragma` declaration.
 *
 * Params:
 *  pd = pragma declaration or statement to evaluate
 *  sc = enclosing scope
 */
void pragmaDeclSemantic(PragmaDeclaration pd, Scope* sc)
{
    import dmd.common.outbuffer;
    import dmd.dmodule;
    import dmd.dsymbolsem;
    import dmd.identifier;
    import dmd.root.rmem;
    import dmd.target;
    import dmd.utils;

    void declarations()
    {
        if (!pd.decl)
            return;

        Scope* sc2 = pd.newScope(sc);
        scope(exit)
            if (sc2 != sc)
                sc2.pop();

        foreach (s; (*pd.decl)[])
        {
            if (pd.ident == Id.printf || pd.ident == Id.scanf)
            {
                s.setPragmaPrintf(pd.ident == Id.printf);
                s.dsymbolSemantic(sc2);
                continue;
            }

            s.dsymbolSemantic(sc2);
        }
    }

    void noDeclarations()
    {
        if (pd.decl)
        {
            .error(pd.loc, "%s `%s` is missing a terminating `;`", pd.kind, pd.toPrettyChars);
            declarations();
            // do them anyway, to avoid segfaults.
        }
    }

    // Should be merged with PragmaStatement
    //printf("\tpragmaDeclSemantic '%s'\n", pd.toChars());
    if (target.supportsLinkerDirective())
    {
        if (pd.ident == Id.linkerDirective)
        {
            if (!pd.args || pd.args.length != 1)
                .error(pd.loc, "%s `%s` one string argument expected for pragma(linkerDirective)", pd.kind, pd.toPrettyChars);
            else
            {
                auto se = semanticString(sc, (*pd.args)[0], "linker directive");
                if (!se)
                    return noDeclarations();
                (*pd.args)[0] = se;
                if (global.params.v.verbose)
                    message("linkopt   %.*s", cast(int)se.len, se.peekString().ptr);
            }
            return noDeclarations();
        }
    }
    if (pd.ident == Id.msg)
    {
        if (!pd.args)
            return noDeclarations();

        if (!pragmaMsgSemantic(pd.loc, sc, pd.args))
            return;

        return noDeclarations();
    }
    else if (pd.ident == Id.lib)
    {
        if (!pd.args || pd.args.length != 1)
            .error(pd.loc, "%s `%s` string expected for library name", pd.kind, pd.toPrettyChars);
        else
        {
            auto se = semanticString(sc, (*pd.args)[0], "library name");
            if (!se)
                return noDeclarations();
            (*pd.args)[0] = se;

            auto name = se.peekString().xarraydup;
            if (global.params.v.verbose)
                message("library   %s", name.ptr);
            if (global.params.moduleDeps.buffer && !global.params.moduleDeps.name)
            {
                OutBuffer* ob = global.params.moduleDeps.buffer;
                Module imod = sc._module;
                ob.writestring("depsLib ");
                ob.writestring(imod.toPrettyChars());
                ob.writestring(" (");
                escapePath(ob, imod.srcfile.toChars());
                ob.writestring(") : ");
                ob.writestring(name);
                ob.writenl();
            }
            mem.xfree(name.ptr);
        }
        return noDeclarations();
    }
    else if (pd.ident == Id.startaddress)
    {
        pragmaStartAddressSemantic(pd.loc, sc, pd.args);
        return noDeclarations();
    }
    else if (pd.ident == Id.Pinline)
    {
        // this pragma now gets evaluated on demand in function semantic

        return declarations();
    }
    else if (pd.ident == Id.mangle)
    {
        Scope* sc2 = pd.newScope(sc);
        pragmaMangleSemantic(pd.loc, sc2, pd.args, pd.decl);
        if (sc2 != sc)
            sc2.pop();
        return;
    }
    else if (pd.ident == Id.crt_constructor || pd.ident == Id.crt_destructor)
    {
        if (pd.args && pd.args.length != 0)
            .error(pd.loc, "%s `%s` takes no argument", pd.kind, pd.toPrettyChars);
        else
        {
            immutable isCtor = pd.ident == Id.crt_constructor;

            static uint recurse(Dsymbol s, bool isCtor)
            {
                if (auto ad = s.isAttribDeclaration())
                {
                    uint nestedCount;
                    auto decls = ad.include(null);
                    if (decls)
                    {
                        for (size_t i = 0; i < decls.length; ++i)
                            nestedCount += recurse((*decls)[i], isCtor);
                    }
                    return nestedCount;
                }
                else if (auto f = s.isFuncDeclaration())
                {
                    if (isCtor)
                        f.isCrtCtor = true;
                    else
                        f.isCrtDtor = true;

                    return 1;
                }
                else
                    return 0;
                assert(0);
            }

            if (recurse(pd, isCtor) > 1)
                .error(pd.loc, "%s `%s` can only apply to a single declaration", pd.kind, pd.toPrettyChars);
        }
        return declarations();
    }
    else if (pd.ident == Id.printf || pd.ident == Id.scanf)
    {
        if (pd.args && pd.args.length != 0)
            .error(pd.loc, "%s `%s` takes no argument", pd.kind, pd.toPrettyChars);
        return declarations();
    }
    else if (!global.params.ignoreUnsupportedPragmas)
    {
        error(pd.loc, "unrecognized `pragma(%s)`", pd.ident.toChars());
        return declarations();
    }

    if (!global.params.v.verbose)
        return declarations();

    /* Print unrecognized pragmas
     */
    OutBuffer buf;
    buf.writestring(pd.ident.toString());
    if (pd.args)
    {
        const errors_save = global.startGagging();
        for (size_t i = 0; i < pd.args.length; i++)
        {
            Expression e = (*pd.args)[i];
            sc = sc.startCTFE();
            e = e.expressionSemantic(sc);
            e = resolveProperties(sc, e);
            sc = sc.endCTFE();
            e = e.ctfeInterpret();
            if (i == 0)
                buf.writestring(" (");
            else
                buf.writeByte(',');
            buf.writestring(e.toChars());
        }
        if (pd.args.length)
            buf.writeByte(')');
        global.endGagging(errors_save);
    }
    message("pragma    %s", buf.peekChars());
    return declarations();
}

/**
 * Run semantic on `pragma` statement.
 *
 * Params:
 *  ps = pragma statement to evaluate
 *  sc = enclosing scope
 *
 * Returns : true if `pragma` is valid, or false if an error was found
 */
bool pragmaStmtSemantic(PragmaStatement ps, Scope* sc)
{
    import dmd.statementsem;

    /* https://dlang.org/spec/statement.html#pragma-statement
     */
    // Should be merged with PragmaDeclaration

    //printf("pragmaStmtSemantic() %s\n", ps.toChars());
    //printf("body = %p\n", ps._body);
    if (ps.ident == Id.msg)
    {
        if (!pragmaMsgSemantic(ps.loc, sc, ps.args))
            return false;
    }
    else if (ps.ident == Id.lib)
    {
        version (all)
        {
            /* Should this be allowed?
             */
            error(ps.loc, "`pragma(lib)` not allowed as statement");
            return false;
        }
        else
        {
            if (!ps.args || ps.args.length != 1)
            {
                error(ps.loc, "`string` expected for library name");
                return false;
            }
            else
            {
                auto se = semanticString(sc, (*ps.args)[0], "library name");
                if (!se)
                    return false;

                if (global.params.v.verbose)
                {
                    message("library   %.*s", cast(int)se.len, se.string);
                }
            }
        }
    }
    else if (ps.ident == Id.linkerDirective)
    {
        /* Should this be allowed?
         */
        error(ps.loc, "`pragma(linkerDirective)` not allowed as statement");
        return false;
    }
    else if (ps.ident == Id.startaddress)
    {
        if (!pragmaStartAddressSemantic(ps.loc, sc, ps.args))
            return false;
    }
    else if (ps.ident == Id.Pinline)
    {
        if (auto fd = sc.func)
        {
            fd.inlining = evalPragmaInline(ps.loc, sc, ps.args);
        }
        else
        {
            error(ps.loc, "`pragma(inline)` is not inside a function");
            return false;
        }
    }
    else if (ps.ident == Id.mangle)
    {
        auto es = ps._body ? ps._body.isExpStatement() : null;
        auto de = es ? es.exp.isDeclarationExp() : null;
        Dsymbols decls = de ? Dsymbols(de.declaration) : Dsymbols();
        if (!pragmaMangleSemantic(ps.loc, sc, ps.args, decls.length ? &decls : null))
            return false;
    }
    else if (!global.params.ignoreUnsupportedPragmas)
    {
        error(ps.loc, "unrecognized `pragma(%s)`", ps.ident.toChars());
        return false;
    }

    if (ps._body)
    {
        if (ps.ident == Id.msg || ps.ident == Id.startaddress)
        {
            error(ps.loc, "`pragma(%s)` is missing a terminating `;`", ps.ident.toChars());
            return false;
        }
        ps._body = ps._body.statementSemantic(sc);
    }
    return true;
}

/***************************************
 * Interpret a `pragma(inline, x)`
 *
 * Params:
 *   loc = location for error messages
 *   sc = scope for evaluation of argument
 *   args = pragma arguments
 * Returns: corresponding `PINLINE` state
 */
package PINLINE evalPragmaInline(Loc loc, Scope* sc, Expressions* args)
{
    if (!args || args.length == 0)
        return PINLINE.default_;

    if (args && args.length > 1)
    {
        .error(loc, "one boolean expression expected for `pragma(inline)`, not %llu", cast(ulong) args.length);
        args.setDim(1);
        (*args)[0] = ErrorExp.get();
    }

    Expression e = (*args)[0];
    if (!e.type)
    {
        sc = sc.startCTFE();
        e = e.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        sc = sc.endCTFE();
        e = e.ctfeInterpret();
        e = e.toBoolean(sc);
        if (e.isErrorExp())
            .error(loc, "pragma(`inline`, `true` or `false`) expected, not `%s`", (*args)[0].toChars());
        (*args)[0] = e;
    }

    const opt = e.toBool();
    if (opt.isEmpty())
        return PINLINE.default_;
    if (opt.get())
        return PINLINE.always;
    return PINLINE.never;
}

/***********************************************************
 * Evaluate and print a `pragma(msg, args)`
 *
 * Params:
 *    loc = location for error messages
 *    sc = scope for argument interpretation
 *    args = expressions to print
 * Returns:
 *    `true` on success
 */
private bool pragmaMsgSemantic(Loc loc, Scope* sc, Expressions* args)
{
    import dmd.tokens;
    import dmd.common.outbuffer;

    if (!args)
        return true;

    OutBuffer buf;
    if (expressionsToString(buf, sc, args, loc, "while evaluating `pragma(msg, %s)`", false))
        return false;

    buf.writestring("\n");
    fprintf(stderr, "%s", buf.extractChars);
    return true;
}

/**
 * Apply pragma printf/scanf to FuncDeclarations under `s`,
 * poking through attribute declarations such as `extern(C)`
 * but not through aggregates or function bodies.
 *
 * Params:
 *    s = symbol to apply
 *    printf = `true` for printf, `false` for scanf
 */
private void setPragmaPrintf(Dsymbol s, bool printf)
{
    if (auto fd = s.isFuncDeclaration())
    {
        fd.printf = printf;
        fd.scanf = !printf;
    }

    if (auto ad = s.isAttribDeclaration())
    {
        ad.include(null).foreachDsymbol( (s) { setPragmaPrintf(s, printf); } );
    }
}

/***********************************************************
 * Evaluate `pragma(startAddress, func)` and store the resolved symbol in `args`
 *
 * Params:
 *    loc = location for error messages
 *    sc = scope for argument interpretation
 *    args = pragma arguments
 * Returns:
 *    `true` on success
 */
private bool pragmaStartAddressSemantic(Loc loc, Scope* sc, Expressions* args)
{
    import dmd.dtemplate;

    if (!args || args.length != 1)
    {
        .error(loc, "function name expected for start address");
        return false;
    }
    else
    {
        /* https://issues.dlang.org/show_bug.cgi?id=11980
         * resolveProperties and ctfeInterpret call are not necessary.
         */
        Expression e = (*args)[0];
        sc = sc.startCTFE();
        e = e.expressionSemantic(sc);
        // e = resolveProperties(sc, e);
        sc = sc.endCTFE();

        // e = e.ctfeInterpret();
        (*args)[0] = e;
        Dsymbol sa = getDsymbol(e);
        if (!sa || !sa.isFuncDeclaration())
        {
            .error(loc, "function name expected for start address, not `%s`", e.toChars());
            return false;
        }
    }
    return true;
}

/***********************************************************
 * Evaluate `pragma(mangle)` and store the mangled string in `decls`
 * This accepts any of the following variants.
 *
 *      pragma(mangle, StringExp) AggregateDeclaration
 *      pragma(mangle, StringExp) VarDeclaration
 *      pragma(mangle, StringExp) FuncDeclaration
 *      pragma(mangle, TypeExp) AggregateDeclaration
 *      pragma(mangle, TypeExp, StringExp) AggregateDeclaration
 *      pragma(mangle, StringExp, TypeExp) AggregateDeclaration
 *
 * Params:
 *    loc = location for error messages
 *    sc = scope for argument interpretation
 *    args = pragma arguments
 *    decls = declarations to set mangled string to
 * Returns:
 *    `true` on success
 */
private bool pragmaMangleSemantic(Loc loc, Scope* sc, Expressions* args, Dsymbols* decls)
{
    import dmd.root.rmem;

    StringExp verifyMangleString(ref Expression e)
    {
        import dmd.mangle : isValidMangling;
        import dmd.root.utf : utf_decodeChar;
        auto se = semanticString(sc, e, "pragma mangle argument");
        if (!se)
            return null;
        e = se;
        if (!se.len)
        {
            error(loc, "`pragma(mangle)` zero-length string not allowed for mangled name");
            return null;
        }
        if (se.sz != 1)
        {
            error(loc, "`pragma(mangle)` mangled name characters can only be of type `char`");
            return null;
        }
        auto slice = se.toStringz();
        if (strlen(slice.ptr) != se.len)
            .error(loc, "pragma `mangle` null character not allowed in mangled name");
        mem.xfree(cast(void*)slice.ptr);
        return se;
    }

    bool applyPragmaMangle(Dsymbols* decls, ref uint count, ref bool ignored)
    {
        if (decls is null)
            return true;

        foreach (s; (*decls)[])
        {
            import dmd.aggregate;
            import dmd.common.outbuffer;
            import dmd.dsymbolsem : dsymbolSemantic;
            import dmd.hdrgen : arrayObjectsToBuffer;
            import dmd.identifier : Identifier;

            s.dsymbolSemantic(sc);

            if (auto ad = s.isAggregateDeclaration())
            {
                /* pragma(mangle) AggregateDeclaration;
                   For aggregates there may be one or two AssignExpressions:
                   - one of which must evaluate at compile time to a string literal
                   - one which must evaluate to a symbol
                   The spec does not specify which order these should be in, so we
                   allow any of:
                   pragma(mangle, "name") struct { ... }
                   pragma(mangle, T)      struct { ... }
                   pragma(mangle, "name", T) struct { ... }
                   pragma(mangle, T, "name") struct { ... }
                 */
                AggregateDeclaration symbol;
                StringExp literal;

                foreach (ref Expression e; (*args)[])
                {
                    sc = sc.startCTFE();
                    e = e.expressionSemantic(sc);
                    e = resolveProperties(sc, e);
                    sc = sc.endCTFE();

                    bool expectedString()
                    {
                        error(e.loc, "`string` expected for pragma mangle argument, not `%s` of type `%s`",
                              e.toChars(), e.type.toChars());
                        return false;
                    }

                    bool expectedType()
                    {
                        error(e.loc, "`class` or `struct` type expected for pragma mangle argument, not `%s` of type `%s`",
                              e.toChars(), e.type.toChars());
                        return false;
                    }

                    // Validate arguments to pragma(mangle)
                    if (e.isTypeExp())
                    {
                        /* Check for and reject:
                           pragma(mangle, TypeExp, TypeExp)
                           where the first TypeExp already resolved to a symbol. */
                        if (symbol)
                            return expectedString();

                        /* Type must be a class or struct symbol. */
                        if (auto tc = e.type.isTypeClass())
                            symbol = tc.sym;
                        else if (auto ts = e.type.isTypeStruct())
                            symbol = ts.sym;
                        else
                            return expectedType();
                    }
                    else
                    {
                        /* Check for and reject:
                           pragma(mangle, StringExp, AssignExpression)
                           where AssignExpression did not resolve to a symbol. */
                        if (literal)
                            return expectedType();

                        /* Must evaluate to a compile time string literal. */
                        auto se = verifyMangleString(e);
                        if (se is null)
                            return false;
                        literal = se;
                    }
                }

                ad.pMangleOverride = new MangleOverride;

                if (symbol)
                {
                    ad.pMangleOverride.agg = symbol;
                    /* The identifier of the symbol is used when no string is supplied. */
                    if (literal is null)
                    {
                        ad.pMangleOverride.id = symbol.ident;
                        count += 1;
                        continue;
                    }
                }

                assert(literal);
                const name = literal.peekString().xarraydup;
                ad.pMangleOverride.id = Identifier.idPool(name);
                count += 1;
            }
            else if (auto td = s.isTemplateDeclaration())
            {
                /* pragma(mangle) TemplateDeclaration
                   Give an informative error message to avoid pragma(mangle)
                   silently ignoring the template symbol. */
                error(loc, "`pragma(mangle)` cannot apply to a template declaration");
                OutBuffer buf;
                buf.arrayObjectsToBuffer(cast(Objects*)args);
                errorSupplemental(loc, "use `template %s(Args...) { pragma(mangle, %s) ... }`", td.ident.toChars(), buf.peekChars());
                return false;
            }
            else if (auto ad = s.isAttribDeclaration())
            {
                /* pragma(mangle) AttribDeclaration
                   Poke through the attribute to get to the underlying declaration. */
                if (!applyPragmaMangle(ad.include(null), count, ignored))
                    return false;
            }
            else if (s.isFuncDeclaration() || s.isVarDeclaration())
            {
                /* pragma(mangle) Declaration;
                   For all other symbols, there must be one AssignExpression and it
                   must evaluate at compile time to a string literal. */
                if (args.length != 1)
                {
                    error(loc, "`pragma(mangle)` takes a single argument that must be a string literal");
                    return false;
                }
                auto se = verifyMangleString((*args)[0]);
                if (!se)
                    return false;

                const name = se.peekString().xarraydup;
                s.isDeclaration().mangleOverride = name;
                count += 1;
            }
            else
            {
                /* pragma(mangle) only applies to function and variable
                   symbols. Other symbols are ignored. */
                ignored = true;
            }
        }
        return true;
    }

    if (args is null)
    {
        error(loc, "`pragma(mangle)` expects string literal argument for mangled name");
        return false;
    }
    if (args.length > 2)
    {
        error(loc, "`pragma(mangle)` expects 1 or 2 arguments");
        return false;
    }

    uint count = 0;
    bool ignored = false;
    if (!applyPragmaMangle(decls, count, ignored))
        return false;

    if (count == 0 && !ignored)
    {
        error(loc, "`pragma(mangle)` must be attached to a declaration");
        return false;
    }
    if (count > 1)
    {
        error(loc, "`pragma(mangle)` can only apply to a single declaration");
        return false;
    }
    return true;
}
