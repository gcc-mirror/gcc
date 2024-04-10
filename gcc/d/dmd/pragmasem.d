/**
 * Does semantic analysis for pragmas.
 *
 * Specification: $(LINK2 https://dlang.org/spec/pragma.html, Pragmas)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/pragmasem.d, _pragmasem.d)
 * Documentation:  https://dlang.org/phobos/dmd_pragmasem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/pragmasem.d
 */

module dmd.pragmasem;

import core.stdc.stdio;

import dmd.astenums;
import dmd.arraytypes;
import dmd.attrib;
import dmd.dinterpret;
import dmd.dscope;
import dmd.dsymbol;
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
    import dmd.aggregate;
    import dmd.common.outbuffer;
    import dmd.dmangle;
    import dmd.dmodule;
    import dmd.dsymbolsem;
    import dmd.identifier;
    import dmd.root.rmem;
    import dmd.root.utf;
    import dmd.target;
    import dmd.utils;

    StringExp verifyMangleString(ref Expression e)
    {
        auto se = semanticString(sc, e, "mangled name");
        if (!se)
            return null;
        e = se;
        if (!se.len)
        {
            .error(pd.loc, "%s `%s` - zero-length string not allowed for mangled name", pd.kind, pd.toPrettyChars);
            return null;
        }
        if (se.sz != 1)
        {
            .error(pd.loc, "%s `%s` - mangled name characters can only be of type `char`", pd.kind, pd.toPrettyChars);
            return null;
        }
        version (all)
        {
            /* Note: D language specification should not have any assumption about backend
             * implementation. Ideally pragma(mangle) can accept a string of any content.
             *
             * Therefore, this validation is compiler implementation specific.
             */
            auto slice = se.peekString();
            for (size_t i = 0; i < se.len;)
            {
                dchar c = slice[i];
                if (c < 0x80)
                {
                    if (c.isValidMangling)
                    {
                        ++i;
                        continue;
                    }
                    else
                    {
                        .error(pd.loc, "%s `%s` char 0x%02x not allowed in mangled name", pd.kind, pd.toPrettyChars, c);
                        break;
                    }
                }
                if (const msg = utf_decodeChar(slice, i, c))
                {
                    .error(pd.loc, "%s `%s` %.*s", pd.kind, pd.toPrettyChars, cast(int)msg.length, msg.ptr);
                    break;
                }
                if (!isUniAlpha(c))
                {
                    .error(pd.loc, "%s `%s` char `0x%04x` not allowed in mangled name", pd.kind, pd.toPrettyChars, c);
                    break;
                }
            }
        }
        return se;
    }
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
            if (pd.ident != Id.mangle)
                continue;
            assert(pd.args);
            if (auto ad = s.isAggregateDeclaration())
            {
                Expression e = (*pd.args)[0];
                sc2 = sc2.startCTFE();
                e = e.expressionSemantic(sc);
                e = resolveProperties(sc2, e);
                sc2 = sc2.endCTFE();
                AggregateDeclaration agg;
                if (auto tc = e.type.isTypeClass())
                    agg = tc.sym;
                else if (auto ts = e.type.isTypeStruct())
                    agg = ts.sym;
                ad.pMangleOverride = new MangleOverride;
                void setString(ref Expression e)
                {
                    if (auto se = verifyMangleString(e))
                    {
                        const name = (cast(const(char)[])se.peekData()).xarraydup;
                        ad.pMangleOverride.id = Identifier.idPool(name);
                        e = se;
                    }
                    else
                        error(e.loc, "must be a string");
                }
                if (agg)
                {
                    ad.pMangleOverride.agg = agg;
                    if (pd.args.length == 2)
                    {
                        setString((*pd.args)[1]);
                    }
                    else
                        ad.pMangleOverride.id = agg.ident;
                }
                else
                    setString((*pd.args)[0]);
            }
            else if (auto td = s.isTemplateDeclaration())
            {
                .error(pd.loc, "%s `%s` cannot apply to a template declaration", pd.kind, pd.toPrettyChars);
                errorSupplemental(pd.loc, "use `template Class(Args...){ pragma(mangle, \"other_name\") class Class {} }`");
            }
            else if (auto se = verifyMangleString((*pd.args)[0]))
            {
                const name = (cast(const(char)[])se.peekData()).xarraydup;
                uint cnt = setMangleOverride(s, name);
                if (cnt > 1)
                    .error(pd.loc, "%s `%s` can only apply to a single declaration", pd.kind, pd.toPrettyChars);
            }
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
        if (!pd.args)
            pd.args = new Expressions();
        if (pd.args.length == 0 || pd.args.length > 2)
        {
            .error(pd.loc, pd.args.length == 0 ? "%s `%s` - string expected for mangled name"
                                      : "%s `%s` expected 1 or 2 arguments", pd.kind, pd.toPrettyChars);
            pd.args.setDim(1);
            (*pd.args)[0] = ErrorExp.get(); // error recovery
        }
        return declarations();
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
        if (!de)
        {
            error(ps.loc, "`pragma(mangle)` must be attached to a declaration");
            return false;
        }
        const se = ps.args && (*ps.args).length == 1 ? semanticString(sc, (*ps.args)[0], "pragma mangle argument") : null;
        if (!se)
        {
            error(ps.loc, "`pragma(mangle)` takes a single argument that must be a string literal");
            return false;
        }
        const cnt = setMangleOverride(de.declaration, cast(const(char)[])se.peekData());
        if (cnt != 1)
            assert(0);
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
    else if (opt.get())
        return PINLINE.always;
    else
        return PINLINE.never;
}

/**
 * Apply pragma mangle to FuncDeclarations and VarDeclarations
 * under `s`, poking through attribute declarations such as
 * `extern(C)` but not through aggregates or function bodies.
 *
 * Params:
 *    s = symbol to apply
 *    sym = overriding symbol name
 */
private uint setMangleOverride(Dsymbol s, const(char)[] sym)
{
    if (s.isFuncDeclaration() || s.isVarDeclaration())
    {
        s.isDeclaration().mangleOverride = sym;
        return 1;
    }

    if (auto ad = s.isAttribDeclaration())
    {
        uint nestedCount = 0;

        ad.include(null).foreachDsymbol( (s) { nestedCount += setMangleOverride(s, sym); } );

        return nestedCount;
    }
    return 0;
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

    if (!args)
        return true;
    foreach (arg; *args)
    {
        sc = sc.startCTFE();
        auto e = arg.expressionSemantic(sc);
        e = resolveProperties(sc, e);
        sc = sc.endCTFE();

        // pragma(msg) is allowed to contain types as well as expressions
        e = ctfeInterpretForPragmaMsg(e);
        if (e.op == EXP.error)
        {
            errorSupplemental(loc, "while evaluating `pragma(msg, %s)`", arg.toChars());
            return false;
        }
        if (auto se = e.toStringExp())
        {
            const slice = se.toUTF8(sc).peekString();
            fprintf(stderr, "%.*s", cast(int)slice.length, slice.ptr);
        }
        else
            fprintf(stderr, "%s", e.toChars());
    }
    fprintf(stderr, "\n");
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
