/**
 * Implements the `alias this` symbol.
 *
 * Specification: $(LINK2 https://dlang.org/spec/class.html#alias-this, Alias This)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/aliasthis.d, _aliasthis.d)
 * Documentation:  https://dlang.org/phobos/dmd_aliasthis.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/aliasthis.d
 */

module dmd.aliasthis;

import core.stdc.stdio;
import dmd.aggregate;
import dmd.dscope;
import dmd.dsymbol;
import dmd.expression;
import dmd.expressionsem;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.tokens;
import dmd.visitor;

/***********************************************************
 * alias ident this;
 */
extern (C++) final class AliasThis : Dsymbol
{
    Identifier ident;
    /// The symbol this `alias this` resolves to
    Dsymbol sym;
    /// Whether this `alias this` is deprecated or not
    bool isDeprecated_;

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, null);    // it's anonymous (no identifier)
        this.ident = ident;
    }

    override AliasThis syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto at = new AliasThis(loc, ident);
        at.comment = comment;
        return at;
    }

    override const(char)* kind() const
    {
        return "alias this";
    }

    AliasThis isAliasThis()
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    override bool isDeprecated() const
    {
        return this.isDeprecated_;
    }
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
    import dmd.typesem : dotExp;
    for (AggregateDeclaration ad = isAggregate(e.type); ad;)
    {
        if (ad.aliasthis)
        {
            Loc loc = e.loc;
            Type tthis = (e.op == EXP.type ? e.type : null);
            const flags = cast(DotExpFlag) (DotExpFlag.noAliasThis | (gag * DotExpFlag.gag));
            uint olderrors = gag ? global.startGagging() : 0;
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
                    int save = sc.intypeof;
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
bool checkDeprecatedAliasThis(AliasThis at, const ref Loc loc, Scope* sc)
{
    import dmd.errors : deprecation, Classification;
    import dmd.dsymbolsem : getMessage;

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

/**************************************
 * Check and set 'att' if 't' is a recursive 'alias this' type
 *
 * The goal is to prevent endless loops when there is a cycle in the alias this chain.
 * Since there is no multiple `alias this`, the chain either ends in a leaf,
 * or it loops back on itself as some point.
 *
 * Example: S0 -> (S1 -> S2 -> S3 -> S1)
 *
 * `S0` is not a recursive alias this, so this returns `false`, and a rewrite to `S1` can be tried.
 * `S1` is a recursive alias this type, but since `att` is initialized to `null`,
 * this still returns `false`, but `att1` is set to `S1`.
 * A rewrite to `S2` and `S3` can be tried, but when we want to try a rewrite to `S1` again,
 * we notice `att == t`, so we're back at the start of the loop, and this returns `true`.
 *
 * Params:
 *   att = type reference used to detect recursion. Should be initialized to `null`.
 *   t   = type of 'alias this' rewrite to attempt
 *
 * Returns:
 *   `false` if the rewrite is safe, `true` if it would loop back around
 */
bool isRecursiveAliasThis(ref Type att, Type t)
{
    //printf("+isRecursiveAliasThis(att = %s, t = %s)\n", att ? att.toChars() : "null", t.toChars());
    auto tb = t.toBasetype();
    if (att && tb.equivalent(att))
        return true;
    else if (!att && tb.checkAliasThisRec())
        att = tb;
    return false;
}
