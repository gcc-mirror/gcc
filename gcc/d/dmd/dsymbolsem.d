/**
 * Does the semantic 1 pass on the AST, which looks at symbol declarations but not initializers
 * or function bodies.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dsymbolsem.d, _dsymbolsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_dsymbolsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dsymbolsem.d
 */

module dmd.dsymbolsem;

import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.clone;
import dmd.cond;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
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
import dmd.escape;
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
import dmd.intrange;
import dmd.hdrgen;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.nspace;
import dmd.objc;
import dmd.opover;
import dmd.optimize;
import dmd.parse;
import dmd.root.array;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.rootobject;
import dmd.semantic2;
import dmd.semantic3;
import dmd.sideeffect;
import dmd.staticassert;
import dmd.tokens;
import dmd.utils;
import dmd.statement;
import dmd.target;
import dmd.templateparamsem;
import dmd.templatesem;
import dmd.typesem;
import dmd.visitor;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

enum LOG = false;

/*************************************
 * Does semantic analysis on the public face of declarations.
 */
void dsymbolSemantic(Dsymbol dsym, Scope* sc)
{
    scope v = new DsymbolSemanticVisitor(sc);
    dsym.accept(v);
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
            if (sc.flags & SCOPE.Cfile && n == 0)       // C11 6.7.5-6 allows 0 for alignment
                continue;

            if (n < 1 || n & (n - 1) || ushort.max < n || !e.type.isintegral())
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

bool checkDeprecated(Dsymbol d, const ref Loc loc, Scope* sc)
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
    if (sc.flags & SCOPE.constraint)
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
private void checkDeprecated(Type type, const ref Loc loc, Scope* sc)
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

/*
Tests whether the `ctor` that is part of `ti` is an rvalue constructor
(i.e. a constructor that receives a single parameter of the same type as
`Unqual!typeof(this)`). If that is the case and `sd` contains a copy
constructor, than an error is issued.

Params:
    sd = struct declaration that may contin both an rvalue and copy constructor
    ctor = constructor that will be checked if it is an evalue constructor
    ti = template instance the ctor is part of

Return:
    `false` if ctor is not an rvalue constructor or if `sd` does not contain a
    copy constructor. `true` otherwise
*/
bool checkHasBothRvalueAndCpCtor(StructDeclaration sd, CtorDeclaration ctor, TemplateInstance ti)
{
    auto loc = ctor.loc;
    auto tf = cast(TypeFunction)ctor.type;
    auto dim = tf.parameterList.length;
    if (sd && sd.hasCopyCtor && (dim == 1 || (dim > 1 && tf.parameterList[1].defaultArg)))
    {
        auto param = tf.parameterList[0];
        if (!(param.storageClass & STC.ref_) && param.type.mutableOf().unSharedOf() == sd.type.mutableOf().unSharedOf())
        {
            .error(loc, "cannot define both an rvalue constructor and a copy constructor for `struct %s`", sd.toChars());
            .errorSupplemental(ti.loc, "Template instance `%s` creates an rvalue constructor for `struct %s`",
                    ti.toPrettyChars(), sd.toChars());

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
private bool checkDeprecatedAliasThis(AliasThis at, const ref Loc loc, Scope* sc)
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
void deferDsymbolSemantic(Scope* sc, Dsymbol s, Scope *scx)
{
    s._scope = scx ? scx : sc.copy();
    s._scope.setNoFree();
    Module.addDeferredSemantic(s);
}


private extern(C++) final class DsymbolSemanticVisitor : Visitor
{
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
            .error(dsym.loc, "%s `%s` extern symbols cannot have initializers", dsym.kind, dsym.toPrettyChars);

        AggregateDeclaration ad = dsym.isThis();
        if (ad)
            dsym.storage_class |= ad.storage_class & STC.TYPECTOR;

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
                sc.flags |= SCOPE.condition;
                sc = sc.startCTFE();
            }
            //printf("inferring type for %s with init %s\n", dsym.toChars(), dsym._init.toChars());
            dsym._init = dsym._init.inferType(sc);
            dsym.type = dsym._init.initializerToExpression(null, (sc.flags & SCOPE.Cfile) != 0).type;
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
        if (dsym.alignment.isDefault())
            dsym.alignment = dsym.type.alignment(); // use type's alignment

        //printf("sc.stc = %x\n", sc.stc);
        //printf("storage_class = x%x\n", storage_class);

        dsym.type.checkComplexTransition(dsym.loc, sc);

        // Calculate type size + safety checks
        if (dsym.storage_class & STC.gshared && !dsym.isMember())
        {
            sc.setUnsafe(false, dsym.loc, "__gshared not allowed in safe functions; use shared");
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
        if ((dsym.storage_class & STC.auto_) && !inferred)
            .error(dsym.loc, "%s `%s` - storage class `auto` has no effect if type is not inferred, did you mean `scope`?", dsym.kind, dsym.toPrettyChars);

        if (auto tt = tb.isTypeTuple())
        {
            /* Instead, declare variables for each of the tuple elements
             * and add those.
             */
            size_t nelems = Parameter.dim(tt.arguments);
            Expression ie = (dsym._init && !dsym._init.isVoidInitializer()) ? dsym._init.initializerToExpression(null, (sc.flags & SCOPE.Cfile) != 0) : null;
            if (ie)
                ie = ie.expressionSemantic(sc);
            if (nelems > 0 && ie)
            {
                auto iexps = new Expressions();
                iexps.push(ie);
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
                        auto v = copyToTemp(0, "__tup", e);
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

                StorageClass storage_class = STC.temp | dsym.storage_class;
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

        if (StorageClass stc = dsym.storage_class & (STC.synchronized_ | STC.override_ | STC.abstract_ | STC.final_))
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

        if (dsym.storage_class & STC.scope_)
        {
            StorageClass stc = dsym.storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.gshared);
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
                if (ad2 && dsym.storage_class != STC.undefined_)
                {
                    .error(dsym.loc, "%s `%s` - cannot use template to add field to aggregate `%s`", dsym.kind, dsym.toPrettyChars, ad2.toChars());
                }
            }
        }

        /* If the alignment of a stack local is greater than the stack alignment,
         * note it in the enclosing function's alignSectionVars
         */
        version (MARS)
        {
            if (!dsym.alignment.isDefault() && sc.func &&
                dsym.alignment.get() > target.stackAlign() &&
                sc.func && !dsym.isDataseg() && !dsym.isParameter() && !dsym.isField())
            {
                auto fd = sc.func;
                if (!fd.alignSectionVars)
                    fd.alignSectionVars = new VarDeclarations();
                fd.alignSectionVars.push(dsym);
            }
        }

        if ((dsym.storage_class & (STC.ref_ | STC.parameter | STC.foreach_ | STC.temp | STC.result)) == STC.ref_ && dsym.ident != Id.This)
        {
            .error(dsym.loc, "%s `%s` - only parameters, functions and `foreach` declarations can be `ref`", dsym.kind, dsym.toPrettyChars);
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

        FuncDeclaration fd = parent.isFuncDeclaration();
        if (dsym.type.isscope() && !(dsym.storage_class & STC.nodtor))
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
            if (dsym._init && dsym._init.isVoidInitializer())
            {

                if (dsym.type.hasPointers()) // also computes type size
                    sc.setUnsafe(false, dsym.loc,
                        "`void` initializers for pointers not allowed in safe functions");
                else if (dsym.type.hasInvariant())
                    sc.setUnsafe(false, dsym.loc,
                        "`void` initializers for structs with invariants are not allowed in safe functions");
                else if (dsym.type.hasSystemFields())
                    sc.setUnsafePreview(global.params.systemVariables, false, dsym.loc,
                        "`void` initializers for `@system` variables not allowed in safe functions");
            }
            else if (!dsym._init &&
                     !(dsym.storage_class & (STC.static_ | STC.extern_ | STC.gshared | STC.manifest | STC.field | STC.parameter)) &&
                     dsym.type.hasVoidInitPointers())
            {
                sc.setUnsafe(false, dsym.loc, "`void` initializers for pointers not allowed in safe functions");
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
        if (sc.flags & SCOPE.Cfile && !dsym._init)
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

            if (sc.flags & SCOPE.Cfile &&
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
                            e = dsym._init.initializerToExpression(null, (sc.flags & SCOPE.Cfile) != 0);
                        if (!e)
                        {
                            // Run semantic, but don't need to interpret
                            dsym._init = dsym._init.initializerSemantic(sc, dsym.type, INITnointerpret);
                            e = dsym._init.initializerToExpression(null, (sc.flags & SCOPE.Cfile) != 0);
                            if (!e)
                            {
                                .error(dsym.loc, "%s `%s` is not a static and cannot have static initializer", dsym.kind, dsym.toPrettyChars);
                                e = ErrorExp.get();
                            }
                        }
                        ei = new ExpInitializer(dsym._init.loc, e);
                        dsym._init = ei;
                    }
                    else if (sc.flags & SCOPE.Cfile && dsym.type.isTypeSArray() &&
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
                            /* See if initializer is a NewExp that can be allocated on the stack.
                             */
                            if (dsym.type.toBasetype().ty == Tclass)
                            {
                                /* Unsafe to allocate on stack if constructor is not `scope` because the `this` can leak.
                                 * https://issues.dlang.org/show_bug.cgi?id=23145
                                 */
                                if (ne.member && !(ne.member.storage_class & STC.scope_))
                                {
                                    import dmd.escape : setUnsafeDIP1000;
                                    const inSafeFunc = sc.func && sc.func.isSafeBypassingInference();   // isSafeBypassingInference may call setUnsafe().
                                    if (sc.setUnsafeDIP1000(false, dsym.loc, "`scope` allocation of `%s` requires that constructor be annotated with `scope`", dsym))
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
                    if (isBlit)
                        exp = new BlitExp(dsym.loc, e1, exp);
                    else
                        exp = new ConstructExp(dsym.loc, e1, exp);
                    dsym.canassign++;
                    exp = exp.expressionSemantic(sc);
                    dsym.canassign--;
                    exp = exp.optimize(WANTvalue);
                    if (exp.op == EXP.error)
                    {
                        dsym._init = new ErrorInitializer();
                        ei = null;
                    }
                    else
                        ei.exp = exp;
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
                     sc.flags & SCOPE.Cfile)
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
                    uint errors = global.errors;
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

        if (!(global.params.bitfields || sc.flags & SCOPE.Cfile))
        {
            version (IN_GCC)
                .error(dsym.loc, "%s `%s` use `-fpreview=bitfields` for bitfield support", dsym.kind, dsym.toPrettyChars);
            else
                .error(dsym.loc, "%s `%s` use -preview=bitfields for bitfield support", dsym.kind, dsym.toPrettyChars);
        }

        if (!dsym.parent.isStructDeclaration() && !dsym.parent.isClassDeclaration())
        {
            .error(dsym.loc, "%s `%s` - bit-field must be member of struct, union, or class", dsym.kind, dsym.toPrettyChars);
        }

        sc = sc.startCTFE();
        auto width = dsym.width.expressionSemantic(sc);
        sc = sc.endCTFE();
        width = width.ctfeInterpret();
        if (!dsym.type.isintegral())
        {
            // C11 6.7.2.1-5
            error(width.loc, "bit-field type `%s` is not an integer type", dsym.type.toChars());
            dsym.errors = true;
        }
        if (!width.isIntegerExp())
        {
            error(width.loc, "bit-field width `%s` is not an integer constant", dsym.width.toChars());
            dsym.errors = true;
        }
        const uwidth = width.toInteger(); // uwidth is unsigned
        if (uwidth == 0 && !dsym.isAnonymous())
        {
            error(width.loc, "bit-field `%s` has zero width", dsym.toChars());
            dsym.errors = true;
        }
        const sz = dsym.type.size();
        if (sz == SIZE_INVALID)
            dsym.errors = true;
        const max_width = sz * 8;
        if (uwidth > max_width)
        {
            error(width.loc, "width `%lld` of bit-field `%s` does not fit in type `%s`", cast(long)uwidth, dsym.toChars(), dsym.type.toChars());
            dsym.errors = true;
        }
        dsym.fieldWidth = cast(uint)uwidth;
    }

    override void visit(Import imp)
    {
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
        if (imp.mod)
        {
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
        }

        imp.semanticRun = PASS.semanticdone;

        // object self-imports itself, so skip that
        // https://issues.dlang.org/show_bug.cgi?id=7547
        // don't list pseudo modules __entrypoint.d, __main.d
        // https://issues.dlang.org/show_bug.cgi?id=11117
        // https://issues.dlang.org/show_bug.cgi?id=11164
        if (global.params.moduleDeps.buffer is null || (imp.id == Id.object && sc._module.ident == Id.object) ||
            strcmp(sc._module.ident.toChars(), "__main") == 0)
            return;

        /* The grammar of the file is:
         *      ImportDeclaration
         *          ::= BasicImportDeclaration [ " : " ImportBindList ] [ " -> "
         *      ModuleAliasIdentifier ] "\n"
         *
         *      BasicImportDeclaration
         *          ::= ModuleFullyQualifiedName " (" FilePath ") : " Protection|"string"
         *              " [ " static" ] : " ModuleFullyQualifiedName " (" FilePath ")"
         *
         *      FilePath
         *          - any string with '(', ')' and '\' escaped with the '\' character
         */
        OutBuffer* ob = global.params.moduleDeps.buffer;
        Module imod = sc._module;
        if (!global.params.moduleDeps.name)
            ob.writestring("depsImport ");
        ob.writestring(imod.toPrettyChars());
        ob.writestring(" (");
        escapePath(ob, imod.srcfile.toChars());
        ob.writestring(") : ");
        // use visibility instead of sc.visibility because it couldn't be
        // resolved yet, see the comment above
        visibilityToBuffer(*ob, imp.visibility);
        ob.writeByte(' ');
        if (imp.isstatic)
        {
            stcToBuffer(*ob, STC.static_);
            ob.writeByte(' ');
        }
        ob.writestring(": ");
        foreach (pid; imp.packages)
        {
            ob.printf("%s.", pid.toChars());
        }
        ob.writestring(imp.id.toString());
        ob.writestring(" (");
        if (imp.mod)
            escapePath(ob, imp.mod.srcfile.toChars());
        else
            ob.writestring("???");
        ob.writeByte(')');
        foreach (i, name; imp.names)
        {
            if (i == 0)
                ob.writeByte(':');
            else
                ob.writeByte(',');
            Identifier _alias = imp.aliases[i];
            if (!_alias)
            {
                ob.printf("%s", name.toChars());
                _alias = name;
            }
            else
                ob.printf("%s=%s", _alias.toChars(), name.toChars());
        }
        if (imp.aliasId)
            ob.printf(" -> %s", imp.aliasId.toChars());
        ob.writenl();
    }

    void attribSemantic(AttribDeclaration ad)
    {
        if (ad.semanticRun != PASS.initial)
            return;
        ad.semanticRun = PASS.semantic;
        Dsymbols* d = ad.include(sc);
        //printf("\tAttribDeclaration::semantic '%s', d = %p\n",toChars(), d);
        if (d)
        {
            Scope* sc2 = ad.newScope(sc);
            bool errors;
            for (size_t i = 0; i < d.length; i++)
            {
                Dsymbol s = (*d)[i];
                s.dsymbolSemantic(sc2);
                errors |= s.errors;
            }
            ad.errors |= errors;
            if (sc2 != sc)
                sc2.pop();
        }
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
        sc.flags = 0;
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
        if (expressionsToString(buf, sc, cd.exps))
            return null;

        const errors = global.errors;
        const len = buf.length;
        buf.writeByte(0);
        const str = buf.extractSlice()[0 .. len];
        const bool doUnittests = global.params.parsingUnittestsRequired();
        auto loc = adjustLocForMixin(str, cd.loc, global.params.mixinOut);
        scope p = new Parser!ASTCodegen(loc, sc._module, str, false, global.errorSink, &global.compileEnv, doUnittests);
        p.transitionIn = global.params.v.vin;
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
                error(ns.exp.loc, "expected valid identifier for C++ namespace but got `%.*s`",
                             cast(int)sident.length, sident.ptr);
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
        //printf("+Module::semantic(this = %p, '%s'): parent = %p\n", this, toChars(), parent);
        m.semanticRun = PASS.semantic;
        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope* sc = m._scope; // see if already got one from importAll()
        if (!sc)
        {
            sc = Scope.createGlobal(m, global.errorSink); // create root scope
        }

        //printf("Module = %p, linkage = %d\n", sc.scopesym, sc.linkage);
        // Pass 1 semantic routines: do public side of the definition
        m.members.foreachDsymbol( (s)
        {
            //printf("\tModule('%s'): '%s'.dsymbolSemantic()\n", toChars(), s.toChars());
            s.dsymbolSemantic(sc);
            m.runDeferredSemantic();
        });

        if (m.userAttribDecl)
        {
            m.userAttribDecl.dsymbolSemantic(sc);
        }
        if (!m._scope)
        {
            sc = sc.pop();
            sc.pop(); // 2 pops because Scope.createGlobal() created 2
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
        if (!tm.findTempDecl(sc) || !tm.semanticTiargs(sc) || !tm.findBestMatch(sc, ArgumentList()))
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
                    if (!tme || !ea.equals(tme))
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

        uint errorsave = global.errors;

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
         * Because the members would already call Module.addDeferredSemantic() for themselves.
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
        UserAttributeDeclaration.checkGNUABITag(ns, LINK.cpp);

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
        //printf("CtorDeclaration::semantic() %s\n", toChars());
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
                //printf("tf: %s\n", tf.toChars());
                auto param = tf.parameterList[0];
                if (param.storageClass & STC.ref_ && param.type.mutableOf().unSharedOf() == sd.type.mutableOf().unSharedOf())
                {
                    //printf("copy constructor\n");
                    ctd.isCpCtor = true;
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

    override void visit(StaticCtorDeclaration scd)
    {
        //printf("StaticCtorDeclaration::semantic()\n");
        if (scd.semanticRun >= PASS.semanticdone)
            return;
        if (scd._scope)
        {
            sc = scd._scope;
            scd._scope = null;
        }

        scd.parent = sc.parent;
        Dsymbol p = scd.parent.pastMixin();
        if (!p.isScopeDsymbol())
        {
            const(char)* s = (scd.isSharedStaticCtorDeclaration() ? "shared " : "");
            error(scd.loc, "`%sstatic` constructor can only be member of module/aggregate/template, not %s `%s`", s, p.kind(), p.toChars());
            scd.type = Type.terror;
            scd.errors = true;
            return;
        }
        if (!scd.type)
            scd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, scd.storage_class);

        /* If the static ctor appears within a template instantiation,
         * it could get called multiple times by the module constructors
         * for different modules. Thus, protect it with a gate.
         */
        if (scd.isInstantiated() && scd.semanticRun < PASS.semantic)
        {
            /* Add this prefix to the constructor:
             * ```
             * static int gate;
             * if (++gate != 1) return;
             * ```
             * or, for shared constructor:
             * ```
             * shared int gate;
             * if (core.atomic.atomicOp!"+="(gate, 1) != 1) return;
             * ```
             */
            const bool isShared = !!scd.isSharedStaticCtorDeclaration();
            auto v = new VarDeclaration(Loc.initial, Type.tint32, Id.gate, null);
            v.storage_class = STC.temp | STC.static_ | (isShared ? STC.shared_ : 0);

            auto sa = new Statements();
            Statement s = new ExpStatement(Loc.initial, v);
            sa.push(s);

            Expression e;
            if (isShared)
            {
                e = doAtomicOp("+=", v.ident, IntegerExp.literal!(1));
                if (e is null)
                {
                    .error(scd.loc, "%s `%s` shared static constructor within a template require `core.atomic : atomicOp` to be present", scd.kind, scd.toPrettyChars);
                    return;
                }
            }
            else
            {
                e = new AddAssignExp(
                    Loc.initial, new IdentifierExp(Loc.initial, v.ident), IntegerExp.literal!1);
            }

            e = new EqualExp(EXP.notEqual, Loc.initial, e, IntegerExp.literal!1);
            s = new IfStatement(Loc.initial, null, e, new ReturnStatement(Loc.initial, null), null, Loc.initial);

            sa.push(s);
            if (scd.fbody)
                sa.push(scd.fbody);

            scd.fbody = new CompoundStatement(Loc.initial, sa);
        }

        const LINK save = sc.linkage;
        if (save != LINK.d)
        {
            const(char)* s = (scd.isSharedStaticCtorDeclaration() ? "shared " : "");
            deprecation(scd.loc, "`%sstatic` constructor can only be of D linkage", s);
            // Just correct it
            sc.linkage = LINK.d;
        }
        funcDeclarationSemantic(sc, scd);
        sc.linkage = save;

        // We're going to need ModuleInfo
        Module m = scd.getModule();
        if (!m)
            m = sc._module;
        if (m)
        {
            m.needmoduleinfo = 1;
            //printf("module1 %s needs moduleinfo\n", m.toChars());
        }

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
        if (sdd.semanticRun >= PASS.semanticdone)
            return;
        if (sdd._scope)
        {
            sc = sdd._scope;
            sdd._scope = null;
        }

        sdd.parent = sc.parent;
        Dsymbol p = sdd.parent.pastMixin();
        if (!p.isScopeDsymbol())
        {
            const(char)* s = (sdd.isSharedStaticDtorDeclaration() ? "shared " : "");
            error(sdd.loc, "`%sstatic` destructor can only be member of module/aggregate/template, not %s `%s`", s, p.kind(), p.toChars());
            sdd.type = Type.terror;
            sdd.errors = true;
            return;
        }
        if (!sdd.type)
            sdd.type = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, sdd.storage_class);

        /* If the static ctor appears within a template instantiation,
         * it could get called multiple times by the module constructors
         * for different modules. Thus, protect it with a gate.
         */
        if (sdd.isInstantiated() && sdd.semanticRun < PASS.semantic)
        {
            /* Add this prefix to the constructor:
             * ```
             * static int gate;
             * if (--gate != 0) return;
             * ```
             * or, for shared constructor:
             * ```
             * shared int gate;
             * if (core.atomic.atomicOp!"-="(gate, 1) != 0) return;
             * ```
             */
            const bool isShared = !!sdd.isSharedStaticDtorDeclaration();
            auto v = new VarDeclaration(Loc.initial, Type.tint32, Id.gate, null);
            v.storage_class = STC.temp | STC.static_ | (isShared ? STC.shared_ : 0);

            auto sa = new Statements();
            Statement s = new ExpStatement(Loc.initial, v);
            sa.push(s);

            Expression e;
            if (isShared)
            {
                e = doAtomicOp("-=", v.ident, IntegerExp.literal!(1));
                if (e is null)
                {
                    .error(sdd.loc, "%s `%s` shared static destructo within a template require `core.atomic : atomicOp` to be present", sdd.kind, sdd.toPrettyChars);
                    return;
                }
            }
            else
            {
                e = new AddAssignExp(
                    Loc.initial, new IdentifierExp(Loc.initial, v.ident), IntegerExp.literal!(-1));
            }

            e = new EqualExp(EXP.notEqual, Loc.initial, e, IntegerExp.literal!0);
            s = new IfStatement(Loc.initial, null, e, new ReturnStatement(Loc.initial, null), null, Loc.initial);

            sa.push(s);
            if (sdd.fbody)
                sa.push(sdd.fbody);

            sdd.fbody = new CompoundStatement(Loc.initial, sa);

            sdd.vgate = v;
        }

        const LINK save = sc.linkage;
        if (save != LINK.d)
        {
            const(char)* s = (sdd.isSharedStaticDtorDeclaration() ? "shared " : "");
            deprecation(sdd.loc, "`%sstatic` destructor can only be of D linkage", s);
            // Just correct it
            sc.linkage = LINK.d;
        }
        funcDeclarationSemantic(sc, sdd);
        sc.linkage = save;

        // We're going to need ModuleInfo
        Module m = sdd.getModule();
        if (!m)
            m = sc._module;
        if (m)
        {
            m.needmoduleinfo = 1;
            //printf("module2 %s needs moduleinfo\n", m.toChars());
        }
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
        sc.flags = (sc.flags & ~SCOPE.contract) | SCOPE.invariant_;
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
        int errors = global.errors;

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
        UserAttributeDeclaration.checkGNUABITag(sd, sc.linkage);

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
        sd.members.foreachDsymbol( (s) { s.dsymbolSemantic(sc2); sd.errors |= s.errors; } );

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

        sd.hasCopyCtor = buildCopyCtor(sd, sc2);
        sd.postblit = buildPostBlit(sd, sc2);

        buildOpAssign(sd, sc2);
        buildOpEquals(sd, sc2);

        if (!(sc2.flags & SCOPE.Cfile) &&
            global.params.useTypeInfo && Type.dtypeinfo)  // these functions are used for TypeInfo
        {
            sd.xeq = buildXopEquals(sd, sc2);
            sd.xcmp = buildXopCmp(sd, sc2);
            sd.xhash = buildXtoHash(sd, sc2);
        }

        sd.inv = buildInv(sd, sc2);

        sd.semanticRun = PASS.semanticdone;
        if (log) printf("-StructDeclaration::semantic(this=%p, '%s', sizeok = %d)\n", sd, sd.toPrettyChars(), sd.sizeok);

        sc2.pop();

        if (sd.ctor)
        {
            Dsymbol scall = sd.search(Loc.initial, Id.call);
            if (scall)
            {
                uint xerrors = global.startGagging();
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
                /* This is two structs imported from different C files.
                 * Just ignore sd, the second one. The first one will always
                 * be found when going through the type.
                 */
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
        int errors = global.errors;

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
        cldec.semanticRun = PASS.semantic;
        UserAttributeDeclaration.checkGNUABITag(cldec, sc.linkage);
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
                        Module.addDeferredSemantic(tc.sym);
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
                        Module.addDeferredSemantic(tc.sym);
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
                    .error(cldec.loc, "%s `%s` missing or corrupt object.d", cldec.kind, cldec.toPrettyChars);
                    fatal();
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
                    Module.addDeferredSemantic(tc.sym);
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

                auto ctor = new CtorDeclaration(cldec.loc, Loc.initial, 0, tf);
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
        int errors = global.errors;

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
            UserAttributeDeclaration.checkGNUABITag(idec, sc.linkage);
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
                        Module.addDeferredSemantic(tc.sym);
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
                    Module.addDeferredSemantic(tc.sym);
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

    this(Scope* sc, ScopeDsymbol sds)
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
            if (sc.flags & SCOPE.Cfile && !dsym.isImport())
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
                sds.multiplyDefined(Loc.initial, dsym, s2);
                dsym.errors = true;
            }
        }
        if (sds.isAggregateDeclaration() || sds.isEnumDeclaration())
        {
            if (dsym.ident == Id.__sizeof ||
                !(sc && sc.flags & SCOPE.Cfile) && (dsym.ident == Id.__xalignof || dsym.ident == Id._mangleof))
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
        if (ds.ident)
        {
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
        else
        {
            if (!m)
            {
                .error(ds.loc, "%s `%s` level declaration must be at module level", ds.kind, ds.toPrettyChars);
                ds.errors = true;
            }
            else
                m.debuglevel = ds.level;
        }
    }

    override void visit(VersionSymbol vs)
    {
        //printf("VersionSymbol::addMember('%s') %s\n", sds.toChars(), vs.toChars());
        Module m = sds.isModule();
        // Do not add the member to the symbol table,
        // just make sure subsequent debug declarations work.
        if (vs.ident)
        {
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
        else
        {
            if (!m)
            {
                .error(vs.loc, "%s `%s` level declaration must be at module level", vs.kind, vs.toPrettyChars);
                vs.errors = true;
            }
            else
                m.versionlevel = vs.level;
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
    const bool isCEnum = (sc.flags & SCOPE.Cfile) != 0; // it's an ImportC enum
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

/******************************************************
 * Verifies if the given Identifier is a DRuntime hook. It uses the hooks
 * defined in `id.d`.
 *
 * Params:
 *  id = Identifier to verify
 * Returns:
 *  true if `id` is a DRuntime hook
 *  false otherwise
 */
private bool isDRuntimeHook(Identifier id)
{
    return id == Id._d_HookTraceImpl ||
        id == Id._d_newclassT || id == Id._d_newclassTTrace ||
        id == Id._d_arraycatnTX || id == Id._d_arraycatnTXTrace ||
        id == Id._d_newThrowable || id == Id._d_delThrowable ||
        id == Id._d_arrayassign_l || id == Id._d_arrayassign_r ||
        id == Id._d_arraysetassign || id == Id._d_arraysetctor ||
        id == Id._d_arrayctor ||
        id == Id._d_arraysetlengthTImpl || id == Id._d_arraysetlengthT ||
        id == Id._d_arraysetlengthTTrace ||
        id == Id._d_arrayappendT || id == Id._d_arrayappendTTrace ||
        id == Id._d_arrayappendcTX;
}

void templateInstanceSemantic(TemplateInstance tempinst, Scope* sc, ArgumentList argumentList)
{
    //printf("[%s] TemplateInstance.dsymbolSemantic('%s', this=%p, gag = %d, sc = %p)\n", tempinst.loc.toChars(), tempinst.toChars(), tempinst, global.gag, sc);
    version (none)
    {
        for (Dsymbol s = tempinst; s; s = s.parent)
        {
            printf("\t%s\n", s.toChars());
        }
        printf("Scope\n");
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            printf("\t%s parent %s\n", scx._module ? scx._module.toChars() : "null", scx.parent ? scx.parent.toChars() : "null");
        }
    }

    static if (LOG)
    {
        printf("\n+TemplateInstance.dsymbolSemantic('%s', this=%p)\n", tempinst.toChars(), tempinst);
    }
    if (tempinst.inst) // if semantic() was already run
    {
        static if (LOG)
        {
            printf("-TemplateInstance.dsymbolSemantic('%s', this=%p) already run\n",
                   tempinst.inst.toChars(), tempinst.inst);
        }
        return;
    }
    if (tempinst.semanticRun != PASS.initial)
    {
        static if (LOG)
        {
            printf("Recursive template expansion\n");
        }
        auto ungag = Ungag(global.gag);
        if (!tempinst.gagged)
            global.gag = 0;
        .error(tempinst.loc, "%s `%s` recursive template expansion", tempinst.kind, tempinst.toPrettyChars);
        if (tempinst.gagged)
            tempinst.semanticRun = PASS.initial;
        else
            tempinst.inst = tempinst;
        tempinst.errors = true;
        return;
    }

    // Get the enclosing template instance from the scope tinst
    tempinst.tinst = sc.tinst;

    // Get the instantiating module from the scope minst
    tempinst.minst = sc.minst;
    // https://issues.dlang.org/show_bug.cgi?id=10920
    // If the enclosing function is non-root symbol,
    // this instance should be speculative.
    if (!tempinst.tinst && sc.func && sc.func.inNonRoot())
    {
        tempinst.minst = null;
    }

    tempinst.gagged = (global.gag > 0);

    tempinst.semanticRun = PASS.semantic;

    static if (LOG)
    {
        printf("\tdo semantic\n");
    }
    /* Find template declaration first,
     * then run semantic on each argument (place results in tiargs[]),
     * last find most specialized template from overload list/set.
     */
    if (!tempinst.findTempDecl(sc, null) || !tempinst.semanticTiargs(sc) || !tempinst.findBestMatch(sc, argumentList))
    {
    Lerror:
        if (tempinst.gagged)
        {
            // https://issues.dlang.org/show_bug.cgi?id=13220
            // Roll back status for later semantic re-running
            tempinst.semanticRun = PASS.initial;
        }
        else
            tempinst.inst = tempinst;
        tempinst.errors = true;
        return;
    }
    TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();
    assert(tempdecl);

    if (global.params.v.templates)
        TemplateStats.incInstance(tempdecl, tempinst, global.params.v.templatesListInstances);

    tempdecl.checkDeprecated(tempinst.loc, sc);

    // If tempdecl is a mixin, disallow it
    if (tempdecl.ismixin)
    {
        .error(tempinst.loc, "%s `%s` mixin templates are not regular templates", tempinst.kind, tempinst.toPrettyChars);
        goto Lerror;
    }

    tempinst.hasNestedArgs(tempinst.tiargs, tempdecl.isstatic);
    if (tempinst.errors)
        goto Lerror;

    // Copy the tempdecl namespace (not the scope one)
    tempinst.cppnamespace = tempdecl.cppnamespace;
    if (tempinst.cppnamespace)
        tempinst.cppnamespace.dsymbolSemantic(sc);

    /* Greatly simplified semantic processing for AliasSeq templates
     */
    if (tempdecl.isTrivialAliasSeq)
    {
        tempinst.inst = tempinst;
        return aliasSeqInstanceSemantic(tempinst, sc, tempdecl);
    }

    /* Greatly simplified semantic processing for Alias templates
     */
    else if (tempdecl.isTrivialAlias)
    {
        tempinst.inst = tempinst;
        return aliasInstanceSemantic(tempinst, sc, tempdecl);
    }


    /* See if there is an existing TemplateInstantiation that already
     * implements the typeargs. If so, just refer to that one instead.
     */
    tempinst.inst = tempdecl.findExistingInstance(tempinst, argumentList);
    TemplateInstance errinst = null;
    if (!tempinst.inst)
    {
        // So, we need to implement 'this' instance.
    }
    else if (tempinst.inst.gagged && !tempinst.gagged && tempinst.inst.errors)
    {
        // If the first instantiation had failed, re-run semantic,
        // so that error messages are shown.
        errinst = tempinst.inst;
    }
    else
    {
        // It's a match
        tempinst.parent = tempinst.inst.parent;
        tempinst.errors = tempinst.inst.errors;

        // If both this and the previous instantiation were gagged,
        // use the number of errors that happened last time.
        global.errors += tempinst.errors;
        global.gaggedErrors += tempinst.errors;

        // If the first instantiation was gagged, but this is not:
        if (tempinst.inst.gagged)
        {
            // It had succeeded, mark it is a non-gagged instantiation,
            // and reuse it.
            tempinst.inst.gagged = tempinst.gagged;
        }

        tempinst.tnext = tempinst.inst.tnext;
        tempinst.inst.tnext = tempinst;

        /* A module can have explicit template instance and its alias
         * in module scope (e,g, `alias Base64 = Base64Impl!('+', '/');`).
         * If the first instantiation 'inst' had happened in non-root module,
         * compiler can assume that its instantiated code would be included
         * in the separately compiled obj/lib file (e.g. phobos.lib).
         *
         * However, if 'this' second instantiation happened in root module,
         * compiler might need to invoke its codegen
         * (https://issues.dlang.org/show_bug.cgi?id=2500 & https://issues.dlang.org/show_bug.cgi?id=2644).
         * But whole import graph is not determined until all semantic pass finished,
         * so 'inst' should conservatively finish the semantic3 pass for the codegen.
         */
        if (tempinst.minst && tempinst.minst.isRoot() && !(tempinst.inst.minst && tempinst.inst.minst.isRoot()))
        {
            /* Swap the position of 'inst' and 'this' in the instantiation graph.
             * Then, the primary instance `inst` will be changed to a root instance,
             * along with all members of `inst` having their scopes updated.
             *
             * Before:
             *  non-root -> A!() -> B!()[inst] -> C!() { members[non-root] }
             *                      |
             *  root     -> D!() -> B!()[this]
             *
             * After:
             *  non-root -> A!() -> B!()[this]
             *                      |
             *  root     -> D!() -> B!()[inst] -> C!() { members[root] }
             */
            Module mi = tempinst.minst;
            TemplateInstance ti = tempinst.tinst;
            tempinst.minst = tempinst.inst.minst;
            tempinst.tinst = tempinst.inst.tinst;
            tempinst.inst.minst = mi;
            tempinst.inst.tinst = ti;

            /* https://issues.dlang.org/show_bug.cgi?id=21299
               `minst` has been updated on the primary instance `inst` so it is
               now coming from a root module, however all Dsymbol `inst.members`
               of the instance still have their `_scope.minst` pointing at the
               original non-root module. We must now propagate `minst` to all
               members so that forward referenced dependencies that get
               instantiated will also be appended to the root module, otherwise
               there will be undefined references at link-time.  */
            extern (C++) final class InstMemberWalker : Visitor
            {
                alias visit = Visitor.visit;
                TemplateInstance inst;

                extern (D) this(TemplateInstance inst) scope @safe
                {
                    this.inst = inst;
                }

                override void visit(Dsymbol d)
                {
                    if (d._scope)
                        d._scope.minst = inst.minst;
                }

                override void visit(ScopeDsymbol sds)
                {
                    sds.members.foreachDsymbol( s => s.accept(this) );
                    visit(cast(Dsymbol)sds);
                }

                override void visit(AttribDeclaration ad)
                {
                    ad.include(null).foreachDsymbol( s => s.accept(this) );
                    visit(cast(Dsymbol)ad);
                }

                override void visit(ConditionalDeclaration cd)
                {
                    if (cd.condition.inc)
                        visit(cast(AttribDeclaration)cd);
                    else
                        visit(cast(Dsymbol)cd);
                }
            }
            scope v = new InstMemberWalker(tempinst.inst);
            tempinst.inst.accept(v);

            if (!global.params.allInst &&
                tempinst.minst) // if inst was not speculative...
            {
                assert(!tempinst.minst.isRoot()); // ... it was previously appended to a non-root module
                // Append again to the root module members[], so that the instance will
                // get codegen chances (depending on `tempinst.inst.needsCodegen()`).
                tempinst.inst.appendToModuleMember();
            }

            assert(tempinst.inst.memberOf && tempinst.inst.memberOf.isRoot(), "no codegen chances");
        }

        // modules imported by an existing instance should be added to the module
        // that instantiates the instance.
        if (tempinst.minst)
            foreach(imp; tempinst.inst.importedModules)
                if (!tempinst.minst.aimports.contains(imp))
                    tempinst.minst.aimports.push(imp);

        static if (LOG)
        {
            printf("\tit's a match with instance %p, %d\n", tempinst.inst, tempinst.inst.semanticRun);
        }
        return;
    }
    static if (LOG)
    {
        printf("\timplement template instance %s '%s'\n", tempdecl.parent.toChars(), tempinst.toChars());
        printf("\ttempdecl %s\n", tempdecl.toChars());
    }
    uint errorsave = global.errors;

    tempinst.inst = tempinst;
    tempinst.parent = tempinst.enclosing ? tempinst.enclosing : tempdecl.parent;
    //printf("parent = '%s'\n", parent.kind());

    if (global.params.v.templates)
        TemplateStats.incUnique(tempdecl, tempinst);

    TemplateInstance tempdecl_instance_idx = tempdecl.addInstance(tempinst);

    //getIdent();

    // Store the place we added it to in target_symbol_list(_idx) so we can
    // remove it later if we encounter an error.
    Dsymbols* target_symbol_list = tempinst.appendToModuleMember();
    size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list.length - 1 : 0;

    // Copy the syntax trees from the TemplateDeclaration
    tempinst.members = Dsymbol.arraySyntaxCopy(tempdecl.members);

    // resolve TemplateThisParameter
    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        if ((*tempdecl.parameters)[i].isTemplateThisParameter() is null)
            continue;
        Type t = isType((*tempinst.tiargs)[i]);
        assert(t);
        if (StorageClass stc = ModToStc(t.mod))
        {
            //printf("t = %s, stc = x%llx\n", t.toChars(), stc);
            auto s = new Dsymbols();
            s.push(new StorageClassDeclaration(stc, tempinst.members));
            tempinst.members = s;
        }
        break;
    }

    // Create our own scope for the template parameters
    Scope* _scope = tempdecl._scope;
    if (tempdecl.semanticRun == PASS.initial)
    {
        .error(tempinst.loc, "%s `%s` template instantiation `%s` forward references template declaration `%s`",
           tempinst.kind, tempinst.toPrettyChars, tempinst.toChars(), tempdecl.toChars());
        return;
    }

    static if (LOG)
    {
        printf("\tcreate scope for template parameters '%s'\n", tempinst.toChars());
    }
    tempinst.argsym = new ScopeDsymbol();
    tempinst.argsym.parent = _scope.parent;
    _scope = _scope.push(tempinst.argsym);
    _scope.tinst = tempinst;
    _scope.minst = tempinst.minst;
    //scope.stc = 0;

    // Declare each template parameter as an alias for the argument type
    Scope* paramscope = _scope.push();
    paramscope.stc = 0;
    paramscope.visibility = Visibility(Visibility.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=14169
                                              // template parameters should be public
    tempinst.declareParameters(paramscope);
    paramscope.pop();

    // Add members of template instance to template instance symbol table
    //parent = scope.scopesym;
    tempinst.symtab = new DsymbolTable();

    tempinst.members.foreachDsymbol( (s)
    {
        static if (LOG)
        {
            printf("\t adding member '%s' %p kind %s to '%s'\n", s.toChars(), s, s.kind(), tempinst.toChars());
        }
        s.addMember(_scope, tempinst);
    });

    static if (LOG)
    {
        printf("adding members done\n");
    }

    /* See if there is only one member of template instance, and that
     * member has the same name as the template instance.
     * If so, this template instance becomes an alias for that member.
     */
    //printf("members.length = %d\n", tempinst.members.length);
    if (tempinst.members.length)
    {
        Dsymbol s;
        if (Dsymbol.oneMembers(tempinst.members, s, tempdecl.ident) && s)
        {
            //printf("tempdecl.ident = %s, s = `%s %s`\n", tempdecl.ident.toChars(), s.kind(), s.toPrettyChars());
            //printf("setting aliasdecl\n");
            tempinst.aliasdecl = s;
        }
    }

    /* If function template declaration
     */
    if (argumentList.length > 0 && tempinst.aliasdecl)
    {
        if (auto fd = tempinst.aliasdecl.isFuncDeclaration())
        {
            /* Transmit fargs to type so that TypeFunction.dsymbolSemantic() can
             * resolve any "auto ref" storage classes.
             */
            if (fd.type)
                if (auto tf = fd.type.isTypeFunction())
                    tf.inferenceArguments = argumentList;
        }
    }

    // Do semantic() analysis on template instance members
    static if (LOG)
    {
        printf("\tdo semantic() on template instance members '%s'\n", tempinst.toChars());
    }
    Scope* sc2;
    sc2 = _scope.push(tempinst);
    //printf("enclosing = %d, sc.parent = %s\n", tempinst.enclosing, sc.parent.toChars());
    sc2.parent = tempinst;
    sc2.tinst = tempinst;
    sc2.minst = tempinst.minst;
    sc2.stc &= ~STC.deprecated_;
    tempinst.tryExpandMembers(sc2);

    tempinst.semanticRun = PASS.semanticdone;

    /* ConditionalDeclaration may introduce eponymous declaration,
     * so we should find it once again after semantic.
     */
    if (tempinst.members.length)
    {
        Dsymbol s;
        if (Dsymbol.oneMembers(tempinst.members, s, tempdecl.ident) && s)
        {
            if (!tempinst.aliasdecl || tempinst.aliasdecl != s)
            {
                //printf("tempdecl.ident = %s, s = `%s %s`\n", tempdecl.ident.toChars(), s.kind(), s.toPrettyChars());
                //printf("setting aliasdecl 2\n");
                tempinst.aliasdecl = s;
            }
        }
    }

    if (global.errors != errorsave)
        goto Laftersemantic;

    /* If any of the instantiation members didn't get semantic() run
     * on them due to forward references, we cannot run semantic2()
     * or semantic3() yet.
     */
    {
        bool found_deferred_ad = false;
        for (size_t i = 0; i < Module.deferred.length; i++)
        {
            Dsymbol sd = Module.deferred[i];
            AggregateDeclaration ad = sd.isAggregateDeclaration();
            if (ad && ad.parent && ad.parent.isTemplateInstance())
            {
                //printf("deferred template aggregate: %s %s\n",
                //        sd.parent.toChars(), sd.toChars());
                found_deferred_ad = true;
                if (ad.parent == tempinst)
                {
                    ad.deferred = tempinst;
                    break;
                }
            }
        }
        if (found_deferred_ad || Module.deferred.length)
            goto Laftersemantic;
    }

    /* The problem is when to parse the initializer for a variable.
     * Perhaps VarDeclaration.dsymbolSemantic() should do it like it does
     * for initializers inside a function.
     */
    //if (sc.parent.isFuncDeclaration())
    {
        /* https://issues.dlang.org/show_bug.cgi?id=782
         * this has problems if the classes this depends on
         * are forward referenced. Find a way to defer semantic()
         * on this template.
         */
        tempinst.semantic2(sc2);
    }
    if (global.errors != errorsave)
        goto Laftersemantic;

    if ((sc.func || (sc.flags & SCOPE.fullinst)) && !tempinst.tinst)
    {
        /* If a template is instantiated inside function, the whole instantiation
         * should be done at that position. But, immediate running semantic3 of
         * dependent templates may cause unresolved forward reference.
         * https://issues.dlang.org/show_bug.cgi?id=9050
         * To avoid the issue, don't run semantic3 until semantic and semantic2 done.
         */
        TemplateInstances deferred;
        tempinst.deferred = &deferred;

        //printf("Run semantic3 on %s\n", toChars());

        /* https://issues.dlang.org/show_bug.cgi?id=23965
         * DRuntime hooks are not deprecated, but may be used for deprecated
         * types. Deprecations are disabled while analysing hooks to avoid
         * spurious error messages.
         */
        auto saveUseDeprecated = global.params.useDeprecated;
        if (sc.isDeprecated() && isDRuntimeHook(tempinst.name))
            global.params.useDeprecated = DiagnosticReporting.off;

        tempinst.trySemantic3(sc2);

        global.params.useDeprecated = saveUseDeprecated;

        for (size_t i = 0; i < deferred.length; i++)
        {
            //printf("+ run deferred semantic3 on %s\n", deferred[i].toChars());
            deferred[i].semantic3(null);
        }

        tempinst.deferred = null;
    }
    else if (tempinst.tinst)
    {
        bool doSemantic3 = false;
        FuncDeclaration fd;
        if (tempinst.aliasdecl)
            fd = tempinst.aliasdecl.toAlias2().isFuncDeclaration();

        if (fd)
        {
            /* Template function instantiation should run semantic3 immediately
             * for attribute inference.
             */
            scope fld = fd.isFuncLiteralDeclaration();
            if (fld && fld.tok == TOK.reserved)
                doSemantic3 = true;
            else if (sc.func)
                doSemantic3 = true;
        }
        else if (sc.func)
        {
            /* A lambda function in template arguments might capture the
             * instantiated scope context. For the correct context inference,
             * all instantiated functions should run the semantic3 immediately.
             * See also compilable/test14973.d
             */
            foreach (oarg; tempinst.tdtypes)
            {
                auto s = getDsymbol(oarg);
                if (!s)
                    continue;

                if (auto td = s.isTemplateDeclaration())
                {
                    if (!td.literal)
                        continue;
                    assert(td.members && td.members.length == 1);
                    s = (*td.members)[0];
                }
                if (auto fld = s.isFuncLiteralDeclaration())
                {
                    if (fld.tok == TOK.reserved)
                    {
                        doSemantic3 = true;
                        break;
                    }
                }
            }
            //printf("[%s] %s doSemantic3 = %d\n", tempinst.tinst.loc.toChars(), tempinst.tinst.toChars(), doSemantic3);
        }
        if (doSemantic3)
            tempinst.trySemantic3(sc2);

        TemplateInstance ti = tempinst.tinst;
        int nest = 0;
        while (ti && !ti.deferred && ti.tinst)
        {
            ti = ti.tinst;
            if (++nest > global.recursionLimit)
            {
                global.gag = 0; // ensure error message gets printed
                .error(tempinst.loc, "%s `%s` recursive expansion", tempinst.kind, tempinst.toPrettyChars);
                fatal();
            }
        }
        if (ti && ti.deferred)
        {
            //printf("deferred semantic3 of %p %s, ti = %s, ti.deferred = %p\n", this, toChars(), ti.toChars());
            for (size_t i = 0;; i++)
            {
                if (i == ti.deferred.length)
                {
                    ti.deferred.push(tempinst);
                    break;
                }
                if ((*ti.deferred)[i] == tempinst)
                    break;
            }
        }
    }

    if (tempinst.aliasdecl)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=13816
         * AliasDeclaration tries to resolve forward reference
         * twice (See inuse check in AliasDeclaration.toAlias()). It's
         * necessary to resolve mutual references of instantiated symbols, but
         * it will left a true recursive alias in tuple declaration - an
         * AliasDeclaration A refers TupleDeclaration B, and B contains A
         * in its elements.  To correctly make it an error, we strictly need to
         * resolve the alias of eponymous member.
         */
        tempinst.aliasdecl = tempinst.aliasdecl.toAlias2();

        // stop AliasAssign tuple building
        if (auto td = tempinst.aliasdecl.isTupleDeclaration())
            td.building = false;
    }

Laftersemantic:
    sc2.pop();
    _scope.pop();

    // Give additional context info if error occurred during instantiation
    if (global.errors != errorsave)
    {
        if (!tempinst.errors)
        {
            if (!tempdecl.literal)
                .error(tempinst.loc, "%s `%s` error instantiating", tempinst.kind, tempinst.toPrettyChars);
            if (tempinst.tinst)
                tempinst.tinst.printInstantiationTrace();
        }
        tempinst.errors = true;
        if (tempinst.gagged)
        {
            // Errors are gagged, so remove the template instance from the
            // instance/symbol lists we added it to and reset our state to
            // finish clean and so we can try to instantiate it again later
            // (see https://issues.dlang.org/show_bug.cgi?id=4302 and https://issues.dlang.org/show_bug.cgi?id=6602).
            tempdecl.removeInstance(tempdecl_instance_idx);
            if (target_symbol_list)
            {
                // Because we added 'this' in the last position above, we
                // should be able to remove it without messing other indices up.
                assert((*target_symbol_list)[target_symbol_list_idx] == tempinst);
                target_symbol_list.remove(target_symbol_list_idx);
                tempinst.memberOf = null;                    // no longer a member
            }
            tempinst.semanticRun = PASS.initial;
            tempinst.inst = null;
            tempinst.symtab = null;
        }
    }
    else if (errinst)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=14541
         * If the previous gagged instance had failed by
         * circular references, currrent "error reproduction instantiation"
         * might succeed, because of the difference of instantiated context.
         * On such case, the cached error instance needs to be overridden by the
         * succeeded instance.
         */
        //printf("replaceInstance()\n");
        assert(errinst.errors);
        auto ti1 = TemplateInstanceBox(errinst);
        tempdecl.instances.remove(ti1);

        auto ti2 = TemplateInstanceBox(tempinst);
        tempdecl.instances[ti2] = tempinst;
    }

    static if (LOG)
    {
        printf("-TemplateInstance.dsymbolSemantic('%s', this=%p)\n", tempinst.toChars(), tempinst);
    }
}

/******************************************************
 * Do template instance semantic for isAliasSeq templates.
 * This is a greatly simplified version of templateInstanceSemantic().
 */
private
void aliasSeqInstanceSemantic(TemplateInstance tempinst, Scope* sc, TemplateDeclaration tempdecl)
{
    //printf("[%s] aliasSeqInstance.dsymbolSemantic('%s')\n", tempinst.loc.toChars(), tempinst.toChars());
    Scope* paramscope = sc.push();
    paramscope.stc = 0;
    paramscope.visibility = Visibility(Visibility.Kind.public_);

    TemplateTupleParameter ttp = (*tempdecl.parameters)[0].isTemplateTupleParameter();
    Tuple va = tempinst.tdtypes[0].isTuple();
    Declaration d = new TupleDeclaration(tempinst.loc, ttp.ident, &va.objects);
    d.storage_class |= STC.templateparameter;
    d.dsymbolSemantic(sc);

    paramscope.pop();

    tempinst.aliasdecl = d;

    tempinst.semanticRun = PASS.semanticdone;
}

/******************************************************
 * Do template instance semantic for isAlias templates.
 * This is a greatly simplified version of templateInstanceSemantic().
 */
private
void aliasInstanceSemantic(TemplateInstance tempinst, Scope* sc, TemplateDeclaration tempdecl)
{
    //printf("[%s] aliasInstance.dsymbolSemantic('%s')\n", tempinst.loc.toChars(), tempinst.toChars());
    Scope* paramscope = sc.push();
    paramscope.stc = 0;
    paramscope.visibility = Visibility(Visibility.Kind.public_);

    TemplateTypeParameter ttp = (*tempdecl.parameters)[0].isTemplateTypeParameter();
    Type ta = tempinst.tdtypes[0].isType();
    auto ad = tempdecl.onemember.isAliasDeclaration();

    // Note: qualifiers can be in both 'ad.type.mod' and 'ad.storage_class'
    Declaration d = new AliasDeclaration(tempinst.loc, ttp.ident, ta.addMod(ad.type.mod));
    d.storage_class |= STC.templateparameter | ad.storage_class;
    d.dsymbolSemantic(sc);

    paramscope.pop();

    tempinst.aliasdecl = d;

    tempinst.semanticRun = PASS.semanticdone;
}

// function used to perform semantic on AliasDeclaration
void aliasSemantic(AliasDeclaration ds, Scope* sc)
{
    //printf("AliasDeclaration::semantic() %s\n", ds.toChars());

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

    if (aliassym.adFlags & Declaration.wasRead)
    {
        if (!aliassym.errors)
            error(ds.loc, "%s was read, so cannot reassign", aliassym.toChars());
        aliassym.errors = true;
        return errorRet();
    }

    aliassym.adFlags |= Declaration.ignoreRead; // temporarilly allow reads of aliassym

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
        if (!ti.tempinst.findTempDecl(sc, null))
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


    aliassym.adFlags &= ~Declaration.ignoreRead;

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
    if (!TemplateInstance.semanticTiargs(tempinst.loc, sc, tempinst.tiargs, 0, td))
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
            if (!td || !td.isTrivialAliasSeq)
                return 1;
            return 0;
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

/// Do an atomic operation (currently tailored to [shared] static ctors|dtors) needs
private CallExp doAtomicOp (string op, Identifier var, Expression arg)
{
    assert(op == "-=" || op == "+=");

    Module mod = Module.loadCoreAtomic();
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
 *      loc = location to adjust
 *      mixinOut = sink for mixin text data
 * Returns:
 *      adjusted loc suitable for Parser
 */

Loc adjustLocForMixin(const(char)[] input, ref const Loc loc, ref Output mixinOut)
{
    Loc result;
    if (mixinOut.doOutput)
    {
        const lines = mixinOut.bufferLines;
        writeMixin(input, loc, mixinOut.bufferLines, *mixinOut.buffer);
        result = Loc(mixinOut.name.ptr, lines + 2, loc.charnum);
    }
    else if (loc.filename)
    {
        /* Create a pseudo-filename for the mixin string, as it may not even exist
         * in the source file.
         */
        auto len = strlen(loc.filename) + 7 + (loc.linnum).sizeof * 3 + 1;
        char* filename = cast(char*)mem.xmalloc(len);
        snprintf(filename, len, "%s-mixin-%d", loc.filename, cast(int)loc.linnum);
        result = Loc(filename, loc.linnum, loc.charnum);
    }
    else
        result = loc;
    return result;
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
private void writeMixin(const(char)[] s, ref const Loc loc, ref int lines, ref OutBuffer buf)
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

/**
 * Check signature of `pragma(printf)` function, print error if invalid.
 *
 * printf/scanf-like functions must be of the form:
 *    extern (C/C++) T printf([parameters...], const(char)* format, ...);
 * or:
 *    extern (C/C++) T vprintf([parameters...], const(char)* format, va_list);
 *
 * Params:
 *      funcdecl = function to check
 *      f = function type
 *      sc = scope
 */
void checkPrintfScanfSignature(FuncDeclaration funcdecl, TypeFunction f, Scope* sc)
{
    static bool isPointerToChar(Parameter p)
    {
        if (auto tptr = p.type.isTypePointer())
        {
            return tptr.next.ty == Tchar;
        }
        return false;
    }

    bool isVa_list(Parameter p)
    {
        return p.type.equals(target.va_listType(funcdecl.loc, sc));
    }

    const nparams = f.parameterList.length;
    const p = (funcdecl.printf ? Id.printf : Id.scanf).toChars();
    if (!(f.linkage == LINK.c || f.linkage == LINK.cpp))
    {
        .error(funcdecl.loc, "`pragma(%s)` function `%s` must have `extern(C)` or `extern(C++)` linkage,"
            ~" not `extern(%s)`",
            p, funcdecl.toChars(), f.linkage.linkageToChars());
    }
    if (f.parameterList.varargs == VarArg.variadic)
    {
        if (!(nparams >= 1 && isPointerToChar(f.parameterList[nparams - 1])))
        {
            .error(funcdecl.loc, "`pragma(%s)` function `%s` must have"
                ~ " signature `%s %s([parameters...], const(char)*, ...)` not `%s`",
                p, funcdecl.toChars(), f.next.toChars(), funcdecl.toChars(), funcdecl.type.toChars());
        }
    }
    else if (f.parameterList.varargs == VarArg.none)
    {
        if(!(nparams >= 2 && isPointerToChar(f.parameterList[nparams - 2]) &&
            isVa_list(f.parameterList[nparams - 1])))
            .error(funcdecl.loc, "`pragma(%s)` function `%s` must have"~
                " signature `%s %s([parameters...], const(char)*, va_list)`",
                p, funcdecl.toChars(), f.next.toChars(), funcdecl.toChars());
    }
    else
    {
        .error(funcdecl.loc, "`pragma(%s)` function `%s` must have C-style variadic `...` or `va_list` parameter",
            p, funcdecl.toChars());
    }
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
Dsymbol search(Dsymbol d, const ref Loc loc, Identifier ident, SearchOptFlags flags = SearchOpt.all)
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

    this(const ref Loc loc, Identifier ident, SearchOptFlags flags)
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
            return setResult(null);

        VarDeclaration* pvar;
        Expression ce;

        static Dsymbol dollarFromTypeTuple(const ref Loc loc, TypeTuple tt, Scope* sc)
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
                    auto tiargs = new Objects();
                    Expression edim = new IntegerExp(Loc.initial, dim, Type.tsize_t);
                    edim = edim.expressionSemantic(ass._scope);
                    tiargs.push(edim);
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

        uint errors = global.errors;

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
            else if (s == cd) // happens if s is nested in this and derives from this
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

    this(Scope* sc)
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

    this(Scope* sc)
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
        Scope* sc = Scope.createGlobal(m, global.errorSink); // create root scope

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
            auto im = new Import(Loc.initial, null, Id.object, null, 0);
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
        sc.pop(); // 2 pops because Scope.createGlobal() created 2
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
                    uint preverrors = global.errors;
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
    alias visit = Visitor.visit;

    AggregateDeclaration ad;
    FieldState* fieldState;
    bool isunion;

    this(AggregateDeclaration ad, FieldState* fieldState, bool isunion)
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
        vd.offset = placeField(
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

        void startNewField()
        {
            if (log) printf("startNewField()\n");
            uint alignsize;
            if (style == TargetC.BitFieldStyle.Gcc_Clang)
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
            else
                alignsize = memsize; // not memalignsize

            uint dummy;
            bfd.offset = placeField(
                fieldState.offset,
                memsize, alignsize, bfd.alignment,
                ad.structsize,
                (anon && style == TargetC.BitFieldStyle.Gcc_Clang) ? dummy : ad.alignsize,
                isunion);

            fieldState.inFlight = true;
            fieldState.fieldOffset = bfd.offset;
            fieldState.bitOffset = 0;
            fieldState.fieldSize = memsize;
        }

        if (style == TargetC.BitFieldStyle.Gcc_Clang)
        {
            if (bfd.fieldWidth == 0)
            {
                if (!isunion)
                {
                    // Use type of zero width field to align to next field
                    fieldState.offset = (fieldState.offset + memalignsize - 1) & ~(memalignsize - 1);
                    ad.structsize = fieldState.offset;
                }

                fieldState.inFlight = false;
                return;
            }

            if (ad.alignsize == 0)
                ad.alignsize = 1;
            if (!anon &&
                  ad.alignsize < memalignsize)
                ad.alignsize = memalignsize;
        }
        else if (style == TargetC.BitFieldStyle.MS)
        {
            if (ad.alignsize == 0)
                ad.alignsize = 1;
            if (bfd.fieldWidth == 0)
            {
                if (fieldState.inFlight && !isunion)
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
        }
        else if (style == TargetC.BitFieldStyle.DM)
        {
            if (anon && bfd.fieldWidth && (!fieldState.inFlight || fieldState.bitOffset == 0))
                return;  // this probably should be a bug in DMC
            if (ad.alignsize == 0)
                ad.alignsize = 1;
            if (bfd.fieldWidth == 0)
            {
                if (fieldState.inFlight && !isunion)
                {
                    const alsz = memsize;
                    fieldState.offset = (fieldState.offset + alsz - 1) & ~(alsz - 1);
                    ad.structsize = fieldState.offset;
                }

                fieldState.inFlight = false;
                return;
            }
        }

        if (!fieldState.inFlight)
        {
            //printf("not in flight\n");
            startNewField();
        }
        else if (style == TargetC.BitFieldStyle.Gcc_Clang)
        {
            // If the bit-field spans more units of alignment than its type,
            // start a new field at the next alignment boundary.
            if (fieldState.bitOffset == fieldState.fieldSize * 8 &&
                fieldState.bitOffset + bfd.fieldWidth > memalignsize * 8)
            {
                if (log) printf("more units of alignment than its type\n");
                startNewField();        // the bit field is full
            }
            else
            {
                // if alignment boundary is crossed
                uint start = fieldState.fieldOffset * 8 + fieldState.bitOffset;
                uint end   = start + bfd.fieldWidth;
                //printf("%s start: %d end: %d memalignsize: %d\n", ad.toChars(), start, end, memalignsize);
                if (start / (memalignsize * 8) != (end - 1) / (memalignsize * 8))
                {
                    if (log) printf("alignment is crossed\n");
                    startNewField();
                }
            }
        }
        else if (style == TargetC.BitFieldStyle.DM ||
                 style == TargetC.BitFieldStyle.MS)
        {
            if (memsize != fieldState.fieldSize ||
                fieldState.bitOffset + bfd.fieldWidth > fieldState.fieldSize * 8)
            {
                //printf("new field\n");
                startNewField();
            }
        }
        else
            assert(0);

        bfd.offset = fieldState.fieldOffset;
        bfd.bitOffset = fieldState.bitOffset;

        const pastField = bfd.bitOffset + bfd.fieldWidth;
        if (style == TargetC.BitFieldStyle.Gcc_Clang)
        {
            auto size = (pastField + 7) / 8;
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
        else
            fieldState.fieldSize = memsize;
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
            anond.anonoffset = placeField(
                fieldState.offset,
                anond.anonstructsize, anond.anonalignsize, alignment,
                ad.structsize, ad.alignsize,
                isunion);

            // Add to the anon fields the base offset of this anonymous aggregate
            //printf("anon fields, anonoffset = %d\n", anonoffset);
            foreach (const i; fieldstart .. ad.fields.length)
            {
                VarDeclaration v = ad.fields[i];
                //printf("\t[%d] %s %d\n", i, v.toChars(), v.offset);
                v.offset += anond.anonoffset;
            }
        }
    }
}
