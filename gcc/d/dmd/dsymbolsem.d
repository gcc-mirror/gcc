/**
 * Does the semantic 1 pass on the AST, which looks at symbol declarations but not initializers
 * or function bodies.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
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
import dmd.apply;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.blockexit;
import dmd.clone;
import dmd.compiler;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmangle;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.dversion;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
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
import dmd.nogc;
import dmd.nspace;
import dmd.objc;
import dmd.opover;
import dmd.parse;
import dmd.root.array;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.root.utf;
import dmd.semantic2;
import dmd.semantic3;
import dmd.sideeffect;
import dmd.statementsem;
import dmd.staticassert;
import dmd.tokens;
import dmd.utils;
import dmd.statement;
import dmd.target;
import dmd.templateparamsem;
import dmd.typesem;
import dmd.visitor;

enum LOG = false;

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

/*************************************
 * Does semantic analysis on the public face of declarations.
 */
extern(C++) void dsymbolSemantic(Dsymbol dsym, Scope* sc)
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
            dd.msg.error("compile time constant expected, not `%s`", dd.msg.toChars());
    }
    return dd.msgstr;
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

private extern(C++) final class DsymbolSemanticVisitor : Visitor
{
    alias visit = Visitor.visit;

    Scope* sc;
    this(Scope* sc) scope
    {
        this.sc = sc;
    }

    // Save the scope and defer semantic analysis on the Dsymbol.
    private void deferDsymbolSemantic(Dsymbol s, Scope *scx)
    {
        s._scope = scx ? scx : sc.copy();
        s._scope.setNoFree();
        Module.addDeferredSemantic(s);
    }

    override void visit(Dsymbol dsym)
    {
        dsym.error("%p has no semantic routine", dsym);
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

        // @@@DEPRECATED_2.121@@@
        // Deprecated in 2.101 - Can be removed in 2.121
        if (ad.isClassDeclaration() || ad.isInterfaceDeclaration())
            deprecation(dsym.loc, "alias this for classes/interfaces is deprecated");

        assert(ad.members);
        Dsymbol s = ad.search(dsym.loc, dsym.ident);
        if (!s)
        {
            s = sc.search(dsym.loc, dsym.ident, null);
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
            printf("linkage = %d\n", dsym.linkage);
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
            dsym.error("extern symbols cannot have initializers");

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
            bool needctfe = (dsym.storage_class & (STC.manifest | STC.static_)) != 0;
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
                dsym.error("- type `%s` is inferred from initializer `%s`, and variables cannot be of type `void`", dsym.type.toChars(), dsym._init.toChars());
            }
            else
                dsym.error("- variables cannot be of type `void`");
            dsym.type = Type.terror;
            tb = dsym.type;
        }
        if (tb.ty == Tfunction)
        {
            dsym.error("cannot be declared to be a function");
            dsym.type = Type.terror;
            tb = dsym.type;
        }
        if (auto ts = tb.isTypeStruct())
        {
            // Require declarations, except when it's just a reference (as done for pointers)
            // or when the variable is defined externally
            if (!ts.sym.members && !(dsym.storage_class & (STC.ref_ | STC.extern_)))
            {
                dsym.error("- no definition of struct `%s`", ts.toChars());

                // Explain why the definition is required when it's part of another type
                if (!dsym.type.isTypeStruct())
                {
                    // Prefer Loc of the dependant type
                    const s = dsym.type.toDsymbol(sc);
                    const loc = (s ? s : dsym).loc;
                    loc.errorSupplemental("required by type `%s`", dsym.type.toChars());
                }

                // Flag variable as error to avoid invalid error messages due to unknown size
                dsym.type = Type.terror;
            }
        }
        if ((dsym.storage_class & STC.auto_) && !inferred)
            dsym.error("- storage class `auto` has no effect if type is not inferred, did you mean `scope`?");

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
                    error(dsym.loc, "tuple of %d elements cannot be assigned to tuple of %d elements", cast(int)tedim, cast(int)nelems);
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
                dsym.error("cannot be `final`, perhaps you meant `const`?");
            else
            {
                OutBuffer buf;
                stcToBuffer(&buf, stc);
                dsym.error("cannot be `%s`", buf.peekChars());
            }
            dsym.storage_class &= ~stc; // strip off
        }

        // At this point we can add `scope` to the STC instead of `in`,
        // because we are never going to use this variable's STC for user messages
        if (dsym.storage_class & STC.in_ && global.params.previewIn)
            dsym.storage_class |= STC.scope_;

        if (dsym.storage_class & STC.scope_)
        {
            StorageClass stc = dsym.storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.gshared);
            if (stc)
            {
                OutBuffer buf;
                stcToBuffer(&buf, stc);
                dsym.error("cannot be `scope` and `%s`", buf.peekChars());
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
                if (global.params.vfield && dsym.storage_class & (STC.const_ | STC.immutable_) && dsym._init && !dsym._init.isVoidInitializer())
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
                    dsym.error("- cannot use template to add field to aggregate `%s`", ad2.toChars());
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
            dsym.error("- only parameters, functions and `foreach` declarations can be `ref`");
        }

        if (dsym.type.hasWild())
        {
            if (dsym.storage_class & (STC.static_ | STC.extern_ | STC.gshared | STC.manifest | STC.field) || dsym.isDataseg())
            {
                dsym.error("- only parameters or stack-based variables can be `inout`");
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
                    dsym.error("- `inout` variables can only be declared inside `inout` functions");
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
                    dsym.error("- default construction is disabled for type `%s`", dsym.type.toChars());
            }
        }

        FuncDeclaration fd = parent.isFuncDeclaration();
        if (dsym.type.isscope() && !(dsym.storage_class & STC.nodtor))
        {
            if (dsym.storage_class & (STC.field | STC.out_ | STC.ref_ | STC.static_ | STC.manifest | STC.gshared) || !fd)
            {
                dsym.error("globals, statics, fields, manifest constants, ref and out parameters cannot be `scope`");
            }

            // @@@DEPRECATED_2.097@@@  https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
            // Deprecated in 2.087
            // Remove this when the feature is removed from the language
            if (!(dsym.storage_class & STC.scope_))
            {
                if (!(dsym.storage_class & STC.parameter) && dsym.ident != Id.withSym)
                    dsym.error("reference to `scope class` must be `scope`");
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
            dsym.error("- manifest constants must have initializers");

        // Don't allow non-extern, non-__gshared variables to be interfaced with C++
        if (dsym._linkage == LINK.cpp && !(dsym.storage_class & (STC.ctfe | STC.extern_ | STC.gshared)) && dsym.isDataseg())
        {
            const char* p = (dsym.storage_class & STC.shared_) ? "shared" : "static";
            dsym.error("cannot have `extern(C++)` linkage because it is `%s`", p);
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
                dsym.error("- size of type `%s` is invalid", dsym.type.toChars());

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
                dsym.error("of type `%s` does not have a default initializer", dsym.type.toChars());
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
                dsym.error("- incomplete array type must have initializer");
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
                                dsym.error("is not a static and cannot have static initializer");
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
                                    if (sc.func.isSafe())
                                    {
                                        // @@@DEPRECATED_2.112@@@
                                        deprecation(dsym.loc,
                                            "`scope` allocation of `%s` requires that constructor be annotated with `scope`",
                                            dsym.toChars());
                                        deprecationSupplemental(ne.member.loc, "is the location of the constructor");
                                     }
                                     else
                                         sc.func.setUnsafe();
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
                            if (global.params.useDIP1000 == FeatureState.enabled
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
                    const init_err = dsym._init.isExpInitializer();
                    if (init_err && init_err.exp.op == EXP.showCtfeContext)
                    {
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
                            exp = exp.expressionSemantic(sc);
                            exp = resolveProperties(sc, exp);
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
                                    dsym.error("of type struct `%s` uses `this(this)`, which is not allowed in static initialization", tb2.toChars());
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
                    dsym.error("static storage variables cannot have destructors");
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

    override void visit(BitFieldDeclaration dsym)
    {
        //printf("BitField::semantic('%s')\n", dsym.toChars());
        if (dsym.semanticRun >= PASS.semanticdone)
            return;

        visit(cast(VarDeclaration)dsym);
        if (dsym.errors)
            return;

        if (!dsym.parent.isStructDeclaration() && !dsym.parent.isClassDeclaration())
        {
            dsym.error("- bit-field must be member of struct, union, or class");
        }

        sc = sc.startCTFE();
        auto width = dsym.width.expressionSemantic(sc);
        sc = sc.endCTFE();
        width = width.ctfeInterpret();
        if (!dsym.type.isintegral())
        {
            // C11 6.7.2.1-5
            width.error("bit-field type `%s` is not an integer type", dsym.type.toChars());
            dsym.errors = true;
        }
        if (!width.isIntegerExp())
        {
            width.error("bit-field width `%s` is not an integer constant", dsym.width.toChars());
            dsym.errors = true;
        }
        const uwidth = width.toInteger(); // uwidth is unsigned
        if (uwidth == 0 && !dsym.isAnonymous())
        {
            width.error("bit-field `%s` has zero width", dsym.toChars());
            dsym.errors = true;
        }
        const sz = dsym.type.size();
        if (sz == SIZE_INVALID)
            dsym.errors = true;
        const max_width = sz * 8;
        if (uwidth > max_width)
        {
            width.error("width `%lld` of bit-field `%s` does not fit in type `%s`", cast(long)uwidth, dsym.toChars(), dsym.type.toChars());
            dsym.errors = true;
        }
        dsym.fieldWidth = cast(uint)uwidth;
    }

    override void visit(Import imp)
    {
        static if (LOG)
        {
            printf("Import::semantic('%s') %s\n", toPrettyChars(), id.toChars());
            scope(exit)
                printf("-Import::semantic('%s'), pkg = %p\n", toChars(), pkg);
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
                return;

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
                Dsymbol sym = imp.mod.search(imp.loc, imp.names[i], IgnorePrivateImports);
                if (sym)
                {
                    import dmd.access : symbolIsVisible;
                    if (!symbolIsVisible(sc, sym))
                        imp.mod.error(imp.loc, "member `%s` is not visible from module `%s`",
                            imp.names[i].toChars(), sc._module.toChars());
                    ad.dsymbolSemantic(sc);
                    // If the import declaration is in non-root module,
                    // analysis of the aliased symbol is deferred.
                    // Therefore, don't see the ad.aliassym or ad.type here.
                }
                else
                {
                    Dsymbol s = imp.mod.search_correct(imp.names[i]);
                    if (s)
                        imp.mod.error(imp.loc, "import `%s` not found, did you mean %s `%s`?", imp.names[i].toChars(), s.kind(), s.toPrettyChars());
                    else
                        imp.mod.error(imp.loc, "import `%s` not found", imp.names[i].toChars());
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
        visibilityToBuffer(ob, imp.visibility);
        ob.writeByte(' ');
        if (imp.isstatic)
        {
            stcToBuffer(ob, STC.static_);
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
        StringExp verifyMangleString(ref Expression e)
        {
            auto se = semanticString(sc, e, "mangled name");
            if (!se)
                return null;
            e = se;
            if (!se.len)
            {
                pd.error("- zero-length string not allowed for mangled name");
                return null;
            }
            if (se.sz != 1)
            {
                pd.error("- mangled name characters can only be of type `char`");
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
                            pd.error("char 0x%02x not allowed in mangled name", c);
                            break;
                        }
                    }
                    if (const msg = utf_decodeChar(slice, i, c))
                    {
                        pd.error("%.*s", cast(int)msg.length, msg.ptr);
                        break;
                    }
                    if (!isUniAlpha(c))
                    {
                        pd.error("char `0x%04x` not allowed in mangled name", c);
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
                            e.error("must be a string");
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
                    pd.error("cannot apply to a template declaration");
                    errorSupplemental(pd.loc, "use `template Class(Args...){ pragma(mangle, \"other_name\") class Class {} }`");
                }
                else if (auto se = verifyMangleString((*pd.args)[0]))
                {
                    const name = (cast(const(char)[])se.peekData()).xarraydup;
                    uint cnt = setMangleOverride(s, name);
                    if (cnt > 1)
                        pd.error("can only apply to a single declaration");
                }
            }
        }

        void noDeclarations()
        {
            if (pd.decl)
            {
                pd.error("is missing a terminating `;`");
                declarations();
                // do them anyway, to avoid segfaults.
            }
        }

        // Should be merged with PragmaStatement
        //printf("\tPragmaDeclaration::semantic '%s'\n", pd.toChars());
        if (target.supportsLinkerDirective())
        {
            if (pd.ident == Id.linkerDirective)
            {
                if (!pd.args || pd.args.length != 1)
                    pd.error("one string argument expected for pragma(linkerDirective)");
                else
                {
                    auto se = semanticString(sc, (*pd.args)[0], "linker directive");
                    if (!se)
                        return noDeclarations();
                    (*pd.args)[0] = se;
                    if (global.params.verbose)
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
                pd.error("string expected for library name");
            else
            {
                auto se = semanticString(sc, (*pd.args)[0], "library name");
                if (!se)
                    return noDeclarations();
                (*pd.args)[0] = se;

                auto name = se.peekString().xarraydup;
                if (global.params.verbose)
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
                pd.error(pd.args.length == 0 ? "- string expected for mangled name"
                                          : "expected 1 or 2 arguments");
                pd.args.setDim(1);
                (*pd.args)[0] = ErrorExp.get(); // error recovery
            }
            return declarations();
        }
        else if (pd.ident == Id.crt_constructor || pd.ident == Id.crt_destructor)
        {
            if (pd.args && pd.args.length != 0)
                pd.error("takes no argument");
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
                    pd.error("can only apply to a single declaration");
            }
            return declarations();
        }
        else if (pd.ident == Id.printf || pd.ident == Id.scanf)
        {
            if (pd.args && pd.args.length != 0)
                pd.error("takes no argument");
            return declarations();
        }
        else if (!global.params.ignoreUnsupportedPragmas)
        {
            error(pd.loc, "unrecognized `pragma(%s)`", pd.ident.toChars());
            return declarations();
        }

        if (!global.params.verbose)
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

    override void visit(StaticIfDeclaration sid)
    {
        attribSemantic(sid);
    }

    override void visit(StaticForeachDeclaration sfd)
    {
        attribSemantic(sfd);
    }

    private Dsymbols* compileIt(CompileDeclaration cd)
    {
        //printf("CompileDeclaration::compileIt(loc = %d) %s\n", cd.loc.linnum, cd.exp.toChars());
        OutBuffer buf;
        if (expressionsToString(buf, sc, cd.exps))
            return null;

        const errors = global.errors;
        const len = buf.length;
        buf.writeByte(0);
        const str = buf.extractSlice()[0 .. len];
        scope p = new Parser!ASTCodegen(cd.loc, sc._module, str, false, global.errorSink);
        p.nextToken();

        auto d = p.parseDeclDefs(0);
        if (global.errors != errors)
            return null;

        if (p.token.value != TOK.endOfFile)
        {
            cd.error("incomplete mixin declaration `%s`", str.ptr);
            return null;
        }
        return d;
    }

    /***********************************************************
     * https://dlang.org/spec/module.html#mixin-declaration
     */
    override void visit(CompileDeclaration cd)
    {
        //printf("CompileDeclaration::semantic()\n");
        if (!cd.compiled)
        {
            cd.decl = compileIt(cd);
            cd.AttribDeclaration.addMember(sc, cd.scopesym);
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
                ns.exp.error("expected valid identifier for C++ namespace but got `%.*s`",
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
                    ns.exp.error("`%s`: index %llu is not a string constant, it is a `%s`",
                                 ns.exp.toChars(), cast(ulong) d, ns.exp.type.toChars());
            }
        }
        else if (auto se = ns.exp.toStringExp())
            ns.ident = identFromSE(se);
        // Empty Tuple
        else if (ns.exp.isTypeExp() && ns.exp.isTypeExp().type.toBasetype().isTypeTuple())
        {
        }
        else
            ns.exp.error("compile time string constant (or tuple) expected, not `%s`",
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
            sc = Scope.createGlobal(m); // create root scope
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
        //printf("EnumDeclaration::semantic(sd = %p, '%s') %s\n", sc.scopesym, sc.scopesym.toChars(), ed.toChars());
        //printf("EnumDeclaration::semantic() %p %s\n", ed, ed.toChars());
        if (ed.semanticRun >= PASS.semanticdone)
            return; // semantic() already completed
        if (ed.semanticRun == PASS.semantic)
        {
            assert(ed.memtype);
            error(ed.loc, "circular reference to enum base type `%s`", ed.memtype.toChars());
            ed.errors = true;
            ed.semanticRun = PASS.semanticdone;
            return;
        }
        Scope* scx = null;
        if (ed._scope)
        {
            sc = ed._scope;
            scx = ed._scope; // save so we don't make redundant copies
            ed._scope = null;
        }

        if (!sc)
            return;

        ed.parent = sc.parent;
        ed.type = ed.type.typeSemantic(ed.loc, sc);

        ed.visibility = sc.visibility;
        if (sc.stc & STC.deprecated_)
            ed.isdeprecated = true;
        ed.userAttribDecl = sc.userAttribDecl;
        ed.cppnamespace = sc.namespace;

        ed.semanticRun = PASS.semantic;
        UserAttributeDeclaration.checkGNUABITag(ed, sc.linkage);
        checkMustUseReserved(ed);

        if (!ed.members && !ed.memtype) // enum ident;
        {
            ed.semanticRun = PASS.semanticdone;
            return;
        }

        if (!ed.symtab)
            ed.symtab = new DsymbolTable();

        /* The separate, and distinct, cases are:
         *  1. enum { ... }
         *  2. enum : memtype { ... }
         *  3. enum ident { ... }
         *  4. enum ident : memtype { ... }
         *  5. enum ident : memtype;
         *  6. enum ident;
         */

        if (ed.memtype)
        {
            ed.memtype = ed.memtype.typeSemantic(ed.loc, sc);

            /* Check to see if memtype is forward referenced
             */
            if (auto te = ed.memtype.isTypeEnum())
            {
                auto sym = te.toDsymbol(sc).isEnumDeclaration();
                // Special enums like __c_[u]long[long] are fine to forward reference
                // see https://issues.dlang.org/show_bug.cgi?id=20599
                if (!sym.isSpecial() && (!sym.memtype ||  !sym.members || !sym.symtab || sym._scope))
                {
                    // memtype is forward referenced, so try again later
                    deferDsymbolSemantic(ed, scx);
                    //printf("\tdeferring %s\n", toChars());
                    ed.semanticRun = PASS.initial;
                    return;
                }
                else
                    // Ensure that semantic is run to detect. e.g. invalid forward references
                    sym.dsymbolSemantic(sc);
            }
            if (ed.memtype.ty == Tvoid)
            {
                ed.error("base type must not be `void`");
                ed.memtype = Type.terror;
            }
            if (ed.memtype.ty == Terror)
            {
                ed.errors = true;
                // poison all the members
                ed.members.foreachDsymbol( (s) { s.errors = true; } );
                ed.semanticRun = PASS.semanticdone;
                return;
            }
        }

        if (!ed.members) // enum ident : memtype;
        {
            ed.semanticRun = PASS.semanticdone;
            return;
        }

        if (ed.members.length == 0)
        {
            ed.error("enum `%s` must have at least one member", ed.toChars());
            ed.errors = true;
            ed.semanticRun = PASS.semanticdone;
            return;
        }

        if (!(sc.flags & SCOPE.Cfile))  // C enum remains incomplete until members are done
            ed.semanticRun = PASS.semanticdone;

        // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
        // Deprecated in 2.100
        // Make an error in 2.110
        if (sc.stc & STC.scope_)
            deprecation(ed.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");

        Scope* sce;
        if (ed.isAnonymous())
            sce = sc;
        else
        {
            sce = sc.push(ed);
            sce.parent = ed;
        }
        sce = sce.startCTFE();
        sce.setNoFree(); // needed for getMaxMinValue()

        /* Each enum member gets the sce scope
         */
        ed.members.foreachDsymbol( (s)
        {
            EnumMember em = s.isEnumMember();
            if (em)
                em._scope = sce;
        });

        /* addMember() is not called when the EnumDeclaration appears as a function statement,
         * so we have to do what addMember() does and install the enum members in the right symbol
         * table
         */
        addEnumMembers(ed, sc, sc.getScopesym());

        if (sc.flags & SCOPE.Cfile)
        {
            /* C11 6.7.2.2
             */
            assert(ed.memtype);
            int nextValue = 0;        // C11 6.7.2.2-3 first member value defaults to 0

            // C11 6.7.2.2-2 value must be representable as an int.
            // The sizemask represents all values that int will fit into,
            // from 0..uint.max.  We want to cover int.min..uint.max.
            const mask = Type.tint32.sizemask();
            IntRange ir = IntRange(SignExtendedNumber(~(mask >> 1), true),
                                   SignExtendedNumber(mask));

            void emSemantic(EnumMember em, ref int nextValue)
            {
                static void errorReturn(EnumMember em)
                {
                    em.errors = true;
                    em.semanticRun = PASS.semanticdone;
                }

                em.semanticRun = PASS.semantic;
                em.type = Type.tint32;
                em._linkage = LINK.c;
                em.storage_class |= STC.manifest;
                if (em.value)
                {
                    Expression e = em.value;
                    assert(e.dyncast() == DYNCAST.expression);
                    e = e.expressionSemantic(sc);
                    e = resolveProperties(sc, e);
                    e = e.integralPromotions(sc);
                    e = e.ctfeInterpret();
                    if (e.op == EXP.error)
                        return errorReturn(em);
                    auto ie = e.isIntegerExp();
                    if (!ie)
                    {
                        // C11 6.7.2.2-2
                        em.error("enum member must be an integral constant expression, not `%s` of type `%s`", e.toChars(), e.type.toChars());
                        return errorReturn(em);
                    }
                    if (!ir.contains(getIntRange(ie)))
                    {
                        // C11 6.7.2.2-2
                        em.error("enum member value `%s` does not fit in an `int`", e.toChars());
                        return errorReturn(em);
                    }
                    nextValue = cast(int)ie.toInteger();
                    em.value = new IntegerExp(em.loc, nextValue, Type.tint32);
                }
                else
                {
                    // C11 6.7.2.2-3 add 1 to value of previous enumeration constant
                    bool first = (em == (*em.ed.members)[0]);
                    if (!first)
                    {
                        import core.checkedint : adds;
                        bool overflow;
                        nextValue = adds(nextValue, 1, overflow);
                        if (overflow)
                        {
                            em.error("initialization with `%d+1` causes overflow for type `int`", nextValue - 1);
                            return errorReturn(em);
                        }
                    }
                    em.value = new IntegerExp(em.loc, nextValue, Type.tint32);
                }
                em.semanticRun = PASS.semanticdone;
            }

            ed.members.foreachDsymbol( (s)
            {
                if (EnumMember em = s.isEnumMember())
                    emSemantic(em, nextValue);
            });
            ed.semanticRun = PASS.semanticdone;
            return;
        }

        ed.members.foreachDsymbol( (s)
        {
            if (EnumMember em = s.isEnumMember())
                em.dsymbolSemantic(em._scope);
        });
        //printf("defaultval = %lld\n", defaultval);

        //if (defaultval) printf("defaultval: %s %s\n", defaultval.toChars(), defaultval.type.toChars());
        //printf("members = %s\n", members.toChars());
    }

    override void visit(EnumMember em)
    {
        //printf("EnumMember::semantic() %s\n", em.toChars());

        void errorReturn()
        {
            em.errors = true;
            em.semanticRun = PASS.semanticdone;
        }

        if (em.errors || em.semanticRun >= PASS.semanticdone)
            return;
        if (em.semanticRun == PASS.semantic)
        {
            em.error("circular reference to `enum` member");
            return errorReturn();
        }
        assert(em.ed);

        em.ed.dsymbolSemantic(sc);
        if (em.ed.errors)
            return errorReturn();
        if (em.errors || em.semanticRun >= PASS.semanticdone)
            return;

        if (em._scope)
            sc = em._scope;
        if (!sc)
            return;

        em.semanticRun = PASS.semantic;

        em.visibility = em.ed.isAnonymous() ? em.ed.visibility : Visibility(Visibility.Kind.public_);
        em._linkage = LINK.d;
        em.storage_class |= STC.manifest;

        // https://issues.dlang.org/show_bug.cgi?id=9701
        if (em.ed.isAnonymous())
        {
            if (em.userAttribDecl)
                em.userAttribDecl.userAttribDecl = em.ed.userAttribDecl;
            else
                em.userAttribDecl = em.ed.userAttribDecl;
        }

        // Eval UDA in this same scope. Issues 19344, 20835, 21122
        if (em.userAttribDecl)
        {
            // Set scope but avoid extra sc.uda attachment inside setScope()
            auto inneruda = em.userAttribDecl.userAttribDecl;
            em.userAttribDecl.setScope(sc);
            em.userAttribDecl.userAttribDecl = inneruda;
            em.userAttribDecl.dsymbolSemantic(sc);
        }

        // The first enum member is special
        bool first = (em == (*em.ed.members)[0]);

        if (em.origType)
        {
            em.origType = em.origType.typeSemantic(em.loc, sc);
            em.type = em.origType;
            assert(em.value); // "type id;" is not a valid enum member declaration
        }

        if (em.value)
        {
            Expression e = em.value;
            assert(e.dyncast() == DYNCAST.expression);
            e = e.expressionSemantic(sc);
            e = resolveProperties(sc, e);
            e = e.ctfeInterpret();
            if (e.op == EXP.error)
                return errorReturn();
            if (first && !em.ed.memtype && !em.ed.isAnonymous())
            {
                em.ed.memtype = e.type;
                if (em.ed.memtype.ty == Terror)
                {
                    em.ed.errors = true;
                    return errorReturn();
                }
                if (em.ed.memtype.ty != Terror)
                {
                    /* https://issues.dlang.org/show_bug.cgi?id=11746
                     * All of named enum members should have same type
                     * with the first member. If the following members were referenced
                     * during the first member semantic, their types should be unified.
                     */
                    em.ed.members.foreachDsymbol( (s)
                    {
                        EnumMember enm = s.isEnumMember();
                        if (!enm || enm == em || enm.semanticRun < PASS.semanticdone || enm.origType)
                            return;

                        //printf("[%d] em = %s, em.semanticRun = %d\n", i, toChars(), em.semanticRun);
                        Expression ev = enm.value;
                        ev = ev.implicitCastTo(sc, em.ed.memtype);
                        ev = ev.ctfeInterpret();
                        ev = ev.castTo(sc, em.ed.type);
                        if (ev.op == EXP.error)
                            em.ed.errors = true;
                        enm.value = ev;
                    });

                    if (em.ed.errors)
                    {
                        em.ed.memtype = Type.terror;
                        return errorReturn();
                    }
                }
            }

            if (em.ed.memtype && !em.origType)
            {
                e = e.implicitCastTo(sc, em.ed.memtype);
                e = e.ctfeInterpret();

                // save origValue for better json output
                em.origValue = e;

                if (!em.ed.isAnonymous())
                {
                    e = e.castTo(sc, em.ed.type.addMod(e.type.mod)); // https://issues.dlang.org/show_bug.cgi?id=12385
                    e = e.ctfeInterpret();
                }
            }
            else if (em.origType)
            {
                e = e.implicitCastTo(sc, em.origType);
                e = e.ctfeInterpret();
                assert(em.ed.isAnonymous());

                // save origValue for better json output
                em.origValue = e;
            }
            em.value = e;
        }
        else if (first)
        {
            Type t;
            if (em.ed.memtype)
                t = em.ed.memtype;
            else
            {
                t = Type.tint32;
                if (!em.ed.isAnonymous())
                    em.ed.memtype = t;
            }
            Expression e = new IntegerExp(em.loc, 0, t);
            e = e.ctfeInterpret();

            // save origValue for better json output
            em.origValue = e;

            if (!em.ed.isAnonymous())
            {
                e = e.castTo(sc, em.ed.type);
                e = e.ctfeInterpret();
            }
            em.value = e;
        }
        else
        {
            /* Find the previous enum member,
             * and set this to be the previous value + 1
             */
            EnumMember emprev = null;
            em.ed.members.foreachDsymbol( (s)
            {
                if (auto enm = s.isEnumMember())
                {
                    if (enm == em)
                        return 1;       // found
                    emprev = enm;
                }
                return 0;       // continue
            });

            assert(emprev);
            if (emprev.semanticRun < PASS.semanticdone) // if forward reference
                emprev.dsymbolSemantic(emprev._scope); // resolve it
            if (emprev.errors)
                return errorReturn();

            Expression eprev = emprev.value;
            // .toHeadMutable() due to https://issues.dlang.org/show_bug.cgi?id=18645
            Type tprev = eprev.type.toHeadMutable().equals(em.ed.type.toHeadMutable())
                ? em.ed.memtype
                : eprev.type;
            /*
                https://issues.dlang.org/show_bug.cgi?id=20777
                Previously this used getProperty, which doesn't consider anything user defined,
                this construct does do that and thus fixes the bug.
            */
            Expression emax = DotIdExp.create(em.ed.loc, new TypeExp(em.ed.loc, tprev), Id.max);
            emax = emax.expressionSemantic(sc);
            emax = emax.ctfeInterpret();

            // Set value to (eprev + 1).
            // But first check that (eprev != emax)
            assert(eprev);
            Expression e = new EqualExp(EXP.equal, em.loc, eprev, emax);
            e = e.expressionSemantic(sc);
            e = e.ctfeInterpret();
            if (e.toInteger())
            {
                auto mt = em.ed.memtype;
                if (!mt)
                    mt = eprev.type;
                em.error("initialization with `%s.%s+1` causes overflow for type `%s`",
                    emprev.ed.toChars(), emprev.toChars(), mt.toChars());
                return errorReturn();
            }

            // Now set e to (eprev + 1)
            e = new AddExp(em.loc, eprev, IntegerExp.literal!1);
            e = e.expressionSemantic(sc);
            e = e.castTo(sc, eprev.type);
            e = e.ctfeInterpret();

            // save origValue (without cast) for better json output
            if (e.op != EXP.error) // avoid duplicate diagnostics
            {
                assert(emprev.origValue);
                em.origValue = new AddExp(em.loc, emprev.origValue, IntegerExp.literal!1);
                em.origValue = em.origValue.expressionSemantic(sc);
                em.origValue = em.origValue.ctfeInterpret();
            }

            if (e.op == EXP.error)
                return errorReturn();
            if (e.type.isfloating())
            {
                // Check that e != eprev (not always true for floats)
                Expression etest = new EqualExp(EXP.equal, em.loc, e, eprev);
                etest = etest.expressionSemantic(sc);
                etest = etest.ctfeInterpret();
                if (etest.toInteger())
                {
                    em.error("has inexact value due to loss of precision");
                    return errorReturn();
                }
            }
            em.value = e;
        }
        if (!em.origType)
            em.type = em.value.type;

        assert(em.origValue);
        em.semanticRun = PASS.semanticdone;
    }

    override void visit(TemplateDeclaration tempdecl)
    {
        static if (LOG)
        {
            printf("TemplateDeclaration.dsymbolSemantic(this = %p, id = '%s')\n", this, tempdecl.ident.toChars());
            printf("sc.stc = %llx\n", sc.stc);
            printf("sc.module = %s\n", sc._module.toChars());
        }
        if (tempdecl.semanticRun != PASS.initial)
            return; // semantic() already run

        if (tempdecl._scope)
        {
            sc = tempdecl._scope;
            tempdecl._scope = null;
        }
        if (!sc)
            return;

        // Remember templates defined in module object that we need to know about
        if (sc._module && sc._module.ident == Id.object)
        {
            if (tempdecl.ident == Id.RTInfo)
                Type.rtinfo = tempdecl;
        }

        /* Remember Scope for later instantiations, but make
         * a copy since attributes can change.
         */
        if (!tempdecl._scope)
        {
            tempdecl._scope = sc.copy();
            tempdecl._scope.setNoFree();
        }

        tempdecl.semanticRun = PASS.semantic;

        tempdecl.parent = sc.parent;
        tempdecl.visibility = sc.visibility;
        tempdecl.userAttribDecl = sc.userAttribDecl;
        tempdecl.cppnamespace = sc.namespace;
        tempdecl.isstatic = tempdecl.toParent().isModule() || (tempdecl._scope.stc & STC.static_);
        tempdecl.deprecated_ = !!(sc.stc & STC.deprecated_);

        UserAttributeDeclaration.checkGNUABITag(tempdecl, sc.linkage);

        if (!tempdecl.isstatic)
        {
            if (auto ad = tempdecl.parent.pastMixin().isAggregateDeclaration())
                ad.makeNested();
        }

        // Set up scope for parameters
        auto paramsym = new ScopeDsymbol();
        paramsym.parent = tempdecl.parent;
        Scope* paramscope = sc.push(paramsym);
        paramscope.stc = 0;

        if (global.params.ddoc.doOutput)
        {
            tempdecl.origParameters = new TemplateParameters(tempdecl.parameters.length);
            for (size_t i = 0; i < tempdecl.parameters.length; i++)
            {
                TemplateParameter tp = (*tempdecl.parameters)[i];
                (*tempdecl.origParameters)[i] = tp.syntaxCopy();
            }
        }

        for (size_t i = 0; i < tempdecl.parameters.length; i++)
        {
            TemplateParameter tp = (*tempdecl.parameters)[i];
            if (!tp.declareParameter(paramscope))
            {
                error(tp.loc, "parameter `%s` multiply defined", tp.ident.toChars());
                tempdecl.errors = true;
            }
            if (!tp.tpsemantic(paramscope, tempdecl.parameters))
            {
                tempdecl.errors = true;
            }
            if (i + 1 != tempdecl.parameters.length && tp.isTemplateTupleParameter())
            {
                tempdecl.error("template tuple parameter must be last one");
                tempdecl.errors = true;
            }
        }

        /* Calculate TemplateParameter.dependent
         */
        TemplateParameters tparams = TemplateParameters(1);
        for (size_t i = 0; i < tempdecl.parameters.length; i++)
        {
            TemplateParameter tp = (*tempdecl.parameters)[i];
            tparams[0] = tp;

            for (size_t j = 0; j < tempdecl.parameters.length; j++)
            {
                // Skip cases like: X(T : T)
                if (i == j)
                    continue;

                if (TemplateTypeParameter ttp = (*tempdecl.parameters)[j].isTemplateTypeParameter())
                {
                    if (reliesOnTident(ttp.specType, &tparams))
                        tp.dependent = true;
                }
                else if (TemplateAliasParameter tap = (*tempdecl.parameters)[j].isTemplateAliasParameter())
                {
                    if (reliesOnTident(tap.specType, &tparams) ||
                        reliesOnTident(isType(tap.specAlias), &tparams))
                    {
                        tp.dependent = true;
                    }
                }
            }
        }

        paramscope.pop();

        // Compute again
        tempdecl.onemember = null;
        if (tempdecl.members)
        {
            Dsymbol s;
            if (Dsymbol.oneMembers(tempdecl.members, &s, tempdecl.ident) && s)
            {
                tempdecl.onemember = s;
                s.parent = tempdecl;
            }
        }

        /* BUG: should check:
         *  1. template functions must not introduce virtual functions, as they
         *     cannot be accomodated in the vtbl[]
         *  2. templates cannot introduce non-static data members (i.e. fields)
         *     as they would change the instance size of the aggregate.
         */

        tempdecl.semanticRun = PASS.semanticdone;
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
                return deferDsymbolSemantic(tm, scx);
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
            tm.error("recursive mixin instantiation");
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
            tm.error("recursive expansion");
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
            tm.error("error instantiating");
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

    void funcDeclarationSemantic(FuncDeclaration funcdecl)
    {
        version (none)
        {
            printf("FuncDeclaration::semantic(sc = %p, this = %p, '%s', linkage = %d)\n", sc, funcdecl, funcdecl.toPrettyChars(), sc.linkage);
            if (funcdecl.isFuncLiteralDeclaration())
                printf("\tFuncLiteralDeclaration()\n");
            printf("sc.parent = %s, parent = %s\n", sc.parent.toChars(), funcdecl.parent ? funcdecl.parent.toChars() : "");
            printf("type: %p, %s\n", funcdecl.type, funcdecl.type.toChars());
        }

        if (funcdecl.semanticRun != PASS.initial && funcdecl.isFuncLiteralDeclaration())
        {
            /* Member functions that have return types that are
             * forward references can have semantic() run more than
             * once on them.
             * See test\interface2.d, test20
             */
            return;
        }

        if (funcdecl.semanticRun >= PASS.semanticdone)
            return;
        assert(funcdecl.semanticRun <= PASS.semantic);
        funcdecl.semanticRun = PASS.semantic;

        if (funcdecl._scope)
        {
            sc = funcdecl._scope;
            funcdecl._scope = null;
        }

        if (!sc || funcdecl.errors)
            return;

        funcdecl.cppnamespace = sc.namespace;
        funcdecl.parent = sc.parent;
        Dsymbol parent = funcdecl.toParent();

        funcdecl.foverrides.setDim(0); // reset in case semantic() is being retried for this function

        funcdecl.storage_class |= sc.stc & ~STC.ref_;
        AggregateDeclaration ad = funcdecl.isThis();
        // Don't nest structs b/c of generated methods which should not access the outer scopes.
        // https://issues.dlang.org/show_bug.cgi?id=16627
        if (ad && !funcdecl.isGenerated())
        {
            funcdecl.storage_class |= ad.storage_class & (STC.TYPECTOR | STC.synchronized_);
            ad.makeNested();
        }
        if (sc.func)
            funcdecl.storage_class |= sc.func.storage_class & STC.disable;
        // Remove prefix storage classes silently.
        if ((funcdecl.storage_class & STC.TYPECTOR) && !(ad || funcdecl.isNested()))
            funcdecl.storage_class &= ~STC.TYPECTOR;

        //printf("function storage_class = x%llx, sc.stc = x%llx, %x\n", storage_class, sc.stc, Declaration.isFinal());

        if (sc.flags & SCOPE.compile)
            funcdecl.skipCodegen = true;

        funcdecl._linkage = sc.linkage;
        if (auto fld = funcdecl.isFuncLiteralDeclaration())
        {
            if (fld.treq)
            {
                Type treq = fld.treq;
                assert(treq.nextOf().ty == Tfunction);
                if (treq.ty == Tdelegate)
                    fld.tok = TOK.delegate_;
                else if (treq.isPtrToFunction())
                    fld.tok = TOK.function_;
                else
                    assert(0);
                funcdecl._linkage = treq.nextOf().toTypeFunction().linkage;
            }
        }

        // evaluate pragma(inline)
        if (auto pragmadecl = sc.inlining)
            funcdecl.inlining = evalPragmaInline(pragmadecl.loc, sc, pragmadecl.args);

        funcdecl.visibility = sc.visibility;
        funcdecl.userAttribDecl = sc.userAttribDecl;
        UserAttributeDeclaration.checkGNUABITag(funcdecl, funcdecl._linkage);
        checkMustUseReserved(funcdecl);

        if (!funcdecl.originalType)
            funcdecl.originalType = funcdecl.type.syntaxCopy();

        static TypeFunction getFunctionType(FuncDeclaration fd)
        {
            if (auto tf = fd.type.isTypeFunction())
                return tf;

            if (!fd.type.isTypeError())
            {
                fd.error("`%s` must be a function instead of `%s`", fd.toChars(), fd.type.toChars());
                fd.type = Type.terror;
            }
            fd.errors = true;
            return null;
        }

        if (sc.flags & SCOPE.Cfile)
        {
            /* C11 allows a function to be declared with a typedef, D does not.
             */
            if (auto ti = funcdecl.type.isTypeIdentifier())
            {
                auto tj = ti.typeSemantic(funcdecl.loc, sc);
                if (auto tjf = tj.isTypeFunction())
                {
                    /* Copy the type instead of just pointing to it,
                     * as we don't merge function types
                     */
                    auto tjf2 = new TypeFunction(tjf.parameterList, tjf.next, tjf.linkage);
                    funcdecl.type = tjf2;
                    funcdecl.originalType = tjf2;
                }
            }
        }

        if (!getFunctionType(funcdecl))
            return;

        if (!funcdecl.type.deco)
        {
            sc = sc.push();
            sc.stc |= funcdecl.storage_class & (STC.disable | STC.deprecated_); // forward to function type

            TypeFunction tf = funcdecl.type.toTypeFunction();
            if (sc.func)
            {
                /* If the nesting parent is pure without inference,
                 * then this function defaults to pure too.
                 *
                 *  auto foo() pure {
                 *    auto bar() {}     // become a weak purity function
                 *    class C {         // nested class
                 *      auto baz() {}   // become a weak purity function
                 *    }
                 *
                 *    static auto boo() {}   // typed as impure
                 *    // Even though, boo cannot call any impure functions.
                 *    // See also Expression::checkPurity().
                 *  }
                 */
                if (tf.purity == PURE.impure && (funcdecl.isNested() || funcdecl.isThis()))
                {
                    FuncDeclaration fd = null;
                    for (Dsymbol p = funcdecl.toParent2(); p; p = p.toParent2())
                    {
                        if (AggregateDeclaration adx = p.isAggregateDeclaration())
                        {
                            if (adx.isNested())
                                continue;
                            break;
                        }
                        if ((fd = p.isFuncDeclaration()) !is null)
                            break;
                    }

                    /* If the parent's purity is inferred, then this function's purity needs
                     * to be inferred first.
                     */
                    if (fd && fd.isPureBypassingInference() >= PURE.weak && !funcdecl.isInstantiated())
                    {
                        tf.purity = PURE.fwdref; // default to pure
                    }
                }
            }

            if (tf.isref)
                sc.stc |= STC.ref_;
            if (tf.isScopeQual)
                sc.stc |= STC.scope_;
            if (tf.isnothrow)
                sc.stc |= STC.nothrow_;
            if (tf.isnogc)
                sc.stc |= STC.nogc;
            if (tf.isproperty)
                sc.stc |= STC.property;
            if (tf.purity == PURE.fwdref)
                sc.stc |= STC.pure_;

            if (tf.trust != TRUST.default_)
            {
                sc.stc &= ~STC.safeGroup;
                if (tf.trust == TRUST.safe)
                    sc.stc |= STC.safe;
                else if (tf.trust == TRUST.system)
                    sc.stc |= STC.system;
                else if (tf.trust == TRUST.trusted)
                    sc.stc |= STC.trusted;
            }

            if (funcdecl.isCtorDeclaration())
            {
                tf.isctor = true;
                Type tret = ad.handleType();
                assert(tret);
                tret = tret.addStorageClass(funcdecl.storage_class | sc.stc);
                tret = tret.addMod(funcdecl.type.mod);
                tf.next = tret;
                if (ad.isStructDeclaration())
                    sc.stc |= STC.ref_;
            }

            // 'return' on a non-static class member function implies 'scope' as well
            if (ad && ad.isClassDeclaration() && (tf.isreturn || sc.stc & STC.return_) && !(sc.stc & STC.static_))
                sc.stc |= STC.scope_;

            // If 'this' has no pointers, remove 'scope' as it has no meaning
            // Note: this is already covered by semantic of `VarDeclaration` and `TypeFunction`,
            // but existing code relies on `hasPointers()` being called here to resolve forward references:
            // https://github.com/dlang/dmd/pull/14232#issuecomment-1162906573
            if (sc.stc & STC.scope_ && ad && ad.isStructDeclaration() && !ad.type.hasPointers())
            {
                sc.stc &= ~STC.scope_;
                tf.isScopeQual = false;
                if (tf.isreturnscope)
                {
                    sc.stc &= ~(STC.return_ | STC.returnScope);
                    tf.isreturn = false;
                    tf.isreturnscope = false;
                }
            }

            sc.linkage = funcdecl._linkage;

            if (!tf.isNaked() && !(funcdecl.isThis() || funcdecl.isNested()))
            {
                OutBuffer buf;
                MODtoBuffer(&buf, tf.mod);
                funcdecl.error("without `this` cannot be `%s`", buf.peekChars());
                tf.mod = 0; // remove qualifiers
            }

            /* Apply const, immutable, wild and shared storage class
             * to the function type. Do this before type semantic.
             */
            auto stc = funcdecl.storage_class;
            if (funcdecl.type.isImmutable())
                stc |= STC.immutable_;
            if (funcdecl.type.isConst())
                stc |= STC.const_;
            if (funcdecl.type.isShared() || funcdecl.storage_class & STC.synchronized_)
                stc |= STC.shared_;
            if (funcdecl.type.isWild())
                stc |= STC.wild;
            funcdecl.type = funcdecl.type.addSTC(stc);

            funcdecl.type = funcdecl.type.typeSemantic(funcdecl.loc, sc);
            sc = sc.pop();
        }

        auto f = getFunctionType(funcdecl);
        if (!f)
            return;     // funcdecl's type is not a function

        {
            // Merge back function attributes into 'originalType'.
            // It's used for mangling, ddoc, and json output.
            TypeFunction tfo = funcdecl.originalType.toTypeFunction();
            tfo.mod = f.mod;
            tfo.isScopeQual = f.isScopeQual;
            tfo.isreturninferred = f.isreturninferred;
            tfo.isscopeinferred = f.isscopeinferred;
            tfo.isref = f.isref;
            tfo.isnothrow = f.isnothrow;
            tfo.isnogc = f.isnogc;
            tfo.isproperty = f.isproperty;
            tfo.purity = f.purity;
            tfo.trust = f.trust;

            funcdecl.storage_class &= ~(STC.TYPECTOR | STC.FUNCATTR);
        }

        // check pragma(crt_constructor) signature
        if (funcdecl.isCrtCtor || funcdecl.isCrtDtor)
        {
            const idStr = funcdecl.isCrtCtor ? "crt_constructor" : "crt_destructor";
            if (f.nextOf().ty != Tvoid)
                funcdecl.error("must return `void` for `pragma(%s)`", idStr.ptr);
            if (funcdecl._linkage != LINK.c && f.parameterList.length != 0)
                funcdecl.error("must be `extern(C)` for `pragma(%s)` when taking parameters", idStr.ptr);
            if (funcdecl.isThis())
                funcdecl.error("cannot be a non-static member function for `pragma(%s)`", idStr.ptr);
        }

        if (funcdecl.overnext && funcdecl.isCsymbol())
        {
            /* C does not allow function overloading, but it does allow
             * redeclarations of the same function. If .overnext points
             * to a redeclaration, ok. Error if it is an overload.
             */
            auto fnext = funcdecl.overnext.isFuncDeclaration();
            funcDeclarationSemantic(fnext);
            auto fn = fnext.type.isTypeFunction();
            if (!fn || !cFuncEquivalence(f, fn))
            {
                funcdecl.error("redeclaration with different type");
                //printf("t1: %s\n", f.toChars());
                //printf("t2: %s\n", fn.toChars());
            }
            funcdecl.overnext = null;   // don't overload the redeclarations
        }

        if ((funcdecl.storage_class & STC.auto_) && !f.isref && !funcdecl.inferRetType)
            funcdecl.error("storage class `auto` has no effect if return type is not inferred");

        if (f.isreturn && !funcdecl.needThis() && !funcdecl.isNested())
        {
            /* Non-static nested functions have a hidden 'this' pointer to which
             * the 'return' applies
             */
            if (sc.scopesym && sc.scopesym.isAggregateDeclaration())
                funcdecl.error("`static` member has no `this` to which `return` can apply");
            else
                error(funcdecl.loc, "top-level function `%s` has no `this` to which `return` can apply", funcdecl.toChars());
        }

        if (funcdecl.isAbstract() && !funcdecl.isVirtual())
        {
            const(char)* sfunc;
            if (funcdecl.isStatic())
                sfunc = "static";
            else if (funcdecl.visibility.kind == Visibility.Kind.private_ || funcdecl.visibility.kind == Visibility.Kind.package_)
                sfunc = visibilityToChars(funcdecl.visibility.kind);
            else
                sfunc = "final";
            funcdecl.error("`%s` functions cannot be `abstract`", sfunc);
        }

        if (funcdecl.isOverride() && !funcdecl.isVirtual() && !funcdecl.isFuncLiteralDeclaration())
        {
            Visibility.Kind kind = funcdecl.visible().kind;
            if ((kind == Visibility.Kind.private_ || kind == Visibility.Kind.package_) && funcdecl.isMember())
                funcdecl.error("`%s` method is not virtual and cannot override", visibilityToChars(kind));
            else
                funcdecl.error("cannot override a non-virtual function");
        }

        if (funcdecl.isAbstract() && funcdecl.isFinalFunc())
            funcdecl.error("cannot be both `final` and `abstract`");
        version (none)
        {
            if (funcdecl.isAbstract() && funcdecl.fbody)
                funcdecl.error("`abstract` functions cannot have bodies");
        }

        version (none)
        {
            if (funcdecl.isStaticConstructor() || funcdecl.isStaticDestructor())
            {
                if (!funcdecl.isStatic() || funcdecl.type.nextOf().ty != Tvoid)
                    funcdecl.error("static constructors / destructors must be `static void`");
                if (f.arguments && f.arguments.length)
                    funcdecl.error("static constructors / destructors must have empty parameter list");
                // BUG: check for invalid storage classes
            }
        }

        if (funcdecl.printf || funcdecl.scanf)
        {
            /* printf/scanf-like functions must be of the form:
             *    extern (C/C++) T printf([parameters...], const(char)* format, ...);
             * or:
             *    extern (C/C++) T vprintf([parameters...], const(char)* format, va_list);
             */

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
            if ((f.linkage == LINK.c || f.linkage == LINK.cpp) &&

                (f.parameterList.varargs == VarArg.variadic &&
                 nparams >= 1 &&
                 isPointerToChar(f.parameterList[nparams - 1]) ||

                 f.parameterList.varargs == VarArg.none &&
                 nparams >= 2 &&
                 isPointerToChar(f.parameterList[nparams - 2]) &&
                 isVa_list(f.parameterList[nparams - 1])
                )
               )
            {
                // the signature is valid for printf/scanf, no error
            }
            else
            {
                const p = (funcdecl.printf ? Id.printf : Id.scanf).toChars();
                if (f.parameterList.varargs == VarArg.variadic)
                {
                    funcdecl.error("`pragma(%s)` functions must be `extern(C) %s %s([parameters...], const(char)*, ...)`"
                                   ~ " not `%s`",
                        p, f.next.toChars(), funcdecl.toChars(), funcdecl.type.toChars());
                }
                else
                {
                    funcdecl.error("`pragma(%s)` functions must be `extern(C) %s %s([parameters...], const(char)*, va_list)`",
                        p, f.next.toChars(), funcdecl.toChars());
                }
            }
        }

        if (auto id = parent.isInterfaceDeclaration())
        {
            funcdecl.storage_class |= STC.abstract_;
            if (funcdecl.isCtorDeclaration() || funcdecl.isPostBlitDeclaration() || funcdecl.isDtorDeclaration() || funcdecl.isInvariantDeclaration() || funcdecl.isNewDeclaration() || funcdecl.isDelete())
                funcdecl.error("constructors, destructors, postblits, invariants, new and delete functions are not allowed in interface `%s`", id.toChars());
            if (funcdecl.fbody && funcdecl.isVirtual())
                funcdecl.error("function body only allowed in `final` functions in interface `%s`", id.toChars());
        }

        if (UnionDeclaration ud = parent.isUnionDeclaration())
        {
            if (funcdecl.isPostBlitDeclaration() || funcdecl.isDtorDeclaration() || funcdecl.isInvariantDeclaration())
                funcdecl.error("destructors, postblits and invariants are not allowed in union `%s`", ud.toChars());
        }

        if (StructDeclaration sd = parent.isStructDeclaration())
        {
            if (funcdecl.isCtorDeclaration())
            {
                goto Ldone;
            }
        }

        if (ClassDeclaration cd = parent.isClassDeclaration())
        {
            parent = cd = objc.getParent(funcdecl, cd);

            if (funcdecl.isCtorDeclaration())
            {
                goto Ldone;
            }

            if (funcdecl.storage_class & STC.abstract_)
                cd.isabstract = ThreeState.yes;

            // if static function, do not put in vtbl[]
            if (!funcdecl.isVirtual())
            {
                //printf("\tnot virtual\n");
                goto Ldone;
            }
            // Suppress further errors if the return type is an error
            if (funcdecl.type.nextOf() == Type.terror)
                goto Ldone;

            bool may_override = false;
            for (size_t i = 0; i < cd.baseclasses.length; i++)
            {
                BaseClass* b = (*cd.baseclasses)[i];
                ClassDeclaration cbd = b.type.toBasetype().isClassHandle();
                if (!cbd)
                    continue;
                for (size_t j = 0; j < cbd.vtbl.length; j++)
                {
                    FuncDeclaration f2 = cbd.vtbl[j].isFuncDeclaration();
                    if (!f2 || f2.ident != funcdecl.ident)
                        continue;
                    if (cbd.parent && cbd.parent.isTemplateInstance())
                    {
                        if (!f2.functionSemantic())
                            goto Ldone;
                    }
                    may_override = true;
                }
            }
            if (may_override && funcdecl.type.nextOf() is null)
            {
                /* If same name function exists in base class but 'this' is auto return,
                 * cannot find index of base class's vtbl[] to override.
                 */
                funcdecl.error("return type inference is not supported if may override base class function");
            }

            /* Find index of existing function in base class's vtbl[] to override
             * (the index will be the same as in cd's current vtbl[])
             */
            int vi = cd.baseClass ? funcdecl.findVtblIndex(&cd.baseClass.vtbl, cast(int)cd.baseClass.vtbl.length) : -1;

            bool doesoverride = false;
            switch (vi)
            {
            case -1:
            Lintro:
                /* Didn't find one, so
                 * This is an 'introducing' function which gets a new
                 * slot in the vtbl[].
                 */

                // Verify this doesn't override previous final function
                if (cd.baseClass)
                {
                    Dsymbol s = cd.baseClass.search(funcdecl.loc, funcdecl.ident);
                    if (s)
                    {
                        if (auto f2 = s.isFuncDeclaration())
                        {
                            f2 = f2.overloadExactMatch(funcdecl.type);
                            if (f2 && f2.isFinalFunc() && f2.visible().kind != Visibility.Kind.private_)
                                funcdecl.error("cannot override `final` function `%s`", f2.toPrettyChars());
                        }
                    }
                }

                /* These quirky conditions mimic what happens when virtual
                   inheritance is implemented by producing a virtual base table
                   with offsets to each of the virtual bases.
                 */
                if (target.cpp.splitVBasetable && cd.classKind == ClassKind.cpp &&
                    cd.baseClass && cd.baseClass.vtbl.length)
                {
                    /* if overriding an interface function, then this is not
                     * introducing and don't put it in the class vtbl[]
                     */
                    funcdecl.interfaceVirtual = funcdecl.overrideInterface();
                    if (funcdecl.interfaceVirtual)
                    {
                        //printf("\tinterface function %s\n", toChars());
                        cd.vtblFinal.push(funcdecl);
                        goto Linterfaces;
                    }
                }

                if (funcdecl.isFinalFunc())
                {
                    // Don't check here, as it may override an interface function
                    //if (isOverride())
                    //    error("is marked as override, but does not override any function");
                    cd.vtblFinal.push(funcdecl);
                }
                else
                {
                    //printf("\tintroducing function %s\n", funcdecl.toChars());
                    funcdecl.isIntroducing = true;
                    if (cd.classKind == ClassKind.cpp && target.cpp.reverseOverloads)
                    {
                        /* Overloaded functions with same name are grouped and in reverse order.
                         * Search for first function of overload group, and insert
                         * funcdecl into vtbl[] immediately before it.
                         */
                        funcdecl.vtblIndex = cast(int)cd.vtbl.length;
                        bool found;
                        foreach (const i, s; cd.vtbl)
                        {
                            if (found)
                                // the rest get shifted forward
                                ++s.isFuncDeclaration().vtblIndex;
                            else if (s.ident == funcdecl.ident && s.parent == parent)
                            {
                                // found first function of overload group
                                funcdecl.vtblIndex = cast(int)i;
                                found = true;
                                ++s.isFuncDeclaration().vtblIndex;
                            }
                        }
                        cd.vtbl.insert(funcdecl.vtblIndex, funcdecl);

                        debug foreach (const i, s; cd.vtbl)
                        {
                            // a C++ dtor gets its vtblIndex later (and might even be added twice to the vtbl),
                            // e.g. when compiling druntime with a debug compiler, namely with core.stdcpp.exception.
                            if (auto fd = s.isFuncDeclaration())
                                assert(fd.vtblIndex == i ||
                                       (cd.classKind == ClassKind.cpp && fd.isDtorDeclaration) ||
                                       funcdecl.parent.isInterfaceDeclaration); // interface functions can be in multiple vtbls
                        }
                    }
                    else
                    {
                        // Append to end of vtbl[]
                        vi = cast(int)cd.vtbl.length;
                        cd.vtbl.push(funcdecl);
                        funcdecl.vtblIndex = vi;
                    }
                }
                break;

            case -2:
                // can't determine because of forward references
                funcdecl.errors = true;
                return;

            default:
                {
                    if (vi >= cd.vtbl.length)
                    {
                        /* the derived class cd doesn't have its vtbl[] allocated yet.
                         * https://issues.dlang.org/show_bug.cgi?id=21008
                         */
                        funcdecl.error("circular reference to class `%s`", cd.toChars());
                        funcdecl.errors = true;
                        return;
                    }
                    FuncDeclaration fdv = cd.baseClass.vtbl[vi].isFuncDeclaration();
                    FuncDeclaration fdc = cd.vtbl[vi].isFuncDeclaration();
                    // This function is covariant with fdv

                    if (fdc == funcdecl)
                    {
                        doesoverride = true;
                        break;
                    }

                    auto vtf = getFunctionType(fdv);
                    if (vtf.trust > TRUST.system && f.trust == TRUST.system)
                        funcdecl.error("cannot override `@safe` method `%s` with a `@system` attribute",
                                       fdv.toPrettyChars);

                    if (fdc.toParent() == parent)
                    {
                        //printf("vi = %d,\tthis = %p %s %s @ [%s]\n\tfdc  = %p %s %s @ [%s]\n\tfdv  = %p %s %s @ [%s]\n",
                        //        vi, this, this.toChars(), this.type.toChars(), this.loc.toChars(),
                        //            fdc,  fdc .toChars(), fdc .type.toChars(), fdc .loc.toChars(),
                        //            fdv,  fdv .toChars(), fdv .type.toChars(), fdv .loc.toChars());

                        // fdc overrides fdv exactly, then this introduces new function.
                        if (fdc.type.mod == fdv.type.mod && funcdecl.type.mod != fdv.type.mod)
                            goto Lintro;
                    }

                    if (fdv.isDeprecated && !funcdecl.isDeprecated)
                        deprecation(funcdecl.loc, "`%s` is overriding the deprecated method `%s`",
                                    funcdecl.toPrettyChars, fdv.toPrettyChars);

                    // This function overrides fdv
                    if (fdv.isFinalFunc())
                        funcdecl.error("cannot override `final` function `%s`", fdv.toPrettyChars());

                    if (!funcdecl.isOverride())
                    {
                        if (fdv.isFuture())
                        {
                            deprecation(funcdecl.loc, "`@__future` base class method `%s` is being overridden by `%s`; rename the latter", fdv.toPrettyChars(), funcdecl.toPrettyChars());
                            // Treat 'this' as an introducing function, giving it a separate hierarchy in the vtbl[]
                            goto Lintro;
                        }
                        else
                        {
                            // https://issues.dlang.org/show_bug.cgi?id=17349
                            error(funcdecl.loc, "cannot implicitly override base class method `%s` with `%s`; add `override` attribute",
                                  fdv.toPrettyChars(), funcdecl.toPrettyChars());
                        }
                    }
                    doesoverride = true;
                    if (fdc.toParent() == parent)
                    {
                        // If both are mixins, or both are not, then error.
                        // If either is not, the one that is not overrides the other.
                        bool thismixin = funcdecl.parent.isClassDeclaration() !is null;
                        bool fdcmixin = fdc.parent.isClassDeclaration() !is null;
                        if (thismixin == fdcmixin)
                        {
                            funcdecl.error("multiple overrides of same function");
                        }
                        /*
                         * https://issues.dlang.org/show_bug.cgi?id=711
                         *
                         * If an overriding method is introduced through a mixin,
                         * we need to update the vtbl so that both methods are
                         * present.
                         */
                        else if (thismixin)
                        {
                            /* if the mixin introduced the overriding method, then reintroduce it
                             * in the vtbl. The initial entry for the mixined method
                             * will be updated at the end of the enclosing `if` block
                             * to point to the current (non-mixined) function.
                             */
                            auto vitmp = cast(int)cd.vtbl.length;
                            cd.vtbl.push(fdc);
                            fdc.vtblIndex = vitmp;
                        }
                        else if (fdcmixin)
                        {
                            /* if the current overriding function is coming from a
                             * mixined block, then push the current function in the
                             * vtbl, but keep the previous (non-mixined) function as
                             * the overriding one.
                             */
                            auto vitmp = cast(int)cd.vtbl.length;
                            cd.vtbl.push(funcdecl);
                            funcdecl.vtblIndex = vitmp;
                            break;
                        }
                        else // fdc overrides fdv
                        {
                            // this doesn't override any function
                            break;
                        }
                    }
                    cd.vtbl[vi] = funcdecl;
                    funcdecl.vtblIndex = vi;

                    /* Remember which functions this overrides
                     */
                    funcdecl.foverrides.push(fdv);

                    /* This works by whenever this function is called,
                     * it actually returns tintro, which gets dynamically
                     * cast to type. But we know that tintro is a base
                     * of type, so we could optimize it by not doing a
                     * dynamic cast, but just subtracting the isBaseOf()
                     * offset if the value is != null.
                     */

                    if (fdv.tintro)
                        funcdecl.tintro = fdv.tintro;
                    else if (!funcdecl.type.equals(fdv.type))
                    {
                        auto tnext = funcdecl.type.nextOf();
                        if (auto handle = tnext.isClassHandle())
                        {
                            if (handle.semanticRun < PASS.semanticdone && !handle.isBaseInfoComplete())
                                handle.dsymbolSemantic(null);
                        }
                        /* Only need to have a tintro if the vptr
                         * offsets differ
                         */
                        int offset;
                        if (fdv.type.nextOf().isBaseOf(tnext, &offset))
                        {
                            funcdecl.tintro = fdv.type;
                        }
                    }
                    break;
                }
            }

            /* Go through all the interface bases.
             * If this function is covariant with any members of those interface
             * functions, set the tintro.
             */
        Linterfaces:
            bool foundVtblMatch = false;

            for (ClassDeclaration bcd = cd; !foundVtblMatch && bcd; bcd = bcd.baseClass)
            {
                foreach (b; bcd.interfaces)
                {
                    vi = funcdecl.findVtblIndex(&b.sym.vtbl, cast(int)b.sym.vtbl.length);
                    switch (vi)
                    {
                    case -1:
                        break;

                    case -2:
                        // can't determine because of forward references
                        funcdecl.errors = true;
                        return;

                    default:
                        {
                            auto fdv = cast(FuncDeclaration)b.sym.vtbl[vi];
                            Type ti = null;

                            foundVtblMatch = true;

                            /* Remember which functions this overrides
                             */
                            funcdecl.foverrides.push(fdv);

                            /* Should we really require 'override' when implementing
                             * an interface function?
                             */
                            //if (!isOverride())
                            //    warning(loc, "overrides base class function %s, but is not marked with 'override'", fdv.toPrettyChars());

                            if (fdv.tintro)
                                ti = fdv.tintro;
                            else if (!funcdecl.type.equals(fdv.type))
                            {
                                /* Only need to have a tintro if the vptr
                                 * offsets differ
                                 */
                                int offset;
                                if (fdv.type.nextOf().isBaseOf(funcdecl.type.nextOf(), &offset))
                                {
                                    ti = fdv.type;
                                }
                            }
                            if (ti)
                            {
                                if (funcdecl.tintro)
                                {
                                    if (!funcdecl.tintro.nextOf().equals(ti.nextOf()) && !funcdecl.tintro.nextOf().isBaseOf(ti.nextOf(), null) && !ti.nextOf().isBaseOf(funcdecl.tintro.nextOf(), null))
                                    {
                                        funcdecl.error("incompatible covariant types `%s` and `%s`", funcdecl.tintro.toChars(), ti.toChars());
                                    }
                                }
                                else
                                {
                                    funcdecl.tintro = ti;
                                }
                            }
                        }
                    }
                }
            }
            if (foundVtblMatch)
            {
                goto L2;
            }

            if (!doesoverride && funcdecl.isOverride() && (funcdecl.type.nextOf() || !may_override))
            {
                BaseClass* bc = null;
                Dsymbol s = null;
                for (size_t i = 0; i < cd.baseclasses.length; i++)
                {
                    bc = (*cd.baseclasses)[i];
                    s = bc.sym.search_correct(funcdecl.ident);
                    if (s)
                        break;
                }

                if (s)
                {
                    HdrGenState hgs;
                    OutBuffer buf;

                    auto fd = s.isFuncDeclaration();
                    functionToBufferFull(cast(TypeFunction)(funcdecl.type), &buf,
                        new Identifier(funcdecl.toPrettyChars()), &hgs, null);
                    const(char)* funcdeclToChars = buf.peekChars();

                    if (fd)
                    {
                        OutBuffer buf1;

                        if (fd.ident == funcdecl.ident)
                            hgs.fullQual = true;

                        // https://issues.dlang.org/show_bug.cgi?id=23745
                        // If the potentially overriden function contains errors,
                        // inform the user to fix that one first
                        if (fd.errors)
                        {
                            error(funcdecl.loc, "function `%s` does not override any function, did you mean to override `%s`?",
                                funcdecl.toChars(), fd.toPrettyChars());
                            errorSupplemental(fd.loc, "Function `%s` contains errors in its declaration, therefore it cannot be correctly overriden",
                                fd.toPrettyChars());
                        }
                        else
                        {
                            functionToBufferFull(cast(TypeFunction)(fd.type), &buf1,
                                new Identifier(fd.toPrettyChars()), &hgs, null);

                            error(funcdecl.loc, "function `%s` does not override any function, did you mean to override `%s`?",
                                funcdeclToChars, buf1.peekChars());
                       }
                    }
                    else
                    {
                        error(funcdecl.loc, "function `%s` does not override any function, did you mean to override %s `%s`?",
                            funcdeclToChars, s.kind, s.toPrettyChars());
                        errorSupplemental(funcdecl.loc, "Functions are the only declarations that may be overriden");
                    }
                }
                else
                    funcdecl.error("does not override any function");
            }

        L2:
            objc.setSelector(funcdecl, sc);
            objc.checkLinkage(funcdecl);
            objc.addToClassMethodList(funcdecl, cd);
            objc.setAsOptional(funcdecl, sc);

            /* Go through all the interface bases.
             * Disallow overriding any final functions in the interface(s).
             */
            foreach (b; cd.interfaces)
            {
                if (b.sym)
                {
                    if (auto s = search_function(b.sym, funcdecl.ident))
                    {
                        if (auto f2 = s.isFuncDeclaration())
                        {
                            f2 = f2.overloadExactMatch(funcdecl.type);
                            if (f2 && f2.isFinalFunc() && f2.visible().kind != Visibility.Kind.private_)
                                funcdecl.error("cannot override `final` function `%s.%s`", b.sym.toChars(), f2.toPrettyChars());
                        }
                    }
                }
            }

            if (funcdecl.isOverride)
            {
                if (funcdecl.storage_class & STC.disable)
                    deprecation(funcdecl.loc,
                                "`%s` cannot be annotated with `@disable` because it is overriding a function in the base class",
                                funcdecl.toPrettyChars);

                if (funcdecl.isDeprecated && !(funcdecl.foverrides.length && funcdecl.foverrides[0].isDeprecated))
                    deprecation(funcdecl.loc,
                                "`%s` cannot be marked as `deprecated` because it is overriding a function in the base class",
                                funcdecl.toPrettyChars);
            }

        }
        else if (funcdecl.isOverride() && !parent.isTemplateInstance())
            funcdecl.error("`override` only applies to class member functions");

        if (auto ti = parent.isTemplateInstance)
        {
            objc.setSelector(funcdecl, sc);
            objc.setAsOptional(funcdecl, sc);
        }

        objc.validateSelector(funcdecl);
        objc.validateOptional(funcdecl);
        // Reflect this.type to f because it could be changed by findVtblIndex
        f = funcdecl.type.toTypeFunction();

    Ldone:
        if (!funcdecl.fbody && !funcdecl.allowsContractWithoutBody())
            funcdecl.error("`in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract");

        /* Do not allow template instances to add virtual functions
         * to a class.
         */
        if (funcdecl.isVirtual())
        {
            if (auto ti = parent.isTemplateInstance())
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
                ClassDeclaration cd = ti.tempdecl.isClassMember();
                if (cd)
                {
                    funcdecl.error("cannot use template to add virtual function to class `%s`", cd.toChars());
                }
            }
        }

        funcdecl.checkMain();       // Check main() parameters and return type

        /* Purity and safety can be inferred for some functions by examining
         * the function body.
         */
        if (funcdecl.canInferAttributes(sc))
            funcdecl.initInferAttributes();

        funcdecl.semanticRun = PASS.semanticdone;

        /* Save scope for possible later use (if we need the
         * function internals)
         */
        funcdecl._scope = sc.copy();
        funcdecl._scope.setNoFree();

        __gshared bool printedMain = false; // semantic might run more than once
        if (global.params.verbose && !printedMain)
        {
            const(char)* type = funcdecl.isMain() ? "main" : funcdecl.isWinMain() ? "winmain" : funcdecl.isDllMain() ? "dllmain" : cast(const(char)*)null;
            Module mod = sc._module;

            if (type && mod)
            {
                printedMain = true;
                auto name = mod.srcfile.toChars();
                auto path = FileName.searchPath(global.path, name, true);
                message("entry     %-10s\t%s", type, path ? path : name);
            }
        }

        if (funcdecl.fbody && sc._module.isRoot() &&
            (funcdecl.isMain() || funcdecl.isWinMain() || funcdecl.isDllMain() || funcdecl.isCMain()))
            global.hasMainFunction = true;

        if (funcdecl.fbody && funcdecl.isMain() && sc._module.isRoot())
        {
            // check if `_d_cmain` is defined
            bool cmainTemplateExists()
            {
                auto rootSymbol = sc.search(funcdecl.loc, Id.empty, null);
                if (auto moduleSymbol = rootSymbol.search(funcdecl.loc, Id.object))
                    if (moduleSymbol.search(funcdecl.loc, Id.CMain))
                        return true;

                return false;
            }

            // Only mixin `_d_cmain` if it is defined
            if (cmainTemplateExists())
            {
                // add `mixin _d_cmain!();` to the declaring module
                auto tqual = new TypeIdentifier(funcdecl.loc, Id.CMain);
                auto tm = new TemplateMixin(funcdecl.loc, null, tqual, null);
                sc._module.members.push(tm);
            }
        }

        assert(funcdecl.type.ty != Terror || funcdecl.errors);

        // semantic for parameters' UDAs
        foreach (i, param; f.parameterList)
        {
            if (param && param.userAttribDecl)
                param.userAttribDecl.dsymbolSemantic(sc);
        }
    }

     /// Do the semantic analysis on the external interface to the function.
    override void visit(FuncDeclaration funcdecl)
    {
        funcDeclarationSemantic(funcdecl);
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

        funcDeclarationSemantic(ctd);

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
                    ctd.error("default constructor for structs only allowed " ~
                        "with `@disable`, no body, and no parameters");
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
                    ctd.error("is marked `@disable`, so it cannot have default "~
                              "arguments for all parameters.");
                    errorSupplemental(ctd.loc, "Use `@disable this();` if you want to disable default initialization.");
                }
                else
                    ctd.error("all parameters have default arguments, "~
                              "but structs cannot have default constructors.");
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

        funcDeclarationSemantic(pbd);

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

        funcDeclarationSemantic(dd);

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
                    scd.error("shared static constructor within a template require `core.atomic : atomicOp` to be present");
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
        funcDeclarationSemantic(scd);
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
                    sdd.error("shared static destructo within a template require `core.atomic : atomicOp` to be present");
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
        funcDeclarationSemantic(sdd);
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

        funcDeclarationSemantic(invd);

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
            funcDeclarationSemantic(utd);
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

        funcDeclarationSemantic(nd);
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
                auto ti = ts.sym.isInstantiated();
                if (ti && isError(ti))
                    ts.sym = sd;
            }
        }

        // Ungag errors when not speculative
        Ungag ungag = sd.ungagSpeculative();

        if (sd.semanticRun == PASS.initial)
        {
            sd.visibility = sc.visibility;

            sd.alignment = sc.alignment();

            sd.storage_class |= sc.stc;
            if (sd.storage_class & STC.abstract_)
                sd.error("structs, unions cannot be `abstract`");

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
                sd.error(sd.loc, "circular or forward reference");
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
            return deferDsymbolSemantic(sd, scx);
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
                    sd.error(fcall.loc, "`static opCall` is hidden by constructors and can never be called");
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
                sd.error("already exists at %s. Perhaps in another function with the same name?", sym.loc.toChars());
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

        // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
        // Deprecated in 2.100
        // Make an error in 2.110
        if (sd.storage_class & STC.scope_)
            deprecation(sd.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");
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
                cldec.error("storage class `auto` is invalid when declaring a class, did you mean to use `scope`?");
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
                        cldec.error("base type must be `class` or `interface`, not `%s`", b.type.toChars());
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
                        cldec.error("circular inheritance");
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
                            error(cldec.loc,"`%s`: base class must be specified first, " ~
                                  "before any interfaces.", cldec.toPrettyChars());
                            multiClassError += 1;
                        }
                        else if (multiClassError >= 1)
                        {
                                if(multiClassError == 1)
                                    error(cldec.loc,"`%s`: multiple class inheritance is not supported." ~
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
                        cldec.error("inherits from duplicate interface `%s`", b2.sym.toChars());
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
                return deferDsymbolSemantic(cldec, scx);
            }
            cldec.baseok = Baseok.done;

            if (cldec.classKind == ClassKind.objc || (cldec.baseClass && cldec.baseClass.classKind == ClassKind.objc))
                cldec.classKind = ClassKind.objc; // Objective-C classes do not inherit from Object

            // If no base class, and this is not an Object, use Object as base class
            if (!cldec.baseClass && cldec.ident != Id.Object && cldec.object && cldec.classKind == ClassKind.d)
            {
                void badObjectDotD()
                {
                    cldec.error("missing or corrupt object.d");
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
                    cldec.error("cannot inherit from class `%s` because it is `final`", cldec.baseClass.toChars());

                // Inherit properties from base class
                if (cldec.baseClass.isCOMclass())
                    cldec.com = true;
                if (cldec.baseClass.isCPPclass())
                    cldec.classKind = ClassKind.cpp;
                if (cldec.classKind != cldec.baseClass.classKind)
                    cldec.error("with %s linkage cannot inherit from class `%s` with %s linkage",
                        cldec.classKind.toChars(), cldec.baseClass.toChars(), cldec.baseClass.classKind.toChars());

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
                    error(cldec.loc, "C++ class `%s` cannot implement D interface `%s`",
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
                return deferDsymbolSemantic(cldec, scx);
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
                    cldec.error("C++ base class `%s` needs at least one virtual function", cldec.baseClass.toChars());
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
                    cldec.error("static class cannot inherit from nested class `%s`", cldec.baseClass.toChars());
                if (cldec.toParentLocal() != cldec.baseClass.toParentLocal() &&
                    (!cldec.toParentLocal() ||
                     !cldec.baseClass.toParentLocal().getType() ||
                     !cldec.baseClass.toParentLocal().getType().isBaseOf(cldec.toParentLocal().getType(), null)))
                {
                    if (cldec.toParentLocal())
                    {
                        cldec.error("is nested within `%s`, but super class `%s` is nested within `%s`",
                            cldec.toParentLocal().toChars(),
                            cldec.baseClass.toChars(),
                            cldec.baseClass.toParentLocal().toChars());
                    }
                    else
                    {
                        cldec.error("is not nested, but super class `%s` is nested within `%s`",
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
                            cldec.error("needs the frame pointer of `%s`, but super class `%s` needs the frame pointer of `%s`",
                                cldec.toParent2().toChars(),
                                cldec.baseClass.toChars(),
                                cldec.baseClass.toParent2().toChars());
                        }
                        else
                        {
                            cldec.error("doesn't need a frame pointer, but super class `%s` needs the frame pointer of `%s`",
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
            return deferDsymbolSemantic(cldec, scx);
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
                cldec.error("cannot implicitly generate a default constructor when base class `%s` is missing a default constructor",
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
                cldec.error(f.loc, "identity assignment operator overload is illegal");
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
                cldec.error("cannot infer `abstract` attribute due to circular dependencies");
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
            cldec.error("already exists at %s. Perhaps in another function with the same name?", cd.loc.toChars());
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
                    vd.error("Field members of a `synchronized` class cannot be `%s`",
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

        // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
        // Deprecated in 2.100
        // Make an error in 2.110
        // Don't forget to remove code at https://github.com/dlang/dmd/blob/b2f8274ba76358607fc3297a1e9f361480f9bcf9/src/dmd/dsymbolsem.d#L1032-L1036
        if (cldec.storage_class & STC.scope_)
            deprecation(cldec.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");
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
                        idec.error("base type must be `interface`, not `%s`", b.type.toChars());
                    idec.baseclasses.remove(i);
                    continue;
                }

                // Check for duplicate interfaces
                for (size_t j = 0; j < i; j++)
                {
                    BaseClass* b2 = (*idec.baseclasses)[j];
                    if (b2.sym == tc.sym)
                    {
                        idec.error("inherits from duplicate interface `%s`", b2.sym.toChars());
                        idec.baseclasses.remove(i);
                        continue BCLoop;
                    }
                }
                if (tc.sym == idec || idec.isBaseOf2(tc.sym))
                {
                    idec.error("circular inheritance of interface");
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
                return deferDsymbolSemantic(idec, scx);
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
                return deferDsymbolSemantic(idec, scx);
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

        // @@@DEPRECATED_2.120@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
        // Deprecated in 2.087
        // Made an error in 2.100, but removal depends on `scope class` being removed too
        // Don't forget to remove code at https://github.com/dlang/dmd/blob/b2f8274ba76358607fc3297a1e9f361480f9bcf9/src/dmd/dsymbolsem.d#L1032-L1036
        if (idec.storage_class & STC.scope_)
            error(idec.loc, "`scope` as a type constraint is obsolete.  Use `scope` at the usage site.");
    }
}

/*******************************************
 * Add members of EnumDeclaration to the symbol table(s).
 * Params:
 *      ed = EnumDeclaration
 *      sc = context of `ed`
 *      sds = symbol table that `ed` resides in
 */
void addEnumMembers(EnumDeclaration ed, Scope* sc, ScopeDsymbol sds)
{
    //printf("addEnumMembers(ed: %p)\n", ed);
    if (ed.added)
        return;
    ed.added = true;

    if (!ed.members)
        return;

    const bool isCEnum = (sc.flags & SCOPE.Cfile) != 0; // it's an ImportC enum
    const bool isAnon = ed.isAnonymous();

    if ((isCEnum || isAnon) && !sds.symtab)
        sds.symtab = new DsymbolTable();

    if ((isCEnum || !isAnon) && !ed.symtab)
        ed.symtab = new DsymbolTable();

    ed.members.foreachDsymbol( (s)
    {
        if (EnumMember em = s.isEnumMember())
        {
            em.ed = ed;
            if (isCEnum)
            {
                //printf("adding EnumMember %s to %p\n", em.toChars(), ed);
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
        tempinst.error(tempinst.loc, "recursive template expansion");
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

    TemplateStats.incInstance(tempdecl, tempinst);

    tempdecl.checkDeprecated(tempinst.loc, sc);

    // If tempdecl is a mixin, disallow it
    if (tempdecl.ismixin)
    {
        tempinst.error("mixin templates are not regular templates");
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

    Expressions* fargs = argumentList.arguments; // TODO: resolve named args

    /* See if there is an existing TemplateInstantiation that already
     * implements the typeargs. If so, just refer to that one instead.
     */
    tempinst.inst = tempdecl.findExistingInstance(tempinst, fargs);
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

                extern (D) this(TemplateInstance inst) scope
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
        tempinst.error("template instantiation `%s` forward references template declaration `%s`", tempinst.toChars(), tempdecl.toChars());
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
        if (Dsymbol.oneMembers(tempinst.members, &s, tempdecl.ident) && s)
        {
            //printf("tempdecl.ident = %s, s = `%s %s`\n", tempdecl.ident.toChars(), s.kind(), s.toPrettyChars());
            //printf("setting aliasdecl\n");
            tempinst.aliasdecl = s;
        }
    }

    /* If function template declaration
     */
    if (fargs && tempinst.aliasdecl)
    {
        if (auto fd = tempinst.aliasdecl.isFuncDeclaration())
        {
            /* Transmit fargs to type so that TypeFunction.dsymbolSemantic() can
             * resolve any "auto ref" storage classes.
             */
            if (fd.type)
                if (auto tf = fd.type.isTypeFunction())
                    tf.fargs = fargs;
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
        if (Dsymbol.oneMembers(tempinst.members, &s, tempdecl.ident) && s)
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
        tempinst.trySemantic3(sc2);

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
                tempinst.error("recursive expansion");
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
                tempinst.error(tempinst.loc, "error instantiating");
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
        ds.error("cannot resolve");
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
                        ds.error("cannot alias an expression `%s`", e.toChars());
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
        Dsymbol as = sc.search(ds.loc, ds.ident, &scopesym);
        if (!as)
        {
            ds.error("undefined identifier `%s`", ds.ident.toChars());
            return null;
        }
        if (as.errors)
            return null;

        auto ad = as.isAliasDeclaration();
        if (!ad)
        {
            ds.error("identifier `%s` must be an alias declaration", as.toChars());
            return null;
        }

        if (ad.overnext)
        {
            ds.error("cannot reassign overloaded alias");
            return null;
        }

        // Check constraints on the parent
        auto adParent = ad.toParent();
        if (adParent != ds.toParent())
        {
            if (!adParent)
                adParent = ds.toParent();
            error(ds.loc, "`%s` must have same parent `%s` as alias `%s`", ds.ident.toChars(), adParent.toChars(), ad.toChars());
            return null;
        }
        if (!adParent.isTemplateInstance())
        {
            ds.error("must be a member of a template");
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
        ds.error("cannot resolve");
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
                        ds.error("cannot alias an expression `%s`", e.toChars());
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

    static int func(Dsymbol s, AggregateDeclaration ad)
    {
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
            return v.aliasTuple.foreachVar(tv => tv.apply(&func, ad));
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
                ad.error("cannot have field `%s` with %ssame struct type", v.toChars(), psz);
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
            if (s.apply(&func, ad))
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

/***************************************
 * Interpret a `pragma(inline, x)`
 *
 * Params:
 *   loc = location for error messages
 *   sc = scope for evaluation of argument
 *   args = pragma arguments
 * Returns: corresponding `PINLINE` state
 */
PINLINE evalPragmaInline(Loc loc, Scope* sc, Expressions* args)
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
