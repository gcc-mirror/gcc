/**
 * Does the semantic passes on enums.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/enumsem.d, _enumsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_enumsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/enumsem.d
 */

module dmd.enumsem;

import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.blockexit;
import dmd.clone;
import dmd.cond;
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
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.dversion;
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
import dmd.nogc;
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


/*********************************
 * Perform semantic analysis on enum declaration `em`
 */
void enumSemantic(Scope* sc, EnumDeclaration ed)
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
                deferDsymbolSemantic(sc, ed, scx);
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
            .error(ed.loc, "%s `%s` base type must not be `void`", ed.kind, ed.toPrettyChars);
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
        .error(ed.loc, "%s `%s enum `%s` must have at least one member", ed.kind, ed.toPrettyChars, ed.toChars());
        ed.errors = true;
        ed.semanticRun = PASS.semanticdone;
        return;
    }

    if (!(sc.flags & SCOPE.Cfile))  // C enum remains incomplete until members are done
        ed.semanticRun = PASS.semanticdone;

    version (none)
    {
        // @@@DEPRECATED_2.110@@@ https://dlang.org/deprecate.html#scope%20as%20a%20type%20constraint
        // Deprecated in 2.100
        // Make an error in 2.110
        if (sc.stc & STC.scope_)
            deprecation(ed.loc, "`scope` as a type constraint is deprecated.  Use `scope` at the usage site.");
    }

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
    addEnumMembersToSymtab(ed, sc, sc.getScopesym());

    if (sc.flags & SCOPE.Cfile)
    {
        /* C11 6.7.2.2
         */
        Type commonType = ed.memtype;
        if (!commonType)
            commonType = Type.tint32;
        ulong nextValue = 0;        // C11 6.7.2.2-3 first member value defaults to 0

        // C11 6.7.2.2-2 value must be representable as an int.
        // The sizemask represents all values that int will fit into,
        // from 0..uint.max.  We want to cover int.min..uint.max.
        IntRange ir = IntRange.fromType(commonType);

        void emSemantic(EnumMember em, ref ulong nextValue)
        {
            static void errorReturn(EnumMember em)
            {
                em.value = ErrorExp.get();
                em.errors = true;
                em.semanticRun = PASS.semanticdone;
            }

            em.semanticRun = PASS.semantic;
            em.type = commonType;
            em._linkage = LINK.c;
            em.storage_class |= STC.manifest;
            if (em.value)
            {
                Expression e = em.value;
                assert(e.dyncast() == DYNCAST.expression);

                /* To merge the type of e with commonType, add 0 of type commonType
                 */
                if (!ed.memtype)
                    e = new AddExp(em.loc, e, new IntegerExp(em.loc, 0, commonType));

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
                    .error(em.loc, "%s `%s` enum member must be an integral constant expression, not `%s` of type `%s`", em.kind, em.toPrettyChars, e.toChars(), e.type.toChars());
                    return errorReturn(em);
                }
                if (ed.memtype && !ir.contains(getIntRange(ie)))
                {
                    // C11 6.7.2.2-2
                    .error(em.loc, "%s `%s` enum member value `%s` does not fit in `%s`", em.kind, em.toPrettyChars, e.toChars(), commonType.toChars());
                    return errorReturn(em);
                }
                nextValue = ie.toInteger();
                if (!ed.memtype)
                    commonType = e.type;
                em.value = new IntegerExp(em.loc, nextValue, commonType);
            }
            else
            {
                // C11 6.7.2.2-3 add 1 to value of previous enumeration constant
                bool first = (em == (*em.ed.members)[0]);
                if (!first)
                {
                    Expression max = getProperty(commonType, null, em.loc, Id.max, 0);
                    if (nextValue == max.toInteger())
                    {
                        .error(em.loc, "%s `%s` initialization with `%s+1` causes overflow for type `%s`", em.kind, em.toPrettyChars, max.toChars(), commonType.toChars());
                        return errorReturn(em);
                    }
                    nextValue += 1;
                }
                em.value = new IntegerExp(em.loc, nextValue, commonType);
            }
            em.type = commonType;
            em.semanticRun = PASS.semanticdone;
        }

        ed.members.foreachDsymbol( (s)
        {
            if (EnumMember em = s.isEnumMember())
                emSemantic(em, nextValue);
        });

        if (!ed.memtype)
        {
            // cast all members to commonType
            ed.members.foreachDsymbol( (s)
            {
                if (EnumMember em = s.isEnumMember())
                {
                    em.type = commonType;
                    em.value = em.value.castTo(sc, commonType);
                }
            });
        }

        ed.memtype = commonType;
        ed.semanticRun = PASS.semanticdone;
        return;
    }

    ed.members.foreachDsymbol( (s)
    {
        if (EnumMember em = s.isEnumMember())
            em.dsymbolSemantic(em._scope);
    });
    //printf("ed.defaultval = %lld\n", ed.defaultval);

    //if (ed.defaultval) printf("ed.defaultval: %s %s\n", ed.defaultval.toChars(), ed.defaultval.type.toChars());
    //printf("members = %s\n", members.toChars());
}

Expression getDefaultValue(EnumDeclaration ed, const ref Loc loc)
{
    Expression handleErrors(){
        ed.defaultval = ErrorExp.get();
        return ed.defaultval;
    }
    //printf("EnumDeclaration::getDefaultValue() %p %s\n", this, toChars());
    // https://issues.dlang.org/show_bug.cgi?id=23904
    // Return ed.defaultval only if it is not ErrorExp.
    // A speculative context may set ed.defaultval to ErrorExp;
    // subsequent non-speculative contexts need to be able
    // to print the error.
    if (ed.defaultval && !ed.defaultval.isErrorExp())
        return ed.defaultval;

    if (ed.isCsymbol())
        return ed.memtype.defaultInit(loc, true);

    if (ed._scope)
        dsymbolSemantic(ed, ed._scope);
    if (ed.errors)
        return handleErrors();
    if (!ed.members)
    {
        if (ed.isSpecial())
        {
            /* Allow these special enums to not need a member list
             */
            return ed.defaultval = ed.memtype.defaultInit(loc);
        }

        error(loc, "%s `%s` is opaque and has no default initializer", ed.kind, ed.toPrettyChars);
        return handleErrors();
    }

    foreach (const i; 0 .. ed.members.length)
    {
        EnumMember em = (*ed.members)[i].isEnumMember();
        if (em)
        {
            if (em.semanticRun < PASS.semanticdone)
            {
                error(loc, "%s `%s` forward reference of `%s.init`", ed.kind, ed.toPrettyChars, ed.toChars());
                return handleErrors();
            }

            ed.defaultval = em.value;
            return ed.defaultval;
        }
    }
    return handleErrors();
}

Type getMemtype(EnumDeclaration ed, const ref Loc loc)
{
    if (ed._scope)
    {
        /* Enum is forward referenced. We don't need to resolve the whole thing,
         * just the base type
         */
        if (ed.memtype)
        {
            Loc locx = loc.isValid() ? loc : ed.loc;
            ed.memtype = ed.memtype.typeSemantic(locx, ed._scope);
        }
        else
        {
            // Run semantic to get the type from a possible first member value
            dsymbolSemantic(ed, ed._scope);
        }
    }
    if (!ed.memtype)
    {
        if (!ed.isAnonymous() && (ed.members || ed.semanticRun >= PASS.semanticdone))
            ed.memtype = Type.tint32;
        else
        {
            Loc locx = loc.isValid() ? loc : ed.loc;
            error(locx, "is forward referenced looking for base type");
            return Type.terror;
        }
    }
    return ed.memtype;
}

/*********************************
 * Perform semantic analysis on enum member `em`
 */
void enumMemberSemantic(Scope* sc, EnumMember em)
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
        .error(em.loc, "%s `%s` circular reference to `enum` member", em.kind, em.toPrettyChars);
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
        if (em.ed.memtype)
            e = inferType(e, em.ed.memtype);
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
        // https://issues.dlang.org/show_bug.cgi?id=24311
        // First enum member is .init value, which gets put into static segment
        if (first)
            lowerStaticAAs(e, sc);
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
        const errors = global.startGagging();
        Expression e = new IntegerExp(em.loc, 0, t);
        e = e.ctfeInterpret();
        if (global.endGagging(errors))
        {
            error(em.loc, "cannot generate 0 value of type `%s` for `%s`",
                t.toChars(), em.toChars());
        }
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

        auto errors = global.startGagging();
        Expression eprev = emprev.value;
        assert(eprev);
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

        // check that (eprev != emax)
        Expression e = new EqualExp(EXP.equal, em.loc, eprev, emax);
        e = e.expressionSemantic(sc);
        e = e.ctfeInterpret();
        if (global.endGagging(errors))
        {
            // display an introductory error before showing what actually failed
            error(em.loc, "cannot check `%s` value for overflow", em.toPrettyChars());
            // rerun to show errors
            Expression e2 = DotIdExp.create(em.ed.loc, new TypeExp(em.ed.loc, tprev), Id.max);
            e2 = e2.expressionSemantic(sc);
            e2 = e2.ctfeInterpret();
            e2 = new EqualExp(EXP.equal, em.loc, eprev, e2);
            e2 = e2.expressionSemantic(sc);
            e2 = e2.ctfeInterpret();
        }
        // now any errors are for generating a value
        if (e.toInteger())
        {
            auto mt = em.ed.memtype;
            if (!mt)
                mt = eprev.type;
            .error(em.loc, "%s `%s` initialization with `%s.%s+1` causes overflow for type `%s`", em.kind, em.toPrettyChars,
                emprev.ed.toChars(), emprev.toChars(), mt.toChars());
            return errorReturn();
        }
        errors = global.startGagging();
        // Now set e to (eprev + 1)
        e = new AddExp(em.loc, eprev, IntegerExp.literal!1);
        e = e.expressionSemantic(sc);
        e = e.castTo(sc, eprev.type);
        e = e.ctfeInterpret();
        if (global.endGagging(errors))
        {
            error(em.loc, "cannot generate value for `%s`", em.toPrettyChars());
            // rerun to show errors
            Expression e2 = new AddExp(em.loc, eprev, IntegerExp.literal!1);
            e2 = e2.expressionSemantic(sc);
            e2 = e2.castTo(sc, eprev.type);
            e2 = e2.ctfeInterpret();
        }
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
                .error(em.loc, "%s `%s` has inexact value due to loss of precision", em.kind, em.toPrettyChars);
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
