
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/clone.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "aggregate.h"
#include "scope.h"
#include "mtype.h"
#include "declaration.h"
#include "module.h"
#include "id.h"
#include "expression.h"
#include "statement.h"
#include "init.h"
#include "template.h"
#include "tokens.h"

Expression *semantic(Expression *e, Scope *sc);

/*******************************************
 * Merge function attributes pure, nothrow, @safe, @nogc, and @disable
 */
StorageClass mergeFuncAttrs(StorageClass s1, FuncDeclaration *f)
{
    if (!f)
        return s1;

    StorageClass s2 = (f->storage_class & STCdisable);
    TypeFunction *tf = (TypeFunction *)f->type;
    if (tf->trust == TRUSTsafe)
        s2 |= STCsafe;
    else if (tf->trust == TRUSTsystem)
        s2 |= STCsystem;
    else if (tf->trust == TRUSTtrusted)
        s2 |= STCtrusted;
    if (tf->purity != PUREimpure)
        s2 |= STCpure;
    if (tf->isnothrow)
        s2 |= STCnothrow;
    if (tf->isnogc)
        s2 |= STCnogc;

    StorageClass stc = 0;
    StorageClass sa = s1 & s2;
    StorageClass so = s1 | s2;

    if (so & STCsystem)
        stc |= STCsystem;
    else if (sa & STCtrusted)
        stc |= STCtrusted;
    else if ((so & (STCtrusted | STCsafe)) == (STCtrusted | STCsafe))
        stc |= STCtrusted;
    else if (sa & STCsafe)
        stc |= STCsafe;

    if (sa & STCpure)
        stc |= STCpure;

    if (sa & STCnothrow)
        stc |= STCnothrow;

    if (sa & STCnogc)
        stc |= STCnogc;

    if (so & STCdisable)
        stc |= STCdisable;

    return stc;
}

/*******************************************
 * Check given aggregate actually has an identity opAssign or not.
 * Params:
 *      ad = struct or class
 *      sc = current scope
 * Returns:
 *      if found, returns FuncDeclaration of opAssign, otherwise null
 */
FuncDeclaration *hasIdentityOpAssign(AggregateDeclaration *ad, Scope *sc)
{
    Dsymbol *assign = search_function(ad, Id::assign);
    if (assign)
    {
        /* check identity opAssign exists
         */
        UnionExp er; new(&er) NullExp(ad->loc, ad->type);    // dummy rvalue
        UnionExp el; new(&el) IdentifierExp(ad->loc, Id::p); // dummy lvalue
        el.exp()->type = ad->type;
        Expressions a;
        a.setDim(1);

        unsigned errors = global.startGagging();    // Do not report errors, even if the template opAssign fbody makes it.
        sc = sc->push();
        sc->tinst = NULL;
        sc->minst = NULL;

        a[0] = er.exp();
        FuncDeclaration *f = resolveFuncCall(ad->loc, sc, assign, NULL, ad->type, &a, 1);
        if (!f)
        {
            a[0] = el.exp();
            f = resolveFuncCall(ad->loc, sc, assign, NULL, ad->type, &a, 1);
        }

        sc = sc->pop();
        global.endGagging(errors);

        if (f)
        {
            if (f->errors)
                return NULL;
            ParameterList fparams = f->getParameterList();
            if (fparams.length())
            {
                Parameter *fparam0 = fparams[0];
                if (fparam0->type->toDsymbol(NULL) != ad)
                    f = NULL;
            }
        }
        // BUGS: This detection mechanism cannot find some opAssign-s like follows:
        // struct S { void opAssign(ref immutable S) const; }
        return f;
    }
    return NULL;
}

/*******************************************
 * We need an opAssign for the struct if
 * it has a destructor or a postblit.
 * We need to generate one if a user-specified one does not exist.
 */
bool needOpAssign(StructDeclaration *sd)
{
    //printf("StructDeclaration::needOpAssign() %s\n", sd->toChars());
    if (sd->isUnionDeclaration())
        return false;

    if (sd->hasIdentityAssign)
        goto Lneed;         // because has identity==elaborate opAssign

    if (sd->dtor || sd->postblit)
        goto Lneed;

    /* If any of the fields need an opAssign, then we
     * need it too.
     */
    for (size_t i = 0; i < sd->fields.length; i++)
    {
        VarDeclaration *v = sd->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)              // if field of a union
            continue;                   // user must handle it themselves
        Type *tv = v->type->baseElemOf();
        if (tv->ty == Tstruct)
        {
            TypeStruct *ts = (TypeStruct *)tv;
            if (ts->sym->isUnionDeclaration())
                continue;
            if (needOpAssign(ts->sym))
                goto Lneed;
        }
    }
    //printf("\tdontneed\n");
    return false;

Lneed:
    //printf("\tneed\n");
    return true;
}

/******************************************
 * Build opAssign for struct.
 *      ref S opAssign(S s) { ... }
 *
 * Note that s will be constructed onto the stack, and probably
 * copy-constructed in caller site.
 *
 * If S has copy copy construction and/or destructor,
 * the body will make bit-wise object swap:
 *          S __swap = this; // bit copy
 *          this = s;        // bit copy
 *          __swap.dtor();
 * Instead of running the destructor on s, run it on tmp instead.
 *
 * Otherwise, the body will make member-wise assignments:
 * Then, the body is:
 *          this.field1 = s.field1;
 *          this.field2 = s.field2;
 *          ...;
 */
FuncDeclaration *buildOpAssign(StructDeclaration *sd, Scope *sc)
{
    if (FuncDeclaration *f = hasIdentityOpAssign(sd, sc))
    {
        sd->hasIdentityAssign = true;
        return f;
    }
    // Even if non-identity opAssign is defined, built-in identity opAssign
    // will be defined.

    if (!needOpAssign(sd))
        return NULL;

    //printf("StructDeclaration::buildOpAssign() %s\n", sd->toChars());
    StorageClass stc = STCsafe | STCnothrow | STCpure | STCnogc;
    Loc declLoc = sd->loc;
    Loc loc = Loc();    // internal code should have no loc to prevent coverage

    // One of our sub-field might have `@disable opAssign` so we need to
    // check for it.
    // In this event, it will be reflected by having `stc` (opAssign's
    // storage class) include `STCdisabled`.
    for (size_t i = 0; i < sd->fields.length; i++)
    {
        VarDeclaration *v = sd->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)
            continue;
        Type *tv = v->type->baseElemOf();
        if (tv->ty != Tstruct)
            continue;

        StructDeclaration *sdv = ((TypeStruct *)tv)->sym;
        stc = mergeFuncAttrs(stc, hasIdentityOpAssign(sdv, sc));
    }

    if (sd->dtor || sd->postblit)
    {
        if (!sd->type->isAssignable())  // Bugzilla 13044
            return NULL;
        stc = mergeFuncAttrs(stc, sd->dtor);
        if (stc & STCsafe)
            stc = (stc & ~STCsafe) | STCtrusted;
    }

    Parameters *fparams = new Parameters;
    fparams->push(new Parameter(STCnodtor, sd->type, Id::p, NULL));
    TypeFunction *tf = new TypeFunction(ParameterList(fparams), sd->handleType(), LINKd, stc | STCref);

    FuncDeclaration *fop = new FuncDeclaration(declLoc, Loc(), Id::assign, stc, tf);
    fop->storage_class |= STCinference;
    fop->generated = true;
    Expression *e = NULL;
    if (stc & STCdisable)
    {
    }
    else if (sd->dtor || sd->postblit)
    {
        /* Do swap this and rhs.
         *    __swap = this; this = s; __swap.dtor();
         */
        //printf("\tswap copy\n");
        Identifier *idtmp = Identifier::generateId("__swap");
        VarDeclaration *tmp = NULL;
        AssignExp *ec = NULL;
        if (sd->dtor)
        {
            tmp = new VarDeclaration(loc, sd->type, idtmp, new VoidInitializer(loc));
            tmp->storage_class |= STCnodtor | STCtemp | STCctfe;
            e = new DeclarationExp(loc, tmp);
            ec = new BlitExp(loc, new VarExp(loc, tmp), new ThisExp(loc));
            e = Expression::combine(e, ec);
        }
        ec = new BlitExp(loc, new ThisExp(loc), new IdentifierExp(loc, Id::p));
        e = Expression::combine(e, ec);
        if (sd->dtor)
        {
            /* Instead of running the destructor on s, run it
             * on tmp. This avoids needing to copy tmp back in to s.
             */
            Expression *ec2 = new DotVarExp(loc, new VarExp(loc, tmp), sd->dtor, false);
            ec2 = new CallExp(loc, ec2);
            e = Expression::combine(e, ec2);
        }
    }
    else
    {
        /* Do memberwise copy.
         *
         * If sd is a nested struct, its vthis field assignment is:
         * 1. If it's nested in a class, it's a rebind of class reference.
         * 2. If it's nested in a function or struct, it's an update of void*.
         * In both cases, it will change the parent context.
         */
        //printf("\tmemberwise copy\n");
        for (size_t i = 0; i < sd->fields.length; i++)
        {
            VarDeclaration *v = sd->fields[i];
            // this.v = s.v;
            AssignExp *ec = new AssignExp(loc,
                new DotVarExp(loc, new ThisExp(loc), v),
                new DotVarExp(loc, new IdentifierExp(loc, Id::p), v));
            e = Expression::combine(e, ec);
        }
    }
    if (e)
    {
        Statement *s1 = new ExpStatement(loc, e);

        /* Add:
         *   return this;
         */
        e = new ThisExp(loc);
        Statement *s2 = new ReturnStatement(loc, e);

        fop->fbody = new CompoundStatement(loc, s1, s2);
        tf->isreturn = true;
    }

    sd->members->push(fop);
    fop->addMember(sc, sd);
    sd->hasIdentityAssign = true;        // temporary mark identity assignable

    unsigned errors = global.startGagging();    // Do not report errors, even if the template opAssign fbody makes it.
    Scope *sc2 = sc->push();
    sc2->stc = 0;
    sc2->linkage = LINKd;

    fop->semantic(sc2);
    fop->semantic2(sc2);
    // Bugzilla 15044: fop->semantic3 isn't run here for lazy forward reference resolution.

    sc2->pop();
    if (global.endGagging(errors))    // if errors happened
    {
        // Disable generated opAssign, because some members forbid identity assignment.
        fop->storage_class |= STCdisable;
        fop->fbody = NULL;  // remove fbody which contains the error
    }

    //printf("-StructDeclaration::buildOpAssign() %s, errors = %d\n", sd->toChars(), (fop->storage_class & STCdisable) != 0);

    return fop;
}

/*******************************************
 * We need an opEquals for the struct if
 * any fields has an opEquals.
 * Generate one if a user-specified one does not exist.
 */
bool needOpEquals(StructDeclaration *sd)
{
    //printf("StructDeclaration::needOpEquals() %s\n", sd->toChars());
    if (sd->isUnionDeclaration())
        goto Ldontneed;

    if (sd->hasIdentityEquals)
        goto Lneed;

    /* If any of the fields has an opEquals, then we
     * need it too.
     */
    for (size_t i = 0; i < sd->fields.length; i++)
    {
        VarDeclaration *v = sd->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)
            continue;
        Type *tv = v->type->toBasetype();
        Type *tvbase = tv->baseElemOf();
        if (tvbase->ty == Tstruct)
        {
            TypeStruct *ts = (TypeStruct *)tvbase;
            if (ts->sym->isUnionDeclaration())
                continue;
            if (needOpEquals(ts->sym))
                goto Lneed;
            if (ts->sym->aliasthis)     // Bugzilla 14806
                goto Lneed;
        }
        if (tv->isfloating())
        {
            // This is necessray for:
            //  1. comparison of +0.0 and -0.0 should be true.
            //  2. comparison of NANs should be false always.
            goto Lneed;
        }
        if (tv->ty == Tarray)
            goto Lneed;
        if (tv->ty == Taarray)
            goto Lneed;
        if (tv->ty == Tclass)
            goto Lneed;
    }
Ldontneed:
    //printf("\tdontneed\n");
    return false;

Lneed:
    //printf("\tneed\n");
    return true;
}

/*******************************************
 * Check given aggregate actually has an identity opEquals or not.
 */
FuncDeclaration *hasIdentityOpEquals(AggregateDeclaration *ad,  Scope *sc)
{
    Dsymbol *eq = search_function(ad, Id::eq);
    if (eq)
    {
        /* check identity opEquals exists
         */
        UnionExp er; new(&er) NullExp(ad->loc, NULL);        // dummy rvalue
        UnionExp el; new(&el) IdentifierExp(ad->loc, Id::p); // dummy lvalue
        Expressions a;
        a.setDim(1);
        for (size_t i = 0; i < 5; i++)
        {
            Type *tthis = NULL;         // dead-store to prevent spurious warning
            switch (i)
            {
                case 0:  tthis = ad->type;                  break;
                case 1:  tthis = ad->type->constOf();       break;
                case 2:  tthis = ad->type->immutableOf();   break;
                case 3:  tthis = ad->type->sharedOf();      break;
                case 4:  tthis = ad->type->sharedConstOf(); break;
                default: assert(0);
            }
            FuncDeclaration *f = NULL;

            unsigned errors = global.startGagging();    // Do not report errors, even if the template opAssign fbody makes it.
            sc = sc->push();
            sc->tinst = NULL;
            sc->minst = NULL;

            for (size_t j = 0; j < 2; j++)
            {
                a[0] = (j == 0 ? er.exp() : el.exp());
                a[0]->type = tthis;
                f = resolveFuncCall(ad->loc, sc, eq, NULL, tthis, &a, 1);
                if (f)
                    break;
            }

            sc = sc->pop();
            global.endGagging(errors);

            if (f)
            {
                if (f->errors)
                    return NULL;
                return f;
            }
        }
    }
    return NULL;
}

/******************************************
 * Build opEquals for struct.
 *      const bool opEquals(const S s) { ... }
 *
 * By fixing bugzilla 3789, opEquals is changed to be never implicitly generated.
 * Now, struct objects comparison s1 == s2 is translated to:
 *      s1.tupleof == s2.tupleof
 * to calculate structural equality. See EqualExp::op_overload.
 */
FuncDeclaration *buildOpEquals(StructDeclaration *sd, Scope *sc)
{
    if (hasIdentityOpEquals(sd, sc))
    {
        sd->hasIdentityEquals = true;
    }
    return NULL;
}

/******************************************
 * Build __xopEquals for TypeInfo_Struct
 *      static bool __xopEquals(ref const S p, ref const S q)
 *      {
 *          return p == q;
 *      }
 *
 * This is called by TypeInfo.equals(p1, p2). If the struct does not support
 * const objects comparison, it will throw "not implemented" Error in runtime.
 */
FuncDeclaration *buildXopEquals(StructDeclaration *sd, Scope *sc)
{
    if (!needOpEquals(sd))
        return NULL;        // bitwise comparison would work

    //printf("StructDeclaration::buildXopEquals() %s\n", sd->toChars());
    if (Dsymbol *eq = search_function(sd, Id::eq))
    {
        if (FuncDeclaration *fd = eq->isFuncDeclaration())
        {
            TypeFunction *tfeqptr;
            {
                Scope scx;

                /* const bool opEquals(ref const S s);
                 */
                Parameters *parameters = new Parameters;
                parameters->push(new Parameter(STCref | STCconst, sd->type, NULL, NULL));
                tfeqptr = new TypeFunction(ParameterList(parameters), Type::tbool, LINKd);
                tfeqptr->mod = MODconst;
                tfeqptr = (TypeFunction *)tfeqptr->semantic(Loc(), &scx);
            }
            fd = fd->overloadExactMatch(tfeqptr);
            if (fd)
                return fd;
        }
    }

    if (!sd->xerreq)
    {
        // object._xopEquals
        Identifier *id = Identifier::idPool("_xopEquals");
        Expression *e = new IdentifierExp(sd->loc, Id::empty);
        e = new DotIdExp(sd->loc, e, Id::object);
        e = new DotIdExp(sd->loc, e, id);
        e = semantic(e, sc);
        Dsymbol *s = getDsymbol(e);
        assert(s);
        sd->xerreq = s->isFuncDeclaration();
    }

    Loc declLoc = Loc();    // loc is unnecessary so __xopEquals is never called directly
    Loc loc = Loc();        // loc is unnecessary so errors are gagged

    Parameters *parameters = new Parameters;
    parameters->push(new Parameter(STCref | STCconst, sd->type, Id::p, NULL));
    parameters->push(new Parameter(STCref | STCconst, sd->type, Id::q, NULL));
    TypeFunction *tf = new TypeFunction(ParameterList(parameters), Type::tbool, LINKd);

    Identifier *id = Id::xopEquals;
    FuncDeclaration *fop = new FuncDeclaration(declLoc, Loc(), id, STCstatic, tf);
    fop->generated = true;
    Expression *e1 = new IdentifierExp(loc, Id::p);
    Expression *e2 = new IdentifierExp(loc, Id::q);
    Expression *e = new EqualExp(TOKequal, loc, e1, e2);

    fop->fbody = new ReturnStatement(loc, e);

    unsigned errors = global.startGagging();    // Do not report errors
    Scope *sc2 = sc->push();
    sc2->stc = 0;
    sc2->linkage = LINKd;

    fop->semantic(sc2);
    fop->semantic2(sc2);

    sc2->pop();
    if (global.endGagging(errors))    // if errors happened
        fop = sd->xerreq;

    return fop;
}

/******************************************
 * Build __xopCmp for TypeInfo_Struct
 *      static bool __xopCmp(ref const S p, ref const S q)
 *      {
 *          return p.opCmp(q);
 *      }
 *
 * This is called by TypeInfo.compare(p1, p2). If the struct does not support
 * const objects comparison, it will throw "not implemented" Error in runtime.
 */
FuncDeclaration *buildXopCmp(StructDeclaration *sd, Scope *sc)
{
    //printf("StructDeclaration::buildXopCmp() %s\n", toChars());
    if (Dsymbol *cmp = search_function(sd, Id::cmp))
    {
        if (FuncDeclaration *fd = cmp->isFuncDeclaration())
        {
            TypeFunction *tfcmpptr;
            {
                Scope scx;

                /* const int opCmp(ref const S s);
                 */
                Parameters *parameters = new Parameters;
                parameters->push(new Parameter(STCref | STCconst, sd->type, NULL, NULL));
                tfcmpptr = new TypeFunction(ParameterList(parameters), Type::tint32, LINKd);
                tfcmpptr->mod = MODconst;
                tfcmpptr = (TypeFunction *)tfcmpptr->semantic(Loc(), &scx);
            }
            fd = fd->overloadExactMatch(tfcmpptr);
            if (fd)
                return fd;
        }
    }
    else
    {
        // FIXME: doesn't work for recursive alias this
        return NULL;
    }

    if (!sd->xerrcmp)
    {
        // object._xopCmp
        Identifier *id = Identifier::idPool("_xopCmp");
        Expression *e = new IdentifierExp(sd->loc, Id::empty);
        e = new DotIdExp(sd->loc, e, Id::object);
        e = new DotIdExp(sd->loc, e, id);
        e = semantic(e, sc);
        Dsymbol *s = getDsymbol(e);
        assert(s);
        sd->xerrcmp = s->isFuncDeclaration();
    }

    Loc declLoc = Loc();    // loc is unnecessary so __xopCmp is never called directly
    Loc loc = Loc();        // loc is unnecessary so errors are gagged

    Parameters *parameters = new Parameters;
    parameters->push(new Parameter(STCref | STCconst, sd->type, Id::p, NULL));
    parameters->push(new Parameter(STCref | STCconst, sd->type, Id::q, NULL));
    TypeFunction *tf = new TypeFunction(ParameterList(parameters), Type::tint32, LINKd);

    Identifier *id = Id::xopCmp;
    FuncDeclaration *fop = new FuncDeclaration(declLoc, Loc(), id, STCstatic, tf);
    fop->generated = true;
    Expression *e1 = new IdentifierExp(loc, Id::p);
    Expression *e2 = new IdentifierExp(loc, Id::q);
#ifdef IN_GCC
    Expression *e = new CallExp(loc, new DotIdExp(loc, e1, Id::cmp), e2);
#else
    Expression *e = new CallExp(loc, new DotIdExp(loc, e2, Id::cmp), e1);
#endif

    fop->fbody = new ReturnStatement(loc, e);

    unsigned errors = global.startGagging();    // Do not report errors
    Scope *sc2 = sc->push();
    sc2->stc = 0;
    sc2->linkage = LINKd;

    fop->semantic(sc2);
    fop->semantic2(sc2);

    sc2->pop();
    if (global.endGagging(errors))    // if errors happened
        fop = sd->xerrcmp;

    return fop;
}

/*******************************************
 * We need a toHash for the struct if
 * any fields has a toHash.
 * Generate one if a user-specified one does not exist.
 */
bool needToHash(StructDeclaration *sd)
{
    //printf("StructDeclaration::needToHash() %s\n", sd->toChars());
    if (sd->isUnionDeclaration())
        goto Ldontneed;

    if (sd->xhash)
        goto Lneed;

    /* If any of the fields has an opEquals, then we
     * need it too.
     */
    for (size_t i = 0; i < sd->fields.length; i++)
    {
        VarDeclaration *v = sd->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)
            continue;
        Type *tv = v->type->toBasetype();
        Type *tvbase = tv->baseElemOf();
        if (tvbase->ty == Tstruct)
        {
            TypeStruct *ts = (TypeStruct *)tvbase;
            if (ts->sym->isUnionDeclaration())
                continue;
            if (needToHash(ts->sym))
                goto Lneed;
            if (ts->sym->aliasthis)     // Bugzilla 14948
                goto Lneed;
        }
        if (tv->isfloating())
        {
            // This is necessray for:
            //  1. comparison of +0.0 and -0.0 should be true.
            goto Lneed;
        }
        if (tv->ty == Tarray)
            goto Lneed;
        if (tv->ty == Taarray)
            goto Lneed;
        if (tv->ty == Tclass)
            goto Lneed;
    }
Ldontneed:
    //printf("\tdontneed\n");
    return false;

Lneed:
    //printf("\tneed\n");
    return true;
}

/******************************************
 * Build __xtoHash for non-bitwise hashing
 *      static hash_t xtoHash(ref const S p) nothrow @trusted;
 */
FuncDeclaration *buildXtoHash(StructDeclaration *sd, Scope *sc)
{
    if (Dsymbol *s = search_function(sd, Id::tohash))
    {
        static TypeFunction *tftohash;
        if (!tftohash)
        {
            tftohash = new TypeFunction(ParameterList(), Type::thash_t, LINKd);
            tftohash->mod = MODconst;
            tftohash = (TypeFunction *)tftohash->merge();
        }

        if (FuncDeclaration *fd = s->isFuncDeclaration())
        {
            fd = fd->overloadExactMatch(tftohash);
            if (fd)
                return fd;
        }
    }

    if (!needToHash(sd))
        return NULL;

    //printf("StructDeclaration::buildXtoHash() %s\n", sd->toPrettyChars());
    Loc declLoc = Loc();    // loc is unnecessary so __xtoHash is never called directly
    Loc loc = Loc();        // internal code should have no loc to prevent coverage

    Parameters *parameters = new Parameters();
    parameters->push(new Parameter(STCref | STCconst, sd->type, Id::p, NULL));
    TypeFunction *tf = new TypeFunction(ParameterList(parameters), Type::thash_t,
                                        LINKd, STCnothrow | STCtrusted);

    Identifier *id = Id::xtoHash;
    FuncDeclaration *fop = new FuncDeclaration(declLoc, Loc(), id, STCstatic, tf);
    fop->generated = true;

    /* Do memberwise hashing.
     *
     * If sd is a nested struct, and if it's nested in a class, the calculated
     * hash value will also contain the result of parent class's toHash().
     */
    const char *code =
        "size_t h = 0;"
        "foreach (i, T; typeof(p.tupleof))"
        "    h += typeid(T).getHash(cast(const void*)&p.tupleof[i]);"
        "return h;";
    fop->fbody = new CompileStatement(loc, new StringExp(loc, const_cast<char *>(code)));

    Scope *sc2 = sc->push();
    sc2->stc = 0;
    sc2->linkage = LINKd;

    fop->semantic(sc2);
    fop->semantic2(sc2);

    sc2->pop();

    //printf("%s fop = %s %s\n", sd->toChars(), fop->toChars(), fop->type->toChars());
    return fop;
}

/*****************************************
 * Create inclusive postblit for struct by aggregating
 * all the postblits in postblits[] with the postblits for
 * all the members.
 * Note the close similarity with AggregateDeclaration::buildDtor(),
 * and the ordering changes (runs forward instead of backwards).
 */
FuncDeclaration *buildPostBlit(StructDeclaration *sd, Scope *sc)
{
    //printf("StructDeclaration::buildPostBlit() %s\n", sd->toChars());
    if (sd->isUnionDeclaration())
        return NULL;

    StorageClass stc = STCsafe | STCnothrow | STCpure | STCnogc;
    Loc declLoc = sd->postblits.length ? sd->postblits[0]->loc : sd->loc;
    Loc loc = Loc();    // internal code should have no loc to prevent coverage

    for (size_t i = 0; i < sd->postblits.length; i++)
    {
        stc |= sd->postblits[i]->storage_class & STCdisable;
    }

    Statements *a = new Statements();
    for (size_t i = 0; i < sd->fields.length && !(stc & STCdisable); i++)
    {
        VarDeclaration *v = sd->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)
            continue;
        Type *tv = v->type->baseElemOf();
        if (tv->ty != Tstruct)
            continue;
        StructDeclaration *sdv = ((TypeStruct *)tv)->sym;
        if (!sdv->postblit)
            continue;
        assert(!sdv->isUnionDeclaration());
        sdv->postblit->functionSemantic();

        stc = mergeFuncAttrs(stc, sdv->postblit);
        stc = mergeFuncAttrs(stc, sdv->dtor);
        if (stc & STCdisable)
        {
            a->setDim(0);
            break;
        }

        Expression *ex = NULL;
        tv = v->type->toBasetype();
        if (tv->ty == Tstruct)
        {
            // this.v.__xpostblit()

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call postblits on const/immutable objects.
            ex = new AddrExp(loc, ex);
            ex = new CastExp(loc, ex, v->type->mutableOf()->pointerTo());
            ex = new PtrExp(loc, ex);
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new DotVarExp(loc, ex, sdv->postblit, false);
            ex = new CallExp(loc, ex);
        }
        else
        {
            // __ArrayPostblit((cast(S*)this.v.ptr)[0 .. n])

            uinteger_t n = tv->numberOfElems(loc);
            if (n == 0)
                continue;

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call postblits on const/immutable objects.
            ex = new DotIdExp(loc, ex, Id::ptr);
            ex = new CastExp(loc, ex, sdv->type->pointerTo());
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type::tsize_t),
                                       new IntegerExp(loc, n, Type::tsize_t));
            // Prevent redundant bounds check
            ((SliceExp *)ex)->upperIsInBounds = true;
            ((SliceExp *)ex)->lowerIsLessThanUpper = true;

            ex = new CallExp(loc, new IdentifierExp(loc, Id::__ArrayPostblit), ex);
        }
        a->push(new ExpStatement(loc, ex)); // combine in forward order

        /* Bugzilla 10972: When the following field postblit calls fail,
         * this field should be destructed for Exception Safety.
         */
        if (!sdv->dtor)
            continue;
        sdv->dtor->functionSemantic();

        tv = v->type->toBasetype();
        if (v->type->toBasetype()->ty == Tstruct)
        {
            // this.v.__xdtor()

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call destructors on const/immutable objects.
            ex = new AddrExp(loc, ex);
            ex = new CastExp(loc, ex, v->type->mutableOf()->pointerTo());
            ex = new PtrExp(loc, ex);
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new DotVarExp(loc, ex, sdv->dtor, false);
            ex = new CallExp(loc, ex);
        }
        else
        {
            // __ArrayDtor((cast(S*)this.v.ptr)[0 .. n])

            uinteger_t n = tv->numberOfElems(loc);
            //if (n == 0)
            //    continue;

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call destructors on const/immutable objects.
            ex = new DotIdExp(loc, ex, Id::ptr);
            ex = new CastExp(loc, ex, sdv->type->pointerTo());
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type::tsize_t),
                                       new IntegerExp(loc, n, Type::tsize_t));
            // Prevent redundant bounds check
            ((SliceExp *)ex)->upperIsInBounds = true;
            ((SliceExp *)ex)->lowerIsLessThanUpper = true;

            ex = new CallExp(loc, new IdentifierExp(loc, Id::__ArrayDtor), ex);
        }
        a->push(new ScopeGuardStatement(loc, TOKon_scope_failure, new ExpStatement(loc, ex)));
    }

    // Build our own "postblit" which executes a, but only if needed.
    if (a->length || (stc & STCdisable))
    {
        //printf("Building __fieldPostBlit()\n");
        PostBlitDeclaration *dd = new PostBlitDeclaration(declLoc, Loc(), stc, Id::__fieldPostblit);
        dd->generated = true;
        dd->storage_class |= STCinference;
        dd->fbody = (stc & STCdisable) ? NULL : new CompoundStatement(loc, a);
        sd->postblits.shift(dd);
        sd->members->push(dd);
        dd->semantic(sc);
    }

    FuncDeclaration *xpostblit = NULL;
    switch (sd->postblits.length)
    {
        case 0:
            break;

        case 1:
            xpostblit = sd->postblits[0];
            break;

        default:
            Expression *e = NULL;
            stc = STCsafe | STCnothrow | STCpure | STCnogc;
            for (size_t i = 0; i < sd->postblits.length; i++)
            {
                FuncDeclaration *fd = sd->postblits[i];
                stc = mergeFuncAttrs(stc, fd);
                if (stc & STCdisable)
                {
                    e = NULL;
                    break;
                }
                Expression *ex = new ThisExp(loc);
                ex = new DotVarExp(loc, ex, fd, false);
                ex = new CallExp(loc, ex);
                e = Expression::combine(e, ex);
            }
            PostBlitDeclaration *dd = new PostBlitDeclaration(declLoc, Loc(), stc, Id::__aggrPostblit);
            dd->storage_class |= STCinference;
            dd->fbody = new ExpStatement(loc, e);
            sd->members->push(dd);
            dd->semantic(sc);
            xpostblit = dd;
            break;
    }
    // Add an __xpostblit alias to make the inclusive postblit accessible
    if (xpostblit)
    {
        AliasDeclaration *alias = new AliasDeclaration(Loc(), Id::__xpostblit, xpostblit);
        alias->semantic(sc);
        sd->members->push(alias);
        alias->addMember(sc, sd); // add to symbol table
    }
    return xpostblit;
}

/*****************************************
 * Create inclusive destructor for struct/class by aggregating
 * all the destructors in dtors[] with the destructors for
 * all the members.
 * Note the close similarity with StructDeclaration::buildPostBlit(),
 * and the ordering changes (runs backward instead of forwards).
 */
FuncDeclaration *buildDtor(AggregateDeclaration *ad, Scope *sc)
{
    //printf("AggregateDeclaration::buildDtor() %s\n", ad->toChars());
    if (ad->isUnionDeclaration())
        return NULL;

    StorageClass stc = STCsafe | STCnothrow | STCpure | STCnogc;
    Loc declLoc = ad->dtors.length ? ad->dtors[0]->loc : ad->loc;
    Loc loc = Loc();    // internal code should have no loc to prevent coverage

    Expression *e = NULL;
    for (size_t i = 0; i < ad->fields.length; i++)
    {
        VarDeclaration *v = ad->fields[i];
        if (v->storage_class & STCref)
            continue;
        if (v->overlapped)
            continue;
        Type *tv = v->type->baseElemOf();
        if (tv->ty != Tstruct)
            continue;
        StructDeclaration *sdv = ((TypeStruct *)tv)->sym;
        if (!sdv->dtor)
            continue;
        sdv->dtor->functionSemantic();

        stc = mergeFuncAttrs(stc, sdv->dtor);
        if (stc & STCdisable)
        {
            e = NULL;
            break;
        }

        Expression *ex = NULL;
        tv = v->type->toBasetype();
        if (tv->ty == Tstruct)
        {
            // this.v.__xdtor()

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call destructors on const/immutable objects.
            ex = new AddrExp(loc, ex);
            ex = new CastExp(loc, ex, v->type->mutableOf()->pointerTo());
            ex = new PtrExp(loc, ex);
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new DotVarExp(loc, ex, sdv->dtor, false);
            ex = new CallExp(loc, ex);
        }
        else
        {
            // __ArrayDtor((cast(S*)this.v.ptr)[0 .. n])

            uinteger_t n = tv->numberOfElems(loc);
            if (n == 0)
                continue;

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, v);

            // This is a hack so we can call destructors on const/immutable objects.
            ex = new DotIdExp(loc, ex, Id::ptr);
            ex = new CastExp(loc, ex, sdv->type->pointerTo());
            if (stc & STCsafe)
                stc = (stc & ~STCsafe) | STCtrusted;

            ex = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type::tsize_t),
                                       new IntegerExp(loc, n, Type::tsize_t));
            // Prevent redundant bounds check
            ((SliceExp *)ex)->upperIsInBounds = true;
            ((SliceExp *)ex)->lowerIsLessThanUpper = true;

            ex = new CallExp(loc, new IdentifierExp(loc, Id::__ArrayDtor), ex);
        }
        e = Expression::combine(ex, e); // combine in reverse order
    }

    /* Build our own "destructor" which executes e
     */
    if (e || (stc & STCdisable))
    {
        //printf("Building __fieldDtor()\n");
        DtorDeclaration *dd = new DtorDeclaration(declLoc, Loc(), stc, Id::__fieldDtor);
        dd->generated = true;
        dd->storage_class |= STCinference;
        dd->fbody = new ExpStatement(loc, e);
        ad->dtors.shift(dd);
        ad->members->push(dd);
        dd->semantic(sc);
    }

    FuncDeclaration *xdtor = NULL;
    switch (ad->dtors.length)
    {
        case 0:
            break;

        case 1:
            xdtor = ad->dtors[0];
            break;

        default:
            e = NULL;
            stc = STCsafe | STCnothrow | STCpure | STCnogc;
            for (size_t i = 0; i < ad->dtors.length; i++)
            {
                FuncDeclaration *fd = ad->dtors[i];
                stc = mergeFuncAttrs(stc, fd);
                if (stc & STCdisable)
                {
                    e = NULL;
                    break;
                }
                Expression *ex = new ThisExp(loc);
                ex = new DotVarExp(loc, ex, fd, false);
                ex = new CallExp(loc, ex);
                e = Expression::combine(ex, e);
            }
            DtorDeclaration *dd = new DtorDeclaration(declLoc, Loc(), stc, Id::__aggrDtor);
            dd->generated = true;
            dd->storage_class |= STCinference;
            dd->fbody = new ExpStatement(loc, e);
            ad->members->push(dd);
            dd->semantic(sc);
            xdtor = dd;
            break;
    }
    // Add an __xdtor alias to make the inclusive dtor accessible
    if (xdtor)
    {
        AliasDeclaration *alias = new AliasDeclaration(Loc(), Id::__xdtor, xdtor);
        alias->semantic(sc);
        ad->members->push(alias);
        alias->addMember(sc, ad); // add to symbol table
    }
    return xdtor;
}

/******************************************
 * Create inclusive invariant for struct/class by aggregating
 * all the invariants in invs[].
 *      void __invariant() const [pure nothrow @trusted]
 *      {
 *          invs[0](), invs[1](), ...;
 *      }
 */
FuncDeclaration *buildInv(AggregateDeclaration *ad, Scope *sc)
{
    StorageClass stc = STCsafe | STCnothrow | STCpure | STCnogc;
    Loc declLoc = ad->loc;
    Loc loc = Loc();    // internal code should have no loc to prevent coverage

    switch (ad->invs.length)
    {
        case 0:
            return NULL;

        case 1:
            // Don't return invs[0] so it has uniquely generated name.
            /* fall through */

        default:
            Expression *e = NULL;
            StorageClass stcx = 0;
            for (size_t i = 0; i < ad->invs.length; i++)
            {
                stc = mergeFuncAttrs(stc, ad->invs[i]);
                if (stc & STCdisable)
                {
                    // What should do?
                }
                StorageClass stcy = (ad->invs[i]->storage_class & STCsynchronized) |
                                    (ad->invs[i]->type->mod & MODshared ? STCshared : 0);
                if (i == 0)
                    stcx = stcy;
                else if (stcx ^ stcy)
                {
            #if 1   // currently rejects
                    ad->error(ad->invs[i]->loc, "mixing invariants with shared/synchronized differene is not supported");
                    e = NULL;
                    break;
            #endif
                }
                e = Expression::combine(e, new CallExp(loc, new VarExp(loc, ad->invs[i], false)));
            }
            InvariantDeclaration *inv;
            inv = new InvariantDeclaration(declLoc, Loc(), stc | stcx, Id::classInvariant);
            inv->fbody = new ExpStatement(loc, e);
            ad->members->push(inv);
            inv->semantic(sc);
            return inv;
    }
}
