
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/opover.c
 */

#include "root/dsystem.h"               // memset()
#include "root/rmem.h"

#include "mars.h"
#include "mtype.h"
#include "init.h"
#include "expression.h"
#include "statement.h"
#include "scope.h"
#include "id.h"
#include "declaration.h"
#include "aggregate.h"
#include "template.h"
#include "tokens.h"

static Dsymbol *inferApplyArgTypesX(Expression *ethis, FuncDeclaration *fstart, Parameters *parameters);
static int inferApplyArgTypesY(TypeFunction *tf, Parameters *parameters, int flags = 0);
Expression *compare_overload(BinExp *e, Scope *sc, Identifier *id);
bool MODimplicitConv(MOD modfrom, MOD modto);

/******************************** Expression **************************/


/***********************************
 * Determine if operands of binary op can be reversed
 * to fit operator overload.
 */

bool isCommutative(TOK op)
{
    switch (op)
    {
        case TOKadd:
        case TOKmul:
        case TOKand:
        case TOKor:
        case TOKxor:

        // EqualExp
        case TOKequal:
        case TOKnotequal:

        // CmpExp
        case TOKlt:
        case TOKle:
        case TOKgt:
        case TOKge:
            return true;

        default:
            break;
    }
    return false;
}

/***********************************
 * Get Identifier for operator overload.
 */

static Identifier *opId(Expression *e)
{
    class OpIdVisitor : public Visitor
    {
    public:
        Identifier *id;
        void visit(Expression *)    { assert(0); }
        void visit(UAddExp *)       { id = Id::uadd; }
        void visit(NegExp *)        { id = Id::neg; }
        void visit(ComExp *)        { id = Id::com; }
        void visit(CastExp *)       { id = Id::_cast; }
        void visit(InExp *)         { id = Id::opIn; }
        void visit(PostExp *e)      { id = (e->op == TOKplusplus) ? Id::postinc : Id::postdec; }
        void visit(AddExp *)        { id = Id::add; }
        void visit(MinExp *)        { id = Id::sub; }
        void visit(MulExp *)        { id = Id::mul; }
        void visit(DivExp *)        { id = Id::div; }
        void visit(ModExp *)        { id = Id::mod; }
        void visit(PowExp *)        { id = Id::pow; }
        void visit(ShlExp *)        { id = Id::shl; }
        void visit(ShrExp *)        { id = Id::shr; }
        void visit(UshrExp *)       { id = Id::ushr; }
        void visit(AndExp *)        { id = Id::iand; }
        void visit(OrExp *)         { id = Id::ior; }
        void visit(XorExp *)        { id = Id::ixor; }
        void visit(CatExp *)        { id = Id::cat; }
        void visit(AssignExp *)     { id = Id::assign; }
        void visit(AddAssignExp *)  { id = Id::addass; }
        void visit(MinAssignExp *)  { id = Id::subass; }
        void visit(MulAssignExp *)  { id = Id::mulass; }
        void visit(DivAssignExp *)  { id = Id::divass; }
        void visit(ModAssignExp *)  { id = Id::modass; }
        void visit(AndAssignExp *)  { id = Id::andass; }
        void visit(OrAssignExp *)   { id = Id::orass; }
        void visit(XorAssignExp *)  { id = Id::xorass; }
        void visit(ShlAssignExp *)  { id = Id::shlass; }
        void visit(ShrAssignExp *)  { id = Id::shrass; }
        void visit(UshrAssignExp *) { id = Id::ushrass; }
        void visit(CatAssignExp *)  { id = Id::catass; }
        void visit(PowAssignExp *)  { id = Id::powass; }
        void visit(EqualExp *)      { id = Id::eq; }
        void visit(CmpExp *)        { id = Id::cmp; }
        void visit(ArrayExp *)      { id = Id::index; }
        void visit(PtrExp *)        { id = Id::opStar; }
    };
    OpIdVisitor v;
    e->accept(&v);
    return v.id;
}

/***********************************
 * Get Identifier for reverse operator overload,
 * NULL if not supported for this operator.
 */

static Identifier *opId_r(Expression *e)
{
    class OpIdRVisitor : public Visitor
    {
    public:
        Identifier *id;
        void visit(Expression *) { id = NULL; }
        void visit(InExp *)      { id = Id::opIn_r; }
        void visit(AddExp *)     { id = Id::add_r; }
        void visit(MinExp *)     { id = Id::sub_r; }
        void visit(MulExp *)     { id = Id::mul_r; }
        void visit(DivExp *)     { id = Id::div_r; }
        void visit(ModExp *)     { id = Id::mod_r; }
        void visit(PowExp *)     { id = Id::pow_r; }
        void visit(ShlExp *)     { id = Id::shl_r; }
        void visit(ShrExp *)     { id = Id::shr_r; }
        void visit(UshrExp *)    { id = Id::ushr_r; }
        void visit(AndExp *)     { id = Id::iand_r; }
        void visit(OrExp *)      { id = Id::ior_r; }
        void visit(XorExp *)     { id = Id::ixor_r; }
        void visit(CatExp *)     { id = Id::cat_r; }
    };
    OpIdRVisitor v;
    e->accept(&v);
    return v.id;
}

/************************************
 * If type is a class or struct, return the symbol for it,
 * else NULL
 */
AggregateDeclaration *isAggregate(Type *t)
{
    t = t->toBasetype();
    if (t->ty == Tclass)
    {
        return ((TypeClass *)t)->sym;
    }
    else if (t->ty == Tstruct)
    {
        return ((TypeStruct *)t)->sym;
    }
    return NULL;
}

/*******************************************
 * Helper function to turn operator into template argument list
 */
Objects *opToArg(Scope *sc, TOK op)
{
    /* Remove the = from op=
     */
    switch (op)
    {
        case TOKaddass: op = TOKadd; break;
        case TOKminass: op = TOKmin; break;
        case TOKmulass: op = TOKmul; break;
        case TOKdivass: op = TOKdiv; break;
        case TOKmodass: op = TOKmod; break;
        case TOKandass: op = TOKand; break;
        case TOKorass:  op = TOKor;  break;
        case TOKxorass: op = TOKxor; break;
        case TOKshlass: op = TOKshl; break;
        case TOKshrass: op = TOKshr; break;
        case TOKushrass: op = TOKushr; break;
        case TOKcatass: op = TOKcat; break;
        case TOKpowass: op = TOKpow; break;
        default:                     break;
    }
    Expression *e = new StringExp(Loc(), const_cast<char *>(Token::toChars(op)));
    e = expressionSemantic(e, sc);
    Objects *tiargs = new Objects();
    tiargs->push(e);
    return tiargs;
}

/************************************
 * Operator overload.
 * Check for operator overload, if so, replace
 * with function call.
 * Return NULL if not an operator overload.
 */

Expression *op_overload(Expression *e, Scope *sc)
{
    class OpOverload : public Visitor
    {
    public:
        Scope *sc;
        Expression *result;

        OpOverload(Scope *sc)
            : sc(sc)
        {
            result = NULL;
        }

        void visit(Expression *)
        {
            assert(0);
        }

        void visit(UnaExp *e)
        {
            //printf("UnaExp::op_overload() (%s)\n", e->toChars());

            if (e->e1->op == TOKarray)
            {
                ArrayExp *ae = (ArrayExp *)e->e1;
                ae->e1 = expressionSemantic(ae->e1, sc);
                ae->e1 = resolveProperties(sc, ae->e1);
                Expression *ae1old = ae->e1;

                const bool maybeSlice =
                    (ae->arguments->length == 0 ||
                     (ae->arguments->length == 1 && (*ae->arguments)[0]->op == TOKinterval));
                IntervalExp *ie = NULL;
                if (maybeSlice && ae->arguments->length)
                {
                    assert((*ae->arguments)[0]->op == TOKinterval);
                    ie = (IntervalExp *)(*ae->arguments)[0];
                }

                while (true)
                {
                    if (ae->e1->op == TOKerror)
                    {
                        result = ae->e1;
                        return;
                    }
                    Expression *e0 = NULL;
                    Expression *ae1save = ae->e1;
                    ae->lengthVar = NULL;

                    Type *t1b = ae->e1->type->toBasetype();
                    AggregateDeclaration *ad = isAggregate(t1b);
                    if (!ad)
                        break;
                    if (search_function(ad, Id::opIndexUnary))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, &e0);
                        if (!result)    // op(a[i..j]) might be: a.opSliceUnary!(op)(i, j)
                            goto Lfallback;
                        if (result->op == TOKerror)
                            return;

                        /* Rewrite op(a[arguments]) as:
                         *      a.opIndexUnary!(op)(arguments)
                         */
                        Expressions *a = (Expressions *)ae->arguments->copy();
                        Objects *tiargs = opToArg(sc, e->op);
                        result = new DotTemplateInstanceExp(e->loc, ae->e1, Id::opIndexUnary, tiargs);
                        result = new CallExp(e->loc, result, a);
                        if (maybeSlice) // op(a[]) might be: a.opSliceUnary!(op)()
                            result = trySemantic(result, sc);
                        else
                            result = expressionSemantic(result, sc);
                        if (result)
                        {
                            result = Expression::combine(e0, result);
                            return;
                        }
                    }
                Lfallback:
                    if (maybeSlice && search_function(ad, Id::opSliceUnary))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, ie, &e0);
                        if (result->op == TOKerror)
                            return;

                        /* Rewrite op(a[i..j]) as:
                         *      a.opSliceUnary!(op)(i, j)
                         */
                        Expressions *a = new Expressions();
                        if (ie)
                        {
                            a->push(ie->lwr);
                            a->push(ie->upr);
                        }
                        Objects *tiargs = opToArg(sc, e->op);
                        result = new DotTemplateInstanceExp(e->loc, ae->e1, Id::opSliceUnary, tiargs);
                        result = new CallExp(e->loc, result, a);
                        result = expressionSemantic(result, sc);
                        result = Expression::combine(e0, result);
                        return;
                    }

                    // Didn't find it. Forward to aliasthis
                    if (ad->aliasthis && t1b != ae->att1)
                    {
                        if (!ae->att1 && t1b->checkAliasThisRec())
                            ae->att1 = t1b;

                        /* Rewrite op(a[arguments]) as:
                         *      op(a.aliasthis[arguments])
                         */
                        ae->e1 = resolveAliasThis(sc, ae1save, true);
                        if (ae->e1)
                            continue;
                    }
                    break;
                }
                ae->e1 = ae1old;    // recovery
                ae->lengthVar = NULL;
            }

            e->e1 = expressionSemantic(e->e1, sc);
            e->e1 = resolveProperties(sc, e->e1);
            if (e->e1->op == TOKerror)
            {
                result = e->e1;
                return;
            }

            AggregateDeclaration *ad = isAggregate(e->e1->type);
            if (ad)
            {
                Dsymbol *fd = NULL;
        #if 1 // Old way, kept for compatibility with D1
                if (e->op != TOKpreplusplus && e->op != TOKpreminusminus)
                {
                    fd = search_function(ad, opId(e));
                    if (fd)
                    {
                        // Rewrite +e1 as e1.add()
                        result = build_overload(e->loc, sc, e->e1, NULL, fd);
                        return;
                    }
                }
        #endif

                /* Rewrite as:
                 *      e1.opUnary!(op)()
                 */
                fd = search_function(ad, Id::opUnary);
                if (fd)
                {
                    Objects *tiargs = opToArg(sc, e->op);
                    result = new DotTemplateInstanceExp(e->loc, e->e1, fd->ident, tiargs);
                    result = new CallExp(e->loc, result);
                    result = expressionSemantic(result, sc);
                    return;
                }

                // Didn't find it. Forward to aliasthis
                if (ad->aliasthis && e->e1->type != e->att1)
                {
                    /* Rewrite op(e1) as:
                     *      op(e1.aliasthis)
                     */
                    //printf("att una %s e1 = %s\n", Token::toChars(op), this->e1->type->toChars());
                    Expression *e1 = new DotIdExp(e->loc, e->e1, ad->aliasthis->ident);
                    UnaExp *ue = (UnaExp *)e->copy();
                    if (!ue->att1 && e->e1->type->checkAliasThisRec())
                        ue->att1 = e->e1->type;
                    ue->e1 = e1;
                    result = trySemantic(ue, sc);
                    return;
                }
            }
        }

        void visit(ArrayExp *ae)
        {
            //printf("ArrayExp::op_overload() (%s)\n", ae->toChars());
            ae->e1 = expressionSemantic(ae->e1, sc);
            ae->e1 = resolveProperties(sc, ae->e1);
            Expression *ae1old = ae->e1;

            const bool maybeSlice =
                (ae->arguments->length == 0 ||
                 (ae->arguments->length == 1 && (*ae->arguments)[0]->op == TOKinterval));
            IntervalExp *ie = NULL;
            if (maybeSlice && ae->arguments->length)
            {
                assert((*ae->arguments)[0]->op == TOKinterval);
                ie = (IntervalExp *)(*ae->arguments)[0];
            }

            while (true)
            {
                if (ae->e1->op == TOKerror)
                {
                    result = ae->e1;
                    return;
                }
                Expression *e0 = NULL;
                Expression *ae1save = ae->e1;
                ae->lengthVar = NULL;

                Type *t1b = ae->e1->type->toBasetype();
                AggregateDeclaration *ad = isAggregate(t1b);
                if (!ad)
                {
                    // If the non-aggregate expression ae->e1 is indexable or sliceable,
                    // convert it to the corresponding concrete expression.
                    if (t1b->ty == Tpointer ||
                        t1b->ty == Tsarray ||
                        t1b->ty == Tarray ||
                        t1b->ty == Taarray ||
                        t1b->ty == Ttuple ||
                        t1b->ty == Tvector ||
                        ae->e1->op == TOKtype)
                    {
                        // Convert to SliceExp
                        if (maybeSlice)
                        {
                            result = new SliceExp(ae->loc, ae->e1, ie);
                            result = expressionSemantic(result, sc);
                            return;
                        }
                        // Convert to IndexExp
                        if (ae->arguments->length == 1)
                        {
                            result = new IndexExp(ae->loc, ae->e1, (*ae->arguments)[0]);
                            result = expressionSemantic(result, sc);
                            return;
                        }
                    }
                    break;
                }
                if (search_function(ad, Id::index))
                {
                    // Deal with $
                    result = resolveOpDollar(sc, ae, &e0);
                    if (!result)    // a[i..j] might be: a.opSlice(i, j)
                        goto Lfallback;
                    if (result->op == TOKerror)
                        return;

                    /* Rewrite e1[arguments] as:
                     *      e1.opIndex(arguments)
                     */
                    Expressions *a = (Expressions *)ae->arguments->copy();
                    result = new DotIdExp(ae->loc, ae->e1, Id::index);
                    result = new CallExp(ae->loc, result, a);
                    if (maybeSlice) // a[] might be: a.opSlice()
                        result = trySemantic(result, sc);
                    else
                        result = expressionSemantic(result, sc);
                    if (result)
                    {
                        result = Expression::combine(e0, result);
                        return;
                    }
                }
            Lfallback:
                if (maybeSlice && ae->e1->op == TOKtype)
                {
                    result = new SliceExp(ae->loc, ae->e1, ie);
                    result = expressionSemantic(result, sc);
                    result = Expression::combine(e0, result);
                    return;
                }
                if (maybeSlice && search_function(ad, Id::slice))
                {
                    // Deal with $
                    result = resolveOpDollar(sc, ae, ie, &e0);
                    if (result->op == TOKerror)
                        return;

                    /* Rewrite a[i..j] as:
                     *      a.opSlice(i, j)
                     */
                    Expressions *a = new Expressions();
                    if (ie)
                    {
                        a->push(ie->lwr);
                        a->push(ie->upr);
                    }
                    result = new DotIdExp(ae->loc, ae->e1, Id::slice);
                    result = new CallExp(ae->loc, result, a);
                    result = expressionSemantic(result, sc);
                    result = Expression::combine(e0, result);
                    return;
                }

                // Didn't find it. Forward to aliasthis
                if (ad->aliasthis && t1b != ae->att1)
                {
                    if (!ae->att1 && t1b->checkAliasThisRec())
                        ae->att1 = t1b;
                    //printf("att arr e1 = %s\n", this->e1->type->toChars());

                    /* Rewrite op(a[arguments]) as:
                     *      op(a.aliasthis[arguments])
                     */
                    ae->e1 = resolveAliasThis(sc, ae1save, true);
                    if (ae->e1)
                        continue;
                }
                break;
            }
            ae->e1 = ae1old;    // recovery
            ae->lengthVar = NULL;
        }

        /***********************************************
         * This is mostly the same as UnaryExp::op_overload(), but has
         * a different rewrite.
         */
        void visit(CastExp *e)
        {
            //printf("CastExp::op_overload() (%s)\n", e->toChars());
            AggregateDeclaration *ad = isAggregate(e->e1->type);
            if (ad)
            {
                Dsymbol *fd = NULL;
                /* Rewrite as:
                 *      e1.opCast!(T)()
                 */
                fd = search_function(ad, Id::_cast);
                if (fd)
                {
        #if 1 // Backwards compatibility with D1 if opCast is a function, not a template
                    if (fd->isFuncDeclaration())
                    {
                        // Rewrite as:  e1.opCast()
                        result = build_overload(e->loc, sc, e->e1, NULL, fd);
                        return;
                    }
        #endif
                    Objects *tiargs = new Objects();
                    tiargs->push(e->to);
                    result = new DotTemplateInstanceExp(e->loc, e->e1, fd->ident, tiargs);
                    result = new CallExp(e->loc, result);
                    result = expressionSemantic(result, sc);
                    return;
                }

                // Didn't find it. Forward to aliasthis
                if (ad->aliasthis)
                {
                    /* Rewrite op(e1) as:
                     *      op(e1.aliasthis)
                     */
                    Expression *e1 = new DotIdExp(e->loc, e->e1, ad->aliasthis->ident);
                    result = e->copy();
                    ((UnaExp *)result)->e1 = e1;
                    result = trySemantic(result, sc);
                    return;
                }
            }
        }

        void visit(BinExp *e)
        {
            //printf("BinExp::op_overload() (%s)\n", e->toChars());

            Identifier *id = opId(e);
            Identifier *id_r = opId_r(e);

            Expressions args1;
            Expressions args2;
            int argsset = 0;

            AggregateDeclaration *ad1 = isAggregate(e->e1->type);
            AggregateDeclaration *ad2 = isAggregate(e->e2->type);

            if (e->op == TOKassign && ad1 == ad2)
            {
                StructDeclaration *sd = ad1->isStructDeclaration();
                if (sd && !sd->hasIdentityAssign)
                {
                    /* This is bitwise struct assignment. */
                    return;
                }
            }

            Dsymbol *s = NULL;
            Dsymbol *s_r = NULL;

        #if 1 // the old D1 scheme
            if (ad1 && id)
            {
                s = search_function(ad1, id);
            }
            if (ad2 && id_r)
            {
                s_r = search_function(ad2, id_r);

                // Bugzilla 12778: If both x.opBinary(y) and y.opBinaryRight(x) found,
                // and they are exactly same symbol, x.opBinary(y) should be preferred.
                if (s_r && s_r == s)
                    s_r = NULL;
            }
        #endif

            Objects *tiargs = NULL;
            if (e->op == TOKplusplus || e->op == TOKminusminus)
            {
                // Bug4099 fix
                if (ad1 && search_function(ad1, Id::opUnary))
                    return;
            }
            if (!s && !s_r && e->op != TOKequal && e->op != TOKnotequal && e->op != TOKassign &&
                e->op != TOKplusplus && e->op != TOKminusminus)
            {
                /* Try the new D2 scheme, opBinary and opBinaryRight
                 */
                if (ad1)
                {
                    s = search_function(ad1, Id::opBinary);
                    if (s && !s->isTemplateDeclaration())
                    {
                        e->e1->error("%s.opBinary isn't a template", e->e1->toChars());
                        result = new ErrorExp();
                        return;
                    }
                }
                if (ad2)
                {
                    s_r = search_function(ad2, Id::opBinaryRight);
                    if (s_r && !s_r->isTemplateDeclaration())
                    {
                        e->e2->error("%s.opBinaryRight isn't a template", e->e2->toChars());
                        result = new ErrorExp();
                        return;
                    }
                    if (s_r && s_r == s)    // Bugzilla 12778
                        s_r = NULL;
                }

                // Set tiargs, the template argument list, which will be the operator string
                if (s || s_r)
                {
                    id = Id::opBinary;
                    id_r = Id::opBinaryRight;
                    tiargs = opToArg(sc, e->op);
                }
            }

            if (s || s_r)
            {
                /* Try:
                 *      a.opfunc(b)
                 *      b.opfunc_r(a)
                 * and see which is better.
                 */

                args1.setDim(1);
                args1[0] = e->e1;
                expandTuples(&args1);
                args2.setDim(1);
                args2[0] = e->e2;
                expandTuples(&args2);
                argsset = 1;

                Match m;
                memset(&m, 0, sizeof(m));
                m.last = MATCHnomatch;

                if (s)
                {
                    functionResolve(&m, s, e->loc, sc, tiargs, e->e1->type, &args2);
                    if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                    {
                        result = new ErrorExp();
                        return;
                    }
                }

                FuncDeclaration *lastf = m.lastf;

                if (s_r)
                {
                    functionResolve(&m, s_r, e->loc, sc, tiargs, e->e2->type, &args1);
                    if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                    {
                        result = new ErrorExp();
                        return;
                    }
                }

                if (m.count > 1)
                {
                    // Error, ambiguous
                    e->error("overloads %s and %s both match argument list for %s",
                            m.lastf->type->toChars(),
                            m.nextf->type->toChars(),
                            m.lastf->toChars());
                }
                else if (m.last <= MATCHnomatch)
                {
                    m.lastf = m.anyf;
                    if (tiargs)
                        goto L1;
                }

                if (e->op == TOKplusplus || e->op == TOKminusminus)
                {
                    // Kludge because operator overloading regards e++ and e--
                    // as unary, but it's implemented as a binary.
                    // Rewrite (e1 ++ e2) as e1.postinc()
                    // Rewrite (e1 -- e2) as e1.postdec()
                    result = build_overload(e->loc, sc, e->e1, NULL, m.lastf ? m.lastf : s);
                }
                else if ((lastf && m.lastf == lastf) || (!s_r && m.last <= MATCHnomatch))
                {
                    // Rewrite (e1 op e2) as e1.opfunc(e2)
                    result = build_overload(e->loc, sc, e->e1, e->e2, m.lastf ? m.lastf : s);
                }
                else
                {
                    // Rewrite (e1 op e2) as e2.opfunc_r(e1)
                    result = build_overload(e->loc, sc, e->e2, e->e1, m.lastf ? m.lastf : s_r);
                }
                return;
            }

        L1:
        #if 1 // Retained for D1 compatibility
            if (isCommutative(e->op) && !tiargs)
            {
                s = NULL;
                s_r = NULL;
                if (ad1 && id_r)
                {
                    s_r = search_function(ad1, id_r);
                }
                if (ad2 && id)
                {
                    s = search_function(ad2, id);
                    if (s && s == s_r)  // Bugzilla 12778
                        s = NULL;
                }

                if (s || s_r)
                {
                    /* Try:
                     *  a.opfunc_r(b)
                     *  b.opfunc(a)
                     * and see which is better.
                     */

                    if (!argsset)
                    {
                        args1.setDim(1);
                        args1[0] = e->e1;
                        expandTuples(&args1);
                        args2.setDim(1);
                        args2[0] = e->e2;
                        expandTuples(&args2);
                    }

                    Match m;
                    memset(&m, 0, sizeof(m));
                    m.last = MATCHnomatch;

                    if (s_r)
                    {
                        functionResolve(&m, s_r, e->loc, sc, tiargs, e->e1->type, &args2);
                        if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                        {
                            result = new ErrorExp();
                            return;
                        }
                    }

                    FuncDeclaration *lastf = m.lastf;

                    if (s)
                    {
                        functionResolve(&m, s, e->loc, sc, tiargs, e->e2->type, &args1);
                        if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                        {
                            result = new ErrorExp();
                            return;
                        }
                    }

                    if (m.count > 1)
                    {
                        // Error, ambiguous
                        e->error("overloads %s and %s both match argument list for %s",
                                m.lastf->type->toChars(),
                                m.nextf->type->toChars(),
                                m.lastf->toChars());
                    }
                    else if (m.last <= MATCHnomatch)
                    {
                        m.lastf = m.anyf;
                    }

                    if ((lastf && m.lastf == lastf) || (!s && m.last <= MATCHnomatch))
                    {
                        // Rewrite (e1 op e2) as e1.opfunc_r(e2)
                        result = build_overload(e->loc, sc, e->e1, e->e2, m.lastf ? m.lastf : s_r);
                    }
                    else
                    {
                        // Rewrite (e1 op e2) as e2.opfunc(e1)
                        result = build_overload(e->loc, sc, e->e2, e->e1, m.lastf ? m.lastf : s);
                    }

                    // When reversing operands of comparison operators,
                    // need to reverse the sense of the op
                    switch (e->op)
                    {
                        case TOKlt:     e->op = TOKgt;     break;
                        case TOKgt:     e->op = TOKlt;     break;
                        case TOKle:     e->op = TOKge;     break;
                        case TOKge:     e->op = TOKle;     break;
                        default:                           break;
                    }

                    return;
                }
            }
        #endif

            // Try alias this on first operand
            if (ad1 && ad1->aliasthis &&
                !(e->op == TOKassign && ad2 && ad1 == ad2))   // See Bugzilla 2943
            {
                /* Rewrite (e1 op e2) as:
                 *      (e1.aliasthis op e2)
                 */
                if (e->att1 && e->e1->type == e->att1)
                    return;
                //printf("att bin e1 = %s\n", this->e1->type->toChars());
                Expression *e1 = new DotIdExp(e->loc, e->e1, ad1->aliasthis->ident);
                BinExp *be = (BinExp *)e->copy();
                if (!be->att1 && e->e1->type->checkAliasThisRec())
                    be->att1 = e->e1->type;
                be->e1 = e1;
                result = trySemantic(be, sc);
                return;
            }

            // Try alias this on second operand
            /* Bugzilla 2943: make sure that when we're copying the struct, we don't
             * just copy the alias this member
             */
            if (ad2 && ad2->aliasthis &&
                !(e->op == TOKassign && ad1 && ad1 == ad2))
            {
                /* Rewrite (e1 op e2) as:
                 *      (e1 op e2.aliasthis)
                 */
                if (e->att2 && e->e2->type == e->att2)
                    return;
                //printf("att bin e2 = %s\n", e->e2->type->toChars());
                Expression *e2 = new DotIdExp(e->loc, e->e2, ad2->aliasthis->ident);
                BinExp *be = (BinExp *)e->copy();
                if (!be->att2 && e->e2->type->checkAliasThisRec())
                    be->att2 = e->e2->type;
                be->e2 = e2;
                result = trySemantic(be, sc);
                return;
            }
            return;
        }

        static bool needsDirectEq(Type *t1, Type *t2, Scope *sc)
        {
            Type *t1n = t1->nextOf()->toBasetype();
            Type *t2n = t2->nextOf()->toBasetype();
            if (((t1n->ty == Tchar || t1n->ty == Twchar || t1n->ty == Tdchar) &&
                 (t2n->ty == Tchar || t2n->ty == Twchar || t2n->ty == Tdchar)) ||
                (t1n->ty == Tvoid || t2n->ty == Tvoid))
            {
                return false;
            }
            if (t1n->constOf() != t2n->constOf())
                return true;

            Type *t = t1n;
            while (t->toBasetype()->nextOf())
                t = t->nextOf()->toBasetype();
            if (t->ty != Tstruct)
                return false;

            if (global.params.useTypeInfo && Type::dtypeinfo)
                semanticTypeInfo(sc, t);

            return ((TypeStruct *)t)->sym->hasIdentityEquals;
        }

        void visit(EqualExp *e)
        {
            //printf("EqualExp::op_overload() (%s)\n", e->toChars());

            Type *t1 = e->e1->type->toBasetype();
            Type *t2 = e->e2->type->toBasetype();

            /* Check for array equality.
             */
            if ((t1->ty == Tarray || t1->ty == Tsarray) &&
                (t2->ty == Tarray || t2->ty == Tsarray))
            {
                if (needsDirectEq(t1, t2, sc))
                {
                    /* Rewrite as:
                     *      __ArrayEq(e1, e2)
                     */
                    Expression *eeq = new IdentifierExp(e->loc, Id::__ArrayEq);
                    result = new CallExp(e->loc, eeq, e->e1, e->e2);
                    if (e->op == TOKnotequal)
                        result = new NotExp(e->loc, result);
                    result = trySemantic(result, sc); // for better error message
                    if (!result)
                    {
                        e->error("cannot compare %s and %s", t1->toChars(), t2->toChars());
                        result = new ErrorExp();
                    }
                    return;
                }
            }

            /* Check for class equality with null literal or typeof(null).
             */
            if ((t1->ty == Tclass && e->e2->op == TOKnull) ||
                (t2->ty == Tclass && e->e1->op == TOKnull))
            {
                e->error("use `%s` instead of `%s` when comparing with null",
                    Token::toChars(e->op == TOKequal ? TOKidentity : TOKnotidentity),
                    Token::toChars(e->op));
                result = new ErrorExp();
                return;
            }
            if ((t1->ty == Tclass && t2->ty == Tnull) ||
                (t1->ty == Tnull && t2->ty == Tclass))
            {
                // Comparing a class with typeof(null) should not call opEquals
                return;
            }

            /* Check for class equality.
             */
            if (t1->ty == Tclass && t2->ty == Tclass)
            {
                ClassDeclaration *cd1 = t1->isClassHandle();
                ClassDeclaration *cd2 = t2->isClassHandle();

                if (!(cd1->isCPPclass() || cd2->isCPPclass()))
                {
                    /* Rewrite as:
                     *      .object.opEquals(e1, e2)
                     */
                    Expression *e1x = e->e1;
                    Expression *e2x = e->e2;

                    /* The explicit cast is necessary for interfaces,
                     * see Bugzilla 4088.
                     */
                    Type *to = ClassDeclaration::object->getType();
                    if (cd1->isInterfaceDeclaration())
                        e1x = new CastExp(e->loc, e->e1, t1->isMutable() ? to : to->constOf());
                    if (cd2->isInterfaceDeclaration())
                        e2x = new CastExp(e->loc, e->e2, t2->isMutable() ? to : to->constOf());

                    result = new IdentifierExp(e->loc, Id::empty);
                    result = new DotIdExp(e->loc, result, Id::object);
                    result = new DotIdExp(e->loc, result, Id::eq);
                    result = new CallExp(e->loc, result, e1x, e2x);
                    if (e->op == TOKnotequal)
                        result = new NotExp(e->loc, result);
                    result = expressionSemantic(result, sc);
                    return;
                }
            }

            result = compare_overload(e, sc, Id::eq);
            if (result)
            {
                if (result->op == TOKcall && e->op == TOKnotequal)
                {
                    result = new NotExp(result->loc, result);
                    result = expressionSemantic(result, sc);
                }
                return;
            }

            /* Check for pointer equality.
             */
            if (t1->ty == Tpointer || t2->ty == Tpointer)
            {
                /* Rewrite:
                 *      ptr1 == ptr2
                 * as:
                 *      ptr1 is ptr2
                 *
                 * This is just a rewriting for deterministic AST representation
                 * as the backend input.
                 */
                TOK op2 = e->op == TOKequal ? TOKidentity : TOKnotidentity;
                result = new IdentityExp(op2, e->loc, e->e1, e->e2);
                result = expressionSemantic(result, sc);
                return;
            }

            /* Check for struct equality without opEquals.
             */
            if (t1->ty == Tstruct && t2->ty == Tstruct)
            {
                StructDeclaration *sd = ((TypeStruct *)t1)->sym;
                if (sd != ((TypeStruct *)t2)->sym)
                    return;

                if (!needOpEquals(sd))
                {
                    // Use bitwise equality.
                    TOK op2 = e->op == TOKequal ? TOKidentity : TOKnotidentity;
                    result = new IdentityExp(op2, e->loc, e->e1, e->e2);
                    result = expressionSemantic(result, sc);
                    return;
                }

                /* Do memberwise equality.
                 * Rewrite:
                 *      e1 == e2
                 * as:
                 *      e1.tupleof == e2.tupleof
                 *
                 * If sd is a nested struct, and if it's nested in a class, it will
                 * also compare the parent class's equality. Otherwise, compares
                 * the identity of parent context through void*.
                 */
                if (e->att1 && t1 == e->att1)
                    return;
                if (e->att2 && t2 == e->att2)
                    return;

                e = (EqualExp *)e->copy();
                if (!e->att1)
                    e->att1 = t1;
                if (!e->att2)
                    e->att2 = t2;
                e->e1 = new DotIdExp(e->loc, e->e1, Id::_tupleof);
                e->e2 = new DotIdExp(e->loc, e->e2, Id::_tupleof);
                result = expressionSemantic(e, sc);

                /* Bugzilla 15292, if the rewrite result is same with the original,
                 * the equality is unresolvable because it has recursive definition.
                 */
                if (result->op == e->op &&
                    ((EqualExp *)result)->e1->type->toBasetype() == t1)
                {
                    e->error("cannot compare %s because its auto generated member-wise equality has recursive definition",
                        t1->toChars());
                    result = new ErrorExp();
                }
                return;
            }

            /* Check for tuple equality.
             */
            if (e->e1->op == TOKtuple && e->e2->op == TOKtuple)
            {
                TupleExp *tup1 = (TupleExp *)e->e1;
                TupleExp *tup2 = (TupleExp *)e->e2;
                size_t dim = tup1->exps->length;
                if (dim != tup2->exps->length)
                {
                    e->error("mismatched tuple lengths, %d and %d",
                        (int)dim, (int)tup2->exps->length);
                    result = new ErrorExp();
                    return;
                }

                if (dim == 0)
                {
                    // zero-length tuple comparison should always return true or false.
                    result = new IntegerExp(e->loc, (e->op == TOKequal), Type::tbool);
                }
                else
                {
                    for (size_t i = 0; i < dim; i++)
                    {
                        Expression *ex1 = (*tup1->exps)[i];
                        Expression *ex2 = (*tup2->exps)[i];
                        EqualExp *eeq = new EqualExp(e->op, e->loc, ex1, ex2);
                        eeq->att1 = e->att1;
                        eeq->att2 = e->att2;

                        if (!result)
                            result = eeq;
                        else if (e->op == TOKequal)
                            result = new LogicalExp(e->loc, TOKandand, result, eeq);
                        else
                            result = new LogicalExp(e->loc, TOKoror, result, eeq);
                    }
                    assert(result);
                }
                result = Expression::combine(Expression::combine(tup1->e0, tup2->e0), result);
                result = expressionSemantic(result, sc);
                return;
            }
        }

        void visit(CmpExp *e)
        {
            //printf("CmpExp::op_overload() (%s)\n", e->toChars());

            result = compare_overload(e, sc, Id::cmp);
        }

        /*********************************
         * Operator overloading for op=
         */
        void visit(BinAssignExp *e)
        {
            //printf("BinAssignExp::op_overload() (%s)\n", e->toChars());

            if (e->e1->op == TOKarray)
            {
                ArrayExp *ae = (ArrayExp *)e->e1;
                ae->e1 = expressionSemantic(ae->e1, sc);
                ae->e1 = resolveProperties(sc, ae->e1);
                Expression *ae1old = ae->e1;

                const bool maybeSlice =
                    (ae->arguments->length == 0 ||
                     (ae->arguments->length == 1 && (*ae->arguments)[0]->op == TOKinterval));
                IntervalExp *ie = NULL;
                if (maybeSlice && ae->arguments->length)
                {
                    assert((*ae->arguments)[0]->op == TOKinterval);
                    ie = (IntervalExp *)(*ae->arguments)[0];
                }

                while (true)
                {
                    if (ae->e1->op == TOKerror)
                    {
                        result = ae->e1;
                        return;
                    }
                    Expression *e0 = NULL;
                    Expression *ae1save = ae->e1;
                    ae->lengthVar = NULL;

                    Type *t1b = ae->e1->type->toBasetype();
                    AggregateDeclaration *ad = isAggregate(t1b);
                    if (!ad)
                        break;
                    if (search_function(ad, Id::opIndexOpAssign))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, &e0);
                        if (!result)    // (a[i..j] op= e2) might be: a.opSliceOpAssign!(op)(e2, i, j)
                            goto Lfallback;
                        if (result->op == TOKerror)
                            return;

                        result = expressionSemantic(e->e2, sc);
                        if (result->op == TOKerror)
                            return;
                        e->e2 = result;

                        /* Rewrite a[arguments] op= e2 as:
                         *      a.opIndexOpAssign!(op)(e2, arguments)
                         */
                        Expressions *a = (Expressions *)ae->arguments->copy();
                        a->insert(0, e->e2);
                        Objects *tiargs = opToArg(sc, e->op);
                        result = new DotTemplateInstanceExp(e->loc, ae->e1, Id::opIndexOpAssign, tiargs);
                        result = new CallExp(e->loc, result, a);
                        if (maybeSlice) // (a[] op= e2) might be: a.opSliceOpAssign!(op)(e2)
                            result = trySemantic(result, sc);
                        else
                            result = expressionSemantic(result, sc);
                        if (result)
                        {
                            result = Expression::combine(e0, result);
                            return;
                        }
                    }
                Lfallback:
                    if (maybeSlice && search_function(ad, Id::opSliceOpAssign))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, ie, &e0);
                        if (result->op == TOKerror)
                            return;

                        result = expressionSemantic(e->e2, sc);
                        if (result->op == TOKerror)
                            return;
                        e->e2 = result;

                        /* Rewrite (a[i..j] op= e2) as:
                         *      a.opSliceOpAssign!(op)(e2, i, j)
                         */
                        Expressions *a = new Expressions();
                        a->push(e->e2);
                        if (ie)
                        {
                            a->push(ie->lwr);
                            a->push(ie->upr);
                        }
                        Objects *tiargs = opToArg(sc, e->op);
                        result = new DotTemplateInstanceExp(e->loc, ae->e1, Id::opSliceOpAssign, tiargs);
                        result = new CallExp(e->loc, result, a);
                        result = expressionSemantic(result, sc);
                        result = Expression::combine(e0, result);
                        return;
                    }

                    // Didn't find it. Forward to aliasthis
                    if (ad->aliasthis && t1b != ae->att1)
                    {
                        if (!ae->att1 && t1b->checkAliasThisRec())
                            ae->att1 = t1b;

                        /* Rewrite (a[arguments] op= e2) as:
                         *      a.aliasthis[arguments] op= e2
                         */
                        ae->e1 = resolveAliasThis(sc, ae1save, true);
                        if (ae->e1)
                            continue;
                    }
                    break;
                }
                ae->e1 = ae1old;    // recovery
                ae->lengthVar = NULL;
            }

            result = binSemanticProp(e, sc);
            if (result)
                return;

            // Don't attempt 'alias this' if an error occured
            if (e->e1->type->ty == Terror || e->e2->type->ty == Terror)
            {
                result = new ErrorExp();
                return;
            }

            Identifier *id = opId(e);

            Expressions args2;

            AggregateDeclaration *ad1 = isAggregate(e->e1->type);

            Dsymbol *s = NULL;

        #if 1 // the old D1 scheme
            if (ad1 && id)
            {
                s = search_function(ad1, id);
            }
        #endif

            Objects *tiargs = NULL;
            if (!s)
            {
                /* Try the new D2 scheme, opOpAssign
                 */
                if (ad1)
                {
                    s = search_function(ad1, Id::opOpAssign);
                    if (s && !s->isTemplateDeclaration())
                    {
                        e->error("%s.opOpAssign isn't a template", e->e1->toChars());
                        result = new ErrorExp();
                        return;
                    }
                }

                // Set tiargs, the template argument list, which will be the operator string
                if (s)
                {
                    id = Id::opOpAssign;
                    tiargs = opToArg(sc, e->op);
                }
            }

            if (s)
            {
                /* Try:
                 *      a.opOpAssign(b)
                 */

                args2.setDim(1);
                args2[0] = e->e2;
                expandTuples(&args2);

                Match m;
                memset(&m, 0, sizeof(m));
                m.last = MATCHnomatch;

                if (s)
                {
                    functionResolve(&m, s, e->loc, sc, tiargs, e->e1->type, &args2);
                    if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                    {
                        result = new ErrorExp();
                        return;
                    }
                }

                if (m.count > 1)
                {
                    // Error, ambiguous
                    e->error("overloads %s and %s both match argument list for %s",
                            m.lastf->type->toChars(),
                            m.nextf->type->toChars(),
                            m.lastf->toChars());
                }
                else if (m.last <= MATCHnomatch)
                {
                    m.lastf = m.anyf;
                    if (tiargs)
                        goto L1;
                }

                // Rewrite (e1 op e2) as e1.opOpAssign(e2)
                result = build_overload(e->loc, sc, e->e1, e->e2, m.lastf ? m.lastf : s);
                return;
            }

        L1:

            // Try alias this on first operand
            if (ad1 && ad1->aliasthis)
            {
                /* Rewrite (e1 op e2) as:
                 *      (e1.aliasthis op e2)
                 */
                if (e->att1 && e->e1->type == e->att1)
                    return;
                //printf("att %s e1 = %s\n", Token::toChars(e->op), e->e1->type->toChars());
                Expression *e1 = new DotIdExp(e->loc, e->e1, ad1->aliasthis->ident);
                BinExp *be = (BinExp *)e->copy();
                if (!be->att1 && e->e1->type->checkAliasThisRec())
                    be->att1 = e->e1->type;
                be->e1 = e1;
                result = trySemantic(be, sc);
                return;
            }

            // Try alias this on second operand
            AggregateDeclaration *ad2 = isAggregate(e->e2->type);
            if (ad2 && ad2->aliasthis)
            {
                /* Rewrite (e1 op e2) as:
                 *      (e1 op e2.aliasthis)
                 */
                if (e->att2 && e->e2->type == e->att2)
                    return;
                //printf("att %s e2 = %s\n", Token::toChars(e->op), e->e2->type->toChars());
                Expression *e2 = new DotIdExp(e->loc, e->e2, ad2->aliasthis->ident);
                BinExp *be = (BinExp *)e->copy();
                if (!be->att2 && e->e2->type->checkAliasThisRec())
                    be->att2 = e->e2->type;
                be->e2 = e2;
                result = trySemantic(be, sc);
                return;
            }
        }
    };

    OpOverload v(sc);
    e->accept(&v);
    return v.result;
}

/******************************************
 * Common code for overloading of EqualExp and CmpExp
 */
Expression *compare_overload(BinExp *e, Scope *sc, Identifier *id)
{
    //printf("BinExp::compare_overload(id = %s) %s\n", id->toChars(), e->toChars());

    AggregateDeclaration *ad1 = isAggregate(e->e1->type);
    AggregateDeclaration *ad2 = isAggregate(e->e2->type);

    Dsymbol *s = NULL;
    Dsymbol *s_r = NULL;

    if (ad1)
    {
        s = search_function(ad1, id);
    }
    if (ad2)
    {
        s_r = search_function(ad2, id);
        if (s == s_r)
            s_r = NULL;
    }

    Objects *tiargs = NULL;

    if (s || s_r)
    {
        /* Try:
         *      a.opEquals(b)
         *      b.opEquals(a)
         * and see which is better.
         */

        Expressions args1;
        Expressions args2;

        args1.setDim(1);
        args1[0] = e->e1;
        expandTuples(&args1);
        args2.setDim(1);
        args2[0] = e->e2;
        expandTuples(&args2);

        Match m;
        memset(&m, 0, sizeof(m));
        m.last = MATCHnomatch;

        if (0 && s && s_r)
        {
            printf("s  : %s\n", s->toPrettyChars());
            printf("s_r: %s\n", s_r->toPrettyChars());
        }

        if (s)
        {
            functionResolve(&m, s, e->loc, sc, tiargs, e->e1->type, &args2);
            if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                return new ErrorExp();
        }

        FuncDeclaration *lastf = m.lastf;
        int count = m.count;

        if (s_r)
        {
            functionResolve(&m, s_r, e->loc, sc, tiargs, e->e2->type, &args1);
            if (m.lastf && (m.lastf->errors || m.lastf->semantic3Errors))
                return new ErrorExp();
        }

        if (m.count > 1)
        {
            /* The following if says "not ambiguous" if there's one match
             * from s and one from s_r, in which case we pick s.
             * This doesn't follow the spec, but is a workaround for the case
             * where opEquals was generated from templates and we cannot figure
             * out if both s and s_r came from the same declaration or not.
             * The test case is:
             *   import std.typecons;
             *   void main() {
             *    assert(tuple("has a", 2u) == tuple("has a", 1));
             *   }
             */
            if (!(m.lastf == lastf && m.count == 2 && count == 1))
            {
                // Error, ambiguous
                e->error("overloads %s and %s both match argument list for %s",
                    m.lastf->type->toChars(),
                    m.nextf->type->toChars(),
                    m.lastf->toChars());
            }
        }
        else if (m.last <= MATCHnomatch)
        {
            m.lastf = m.anyf;
        }

        Expression *result;
        if ((lastf && m.lastf == lastf) || (!s_r && m.last <= MATCHnomatch))
        {
            // Rewrite (e1 op e2) as e1.opfunc(e2)
            result = build_overload(e->loc, sc, e->e1, e->e2, m.lastf ? m.lastf : s);
        }
        else
        {
            // Rewrite (e1 op e2) as e2.opfunc_r(e1)
            result = build_overload(e->loc, sc, e->e2, e->e1, m.lastf ? m.lastf : s_r);

            // When reversing operands of comparison operators,
            // need to reverse the sense of the op
            switch (e->op)
            {
                case TOKlt:     e->op = TOKgt;     break;
                case TOKgt:     e->op = TOKlt;     break;
                case TOKle:     e->op = TOKge;     break;
                case TOKge:     e->op = TOKle;     break;

                // The rest are symmetric
                default:
                    break;
            }
        }

        return result;
    }

    // Try alias this on first operand
    if (ad1 && ad1->aliasthis)
    {
        /* Rewrite (e1 op e2) as:
         *      (e1.aliasthis op e2)
         */
        if (e->att1 && e->e1->type == e->att1)
            return NULL;
        //printf("att cmp_bin e1 = %s\n", e->e1->type->toChars());
        Expression *e1 = new DotIdExp(e->loc, e->e1, ad1->aliasthis->ident);
        BinExp *be = (BinExp *)e->copy();
        if (!be->att1 && e->e1->type->checkAliasThisRec())
            be->att1 = e->e1->type;
        be->e1 = e1;
        return trySemantic(be, sc);
    }

    // Try alias this on second operand
    if (ad2 && ad2->aliasthis)
    {
        /* Rewrite (e1 op e2) as:
         *      (e1 op e2.aliasthis)
         */
        if (e->att2 && e->e2->type == e->att2)
            return NULL;
        //printf("att cmp_bin e2 = %s\n", e->e2->type->toChars());
        Expression *e2 = new DotIdExp(e->loc, e->e2, ad2->aliasthis->ident);
        BinExp *be = (BinExp *)e->copy();
        if (!be->att2 && e->e2->type->checkAliasThisRec())
            be->att2 = e->e2->type;
        be->e2 = e2;
        return trySemantic(be, sc);
    }

    return NULL;
}

/***********************************
 * Utility to build a function call out of this reference and argument.
 */

Expression *build_overload(Loc loc, Scope *sc, Expression *ethis, Expression *earg,
        Dsymbol *d)
{
    assert(d);
    Expression *e;

    //printf("build_overload(id = '%s')\n", id->toChars());
    //earg->print();
    //earg->type->print();
    Declaration *decl = d->isDeclaration();
    if (decl)
        e = new DotVarExp(loc, ethis, decl, false);
    else
        e = new DotIdExp(loc, ethis, d->ident);
    e = new CallExp(loc, e, earg);

    e = expressionSemantic(e, sc);
    return e;
}

/***************************************
 * Search for function funcid in aggregate ad.
 */

Dsymbol *search_function(ScopeDsymbol *ad, Identifier *funcid)
{
    Dsymbol *s = ad->search(Loc(), funcid);
    if (s)
    {
        //printf("search_function: s = '%s'\n", s->kind());
        Dsymbol *s2 = s->toAlias();
        //printf("search_function: s2 = '%s'\n", s2->kind());
        FuncDeclaration *fd = s2->isFuncDeclaration();
        if (fd && fd->type->ty == Tfunction)
            return fd;

        TemplateDeclaration *td = s2->isTemplateDeclaration();
        if (td)
            return td;
    }
    return NULL;
}


bool inferAggregate(ForeachStatement *fes, Scope *sc, Dsymbol *&sapply)
{
    //printf("inferAggregate(%s)\n", fes->aggr->toChars());
    Identifier *idapply = (fes->op == TOKforeach) ? Id::apply : Id::applyReverse;
    Identifier *idfront = (fes->op == TOKforeach) ? Id::Ffront : Id::Fback;
    int sliced = 0;
    Type *tab;
    Type *att = NULL;
    Expression *aggr = fes->aggr;
    AggregateDeclaration *ad;

    while (1)
    {
        aggr = expressionSemantic(aggr, sc);
        aggr = resolveProperties(sc, aggr);
        aggr = aggr->optimize(WANTvalue);
        if (!aggr->type || aggr->op == TOKerror)
            goto Lerr;

        tab = aggr->type->toBasetype();
        switch (tab->ty)
        {
            case Tarray:
            case Tsarray:
            case Ttuple:
            case Taarray:
                break;

            case Tclass:
                ad = ((TypeClass *)tab)->sym;
                goto Laggr;

            case Tstruct:
                ad = ((TypeStruct *)tab)->sym;
                goto Laggr;

            Laggr:
                if (!sliced)
                {
                    sapply = search_function(ad, idapply);
                    if (sapply)
                    {
                        // opApply aggregate
                        break;
                    }

                    if (fes->aggr->op != TOKtype)
                    {
                        Expression *rinit = new ArrayExp(fes->aggr->loc, fes->aggr);
                        rinit = trySemantic(rinit, sc);
                        if (rinit)                  // if application of [] succeeded
                        {
                            aggr = rinit;
                            sliced = 1;
                            continue;
                        }
                    }
                }

                if (ad->search(Loc(), idfront))
                {
                    // range aggregate
                    break;
                }

                if (ad->aliasthis)
                {
                    if (att == tab)
                        goto Lerr;
                    if (!att && tab->checkAliasThisRec())
                        att = tab;
                    aggr = resolveAliasThis(sc, aggr);
                    continue;
                }
                goto Lerr;

            case Tdelegate:
                if (aggr->op == TOKdelegate)
                {
                    sapply = ((DelegateExp *)aggr)->func;
                }
                break;

            case Terror:
                break;

            default:
                goto Lerr;
        }
        break;
    }
    fes->aggr = aggr;
    return true;

Lerr:
    return false;
}

/*****************************************
 * Given array of parameters and an aggregate type,
 * if any of the parameter types are missing, attempt to infer
 * them from the aggregate type.
 */

bool inferApplyArgTypes(ForeachStatement *fes, Scope *sc, Dsymbol *&sapply)
{
    if (!fes->parameters || !fes->parameters->length)
        return false;

    if (sapply)     // prefer opApply
    {
        for (size_t u = 0; u < fes->parameters->length; u++)
        {
            Parameter *p = (*fes->parameters)[u];
            if (p->type)
            {
                p->type = typeSemantic(p->type, fes->loc, sc);
                p->type = p->type->addStorageClass(p->storageClass);
            }
        }

        Expression *ethis;
        Type *tab = fes->aggr->type->toBasetype();
        if (tab->ty == Tclass || tab->ty == Tstruct)
            ethis = fes->aggr;
        else
        {   assert(tab->ty == Tdelegate && fes->aggr->op == TOKdelegate);
            ethis = ((DelegateExp *)fes->aggr)->e1;
        }

        /* Look for like an
         *  int opApply(int delegate(ref Type [, ...]) dg);
         * overload
         */
        FuncDeclaration *fd = sapply->isFuncDeclaration();
        if (fd)
        {
            sapply = inferApplyArgTypesX(ethis, fd, fes->parameters);
        }
        return sapply != NULL;
    }

    /* Return if no parameters need types.
     */
    for (size_t u = 0; u < fes->parameters->length; u++)
    {
        Parameter *p = (*fes->parameters)[u];
        if (!p->type)
            break;
    }

    AggregateDeclaration *ad;

    Parameter *p = (*fes->parameters)[0];
    Type *taggr = fes->aggr->type;
    assert(taggr);
    Type *tab = taggr->toBasetype();
    switch (tab->ty)
    {
        case Tarray:
        case Tsarray:
        case Ttuple:
            if (fes->parameters->length == 2)
            {
                if (!p->type)
                {
                    p->type = Type::tsize_t;    // key type
                    p->type = p->type->addStorageClass(p->storageClass);
                }
                p = (*fes->parameters)[1];
            }
            if (!p->type && tab->ty != Ttuple)
            {
                p->type = tab->nextOf();        // value type
                p->type = p->type->addStorageClass(p->storageClass);
            }
            break;

        case Taarray:
        {
            TypeAArray *taa = (TypeAArray *)tab;

            if (fes->parameters->length == 2)
            {
                if (!p->type)
                {
                    p->type = taa->index;       // key type
                    p->type = p->type->addStorageClass(p->storageClass);
                    if (p->storageClass & STCref) // key must not be mutated via ref
                        p->type = p->type->addMod(MODconst);
                }
                p = (*fes->parameters)[1];
            }
            if (!p->type)
            {
                p->type = taa->next;            // value type
                p->type = p->type->addStorageClass(p->storageClass);
            }
            break;
        }

        case Tclass:
            ad = ((TypeClass *)tab)->sym;
            goto Laggr;

        case Tstruct:
            ad = ((TypeStruct *)tab)->sym;
            goto Laggr;

        Laggr:
            if (fes->parameters->length == 1)
            {
                if (!p->type)
                {
                    /* Look for a front() or back() overload
                     */
                    Identifier *id = (fes->op == TOKforeach) ? Id::Ffront : Id::Fback;
                    Dsymbol *s = ad->search(Loc(), id);
                    FuncDeclaration *fd = s ? s->isFuncDeclaration() : NULL;
                    if (fd)
                    {
                        // Resolve inout qualifier of front type
                        p->type = fd->type->nextOf();
                        if (p->type)
                        {
                            p->type = p->type->substWildTo(tab->mod);
                            p->type = p->type->addStorageClass(p->storageClass);
                        }
                    }
                    else if (s && s->isTemplateDeclaration())
                        ;
                    else if (s && s->isDeclaration())
                        p->type = ((Declaration *)s)->type;
                    else
                        break;
                }
                break;
            }
            break;

        case Tdelegate:
        {
            if (!inferApplyArgTypesY((TypeFunction *)tab->nextOf(), fes->parameters))
                return false;
            break;
        }

        default:
            break;              // ignore error, caught later
    }
    return true;
}

static Dsymbol *inferApplyArgTypesX(Expression *ethis, FuncDeclaration *fstart, Parameters *parameters)
{
  struct ParamOpOver
  {
    Parameters *parameters;
    MOD mod;
    MATCH match;
    FuncDeclaration *fd_best;
    FuncDeclaration *fd_ambig;

    static int fp(void *param, Dsymbol *s)
    {
        FuncDeclaration *f = s->isFuncDeclaration();
        if (!f)
            return 0;
        ParamOpOver *p = (ParamOpOver *)param;
        TypeFunction *tf = (TypeFunction *)f->type;
        MATCH m = MATCHexact;

        if (f->isThis())
        {
            if (!MODimplicitConv(p->mod, tf->mod))
                m = MATCHnomatch;
            else if (p->mod != tf->mod)
                m = MATCHconst;
        }
        if (!inferApplyArgTypesY(tf, p->parameters, 1))
            m = MATCHnomatch;

        if (m > p->match)
        {
            p->fd_best = f;
            p->fd_ambig = NULL;
            p->match = m;
        }
        else if (m == p->match)
            p->fd_ambig = f;
        return 0;
    }
  };
    ParamOpOver p;
    p.parameters = parameters;
    p.mod = ethis->type->mod;
    p.match = MATCHnomatch;
    p.fd_best = NULL;
    p.fd_ambig = NULL;
    overloadApply(fstart, &p, &ParamOpOver::fp);
    if (p.fd_best)
    {
        inferApplyArgTypesY((TypeFunction *)p.fd_best->type, parameters);
        if (p.fd_ambig)
        {   ::error(ethis->loc, "%s.%s matches more than one declaration:\n%s:     %s\nand:\n%s:     %s",
                    ethis->toChars(), fstart->ident->toChars(),
                    p.fd_best ->loc.toChars(), p.fd_best ->type->toChars(),
                    p.fd_ambig->loc.toChars(), p.fd_ambig->type->toChars());
            p.fd_best = NULL;
        }
    }
    return p.fd_best;
}

/******************************
 * Infer parameters from type of function.
 * Returns:
 *      1 match for this function
 *      0 no match for this function
 */

static int inferApplyArgTypesY(TypeFunction *tf, Parameters *parameters, int flags)
{   size_t nparams;
    Parameter *p;

    if (tf->parameterList.length() != 1)
        goto Lnomatch;
    p = tf->parameterList[0];
    if (p->type->ty != Tdelegate)
        goto Lnomatch;
    tf = (TypeFunction *)p->type->nextOf();
    assert(tf->ty == Tfunction);

    /* We now have tf, the type of the delegate. Match it against
     * the parameters, filling in missing parameter types.
     */
    nparams = tf->parameterList.length();
    if (nparams == 0 || tf->parameterList.varargs != VARARGnone)
        goto Lnomatch;          // not enough parameters
    if (parameters->length != nparams)
        goto Lnomatch;          // not enough parameters

    for (size_t u = 0; u < nparams; u++)
    {
        p = (*parameters)[u];
        Parameter *param = tf->parameterList[u];
        if (p->type)
        {
            if (!p->type->equals(param->type))
                goto Lnomatch;
        }
        else if (!flags)
        {
            p->type = param->type;
            p->type = p->type->addStorageClass(p->storageClass);
        }
    }
    return 1;

Lnomatch:
    return 0;
}

