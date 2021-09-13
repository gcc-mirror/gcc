
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "root/checkedint.h"
#include "mars.h"
#include "init.h"
#include "expression.h"
#include "statement.h"
#include "declaration.h"
#include "aggregate.h"
#include "scope.h"
#include "mtype.h"
#include "template.h"
#include "id.h"

FuncDeclaration *isFuncAddress(Expression *e, bool *hasOverloads = NULL);
Initializer *inferType(Initializer *init, Scope *sc);
bool hasNonConstPointers(Expression *e);

class InitializerSemanticVisitor : public Visitor
{
public:
    Initializer *result;
    Scope *sc;
    Type *t;
    NeedInterpret needInterpret;

    InitializerSemanticVisitor(Scope *sc, Type *t, NeedInterpret needInterpret)
    {
        this->result = NULL;
        this->sc = sc;
        this->t = t;
        this->needInterpret = needInterpret;
    }

    void visit(ErrorInitializer *i)
    {
        //printf("ErrorInitializer::semantic(t = %p)\n", t);
        result = i;
    }

    void visit(VoidInitializer *i)
    {
        //printf("VoidInitializer::semantic(t = %p)\n", t);
        i->type = t;
        result = i;
    }

    void visit(StructInitializer *i)
    {
        //printf("StructInitializer::semantic(t = %s) %s\n", t->toChars(), toChars());
        t = t->toBasetype();
        if (t->ty == Tsarray && t->nextOf()->toBasetype()->ty == Tstruct)
            t = t->nextOf()->toBasetype();
        if (t->ty == Tstruct)
        {
            StructDeclaration *sd = ((TypeStruct *)t)->sym;
            if (sd->ctor)
            {
                error(i->loc, "%s %s has constructors, cannot use { initializers }, use %s( initializers ) instead",
                      sd->kind(), sd->toChars(), sd->toChars());
                result = new ErrorInitializer();
                return;
            }
            sd->size(i->loc);
            if (sd->sizeok != SIZEOKdone)
            {
                result = new ErrorInitializer();
                return;
            }
            size_t nfields = sd->fields.length - sd->isNested();

            //expandTuples for non-identity arguments?

            Expressions *elements = new Expressions();
            elements->setDim(nfields);
            for (size_t j = 0; j < elements->length; j++)
                (*elements)[j] = NULL;

            // Run semantic for explicitly given initializers
            // TODO: this part is slightly different from StructLiteralExp::semantic.
            bool errors = false;
            for (size_t fieldi = 0, j = 0; j < i->field.length; j++)
            {
                if (Identifier *id = i->field[j])
                {
                    Dsymbol *s = sd->search(i->loc, id);
                    if (!s)
                    {
                        s = sd->search_correct(id);
                        if (s)
                            error(i->loc, "`%s` is not a member of `%s`, did you mean %s `%s`?",
                                  id->toChars(), sd->toChars(), s->kind(), s->toChars());
                        else
                            error(i->loc, "`%s` is not a member of `%s`", id->toChars(), sd->toChars());
                        result = new ErrorInitializer();
                        return;
                    }
                    s = s->toAlias();

                    // Find out which field index it is
                    for (fieldi = 0; 1; fieldi++)
                    {
                        if (fieldi >= nfields)
                        {
                            error(i->loc, "%s.%s is not a per-instance initializable field",
                                  sd->toChars(), s->toChars());
                            result = new ErrorInitializer();
                            return;
                        }
                        if (s == sd->fields[fieldi])
                            break;
                    }
                }
                else if (fieldi >= nfields)
                {
                    error(i->loc, "too many initializers for %s", sd->toChars());
                    result = new ErrorInitializer();
                    return;
                }

                VarDeclaration *vd = sd->fields[fieldi];
                if ((*elements)[fieldi])
                {
                    error(i->loc, "duplicate initializer for field `%s`", vd->toChars());
                    errors = true;
                    continue;
                }
                for (size_t k = 0; k < nfields; k++)
                {
                    VarDeclaration *v2 = sd->fields[k];
                    if (vd->isOverlappedWith(v2) && (*elements)[k])
                    {
                        error(i->loc, "overlapping initialization for field %s and %s",
                              v2->toChars(), vd->toChars());
                        errors = true;
                        continue;
                    }
                }

                assert(sc);
                Initializer *iz = i->value[j];
                iz = initializerSemantic(iz, sc, vd->type->addMod(t->mod), needInterpret);
                Expression *ex = initializerToExpression(iz);
                if (ex->op == TOKerror)
                {
                    errors = true;
                    continue;
                }
                i->value[j] = iz;
                (*elements)[fieldi] = doCopyOrMove(sc, ex);
                ++fieldi;
            }
            if (errors)
            {
                result = new ErrorInitializer();
                return;
            }

            StructLiteralExp *sle = new StructLiteralExp(i->loc, sd, elements, t);
            if (!sd->fill(i->loc, elements, false))
            {
                result = new ErrorInitializer();
                return;
            }
            sle->type = t;

            ExpInitializer *ie = new ExpInitializer(i->loc, sle);
            result = initializerSemantic(ie, sc, t, needInterpret);
            return;
        }
        else if ((t->ty == Tdelegate || (t->ty == Tpointer && t->nextOf()->ty == Tfunction)) && i->value.length == 0)
        {
            TOK tok = (t->ty == Tdelegate) ? TOKdelegate : TOKfunction;
            /* Rewrite as empty delegate literal { }
            */
            Type *tf = new TypeFunction(ParameterList(), NULL, LINKd);
            FuncLiteralDeclaration *fd = new FuncLiteralDeclaration(i->loc, Loc(), tf, tok, NULL);
            fd->fbody = new CompoundStatement(i->loc, new Statements());
            fd->endloc = i->loc;
            Expression *e = new FuncExp(i->loc, fd);
            ExpInitializer *ie = new ExpInitializer(i->loc, e);
            result = initializerSemantic(ie, sc, t, needInterpret);
            return;
        }

        error(i->loc, "a struct is not a valid initializer for a %s", t->toChars());
        result = new ErrorInitializer();
    }

    void visit(ArrayInitializer *i)
    {
        unsigned length;
        const unsigned amax = 0x80000000;
        bool errors = false;

        //printf("ArrayInitializer::semantic(%s)\n", t->toChars());
        if (i->sem)                            // if semantic() already run
        {
            result = i;
            return;
        }
        i->sem = true;
        t = t->toBasetype();
        switch (t->ty)
        {
            case Tsarray:
            case Tarray:
                break;

            case Tvector:
                t = ((TypeVector *)t)->basetype;
                break;

            case Taarray:
            case Tstruct:   // consider implicit constructor call
                {
                    Expression *e;
                    // note: MyStruct foo = [1:2, 3:4] is correct code if MyStruct has a this(int[int])
                    if (t->ty == Taarray || i->isAssociativeArray())
                        e = i->toAssocArrayLiteral();
                    else
                        e = initializerToExpression(i);
                    if (!e) // Bugzilla 13987
                    {
                        error(i->loc, "cannot use array to initialize %s", t->toChars());
                        goto Lerr;
                    }
                    ExpInitializer *ei = new ExpInitializer(e->loc, e);
                    result = initializerSemantic(ei, sc, t, needInterpret);
                    return;
                }
            case Tpointer:
                if (t->nextOf()->ty != Tfunction)
                    break;
                /* fall through */

            default:
                error(i->loc, "cannot use array to initialize %s", t->toChars());
                goto Lerr;
        }

        i->type = t;

        length = 0;
        for (size_t j = 0; j < i->index.length; j++)
        {
            Expression *idx = i->index[j];
            if (idx)
            {
                sc = sc->startCTFE();
                idx = expressionSemantic(idx, sc);
                sc = sc->endCTFE();
                idx = idx->ctfeInterpret();
                i->index[j] = idx;
                const uinteger_t idxvalue = idx->toInteger();
                if (idxvalue >= amax)
                {
                    error(i->loc, "array index %llu overflow", (ulonglong)idxvalue);
                    errors = true;
                }
                length = (unsigned)idx->toInteger();
                if (idx->op == TOKerror)
                    errors = true;
            }

            Initializer *val = i->value[j];
            ExpInitializer *ei = val->isExpInitializer();
            if (ei && !idx)
                ei->expandTuples = true;
            val = initializerSemantic(val, sc, t->nextOf(), needInterpret);
            if (val->isErrorInitializer())
                errors = true;

            ei = val->isExpInitializer();
            // found a tuple, expand it
            if (ei && ei->exp->op == TOKtuple)
            {
                TupleExp *te = (TupleExp *)ei->exp;
                i->index.remove(j);
                i->value.remove(j);

                for (size_t k = 0; k < te->exps->length; ++k)
                {
                    Expression *e = (*te->exps)[k];
                    i->index.insert(j + k, (Expression *)NULL);
                    i->value.insert(j + k, new ExpInitializer(e->loc, e));
                }
                j--;
                continue;
            }
            else
            {
                i->value[j] = val;
            }

            length++;
            if (length == 0)
            {
                error(i->loc, "array dimension overflow");
                goto Lerr;
            }
            if (length > i->dim)
                i->dim = length;
        }
        if (t->ty == Tsarray)
        {
            uinteger_t edim = ((TypeSArray *)t)->dim->toInteger();
            if (i->dim > edim)
            {
                error(i->loc, "array initializer has %u elements, but array length is %llu", i->dim, (ulonglong)edim);
                goto Lerr;
            }
        }
        if (errors)
            goto Lerr;
        else
        {
            d_uns64 sz = t->nextOf()->size();
            bool overflow = false;
            const d_uns64 max = mulu((d_uns64)i->dim, sz, overflow);
            if (overflow || max > amax)
            {
                error(i->loc, "array dimension %llu exceeds max of %llu", (ulonglong)i->dim, (ulonglong)(amax / sz));
                goto Lerr;
            }
            result = i;
            return;
        }

    Lerr:
        result = new ErrorInitializer();
    }

    void visit(ExpInitializer *i)
    {
        //printf("ExpInitializer::semantic(%s), type = %s\n", i->exp->toChars(), t->toChars());
        if (needInterpret) sc = sc->startCTFE();
        i->exp = expressionSemantic(i->exp, sc);
        i->exp = resolveProperties(sc, i->exp);
        if (needInterpret) sc = sc->endCTFE();
        if (i->exp->op == TOKerror)
        {
            result = new ErrorInitializer();
            return;
        }

        unsigned int olderrors = global.errors;
        if (needInterpret)
        {
            // If the result will be implicitly cast, move the cast into CTFE
            // to avoid premature truncation of polysemous types.
            // eg real [] x = [1.1, 2.2]; should use real precision.
            if (i->exp->implicitConvTo(t))
            {
                i->exp = i->exp->implicitCastTo(sc, t);
            }
            if (!global.gag && olderrors != global.errors)
            {
                result = i;
                return;
            }
            i->exp = i->exp->ctfeInterpret();
        }
        else
        {
            i->exp = i->exp->optimize(WANTvalue);
        }
        if (!global.gag && olderrors != global.errors)
        {
            result = i; // Failed, suppress duplicate error messages
            return;
        }

        if (i->exp->type->ty == Ttuple && ((TypeTuple *)i->exp->type)->arguments->length == 0)
        {
            Type *et = i->exp->type;
            i->exp = new TupleExp(i->exp->loc, new Expressions());
            i->exp->type = et;
        }
        if (i->exp->op == TOKtype)
        {
            i->exp->error("initializer must be an expression, not `%s`", i->exp->toChars());
            result = new ErrorInitializer();
            return;
        }

        // Make sure all pointers are constants
        if (needInterpret && hasNonConstPointers(i->exp))
        {
            i->exp->error("cannot use non-constant CTFE pointer in an initializer `%s`", i->exp->toChars());
            result = new ErrorInitializer();
            return;
        }

        Type *tb = t->toBasetype();
        Type *ti = i->exp->type->toBasetype();

        if (i->exp->op == TOKtuple && i->expandTuples && !i->exp->implicitConvTo(t))
        {
            result = new ExpInitializer(i->loc, i->exp);
            return;
        }

        /* Look for case of initializing a static array with a too-short
         * string literal, such as:
         *  char[5] foo = "abc";
         * Allow this by doing an explicit cast, which will lengthen the string
         * literal.
         */
        if (i->exp->op == TOKstring && tb->ty == Tsarray)
        {
            StringExp *se = (StringExp *)i->exp;
            Type *typeb = se->type->toBasetype();
            TY tynto = tb->nextOf()->ty;
            if (!se->committed &&
                (typeb->ty == Tarray || typeb->ty == Tsarray) &&
                (tynto == Tchar || tynto == Twchar || tynto == Tdchar) &&
                se->numberOfCodeUnits(tynto) < ((TypeSArray *)tb)->dim->toInteger())
            {
                i->exp = se->castTo(sc, t);
                goto L1;
            }
        }

        // Look for implicit constructor call
        if (tb->ty == Tstruct &&
            !(ti->ty == Tstruct && tb->toDsymbol(sc) == ti->toDsymbol(sc)) &&
            !i->exp->implicitConvTo(t))
        {
            StructDeclaration *sd = ((TypeStruct *)tb)->sym;
            if (sd->ctor)
            {
                // Rewrite as S().ctor(exp)
                Expression *e;
                e = new StructLiteralExp(i->loc, sd, NULL);
                e = new DotIdExp(i->loc, e, Id::ctor);
                e = new CallExp(i->loc, e, i->exp);
                e = expressionSemantic(e, sc);
                if (needInterpret)
                    i->exp = e->ctfeInterpret();
                else
                    i->exp = e->optimize(WANTvalue);
            }
        }

        // Look for the case of statically initializing an array
        // with a single member.
        if (tb->ty == Tsarray &&
            !tb->nextOf()->equals(ti->toBasetype()->nextOf()) &&
            i->exp->implicitConvTo(tb->nextOf())
           )
        {
            /* If the variable is not actually used in compile time, array creation is
             * redundant. So delay it until invocation of toExpression() or toDt().
             */
            t = tb->nextOf();
        }

        if (i->exp->implicitConvTo(t))
        {
            i->exp = i->exp->implicitCastTo(sc, t);
        }
        else
        {
            // Look for mismatch of compile-time known length to emit
            // better diagnostic message, as same as AssignExp::semantic.
            if (tb->ty == Tsarray &&
                i->exp->implicitConvTo(tb->nextOf()->arrayOf()) > MATCHnomatch)
            {
                uinteger_t dim1 = ((TypeSArray *)tb)->dim->toInteger();
                uinteger_t dim2 = dim1;
                if (i->exp->op == TOKarrayliteral)
                {
                    ArrayLiteralExp *ale = (ArrayLiteralExp *)i->exp;
                    dim2 = ale->elements ? ale->elements->length : 0;
                }
                else if (i->exp->op == TOKslice)
                {
                    Type *tx = toStaticArrayType((SliceExp *)i->exp);
                    if (tx)
                        dim2 = ((TypeSArray *)tx)->dim->toInteger();
                }
                if (dim1 != dim2)
                {
                    i->exp->error("mismatched array lengths, %d and %d", (int)dim1, (int)dim2);
                    i->exp = new ErrorExp();
                }
            }
            i->exp = i->exp->implicitCastTo(sc, t);
        }
    L1:
        if (i->exp->op == TOKerror)
        {
            result = i;
            return;
        }
        if (needInterpret)
            i->exp = i->exp->ctfeInterpret();
        else
            i->exp = i->exp->optimize(WANTvalue);
        //printf("-ExpInitializer::semantic(): "); i->exp->print();
        result = i;
    }
};

// Performs semantic analisys on Initializer AST nodes
Initializer *initializerSemantic(Initializer *init, Scope *sc, Type *t, NeedInterpret needInterpret)
{
    InitializerSemanticVisitor v = InitializerSemanticVisitor(sc, t, needInterpret);
    init->accept(&v);
    return v.result;
}

class InferTypeVisitor : public Visitor
{
public:
    Initializer *result;
    Scope *sc;

    InferTypeVisitor(Scope *sc)
    {
        this->result = NULL;
        this->sc = sc;
    }

    void visit(ErrorInitializer *i)
    {
        result = i;
    }

    void visit(VoidInitializer *i)
    {
        error(i->loc, "cannot infer type from void initializer");
        result = new ErrorInitializer();
    }

    void visit(StructInitializer *i)
    {
        error(i->loc, "cannot infer type from struct initializer");
        result = new ErrorInitializer();
    }

    void visit(ArrayInitializer *init)
    {
        //printf("ArrayInitializer::inferType() %s\n", init->toChars());
        Expressions *keys = NULL;
        Expressions *values;
        if (init->isAssociativeArray())
        {
            keys = new Expressions();
            keys->setDim(init->value.length);
            values = new Expressions();
            values->setDim(init->value.length);

            for (size_t i = 0; i < init->value.length; i++)
            {
                Expression *e = init->index[i];
                if (!e)
                    goto Lno;
                (*keys)[i] = e;

                Initializer *iz = init->value[i];
                if (!iz)
                    goto Lno;
                iz = inferType(iz, sc);
                if (iz->isErrorInitializer())
                {
                    result = iz;
                    return;
                }
                assert(iz->isExpInitializer());
                (*values)[i] = ((ExpInitializer *)iz)->exp;
                assert((*values)[i]->op != TOKerror);
            }

            Expression *e = new AssocArrayLiteralExp(init->loc, keys, values);
            ExpInitializer *ei = new ExpInitializer(init->loc, e);
            result = inferType(ei, sc);
            return;
        }
        else
        {
            Expressions *elements = new Expressions();
            elements->setDim(init->value.length);
            elements->zero();

            for (size_t i = 0; i < init->value.length; i++)
            {
                assert(!init->index[i]);  // already asserted by isAssociativeArray()

                Initializer *iz = init->value[i];
                if (!iz)
                    goto Lno;
                iz = inferType(iz, sc);
                if (iz->isErrorInitializer())
                {
                    result = iz;
                    return;
                }
                assert(iz->isExpInitializer());
                (*elements)[i] = ((ExpInitializer *)iz)->exp;
                assert((*elements)[i]->op != TOKerror);
            }

            Expression *e = new ArrayLiteralExp(init->loc, NULL, elements);
            ExpInitializer *ei = new ExpInitializer(init->loc, e);
            result = inferType(ei, sc);
            return;
        }
    Lno:
        if (keys)
        {
            delete keys;
            delete values;
            error(init->loc, "not an associative array initializer");
        }
        else
        {
            error(init->loc, "cannot infer type from array initializer");
        }
        result = new ErrorInitializer();
    }

    void visit(ExpInitializer *init)
    {
        //printf("ExpInitializer::inferType() %s\n", init->toChars());
        init->exp = expressionSemantic(init->exp, sc);
        init->exp = resolveProperties(sc, init->exp);

        if (init->exp->op == TOKscope)
        {
            ScopeExp *se = (ScopeExp *)init->exp;
            TemplateInstance *ti = se->sds->isTemplateInstance();
            if (ti && ti->semanticRun == PASSsemantic && !ti->aliasdecl)
                se->error("cannot infer type from %s %s, possible circular dependency", se->sds->kind(), se->toChars());
            else
                se->error("cannot infer type from %s %s", se->sds->kind(), se->toChars());
            result = new ErrorInitializer();
            return;
        }

        // Give error for overloaded function addresses
        bool hasOverloads = false;
        if (FuncDeclaration *f = isFuncAddress(init->exp, &hasOverloads))
        {
            if (f->checkForwardRef(init->loc))
            {
                result = new ErrorInitializer();
                return;
            }

            if (hasOverloads && !f->isUnique())
            {
                init->exp->error("cannot infer type from overloaded function symbol %s", init->exp->toChars());
                result = new ErrorInitializer();
                return;
            }
        }
        if (init->exp->op == TOKaddress)
        {
            AddrExp *ae = (AddrExp *)init->exp;
            if (ae->e1->op == TOKoverloadset)
            {
                init->exp->error("cannot infer type from overloaded function symbol %s", init->exp->toChars());
                result = new ErrorInitializer();
                return;
            }
        }

        if (init->exp->op == TOKerror)
        {
            result = new ErrorInitializer();
            return;
        }
        if (!init->exp->type)
        {
            result = new ErrorInitializer();
            return;
        }
        result = init;
    }
};

/* Translates to an expression to infer type.
 * Returns ExpInitializer or ErrorInitializer.
 */
Initializer *inferType(Initializer *init, Scope *sc)
{
    InferTypeVisitor v = InferTypeVisitor(sc);
    init->accept(&v);
    return v.result;
}

class InitToExpressionVisitor : public Visitor
{
public:
    Expression *result;
    Type *itype;

    InitToExpressionVisitor(Type *itype)
    {
        this->result = NULL;
        this->itype = itype;
    }

    void visit(ErrorInitializer *)
    {
        result = new ErrorExp();
    }

    void visit(VoidInitializer *)
    {
        result = NULL;
    }

    /***************************************
     * This works by transforming a struct initializer into
     * a struct literal. In the future, the two should be the
     * same thing.
     */
    void visit(StructInitializer *)
    {
        // cannot convert to an expression without target 'ad'
        result = NULL;
    }

    /********************************
     * If possible, convert array initializer to array literal.
     * Otherwise return NULL.
     */

    void visit(ArrayInitializer *init)
    {
        //printf("ArrayInitializer::toExpression(), dim = %d\n", init->length);
        //static int i; if (++i == 2) halt();

        Expressions *elements;
        unsigned edim;
        const unsigned amax = 0x80000000;
        Type *t = NULL;
        if (init->type)
        {
            if (init->type == Type::terror)
            {
                result = new ErrorExp();
                return;
            }

            t = init->type->toBasetype();
            switch (t->ty)
            {
                case Tvector:
                    t = ((TypeVector *)t)->basetype;
                    /* fall through */

                case Tsarray:
                    {
                        uinteger_t adim = ((TypeSArray *)t)->dim->toInteger();
                        if (adim >= amax)
                            goto Lno;
                        edim = (unsigned)adim;
                        break;
                    }

                case Tpointer:
                case Tarray:
                    edim = init->dim;
                    break;

                default:
                    assert(0);
            }
        }
        else
        {
            edim = (unsigned)init->value.length;
            for (size_t i = 0, j = 0; i < init->value.length; i++, j++)
            {
                if (init->index[i])
                {
                    if (init->index[i]->op == TOKint64)
                    {
                        const uinteger_t idxval = init->index[i]->toInteger();
                        if (idxval >= amax)
                            goto Lno;
                        j = (size_t)idxval;
                    }
                    else
                        goto Lno;
                }
                if (j >= edim)
                    edim = (unsigned)(j + 1);
            }
        }

        elements = new Expressions();
        elements->setDim(edim);
        elements->zero();
        for (size_t i = 0, j = 0; i < init->value.length; i++, j++)
        {
            if (init->index[i])
                j = (size_t)(init->index[i])->toInteger();
            assert(j < edim);
            Initializer *iz = init->value[i];
            if (!iz)
                goto Lno;
            Expression *ex = initializerToExpression(iz);
            if (!ex)
            {
                goto Lno;
            }
            (*elements)[j] = ex;
        }

        /* Fill in any missing elements with the default initializer
         */
        {
            Expression *_init = NULL;
            for (size_t i = 0; i < edim; i++)
            {
                if (!(*elements)[i])
                {
                    if (!init->type)
                        goto Lno;
                    if (!_init)
                        _init = ((TypeNext *)t)->next->defaultInit();
                    (*elements)[i] = _init;
                }
            }

            /* Expand any static array initializers that are a single expression
             * into an array of them
             */
            if (t)
            {
                Type *tn = t->nextOf()->toBasetype();
                if (tn->ty == Tsarray)
                {
                    size_t dim = ((TypeSArray *)tn)->dim->toInteger();
                    Type *te = tn->nextOf()->toBasetype();
                    for (size_t i = 0; i < elements->length; i++)
                    {
                        Expression *e = (*elements)[i];
                        if (te->equals(e->type))
                        {
                            Expressions *elements2 = new Expressions();
                            elements2->setDim(dim);
                            for (size_t j = 0; j < dim; j++)
                                (*elements2)[j] = e;
                            e = new ArrayLiteralExp(e->loc, tn, elements2);
                            (*elements)[i] = e;
                        }
                    }
                }
            }

            /* If any elements are errors, then the whole thing is an error
             */
            for (size_t i = 0; i < edim; i++)
            {
                Expression *e = (*elements)[i];
                if (e->op == TOKerror)
                {
                    result = e;
                    return;
                }
            }

            Expression *e = new ArrayLiteralExp(init->loc, init->type, elements);
            result = e;
            return;
        }

    Lno:
        result = NULL;
    }

    void visit(ExpInitializer *i)
    {
        if (itype)
        {
            //printf("ExpInitializer::toExpression(t = %s) exp = %s\n", itype->toChars(), i->exp->toChars());
            Type *tb = itype->toBasetype();
            Expression *e = (i->exp->op == TOKconstruct || i->exp->op == TOKblit) ? ((AssignExp *)i->exp)->e2 : i->exp;
            if (tb->ty == Tsarray && e->implicitConvTo(tb->nextOf()))
            {
                TypeSArray *tsa = (TypeSArray *)tb;
                size_t d = (size_t)tsa->dim->toInteger();
                Expressions *elements = new Expressions();
                elements->setDim(d);
                for (size_t j = 0; j < d; j++)
                    (*elements)[j] = e;
                ArrayLiteralExp *ae = new ArrayLiteralExp(e->loc, itype, elements);
                result = ae;
                return;
            }
        }
        result = i->exp;
    }
};

Expression *initializerToExpression(Initializer *i, Type *t)
{
    InitToExpressionVisitor v = InitToExpressionVisitor(t);
    i->accept(&v);
    return v.result;
}
