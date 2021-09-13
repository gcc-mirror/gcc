
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "root/dsystem.h"
#include "root/checkedint.h"

#include "mtype.h"
#include "aggregate.h"
#include "enum.h"
#include "errors.h"
#include "expression.h"
#include "hdrgen.h"
#include "id.h"
#include "init.h"
#include "parse.h"
#include "scope.h"
#include "target.h"
#include "template.h"
#include "visitor.h"

Expression *typeToExpression(Type *t);
Expression *typeToExpressionHelper(TypeQualified *t, Expression *e, size_t i = 0);
bool expressionsToString(OutBuffer &buf, Scope *sc, Expressions *exps);
char *MODtoChars(MOD mod);

class TypeToExpressionVisitor : public Visitor
{
public:
    Expression *result;
    Type *itype;

    TypeToExpressionVisitor(Type *itype)
    {
        this->result = NULL;
        this->itype = itype;
    }

    void visit(Type *)
    {
        result = NULL;
    }

    void visit(TypeSArray *t)
    {
        Expression *e = typeToExpression(t->next);
        if (e)
            e = new ArrayExp(t->dim->loc, e, t->dim);
        result = e;
    }

    void visit(TypeAArray *t)
    {
        Expression *e = typeToExpression(t->next);
        if (e)
        {
            Expression *ei = typeToExpression(t->index);
            if (ei)
            {
                result = new ArrayExp(t->loc, e, ei);
                return;
            }
        }
        result = NULL;
    }

    void visit(TypeIdentifier *t)
    {
        result = typeToExpressionHelper(t, new IdentifierExp(t->loc, t->ident));
    }

    void visit(TypeInstance *t)
    {
        result = typeToExpressionHelper(t, new ScopeExp(t->loc, t->tempinst));
    }

    void visit(TypeMixin *t)
    {
        result = new TypeExp(t->loc, t);
    }
};

/* We've mistakenly parsed this as a type.
 * Redo it as an Expression.
 * NULL if cannot.
 */
Expression *typeToExpression(Type *t)
{
    if (t->mod)
        return NULL;
    TypeToExpressionVisitor v = TypeToExpressionVisitor(t);
    t->accept(&v);
    return v.result;
}

/* Helper function for `typeToExpression`. Contains common code
 * for TypeQualified derived classes.
 */
Expression *typeToExpressionHelper(TypeQualified *t, Expression *e, size_t i)
{
    //printf("toExpressionHelper(e = %s %s)\n", Token::toChars(e->op), e->toChars());
    for (; i < t->idents.length; i++)
    {
        RootObject *id = t->idents[i];
        //printf("\t[%d] e: '%s', id: '%s'\n", i, e->toChars(), id->toChars());

        switch (id->dyncast())
        {
            case DYNCAST_IDENTIFIER:
            {
                // ... '. ident'
                e = new DotIdExp(e->loc, e, (Identifier *)id);
                break;
            }
            case DYNCAST_DSYMBOL:
            {
                // ... '. name!(tiargs)'
                TemplateInstance *ti = ((Dsymbol *)id)->isTemplateInstance();
                assert(ti);
                e = new DotTemplateInstanceExp(e->loc, e, ti->name, ti->tiargs);
                break;
            }
            case DYNCAST_TYPE:          // Bugzilla 1215
            {
                // ... '[type]'
                e = new ArrayExp(t->loc, e, new TypeExp(t->loc, (Type *)id));
                break;
            }
            case DYNCAST_EXPRESSION:    // Bugzilla 1215
            {
                // ... '[expr]'
                e = new ArrayExp(t->loc, e, (Expression *)id);
                break;
            }
            default:
                assert(0);
        }
    }
    return e;
}

/**************************
 * This evaluates exp while setting length to be the number
 * of elements in the tuple t.
 */
static Expression *semanticLength(Scope *sc, Type *t, Expression *exp)
{
    if (t->ty == Ttuple)
    {
        ScopeDsymbol *sym = new ArrayScopeSymbol(sc, (TypeTuple *)t);
        sym->parent = sc->scopesym;
        sc = sc->push(sym);

        sc = sc->startCTFE();
        exp = expressionSemantic(exp, sc);
        sc = sc->endCTFE();

        sc->pop();
    }
    else
    {
        sc = sc->startCTFE();
        exp = expressionSemantic(exp, sc);
        sc = sc->endCTFE();
    }

    return exp;
}

static Expression *semanticLength(Scope *sc, TupleDeclaration *s, Expression *exp)
{
    ScopeDsymbol *sym = new ArrayScopeSymbol(sc, s);
    sym->parent = sc->scopesym;
    sc = sc->push(sym);

    sc = sc->startCTFE();
    exp = expressionSemantic(exp, sc);
    sc = sc->endCTFE();

    sc->pop();
    return exp;
}

/******************************************
 * Compile the MixinType, returning the type or expression AST.
 *
 * Doesn't run semantic() on the returned object.
 * Params:
 *      tm = mixin to compile as a type or expression
 *      loc = location for error messages
 *      sc = context
 * Return:
 *      null if error, else RootObject AST as parsed
 */
RootObject *compileTypeMixin(TypeMixin *tm, Loc loc, Scope *sc)
{
    OutBuffer buf;
    if (expressionsToString(buf, sc, tm->exps))
        return NULL;

    const unsigned errors = global.errors;
    const size_t len = buf.length();
    const char *str = buf.extractChars();
    Parser p(loc, sc->_module, (const utf8_t *)str, len, false);
    p.nextToken();
    //printf("p.loc.linnum = %d\n", p.loc.linnum);

    RootObject *o = p.parseTypeOrAssignExp(TOKeof);
    if (errors != global.errors)
    {
        assert(global.errors != errors); // should have caught all these cases
        return NULL;
    }
    if (p.token.value != TOKeof)
    {
        ::error(loc, "incomplete mixin type `%s`", str);
        return NULL;
    }

    Type *t = isType(o);
    Expression *e = t ? typeToExpression(t) : isExpression(o);

    return (!e && t) ? (RootObject *)t : (RootObject *)e;
}

/******************************************
 * Perform semantic analysis on a type.
 * Params:
 *      type = Type AST node
 *      loc = the location of the type
 *      sc = context
 * Returns:
 *      `Type` with completed semantic analysis, `Terror` if errors
 *      were encountered
 */
Type *typeSemantic(Type *type, const Loc &loc, Scope *sc)
{
    class TypeSemanticVisitor : public Visitor
    {
    public:
        Loc loc;
        Scope *sc;
        Type *result;

        TypeSemanticVisitor(const Loc &loc, Scope *sc)
        {
            this->loc = loc;
            this->sc = sc;
            this->result = NULL;
        }

    private:
        void error()
        {
            result = Type::terror;
        }

    public:
        void visit(Type *t)
        {
            if (t->ty == Tint128 || t->ty == Tuns128)
            {
                ::error(loc, "cent and ucent types not implemented");
                return error();
            }

            result = t->merge();
        }

        void visit(TypeVector *mtype)
        {
            unsigned int errors = global.errors;
            mtype->basetype = typeSemantic(mtype->basetype, loc, sc);
            if (errors != global.errors)
                return error();
            mtype->basetype = mtype->basetype->toBasetype()->mutableOf();
            if (mtype->basetype->ty != Tsarray)
            {
                ::error(loc, "T in __vector(T) must be a static array, not %s", mtype->basetype->toChars());
                return error();
            }
            TypeSArray *t = (TypeSArray *)mtype->basetype;
            int sz = (int)t->size(loc);
            switch (target.isVectorTypeSupported(sz, t->nextOf()))
            {
            case 0: // valid
                break;
            case 1: // no support at all
                ::error(loc, "SIMD vector types not supported on this platform");
                return error();
            case 2: // invalid base type
                ::error(loc, "vector type %s is not supported on this platform", mtype->toChars());
                return error();
            case 3: // invalid size
                ::error(loc, "%d byte vector type %s is not supported on this platform", sz, mtype->toChars());
                return error();
            default:
                assert(0);
            }
            result = mtype->merge();
        }

        void visit(TypeSArray *mtype)
        {
            //printf("TypeSArray::semantic() %s\n", mtype->toChars());

            Type *t;
            Expression *e;
            Dsymbol *s;
            mtype->next->resolve(loc, sc, &e, &t, &s);

            if (mtype->dim && s && s->isTupleDeclaration())
            {
                TupleDeclaration *sd = s->isTupleDeclaration();

                mtype->dim = semanticLength(sc, sd, mtype->dim);
                mtype->dim = mtype->dim->ctfeInterpret();
                if(mtype->dim->op == TOKerror)
                    return error();

                uinteger_t d = mtype->dim->toUInteger();
                if (d >= sd->objects->length)
                {
                    ::error(loc, "tuple index %llu exceeds %llu", (unsigned long long)d, (unsigned long long)sd->objects->length);
                    return error();
                }

                RootObject *o = (*sd->objects)[(size_t)d];
                if (o->dyncast() != DYNCAST_TYPE)
                {
                    ::error(loc, "%s is not a type", mtype->toChars());
                    return error();
                }
                result = ((Type *)o)->addMod(mtype->mod);
                return;
            }

            if (t && t->ty == Terror)
                return error();

            Type *tn = typeSemantic(mtype->next, loc, sc);
            if (tn->ty == Terror)
                return error();

            Type *tbn = tn->toBasetype();
            if (mtype->dim)
            {
                unsigned int errors = global.errors;
                mtype->dim = semanticLength(sc, tbn, mtype->dim);
                if (errors != global.errors)
                    return error();

                mtype->dim = mtype->dim->optimize(WANTvalue);
                mtype->dim = mtype->dim->ctfeInterpret();
                if (mtype->dim->op == TOKerror)
                    return error();
                errors = global.errors;
                dinteger_t d1 = mtype->dim->toInteger();
                if (errors != global.errors)
                    return error();

                mtype->dim = mtype->dim->implicitCastTo(sc, Type::tsize_t);
                mtype->dim = mtype->dim->optimize(WANTvalue);
                if (mtype->dim->op == TOKerror)
                    return error();
                errors = global.errors;
                dinteger_t d2 = mtype->dim->toInteger();
                if (errors != global.errors)
                    return error();

                if (mtype->dim->op == TOKerror)
                    return error();

                if (d1 != d2)
                {
                Loverflow:
                    ::error(loc, "%s size %llu * %llu exceeds 0x%llx size limit for static array",
                          mtype->toChars(), (unsigned long long)tbn->size(loc), (unsigned long long)d1, target.maxStaticDataSize);
                    return error();
                }

                Type *tbx = tbn->baseElemOf();
                if ((tbx->ty == Tstruct && !((TypeStruct *)tbx)->sym->members) ||
                    (tbx->ty == Tenum && !((TypeEnum *)tbx)->sym->members))
                {
                    /* To avoid meaningless error message, skip the total size limit check
                     * when the bottom of element type is opaque.
                     */
                }
                else if (tbn->isTypeBasic() ||
                         tbn->ty == Tpointer ||
                         tbn->ty == Tarray ||
                         tbn->ty == Tsarray ||
                         tbn->ty == Taarray ||
                         (tbn->ty == Tstruct && (((TypeStruct *)tbn)->sym->sizeok == SIZEOKdone)) ||
                         tbn->ty == Tclass)
                {
                    /* Only do this for types that don't need to have semantic()
                     * run on them for the size, since they may be forward referenced.
                     */
                    bool overflow = false;
                    if (mulu(tbn->size(loc), d2, overflow) >= target.maxStaticDataSize || overflow)
                        goto Loverflow;
                }
            }
            switch (tbn->ty)
            {
                case Ttuple:
                {
                    // Index the tuple to get the type
                    assert(mtype->dim);
                    TypeTuple *tt = (TypeTuple *)tbn;
                    uinteger_t d = mtype->dim->toUInteger();
                    if (d >= tt->arguments->length)
                    {
                        ::error(loc, "tuple index %llu exceeds %llu", (unsigned long long)d, (unsigned long long)tt->arguments->length);
                        return error();
                    }
                    Type *telem = (*tt->arguments)[(size_t)d]->type;
                    result = telem->addMod(mtype->mod);
                    return;
                }
                case Tfunction:
                case Tnone:
                    ::error(loc, "cannot have array of %s", tbn->toChars());
                    return error();
                default:
                    break;
            }
            if (tbn->isscope())
            {
                ::error(loc, "cannot have array of scope %s", tbn->toChars());
                return error();
            }

            /* Ensure things like const(immutable(T)[3]) become immutable(T[3])
             * and const(T)[3] become const(T[3])
             */
            mtype->next = tn;
            mtype->transitive();
            result = mtype->addMod(tn->mod)->merge();
        }

        void visit(TypeDArray *mtype)
        {
            Type *tn = typeSemantic(mtype->next, loc,sc);
            Type *tbn = tn->toBasetype();
            switch (tbn->ty)
            {
                case Ttuple:
                    result = tbn;
                    return;
                case Tfunction:
                case Tnone:
                    ::error(loc, "cannot have array of %s", tbn->toChars());
                    return error();
                case Terror:
                    return error();
                default:
                    break;
            }
            if (tn->isscope())
            {
                ::error(loc, "cannot have array of scope %s", tn->toChars());
                return error();
            }
            mtype->next = tn;
            mtype->transitive();
            result = mtype->merge();
        }

        void visit(TypeAArray *mtype)
        {
            //printf("TypeAArray::semantic() %s index->ty = %d\n", mtype->toChars(), mtype->index->ty);
            if (mtype->deco)
            {
                result = mtype;
                return;
            }

            mtype->loc = loc;
            mtype->sc = sc;
            if (sc)
                sc->setNoFree();

            // Deal with the case where we thought the index was a type, but
            // in reality it was an expression.
            if (mtype->index->ty == Tident || mtype->index->ty == Tinstance || mtype->index->ty == Tsarray ||
                mtype->index->ty == Ttypeof || mtype->index->ty == Treturn || mtype->index->ty == Tmixin)
            {
                Expression *e;
                Type *t;
                Dsymbol *s;

                mtype->index->resolve(loc, sc, &e, &t, &s);
                if (e)
                {
                    // It was an expression -
                    // Rewrite as a static array
                    TypeSArray *tsa = new TypeSArray(mtype->next, e);
                    result = typeSemantic(tsa, loc, sc);
                    return;
                }
                else if (t)
                    mtype->index = typeSemantic(t, loc, sc);
                else
                {
                    mtype->index->error(loc, "index is not a type or an expression");
                    return error();
                }
            }
            else
                mtype->index = typeSemantic(mtype->index, loc,sc);
            mtype->index = mtype->index->merge2();

            if (mtype->index->nextOf() && !mtype->index->nextOf()->isImmutable())
            {
                mtype->index = mtype->index->constOf()->mutableOf();
            }

            switch (mtype->index->toBasetype()->ty)
            {
                case Tfunction:
                case Tvoid:
                case Tnone:
                case Ttuple:
                    ::error(loc, "cannot have associative array key of %s", mtype->index->toBasetype()->toChars());
                    /* fall through */
                case Terror:
                    return error();
                default:
                    break;
            }
            Type *tbase = mtype->index->baseElemOf();
            while (tbase->ty == Tarray)
                tbase = tbase->nextOf()->baseElemOf();
            if (tbase->ty == Tstruct)
            {
                /* AA's need typeid(index).equals() and getHash(). Issue error if not correctly set up.
                 */
                StructDeclaration *sd = ((TypeStruct *)tbase)->sym;
                if (sd->semanticRun < PASSsemanticdone)
                    dsymbolSemantic(sd, NULL);

                // duplicate a part of StructDeclaration::semanticTypeInfoMembers
                //printf("AA = %s, key: xeq = %p, xerreq = %p xhash = %p\n", mtype->toChars(), sd->xeq, sd->xerreq, sd->xhash);
                if (sd->xeq &&
                    sd->xeq->_scope &&
                    sd->xeq->semanticRun < PASSsemantic3done)
                {
                    unsigned errors = global.startGagging();
                    semantic3(sd->xeq, sd->xeq->_scope);
                    if (global.endGagging(errors))
                        sd->xeq = sd->xerreq;
                }

                const char *s = (mtype->index->toBasetype()->ty != Tstruct) ? "bottom of " : "";
                if (!sd->xeq)
                {
                    // If sd->xhash != NULL:
                    //   sd or its fields have user-defined toHash.
                    //   AA assumes that its result is consistent with bitwise equality.
                    // else:
                    //   bitwise equality & hashing
                }
                else if (sd->xeq == sd->xerreq)
                {
                    if (search_function(sd, Id::eq))
                    {
                        ::error(loc, "%sAA key type %s does not have `bool opEquals(ref const %s) const`",
                                s, sd->toChars(), sd->toChars());
                    }
                    else
                    {
                        ::error(loc, "%sAA key type %s does not support const equality",
                                s, sd->toChars());
                    }
                    return error();
                }
                else if (!sd->xhash)
                {
                    if (search_function(sd, Id::eq))
                    {
                        ::error(loc, "%sAA key type %s should have `size_t toHash() const nothrow @safe` if opEquals defined",
                                s, sd->toChars());
                    }
                    else
                    {
                        ::error(loc, "%sAA key type %s supports const equality but doesn't support const hashing",
                                s, sd->toChars());
                    }
                    return error();
                }
                else
                {
                    // defined equality & hashing
                    assert(sd->xeq && sd->xhash);

                    /* xeq and xhash may be implicitly defined by compiler. For example:
                     *   struct S { int[] arr; }
                     * With 'arr' field equality and hashing, compiler will implicitly
                     * generate functions for xopEquals and xtoHash in TypeInfo_Struct.
                     */
                }
            }
            else if (tbase->ty == Tclass && !((TypeClass *)tbase)->sym->isInterfaceDeclaration())
            {
                ClassDeclaration *cd = ((TypeClass *)tbase)->sym;
                if (cd->semanticRun < PASSsemanticdone)
                    dsymbolSemantic(cd, NULL);

                if (!ClassDeclaration::object)
                {
                    ::error(Loc(), "missing or corrupt object.d");
                    fatal();
                }

                static FuncDeclaration *feq   = NULL;
                static FuncDeclaration *fcmp  = NULL;
                static FuncDeclaration *fhash = NULL;
                if (!feq)   feq   = search_function(ClassDeclaration::object, Id::eq)->isFuncDeclaration();
                if (!fcmp)  fcmp  = search_function(ClassDeclaration::object, Id::cmp)->isFuncDeclaration();
                if (!fhash) fhash = search_function(ClassDeclaration::object, Id::tohash)->isFuncDeclaration();
                assert(fcmp && feq && fhash);

                if (feq->vtblIndex < (int)cd->vtbl.length && cd->vtbl[feq ->vtblIndex] == feq)
                {
                    if (fcmp->vtblIndex < (int)cd->vtbl.length && cd->vtbl[fcmp->vtblIndex] != fcmp)
                    {
                        const char *s = (mtype->index->toBasetype()->ty != Tclass) ? "bottom of " : "";
                        ::error(loc, "%sAA key type %s now requires equality rather than comparison",
                            s, cd->toChars());
                        errorSupplemental(loc, "Please override Object.opEquals and toHash.");
                    }
                }
            }
            mtype->next = typeSemantic(mtype->next, loc,sc)->merge2();
            mtype->transitive();

            switch (mtype->next->toBasetype()->ty)
            {
                case Tfunction:
                case Tvoid:
                case Tnone:
                case Ttuple:
                    ::error(loc, "cannot have associative array of %s", mtype->next->toChars());
                    /* fall through */
                case Terror:
                    return error();
            }
            if (mtype->next->isscope())
            {
                ::error(loc, "cannot have array of scope %s", mtype->next->toChars());
                return error();
            }
            result = mtype->merge();
        }

        void visit(TypePointer *mtype)
        {
            //printf("TypePointer::semantic() %s\n", mtype->toChars());
            if (mtype->deco)
            {
                result = mtype;
                return;
            }
            Type *n = typeSemantic(mtype->next, loc, sc);
            switch (n->toBasetype()->ty)
            {
                case Ttuple:
                    ::error(loc, "cannot have pointer to %s", n->toChars());
                    /* fall through */
                case Terror:
                    return error();
                default:
                    break;
            }
            if (n != mtype->next)
            {
                mtype->deco = NULL;
            }
            mtype->next = n;
            if (mtype->next->ty != Tfunction)
            {
                mtype->transitive();
                result = mtype->merge();
                return;
            }
            mtype->deco = mtype->merge()->deco;
            /* Don't return merge(), because arg identifiers and default args
             * can be different
             * even though the types match
             */
            result = mtype;
        }

        void visit(TypeReference *mtype)
        {
            //printf("TypeReference::semantic()\n");
            Type *n = typeSemantic(mtype->next, loc, sc);
            if (n != mtype->next)
                mtype->deco = NULL;
            mtype->next = n;
            mtype->transitive();
            result = mtype->merge();
        }


        void visit(TypeFunction *mtype)
        {
            if (mtype->deco)                   // if semantic() already run
            {
                //printf("already done\n");
                result = mtype;
                return;
            }
            //printf("TypeFunction::semantic() this = %p\n", this);
            //printf("TypeFunction::semantic() %s, sc->stc = %llx, fargs = %p\n", mtype->toChars(), sc->stc, mtype->fargs);

            bool errors = false;

            if (mtype->inuse > global.recursionLimit)
            {
                mtype->inuse = 0;
                ::error(loc, "recursive type");
                return error();
            }

            /* Copy in order to not mess up original.
             * This can produce redundant copies if inferring return type,
             * as semantic() will get called again on this.
             */
            TypeFunction *tf = mtype->copy()->toTypeFunction();
            if (mtype->parameterList.parameters)
            {
                tf->parameterList.parameters = mtype->parameterList.parameters->copy();
                for (size_t i = 0; i < mtype->parameterList.parameters->length; i++)
                {
                    void *pp = mem.xmalloc(sizeof(Parameter));
                    Parameter *p = (Parameter *)memcpy(pp, (void *)(*mtype->parameterList.parameters)[i],
                                                       sizeof(Parameter));
                    (*tf->parameterList.parameters)[i] = p;
                }
            }

            if (sc->stc & STCpure)
                tf->purity = PUREfwdref;
            if (sc->stc & STCnothrow)
                tf->isnothrow = true;
            if (sc->stc & STCnogc)
                tf->isnogc = true;
            if (sc->stc & STCref)
                tf->isref = true;
            if (sc->stc & STCreturn)
                tf->isreturn = true;
            if (sc->stc & STCscope)
                tf->isscope = true;
            if (sc->stc & STCscopeinferred)
                tf->isscopeinferred = true;
            //if ((sc->stc & (STCreturn | STCref)) == STCreturn)
            //    tf->isscope = true; // return by itself means 'return scope'

            if (tf->trust == TRUSTdefault)
            {
                if (sc->stc & STCsafe)
                    tf->trust = TRUSTsafe;
                if (sc->stc & STCsystem)
                    tf->trust = TRUSTsystem;
                if (sc->stc & STCtrusted)
                    tf->trust = TRUSTtrusted;
            }

            if (sc->stc & STCproperty)
                tf->isproperty = true;

            tf->linkage = sc->linkage;
            bool wildreturn = false;
            if (tf->next)
            {
                sc = sc->push();
                sc->stc &= ~(STC_TYPECTOR | STC_FUNCATTR);
                tf->next = typeSemantic(tf->next, loc, sc);
                sc = sc->pop();
                errors |= tf->checkRetType(loc);
                if (tf->next->isscope() && !(sc->flags & SCOPEctor))
                {
                    ::error(loc, "functions cannot return scope %s", tf->next->toChars());
                    errors = true;
                }
                if (tf->next->hasWild())
                    wildreturn = true;

                if (tf->isreturn && !tf->isref && !tf->next->hasPointers())
                {
                    tf->isreturn = false;
                }
            }

            unsigned char wildparams = 0;
            if (tf->parameterList.parameters)
            {
                /* Create a scope for evaluating the default arguments for the parameters
                 */
                Scope *argsc = sc->push();
                argsc->stc = 0;                 // don't inherit storage class
                argsc->protection = Prot(Prot::public_);
                argsc->func = NULL;

                size_t dim = tf->parameterList.length();
                for (size_t i = 0; i < dim; i++)
                {
                    Parameter *fparam = tf->parameterList[i];
                    mtype->inuse++;
                    fparam->type = typeSemantic(fparam->type, loc, argsc);
                    mtype->inuse--;

                    if (fparam->type->ty == Terror)
                    {
                        errors = true;
                        continue;
                    }

                    fparam->type = fparam->type->addStorageClass(fparam->storageClass);

                    if (fparam->storageClass & (STCauto | STCalias | STCstatic))
                    {
                        if (!fparam->type)
                            continue;
                    }

                    Type *t = fparam->type->toBasetype();

                    if (t->ty == Tfunction)
                    {
                        ::error(loc, "cannot have parameter of function type %s", fparam->type->toChars());
                        errors = true;
                    }
                    else if (!(fparam->storageClass & (STCref | STCout)) &&
                             (t->ty == Tstruct || t->ty == Tsarray || t->ty == Tenum))
                    {
                        Type *tb2 = t->baseElemOf();
                        if ((tb2->ty == Tstruct && !((TypeStruct *)tb2)->sym->members) ||
                            (tb2->ty == Tenum && !((TypeEnum *)tb2)->sym->memtype))
                        {
                            ::error(loc, "cannot have parameter of opaque type %s by value", fparam->type->toChars());
                            errors = true;
                        }
                    }
                    else if (!(fparam->storageClass & STClazy) && t->ty == Tvoid)
                    {
                        ::error(loc, "cannot have parameter of type %s", fparam->type->toChars());
                        errors = true;
                    }

                    if ((fparam->storageClass & (STCref | STCwild)) == (STCref | STCwild))
                    {
                        // 'ref inout' implies 'return'
                        fparam->storageClass |= STCreturn;
                    }

                    if (fparam->storageClass & STCreturn)
                    {
                        if (fparam->storageClass & (STCref | STCout))
                        {
                            // Disabled for the moment awaiting improvement to allow return by ref
                            // to be transformed into return by scope.
                            if (0 && !tf->isref)
                            {
                                StorageClass stc = fparam->storageClass & (STCref | STCout);
                                ::error(loc, "parameter `%s` is `return %s` but function does not return by `ref`",
                                    fparam->ident ? fparam->ident->toChars() : "",
                                    stcToChars(stc));
                                errors = true;
                            }
                        }
                        else
                        {
                            fparam->storageClass |= STCscope;        // 'return' implies 'scope'
                            if (tf->isref)
                            {
                            }
                            else if (!tf->isref && tf->next && !tf->next->hasPointers())
                            {
                                fparam->storageClass &= STCreturn;   // https://issues.dlang.org/show_bug.cgi?id=18963
                            }
                        }
                    }

                    if (fparam->storageClass & (STCref | STClazy))
                    {
                    }
                    else if (fparam->storageClass & STCout)
                    {
                        if (unsigned char m = fparam->type->mod & (MODimmutable | MODconst | MODwild))
                        {
                            ::error(loc, "cannot have %s out parameter of type %s", MODtoChars(m), t->toChars());
                            errors = true;
                        }
                        else
                        {
                            Type *tv = t;
                            while (tv->ty == Tsarray)
                                tv = tv->nextOf()->toBasetype();
                            if (tv->ty == Tstruct && ((TypeStruct *)tv)->sym->noDefaultCtor)
                            {
                                ::error(loc, "cannot have out parameter of type %s because the default construction is disabled",
                                    fparam->type->toChars());
                                errors = true;
                            }
                        }
                    }

                    if (fparam->storageClass & STCscope && !fparam->type->hasPointers() && fparam->type->ty != Ttuple)
                    {
                        fparam->storageClass &= ~STCscope;
                        if (!(fparam->storageClass & STCref))
                            fparam->storageClass &= ~STCreturn;
                    }

                    if (t->hasWild())
                    {
                        wildparams |= 1;
                        //if (tf->next && !wildreturn)
                        //    ::error(loc, "inout on parameter means inout must be on return type as well (if from D1 code, replace with `ref`)");
                    }

                    if (fparam->defaultArg)
                    {
                        Expression *e = fparam->defaultArg;
                        if (fparam->storageClass & (STCref | STCout))
                        {
                            e = expressionSemantic(e, argsc);
                            e = resolveProperties(argsc, e);
                        }
                        else
                        {
                            e = inferType(e, fparam->type);
                            Initializer *iz = new ExpInitializer(e->loc, e);
                            iz = initializerSemantic(iz, argsc, fparam->type, INITnointerpret);
                            e = initializerToExpression(iz);
                        }
                        if (e->op == TOKfunction)               // see Bugzilla 4820
                        {
                            FuncExp *fe = (FuncExp *)e;
                            // Replace function literal with a function symbol,
                            // since default arg expression must be copied when used
                            // and copying the literal itself is wrong.
                            e = new VarExp(e->loc, fe->fd, false);
                            e = new AddrExp(e->loc, e);
                            e = expressionSemantic(e, argsc);
                        }
                        e = e->implicitCastTo(argsc, fparam->type);

                        // default arg must be an lvalue
                        if (fparam->storageClass & (STCout | STCref))
                            e = e->toLvalue(argsc, e);

                        fparam->defaultArg = e;
                        if (e->op == TOKerror)
                            errors = true;
                    }

                    /* If fparam after semantic() turns out to be a tuple, the number of parameters may
                     * change.
                     */
                    if (t->ty == Ttuple)
                    {
                        /* TypeFunction::parameter also is used as the storage of
                         * Parameter objects for FuncDeclaration. So we should copy
                         * the elements of TypeTuple::arguments to avoid unintended
                         * sharing of Parameter object among other functions.
                         */
                        TypeTuple *tt = (TypeTuple *)t;
                        if (tt->arguments && tt->arguments->length)
                        {
                            /* Propagate additional storage class from tuple parameters to their
                             * element-parameters.
                             * Make a copy, as original may be referenced elsewhere.
                             */
                            size_t tdim = tt->arguments->length;
                            Parameters *newparams = new Parameters();
                            newparams->setDim(tdim);
                            for (size_t j = 0; j < tdim; j++)
                            {
                                Parameter *narg = (*tt->arguments)[j];

                                // Bugzilla 12744: If the storage classes of narg
                                // conflict with the ones in fparam, it's ignored.
                                StorageClass stc  = fparam->storageClass | narg->storageClass;
                                StorageClass stc1 = fparam->storageClass & (STCref | STCout | STClazy);
                                StorageClass stc2 =   narg->storageClass & (STCref | STCout | STClazy);
                                if (stc1 && stc2 && stc1 != stc2)
                                {
                                    OutBuffer buf1;  stcToBuffer(&buf1, stc1 | ((stc1 & STCref) ? (fparam->storageClass & STCauto) : 0));
                                    OutBuffer buf2;  stcToBuffer(&buf2, stc2);

                                    ::error(loc, "incompatible parameter storage classes `%s` and `%s`",
                                              buf1.peekChars(), buf2.peekChars());
                                    errors = true;
                                    stc = stc1 | (stc & ~(STCref | STCout | STClazy));
                                }

                                (*newparams)[j] = new Parameter(
                                        stc, narg->type, narg->ident, narg->defaultArg, narg->userAttribDecl);
                            }
                            fparam->type = new TypeTuple(newparams);
                        }
                        fparam->storageClass = 0;

                        /* Reset number of parameters, and back up one to do this fparam again,
                         * now that it is a tuple
                         */
                        dim = tf->parameterList.length();
                        i--;
                        continue;
                    }

                    /* Resolve "auto ref" storage class to be either ref or value,
                     * based on the argument matching the parameter
                     */
                    if (fparam->storageClass & STCauto)
                    {
                        if (mtype->fargs && i < mtype->fargs->length && (fparam->storageClass & STCref))
                        {
                            Expression *farg = (*mtype->fargs)[i];
                            if (farg->isLvalue())
                                ;                               // ref parameter
                            else
                                fparam->storageClass &= ~STCref;        // value parameter
                            fparam->storageClass &= ~STCauto;    // Bugzilla 14656
                            fparam->storageClass |= STCautoref;
                        }
                        else
                        {
                            ::error(loc, "`auto` can only be used as part of `auto ref` for template function parameters");
                            errors = true;
                        }
                    }

                    // Remove redundant storage classes for type, they are already applied
                    fparam->storageClass &= ~(STC_TYPECTOR | STCin);
                }
                argsc->pop();
            }
            if (tf->isWild())
                wildparams |= 2;

            if (wildreturn && !wildparams)
            {
                ::error(loc, "inout on return means inout must be on a parameter as well for %s", mtype->toChars());
                errors = true;
            }
            tf->iswild = wildparams;

            if (tf->isproperty && (tf->parameterList.varargs != VARARGnone || tf->parameterList.length() > 2))
            {
                ::error(loc, "properties can only have zero, one, or two parameter");
                errors = true;
            }

            if (tf->parameterList.varargs == VARARGvariadic && tf->linkage != LINKd && tf->parameterList.length() == 0)
            {
                ::error(loc, "variadic functions with non-D linkage must have at least one parameter");
                errors = true;
            }

            if (errors)
                return error();

            if (tf->next)
                tf->deco = tf->merge()->deco;

            /* Don't return merge(), because arg identifiers and default args
             * can be different
             * even though the types match
             */
            result = tf;
        }

        void visit(TypeDelegate *mtype)
        {
            //printf("TypeDelegate::semantic() %s\n", mtype->toChars());
            if (mtype->deco)                   // if semantic() already run
            {
                //printf("already done\n");
                result = mtype;
                return;
            }
            mtype->next = typeSemantic(mtype->next, loc,sc);
            if (mtype->next->ty != Tfunction)
                return error();

            /* In order to deal with Bugzilla 4028, perhaps default arguments should
             * be removed from next before the merge.
             */

            /* Don't return merge(), because arg identifiers and default args
             * can be different
             * even though the types match
             */
            mtype->deco = mtype->merge()->deco;
            result = mtype;
        }

        void visit(TypeTraits *mtype)
        {
            if (mtype->ty == Terror)
            {
                result = mtype;
                return;
            }

            const int inAlias = (sc->flags & SCOPEalias) != 0;
            if (mtype->exp->ident != Id::allMembers &&
                mtype->exp->ident != Id::derivedMembers &&
                mtype->exp->ident != Id::getMember &&
                mtype->exp->ident != Id::parent &&
                mtype->exp->ident != Id::child &&
                mtype->exp->ident != Id::toType &&
                mtype->exp->ident != Id::getOverloads &&
                mtype->exp->ident != Id::getVirtualFunctions &&
                mtype->exp->ident != Id::getVirtualMethods &&
                mtype->exp->ident != Id::getAttributes &&
                mtype->exp->ident != Id::getUnitTests &&
                mtype->exp->ident != Id::getAliasThis)
            {
                static const char *ctxt[2] = {"as type", "in alias"};
                ::error(loc, "trait `%s` is either invalid or not supported %s",
                        mtype->exp->ident->toChars(), ctxt[inAlias]);
                mtype->ty = Terror;
                result = mtype;
                return;
            }

            if (Expression *e = semanticTraits(mtype->exp, sc))
            {
                switch (e->op)
                {
                case TOKdotvar:
                    mtype->sym = ((DotVarExp *)e)->var;
                    break;
                case TOKvar:
                    mtype->sym = ((VarExp *)e)->var;
                    break;
                case TOKfunction:
                {
                    FuncExp *fe = (FuncExp *)e;
                    if (fe->td)
                        mtype->sym = fe->td;
                    else
                        mtype->sym = fe->fd;
                    break;
                }
                case TOKdottd:
                    mtype->sym = ((DotTemplateExp*)e)->td;
                    break;
                case TOKdsymbol:
                    mtype->sym = ((DsymbolExp *)e)->s;
                    break;
                case TOKtemplate:
                    mtype->sym = ((TemplateExp *)e)->td;
                    break;
                case TOKscope:
                    mtype->sym = ((ScopeExp *)e)->sds;
                    break;
                case TOKtuple:
                {
                    TupleExp *te = e->toTupleExp();
                    Objects *elems = new Objects;
                    elems->setDim(te->exps->length);
                    for (size_t i = 0; i < elems->length; i++)
                    {
                        Expression *src = (*te->exps)[i];
                        switch (src->op)
                        {
                        case TOKtype:
                            (*elems)[i] = ((TypeExp *)src)->type;
                            break;
                        case TOKdottype:
                            (*elems)[i] = ((DotTypeExp *)src)->type;
                            break;
                        case TOKoverloadset:
                            (*elems)[i] = ((OverExp *)src)->type;
                            break;
                        default:
                            if (Dsymbol *sym = isDsymbol(src))
                                (*elems)[i] = sym;
                            else
                                (*elems)[i] = src;
                        }
                    }
                    TupleDeclaration *td = new TupleDeclaration(e->loc,
                        Identifier::generateId("__aliastup"), elems);
                    mtype->sym = td;
                    break;
                }
                case TOKdottype:
                    result = isType(((DotTypeExp *)e)->sym);
                    break;
                case TOKtype:
                    result = ((TypeExp *)e)->type;
                    break;
                case TOKoverloadset:
                    result = ((OverExp *)e)->type;
                    break;
                default:
                    break;
                }
            }

            if (result)
                result = result->addMod(mtype->mod);
            if (!inAlias && !result)
            {
                if (!global.errors)
                    ::error(loc, "`%s` does not give a valid type", mtype->toChars());
                return error();
            }
        }

        void visit(TypeIdentifier *mtype)
        {
            Type *t;
            Expression *e;
            Dsymbol *s;

            //printf("TypeIdentifier::semantic(%s)\n", mtype->toChars());
            mtype->resolve(loc, sc, &e, &t, &s);
            if (t)
            {
                //printf("\tit's a type %d, %s, %s\n", t->ty, t->toChars(), t->deco);
                t = t->addMod(mtype->mod);
            }
            else
            {
                if (s)
                {
                    s->error(loc, "is used as a type");
                    //halt();
                }
                else
                    ::error(loc, "%s is used as a type", mtype->toChars());
                return error();
            }
            //t->print();
            result = t;
        }

        void visit(TypeInstance *mtype)
        {
            Type *t;
            Expression *e;
            Dsymbol *s;

            //printf("TypeInstance::semantic(%p, %s)\n", this, mtype->toChars());
            {
                unsigned errors = global.errors;
                mtype->resolve(loc, sc, &e, &t, &s);
                // if we had an error evaluating the symbol, suppress further errors
                if (!t && errors != global.errors)
                    return error();
            }

            if (!t)
            {
                if (!e && s && s->errors)
                {
                    // if there was an error evaluating the symbol, it might actually
                    // be a type. Avoid misleading error messages.
                    ::error(loc, "%s had previous errors", mtype->toChars());
                }
                else
                    ::error(loc, "%s is used as a type", mtype->toChars());
                return error();
            }
            result = t;
        }

        void visit(TypeTypeof *mtype)
        {
            //printf("TypeTypeof::semantic() %s\n", mtype->toChars());

            Expression *e;
            Type *t;
            Dsymbol *s;
            mtype->resolve(loc, sc, &e, &t, &s);
            if (s && (t = s->getType()) != NULL)
                t = t->addMod(mtype->mod);
            if (!t)
            {
                ::error(loc, "%s is used as a type", mtype->toChars());
                return error();
            }
            result = t;
        }

        void visit(TypeReturn *mtype)
        {
            //printf("TypeReturn::semantic() %s\n", mtype->toChars());

            Expression *e;
            Type *t;
            Dsymbol *s;
            mtype->resolve(loc, sc, &e, &t, &s);
            if (s && (t = s->getType()) != NULL)
                t = t->addMod(mtype->mod);
            if (!t)
            {
                ::error(loc, "%s is used as a type", mtype->toChars());
                return error();
            }
            result = t;
        }

        void visit(TypeEnum *mtype)
        {
            //printf("TypeEnum::semantic() %s\n", mtype->toChars());
            result = mtype->deco ? mtype : mtype->merge();
        }

        void visit(TypeStruct *mtype)
        {
            //printf("TypeStruct::semantic('%s')\n", mtype->toChars());
            if (mtype->deco)
            {
                if (sc && sc->cppmangle != CPPMANGLEdefault)
                {
                    if (mtype->cppmangle == CPPMANGLEdefault)
                        mtype->cppmangle = sc->cppmangle;
                    else
                        assert(mtype->cppmangle == sc->cppmangle);
                }
                result = mtype;
                return;
            }

            /* Don't semantic for sym because it should be deferred until
             * sizeof needed or its members accessed.
             */
            // instead, parent should be set correctly
            assert(mtype->sym->parent);

            if (mtype->sym->type->ty == Terror)
                return error();
            if (sc)
                mtype->cppmangle = sc->cppmangle;
            result = mtype->merge();
        }

        void visit(TypeClass *mtype)
        {
            //printf("TypeClass::semantic(%s)\n", mtype->toChars());
            if (mtype->deco)
            {
                if (sc && sc->cppmangle != CPPMANGLEdefault)
                {
                    if (mtype->cppmangle == CPPMANGLEdefault)
                        mtype->cppmangle = sc->cppmangle;
                    else
                        assert(mtype->cppmangle == sc->cppmangle);
                }
                result = mtype;
                return;
            }

            /* Don't semantic for sym because it should be deferred until
             * sizeof needed or its members accessed.
             */
            // instead, parent should be set correctly
            assert(mtype->sym->parent);

            if (mtype->sym->type->ty == Terror)
                return error();
            if (sc)
                mtype->cppmangle = sc->cppmangle;
            result = mtype->merge();
        }

        void visit(TypeTuple *mtype)
        {
            //printf("TypeTuple::semantic(this = %p)\n", this);
            //printf("TypeTuple::semantic() %p, %s\n", this, mtype->toChars());
            if (!mtype->deco)
                mtype->deco = mtype->merge()->deco;

            /* Don't return merge(), because a tuple with one type has the
             * same deco as that type.
             */
            result = mtype;
        }

        void visit(TypeSlice *mtype)
        {
            //printf("TypeSlice::semantic() %s\n", mtype->toChars());
            Type *tn = typeSemantic(mtype->next, loc, sc);
            //printf("next: %s\n", tn->toChars());

            Type *tbn = tn->toBasetype();
            if (tbn->ty != Ttuple)
            {
                ::error(loc, "can only slice tuple types, not %s", tbn->toChars());
                return error();
            }
            TypeTuple *tt = (TypeTuple *)tbn;

            mtype->lwr = semanticLength(sc, tbn, mtype->lwr);
            mtype->lwr = mtype->lwr->ctfeInterpret();
            uinteger_t i1 = mtype->lwr->toUInteger();

            mtype->upr = semanticLength(sc, tbn, mtype->upr);
            mtype->upr = mtype->upr->ctfeInterpret();
            uinteger_t i2 = mtype->upr->toUInteger();

            if (!(i1 <= i2 && i2 <= tt->arguments->length))
            {
                ::error(loc, "slice `[%llu..%llu]` is out of range of [0..%llu]",
                    (unsigned long long)i1, (unsigned long long)i2, (unsigned long long)tt->arguments->length);
                return error();
            }

            mtype->next = tn;
            mtype->transitive();

            Parameters *args = new Parameters;
            args->reserve((size_t)(i2 - i1));
            for (size_t i = (size_t)i1; i < (size_t)i2; i++)
            {
                Parameter *arg = (*tt->arguments)[i];
                args->push(arg);
            }
            Type *t = new TypeTuple(args);
            result = typeSemantic(t, loc, sc);
        }

        void visit(TypeMixin *mtype)
        {
            //printf("TypeMixin::semantic() %s\n", mtype->toChars());

            Expression *e = NULL;
            Type *t = NULL;
            Dsymbol *s = NULL;
            mtype->resolve(loc, sc, &e, &t, &s);

            if (t && t->ty != Terror)
            {
                result = t;
                return;
            }

            ::error(mtype->loc, "`mixin(%s)` does not give a valid type", mtype->obj->toChars());
            return error();
        }
    };
    TypeSemanticVisitor v(loc, sc);
    type->accept(&v);
    return v.result;
}
