
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/ctfeexpr.c
 */

#include "root/dsystem.h"               // mem{cpy|set}()
#include "root/rmem.h"

#include "mars.h"
#include "expression.h"
#include "declaration.h"
#include "aggregate.h"
// for AssocArray
#include "id.h"
#include "utf.h"
#include "template.h"
#include "ctfe.h"

int RealEquals(real_t x1, real_t x2);

/************** ClassReferenceExp ********************************************/

ClassReferenceExp::ClassReferenceExp(Loc loc, StructLiteralExp *lit, Type *type)
    : Expression(loc, TOKclassreference, sizeof(ClassReferenceExp))
{
    assert(lit && lit->sd && lit->sd->isClassDeclaration());
    this->value = lit;
    this->type = type;
}

ClassDeclaration *ClassReferenceExp::originalClass()
{
    return value->sd->isClassDeclaration();
}

// Return index of the field, or -1 if not found
int ClassReferenceExp::getFieldIndex(Type *fieldtype, unsigned fieldoffset)
{
    ClassDeclaration *cd = originalClass();
    unsigned fieldsSoFar = 0;
    for (size_t j = 0; j < value->elements->dim; j++)
    {
        while (j - fieldsSoFar >= cd->fields.dim)
        {
            fieldsSoFar += cd->fields.dim;
            cd = cd->baseClass;
        }
        VarDeclaration *v2 = cd->fields[j - fieldsSoFar];
        if (fieldoffset == v2->offset &&
            fieldtype->size() == v2->type->size())
        {
            return (int)(value->elements->dim - fieldsSoFar - cd->fields.dim + (j-fieldsSoFar));
        }
    }
    return -1;
}

// Return index of the field, or -1 if not found
// Same as getFieldIndex, but checks for a direct match with the VarDeclaration
int ClassReferenceExp::findFieldIndexByName(VarDeclaration *v)
{
    ClassDeclaration *cd = originalClass();
    size_t fieldsSoFar = 0;
    for (size_t j = 0; j < value->elements->dim; j++)
    {
        while (j - fieldsSoFar >= cd->fields.dim)
        {
            fieldsSoFar += cd->fields.dim;
            cd = cd->baseClass;
        }
        VarDeclaration *v2 = cd->fields[j - fieldsSoFar];
        if (v == v2)
        {
            return (int)(value->elements->dim - fieldsSoFar - cd->fields.dim + (j-fieldsSoFar));
        }
    }
    return -1;
}

/************** VoidInitExp ********************************************/

VoidInitExp::VoidInitExp(VarDeclaration *var, Type *)
    : Expression(var->loc, TOKvoid, sizeof(VoidInitExp))
{
    this->var = var;
    this->type = var->type;
}

const char *VoidInitExp::toChars()
{
    return "void";
}

// Return index of the field, or -1 if not found
// Same as getFieldIndex, but checks for a direct match with the VarDeclaration
int findFieldIndexByName(StructDeclaration *sd, VarDeclaration *v)
{
    for (size_t i = 0; i < sd->fields.dim; ++i)
    {
        if (sd->fields[i] == v)
            return (int)i;
    }
    return -1;
}

/************** ThrownExceptionExp ********************************************/

ThrownExceptionExp::ThrownExceptionExp(Loc loc, ClassReferenceExp *victim) : Expression(loc, TOKthrownexception, sizeof(ThrownExceptionExp))
{
    this->thrown = victim;
    this->type = victim->type;
}

const char *ThrownExceptionExp::toChars()
{
    return "CTFE ThrownException";
}

// Generate an error message when this exception is not caught
void ThrownExceptionExp::generateUncaughtError()
{
    UnionExp ue;
    Expression *e = resolveSlice((*thrown->value->elements)[0], &ue);
    StringExp *se = e->toStringExp();
    thrown->error("uncaught CTFE exception %s(%s)", thrown->type->toChars(), se ? se->toChars() : e->toChars());

    /* Also give the line where the throw statement was. We won't have it
     * in the case where the ThrowStatement is generated internally
     * (eg, in ScopeStatement)
     */
    if (loc.filename && !loc.equals(thrown->loc))
        errorSupplemental(loc, "thrown from here");
}

// True if 'e' is CTFEExp::cantexp, or an exception
bool exceptionOrCantInterpret(Expression *e)
{
    return e && (e->op == TOKcantexp || e->op == TOKthrownexception);
}

/********************** CTFEExp ******************************************/

CTFEExp *CTFEExp::cantexp;
CTFEExp *CTFEExp::voidexp;
CTFEExp *CTFEExp::breakexp;
CTFEExp *CTFEExp::continueexp;
CTFEExp *CTFEExp::gotoexp;

CTFEExp::CTFEExp(TOK tok)
    : Expression(Loc(), tok, sizeof(CTFEExp))
{
    type = Type::tvoid;
}

const char *CTFEExp::toChars()
{
    switch (op)
    {
        case TOKcantexp:    return "<cant>";
        case TOKvoidexp:    return "<void>";
        case TOKbreak:      return "<break>";
        case TOKcontinue:   return "<continue>";
        case TOKgoto:       return "<goto>";
        default:            assert(0);  return NULL;
    }
}

Expression *UnionExp::copy()
{
    Expression *e = exp();
    //if (e->size > sizeof(u)) printf("%s\n", Token::toChars(e->op));
    assert(e->size <= sizeof(u));
    if (e->op == TOKcantexp)    return CTFEExp::cantexp;
    if (e->op == TOKvoidexp)    return CTFEExp::voidexp;
    if (e->op == TOKbreak)      return CTFEExp::breakexp;
    if (e->op == TOKcontinue)   return CTFEExp::continueexp;
    if (e->op == TOKgoto)       return CTFEExp::gotoexp;
    return e->copy();
}

/************** Aggregate literals (AA/string/array/struct) ******************/

// Given expr, which evaluates to an array/AA/string literal,
// return true if it needs to be copied
bool needToCopyLiteral(Expression *expr)
{
    for (;;)
    {
        switch (expr->op)
        {
            case TOKarrayliteral:
                return ((ArrayLiteralExp *)expr)->ownedByCtfe == OWNEDcode;
            case TOKassocarrayliteral:
                return ((AssocArrayLiteralExp *)expr)->ownedByCtfe == OWNEDcode;
            case TOKstructliteral:
                return ((StructLiteralExp *)expr)->ownedByCtfe == OWNEDcode;
            case TOKstring:
            case TOKthis:
            case TOKvar:
                return false;
            case TOKassign:
                return false;
            case TOKindex:
            case TOKdotvar:
            case TOKslice:
            case TOKcast:
                expr = ((UnaExp *)expr)->e1;
                continue;
            case TOKcat:
                return needToCopyLiteral(((BinExp *)expr)->e1) ||
                    needToCopyLiteral(((BinExp *)expr)->e2);
            case TOKcatass:
                expr = ((BinExp *)expr)->e2;
                continue;
            default:
                return false;
        }
    }
}

Expressions *copyLiteralArray(Expressions *oldelems, Expression *basis = NULL)
{
    if (!oldelems)
        return oldelems;
    CtfeStatus::numArrayAllocs++;
    Expressions *newelems = new Expressions();
    newelems->setDim(oldelems->dim);
    for (size_t i = 0; i < oldelems->dim; i++)
    {
        Expression *el = (*oldelems)[i];
        if (!el)
            el = basis;
        (*newelems)[i] = copyLiteral(el).copy();
    }
    return newelems;
}

// Make a copy of the ArrayLiteral, AALiteral, String, or StructLiteral.
// This value will be used for in-place modification.
UnionExp copyLiteral(Expression *e)
{
    UnionExp ue;
    if (e->op == TOKstring) // syntaxCopy doesn't make a copy for StringExp!
    {
        StringExp *se = (StringExp *)e;
        utf8_t *s = (utf8_t *)mem.xcalloc(se->len + 1, se->sz);
        memcpy(s, se->string, se->len * se->sz);
        new(&ue) StringExp(se->loc, s, se->len);
        StringExp *se2 = (StringExp *)ue.exp();
        se2->committed = se->committed;
        se2->postfix = se->postfix;
        se2->type = se->type;
        se2->sz = se->sz;
        se2->ownedByCtfe = OWNEDctfe;
        return ue;
    }
    if (e->op == TOKarrayliteral)
    {
        ArrayLiteralExp *ale = (ArrayLiteralExp *)e;
        Expressions *elements = copyLiteralArray(ale->elements, ale->basis);

        new(&ue) ArrayLiteralExp(e->loc, e->type, elements);

        ArrayLiteralExp *r = (ArrayLiteralExp *)ue.exp();
        r->ownedByCtfe = OWNEDctfe;
        return ue;
    }
    if (e->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *aae = (AssocArrayLiteralExp *)e;
        new(&ue) AssocArrayLiteralExp(e->loc, copyLiteralArray(aae->keys), copyLiteralArray(aae->values));
        AssocArrayLiteralExp *r = (AssocArrayLiteralExp *)ue.exp();
        r->type = e->type;
        r->ownedByCtfe = OWNEDctfe;
        return ue;
    }
    if (e->op == TOKstructliteral)
    {
        /* syntaxCopy doesn't work for struct literals, because of a nasty special
         * case: block assignment is permitted inside struct literals, eg,
         * an int[4] array can be initialized with a single int.
         */
        StructLiteralExp *sle = (StructLiteralExp *)e;
        Expressions *oldelems = sle->elements;
        Expressions * newelems = new Expressions();
        newelems->setDim(oldelems->dim);
        for (size_t i = 0; i < newelems->dim; i++)
        {
            // We need the struct definition to detect block assignment
            VarDeclaration *v = sle->sd->fields[i];
            Expression *m = (*oldelems)[i];

            // If it is a void assignment, use the default initializer
            if (!m)
                m = voidInitLiteral(v->type, v).copy();

            if (v->type->ty == Tarray || v->type->ty == Taarray)
            {
                // Don't have to copy array references
            }
            else
            {
                // Buzilla 15681: Copy the source element always.
                m = copyLiteral(m).copy();

                // Block assignment from inside struct literals
                if (v->type->ty != m->type->ty && v->type->ty == Tsarray)
                {
                    TypeSArray *tsa = (TypeSArray *)v->type;
                    size_t len = (size_t)tsa->dim->toInteger();
                    UnionExp uex;
                    m = createBlockDuplicatedArrayLiteral(&uex, e->loc, v->type, m, len);
                    if (m == uex.exp())
                        m = uex.copy();
                }
            }
            (*newelems)[i] = m;
        }
        new(&ue) StructLiteralExp(e->loc, sle->sd, newelems, sle->stype);
        StructLiteralExp *r = (StructLiteralExp *)ue.exp();
        r->type = e->type;
        r->ownedByCtfe = OWNEDctfe;
        r->origin = ((StructLiteralExp *)e)->origin;
        return ue;
    }
    if (e->op == TOKfunction || e->op == TOKdelegate ||
        e->op == TOKsymoff || e->op == TOKnull ||
        e->op == TOKvar || e->op == TOKdotvar ||
        e->op == TOKint64 || e->op == TOKfloat64 ||
        e->op == TOKchar || e->op == TOKcomplex80 ||
        e->op == TOKvoid || e->op == TOKvector ||
        e->op == TOKtypeid)
    {
        // Simple value types
        // Keep e1 for DelegateExp and DotVarExp
        new(&ue) UnionExp(e);
        Expression *r = ue.exp();
        r->type = e->type;
        return ue;
    }
    if (e->op == TOKslice)
    {
        SliceExp *se = (SliceExp *)e;
        if (se->type->toBasetype()->ty == Tsarray)
        {
            // same with resolveSlice()
            if (se->e1->op == TOKnull)
            {
                new(&ue) NullExp(se->loc, se->type);
                return ue;
            }
            ue = Slice(se->type, se->e1, se->lwr, se->upr);
            assert(ue.exp()->op == TOKarrayliteral);
            ArrayLiteralExp *r = (ArrayLiteralExp *)ue.exp();
            r->elements = copyLiteralArray(r->elements);
            r->ownedByCtfe = OWNEDctfe;
            return ue;
        }
        else
        {
            // Array slices only do a shallow copy
            new(&ue) SliceExp(e->loc, se->e1, se->lwr, se->upr);
            Expression *r = ue.exp();
            r->type = e->type;
            return ue;
        }
    }
    if (isPointer(e->type))
    {
        // For pointers, we only do a shallow copy.
        if (e->op == TOKaddress)
            new(&ue) AddrExp(e->loc, ((AddrExp *)e)->e1);
        else if (e->op == TOKindex)
            new(&ue) IndexExp(e->loc, ((IndexExp *)e)->e1, ((IndexExp *)e)->e2);
        else if (e->op == TOKdotvar)
        {
            new(&ue) DotVarExp(e->loc, ((DotVarExp *)e)->e1,
                ((DotVarExp *)e)->var, ((DotVarExp *)e)->hasOverloads);
        }
        else
            assert(0);
        Expression *r = ue.exp();
        r->type = e->type;
        return ue;
    }
    if (e->op == TOKclassreference)
    {
        new(&ue) ClassReferenceExp(e->loc, ((ClassReferenceExp *)e)->value, e->type);
        return ue;
    }
    if (e->op == TOKerror)
    {
        new(&ue) UnionExp(e);
        return ue;
    }
    e->error("CTFE internal error: literal %s", e->toChars());
    assert(0);
    return ue;
}

/* Deal with type painting.
 * Type painting is a major nuisance: we can't just set
 * e->type = type, because that would change the original literal.
 * But, we can't simply copy the literal either, because that would change
 * the values of any pointers.
 */
Expression *paintTypeOntoLiteral(Type *type, Expression *lit)
{
    if (lit->type->equals(type))
        return lit;
    return paintTypeOntoLiteralCopy(type, lit).copy();
}

Expression *paintTypeOntoLiteral(UnionExp *pue, Type *type, Expression *lit)
{
    if (lit->type->equals(type))
        return lit;
    *pue = paintTypeOntoLiteralCopy(type, lit);
    return pue->exp();
}

UnionExp paintTypeOntoLiteralCopy(Type *type, Expression *lit)
{
    UnionExp ue;

    if (lit->type->equals(type))
    {
        new(&ue) UnionExp(lit);
        return ue;
    }

    // If it is a cast to inout, retain the original type of the referenced part.
    if (type->hasWild() && type->hasPointers())
    {
        new(&ue) UnionExp(lit);
        ue.exp()->type = type;
        return ue;
    }

    if (lit->op == TOKslice)
    {
        SliceExp *se = (SliceExp *)lit;
        new(&ue) SliceExp(lit->loc, se->e1, se->lwr, se->upr);
    }
    else if (lit->op == TOKindex)
    {
        IndexExp *ie = (IndexExp *)lit;
        new(&ue) IndexExp(lit->loc, ie->e1, ie->e2);
    }
    else if (lit->op == TOKarrayliteral)
    {
        new(&ue) SliceExp(lit->loc, lit,
            new IntegerExp(Loc(), 0, Type::tsize_t), ArrayLength(Type::tsize_t, lit).copy());
    }
    else if (lit->op == TOKstring)
    {
        // For strings, we need to introduce another level of indirection
        new(&ue) SliceExp(lit->loc, lit,
            new IntegerExp(Loc(), 0, Type::tsize_t), ArrayLength(Type::tsize_t, lit).copy());
    }
    else if (lit->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *aae = (AssocArrayLiteralExp *)lit;
        // TODO: we should be creating a reference to this AAExp, not
        // just a ref to the keys and values.
        OwnedBy wasOwned = aae->ownedByCtfe;
        new(&ue) AssocArrayLiteralExp(lit->loc, aae->keys, aae->values);
        aae = (AssocArrayLiteralExp *)ue.exp();
        aae->ownedByCtfe = wasOwned;
    }
    else
    {
        // Can't type paint from struct to struct*; this needs another
        // level of indirection
        if (lit->op == TOKstructliteral && isPointer(type))
            lit->error("CTFE internal error: painting %s", type->toChars());
        ue = copyLiteral(lit);
    }
    ue.exp()->type = type;
    return ue;
}

/*************************************
 * If e is a SliceExp, constant fold it.
 * Params:
 *      e = expression to resolve
 *      pue = if not null, store resulting expression here
 * Returns:
 *      resulting expression
 */
Expression *resolveSlice(Expression *e, UnionExp *pue)
{
    if (e->op != TOKslice)
        return e;
    SliceExp *se = (SliceExp *)e;
    if (se->e1->op == TOKnull)
        return se->e1;
    if (pue)
    {
        *pue = Slice(e->type, se->e1, se->lwr, se->upr);
        return pue->exp();
    }
    else
        return Slice(e->type, se->e1, se->lwr, se->upr).copy();
}

/* Determine the array length, without interpreting it.
 * e must be an array literal, or a slice
 * It's very wasteful to resolve the slice when we only
 * need the length.
 */
uinteger_t resolveArrayLength(Expression *e)
{
    if (e->op == TOKvector)
        return ((VectorExp *)e)->dim;

    if (e->op == TOKnull)
        return 0;
    if (e->op == TOKslice)
    {
        uinteger_t ilo = ((SliceExp *)e)->lwr->toInteger();
        uinteger_t iup = ((SliceExp *)e)->upr->toInteger();
        return iup - ilo;
    }
    if (e->op == TOKstring)
    {
        return ((StringExp *)e)->len;
    }
    if (e->op == TOKarrayliteral)
    {
        ArrayLiteralExp *ale = (ArrayLiteralExp *)e;
        return ale->elements ? ale->elements->dim : 0;
    }
    if (e->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *ale = (AssocArrayLiteralExp *)e;
        return ale->keys->dim;
    }
    assert(0);
    return 0;
}

/******************************
 * Helper for NewExp
 * Create an array literal consisting of 'elem' duplicated 'dim' times.
 * Params:
 *      pue = where to store result
 *      loc = source location where the interpretation occurs
 *      type = target type of the result
 *      elem = the source of array element, it will be owned by the result
 *      dim = element number of the result
 * Returns:
 *      Constructed ArrayLiteralExp
 */
ArrayLiteralExp *createBlockDuplicatedArrayLiteral(UnionExp *pue, Loc loc, Type *type,
        Expression *elem, size_t dim)
{
    if (type->ty == Tsarray && type->nextOf()->ty == Tsarray && elem->type->ty != Tsarray)
    {
        // If it is a multidimensional array literal, do it recursively
        TypeSArray *tsa = (TypeSArray *)type->nextOf();
        size_t len = (size_t)tsa->dim->toInteger();
        UnionExp ue;
        elem = createBlockDuplicatedArrayLiteral(&ue, loc, type->nextOf(), elem, len);
        if (elem == ue.exp())
            elem = ue.copy();
    }

    // Buzilla 15681
    Type *tb = elem->type->toBasetype();
    const bool mustCopy = tb->ty == Tstruct || tb->ty == Tsarray;

    Expressions *elements = new Expressions();
    elements->setDim(dim);
    for (size_t i = 0; i < dim; i++)
    {
        (*elements)[i] = mustCopy ? copyLiteral(elem).copy() : elem;
    }
    new(pue) ArrayLiteralExp(loc, type, elements);
    ArrayLiteralExp *ale = (ArrayLiteralExp *)pue->exp();
    ale->ownedByCtfe = OWNEDctfe;
    return ale;
}

/******************************
 * Helper for NewExp
 * Create a string literal consisting of 'value' duplicated 'dim' times.
 */
StringExp *createBlockDuplicatedStringLiteral(UnionExp *pue, Loc loc, Type *type,
        unsigned value, size_t dim, unsigned char sz)
{
    utf8_t *s = (utf8_t *)mem.xcalloc(dim + 1, sz);
    for (size_t elemi = 0; elemi < dim; ++elemi)
    {
        switch (sz)
        {
            case 1:     s[elemi] = (utf8_t)value; break;
            case 2:     ((unsigned short *)s)[elemi] = (unsigned short)value; break;
            case 4:     ((unsigned *)s)[elemi] = value; break;
            default:    assert(0);
        }
    }
    new(pue) StringExp(loc, s, dim);
    StringExp *se = (StringExp *)pue->exp();
    se->type = type;
    se->sz = sz;
    se->committed = true;
    se->ownedByCtfe = OWNEDctfe;
    return se;
}

// Return true if t is an AA
bool isAssocArray(Type *t)
{
    t = t->toBasetype();
    if (t->ty == Taarray)
        return true;
    return false;
}

// Given a template AA type, extract the corresponding built-in AA type
TypeAArray *toBuiltinAAType(Type *t)
{
    t = t->toBasetype();
    if (t->ty == Taarray)
        return (TypeAArray *)t;
    assert(0);
    return NULL;
}

/************** TypeInfo operations ************************************/

// Return true if type is TypeInfo_Class
bool isTypeInfo_Class(Type *type)
{
    return type->ty == Tclass &&
        (Type::dtypeinfo == ((TypeClass *)type)->sym ||
         Type::dtypeinfo->isBaseOf(((TypeClass *)type)->sym, NULL));
}

/************** Pointer operations ************************************/

// Return true if t is a pointer (not a function pointer)
bool isPointer(Type *t)
{
    Type * tb = t->toBasetype();
    return tb->ty == Tpointer && tb->nextOf()->ty != Tfunction;
}

// For CTFE only. Returns true if 'e' is true or a non-null pointer.
bool isTrueBool(Expression *e)
{
    return e->isBool(true) ||
           ((e->type->ty == Tpointer || e->type->ty == Tclass) && e->op != TOKnull);
}

/* Is it safe to convert from srcPointee* to destPointee* ?
 * srcPointee is the genuine type (never void).
 * destPointee may be void.
 */
bool isSafePointerCast(Type *srcPointee, Type *destPointee)
{
    // It's safe to cast S** to D** if it's OK to cast S* to D*
    while (srcPointee->ty == Tpointer && destPointee->ty == Tpointer)
    {
        srcPointee = srcPointee->nextOf();
        destPointee = destPointee->nextOf();
    }

    // It's OK if both are the same (modulo const)
    if (srcPointee->constConv(destPointee))
        return true;

    // It's OK if function pointers differ only in safe/pure/nothrow
    if (srcPointee->ty == Tfunction && destPointee->ty == Tfunction)
        return srcPointee->covariant(destPointee) == 1;

    // it's OK to cast to void*
    if (destPointee->ty == Tvoid)
        return true;

    // It's OK to cast from V[K] to void*
    if (srcPointee->ty == Taarray && destPointee == Type::tvoidptr)
        return true;

    // It's OK if they are the same size (static array of) integers, eg:
    //     int*     --> uint*
    //     int[5][] --> uint[5][]
    if (srcPointee->ty == Tsarray && destPointee->ty == Tsarray)
    {
        if (srcPointee->size() != destPointee->size())
            return false;
        srcPointee = srcPointee->baseElemOf();
        destPointee = destPointee->baseElemOf();
    }
    return srcPointee->isintegral() &&
           destPointee->isintegral() &&
           srcPointee->size() == destPointee->size();
}

Expression *getAggregateFromPointer(Expression *e, dinteger_t *ofs)
{
    *ofs = 0;
    if (e->op == TOKaddress)
        e = ((AddrExp *)e)->e1;
    if (e->op == TOKsymoff)
        *ofs = ((SymOffExp *)e)->offset;
    if (e->op == TOKdotvar)
    {
        Expression *ex = ((DotVarExp *)e)->e1;
        VarDeclaration *v = ((DotVarExp *)e)->var->isVarDeclaration();
        assert(v);
        StructLiteralExp *se = ex->op == TOKclassreference ? ((ClassReferenceExp *)ex)->value : (StructLiteralExp *)ex;
        // We can't use getField, because it makes a copy
        unsigned i;
        if (ex->op == TOKclassreference)
            i = ((ClassReferenceExp *)ex)->getFieldIndex(e->type, v->offset);
        else
            i = se->getFieldIndex(e->type, v->offset);
        e = (*se->elements)[i];
    }
    if (e->op == TOKindex)
    {
        IndexExp *ie = (IndexExp *)e;
        // Note that each AA element is part of its own memory block
        if ((ie->e1->type->ty == Tarray ||
             ie->e1->type->ty == Tsarray ||
             ie->e1->op == TOKstring ||
             ie->e1->op == TOKarrayliteral) &&
            ie->e2->op == TOKint64)
        {
            *ofs = ie->e2->toInteger();
            return ie->e1;
        }
    }
    if (e->op == TOKslice && e->type->toBasetype()->ty == Tsarray)
    {
        SliceExp *se = (SliceExp *)e;
        if ((se->e1->type->ty == Tarray ||
             se->e1->type->ty == Tsarray ||
             se->e1->op == TOKstring ||
             se->e1->op == TOKarrayliteral) &&
            se->lwr->op == TOKint64)
        {
            *ofs = se->lwr->toInteger();
            return se->e1;
        }
    }
    return e;
}

/** Return true if agg1 and agg2 are pointers to the same memory block
*/
bool pointToSameMemoryBlock(Expression *agg1, Expression *agg2)
{
    if (agg1 == agg2)
        return true;

    // For integers cast to pointers, we regard them as non-comparable
    // unless they are identical. (This may be overly strict).
    if (agg1->op == TOKint64 && agg2->op == TOKint64 &&
        agg1->toInteger() == agg2->toInteger())
    {
        return true;
    }

    // Note that type painting can occur with VarExp, so we
    // must compare the variables being pointed to.
    if (agg1->op == TOKvar && agg2->op == TOKvar &&
        ((VarExp *)agg1)->var == ((VarExp *)agg2)->var)
    {
        return true;
    }
    if (agg1->op == TOKsymoff && agg2->op == TOKsymoff &&
        ((SymOffExp *)agg1)->var == ((SymOffExp *)agg2)->var)
    {
        return true;
    }

    return false;
}

// return e1 - e2 as an integer, or error if not possible
UnionExp pointerDifference(Loc loc, Type *type, Expression *e1, Expression *e2)
{
    UnionExp ue;
    dinteger_t ofs1, ofs2;
    Expression *agg1 = getAggregateFromPointer(e1, &ofs1);
    Expression *agg2 = getAggregateFromPointer(e2, &ofs2);
    if (agg1 == agg2)
    {
        Type *pointee = ((TypePointer *)agg1->type)->next;
        dinteger_t sz = pointee->size();
        new(&ue) IntegerExp(loc, (ofs1 - ofs2) * sz, type);
    }
    else if (agg1->op == TOKstring && agg2->op == TOKstring)
    {
        if (((StringExp *)agg1)->string == ((StringExp *)agg2)->string)
        {
            Type *pointee = ((TypePointer *)agg1->type)->next;
            dinteger_t sz = pointee->size();
            new(&ue) IntegerExp(loc, (ofs1 - ofs2) * sz, type);
        }
    }
    else if (agg1->op == TOKsymoff && agg2->op == TOKsymoff &&
             ((SymOffExp *)agg1)->var == ((SymOffExp *)agg2)->var)
    {
        new(&ue) IntegerExp(loc, ofs1 - ofs2, type);
    }
    else
    {
        error(loc, "%s - %s cannot be interpreted at compile time: cannot subtract "
            "pointers to two different memory blocks",
            e1->toChars(), e2->toChars());
        new(&ue) CTFEExp(TOKcantexp);
    }
    return ue;
}

// Return eptr op e2, where eptr is a pointer, e2 is an integer,
// and op is TOKadd or TOKmin
UnionExp pointerArithmetic(Loc loc, TOK op, Type *type,
    Expression *eptr, Expression *e2)
{
    UnionExp ue;

    if (eptr->type->nextOf()->ty == Tvoid)
    {
        error(loc, "cannot perform arithmetic on void* pointers at compile time");
      Lcant:
        new(&ue) CTFEExp(TOKcantexp);
        return ue;
    }

    dinteger_t ofs1;
    if (eptr->op == TOKaddress)
        eptr = ((AddrExp *)eptr)->e1;
    Expression *agg1 = getAggregateFromPointer(eptr, &ofs1);
    if (agg1->op == TOKsymoff)
    {
        if (((SymOffExp *)agg1)->var->type->ty != Tsarray)
        {
            error(loc, "cannot perform pointer arithmetic on arrays of unknown length at compile time");
            goto Lcant;
        }
    }
    else if (agg1->op != TOKstring && agg1->op != TOKarrayliteral)
    {
        error(loc, "cannot perform pointer arithmetic on non-arrays at compile time");
        goto Lcant;
    }
    dinteger_t ofs2 = e2->toInteger();

    Type *pointee = ((TypeNext *)agg1->type->toBasetype())->next;
    dinteger_t sz = pointee->size();

    sinteger_t indx;
    dinteger_t len;
    if (agg1->op == TOKsymoff)
    {
        indx = ofs1 / sz;
        len = ((TypeSArray *)((SymOffExp *)agg1)->var->type)->dim->toInteger();
    }
    else
    {
        Expression *dollar = ArrayLength(Type::tsize_t, agg1).copy();
        assert(!CTFEExp::isCantExp(dollar));
        indx = ofs1;
        len = dollar->toInteger();
    }
    if (op == TOKadd || op == TOKaddass || op == TOKplusplus)
        indx += ofs2 / sz;
    else if (op == TOKmin || op == TOKminass || op == TOKminusminus)
        indx -= ofs2 / sz;
    else
    {
        error(loc, "CTFE internal error: bad pointer operation");
        goto Lcant;
    }

    if (indx < 0 || len < (dinteger_t)indx)
    {
        error(loc, "cannot assign pointer to index %lld inside memory block [0..%lld]", (ulonglong)indx, (ulonglong)len);
        goto Lcant;
    }

    if (agg1->op == TOKsymoff)
    {
        new(&ue) SymOffExp(loc, ((SymOffExp *)agg1)->var, indx * sz);
        SymOffExp *se = (SymOffExp *)ue.exp();
        se->type = type;
        return ue;
    }

    if (agg1->op != TOKarrayliteral && agg1->op != TOKstring)
    {
        error(loc, "CTFE internal error: pointer arithmetic %s", agg1->toChars());
        goto Lcant;
    }

    if (eptr->type->toBasetype()->ty == Tsarray)
    {
        dinteger_t dim = ((TypeSArray *)eptr->type->toBasetype())->dim->toInteger();

        // Create a CTFE pointer &agg1[indx .. indx+dim]
        SliceExp *se = new SliceExp(loc, agg1,
            new IntegerExp(loc, indx,       Type::tsize_t),
            new IntegerExp(loc, indx + dim, Type::tsize_t));
        se->type = type->toBasetype()->nextOf();
        new(&ue) AddrExp(loc, se);
        ue.exp()->type = type;
        return ue;
    }

    // Create a CTFE pointer &agg1[indx]
    IntegerExp *ofs = new IntegerExp(loc, indx, Type::tsize_t);
    Expression *ie = new IndexExp(loc, agg1, ofs);
    ie->type = type->toBasetype()->nextOf();    // Bugzilla 13992
    new(&ue) AddrExp(loc, ie);
    ue.exp()->type = type;
    return ue;
}

// Return 1 if true, 0 if false
// -1 if comparison is illegal because they point to non-comparable memory blocks
int comparePointers(TOK op, Expression *agg1, dinteger_t ofs1, Expression *agg2, dinteger_t ofs2)
{
    if (pointToSameMemoryBlock(agg1, agg2))
    {
        int n;
        switch (op)
        {
        case TOKlt:          n = (ofs1 <  ofs2); break;
        case TOKle:          n = (ofs1 <= ofs2); break;
        case TOKgt:          n = (ofs1 >  ofs2); break;
        case TOKge:          n = (ofs1 >= ofs2); break;
        case TOKidentity:
        case TOKequal:       n = (ofs1 == ofs2); break;
        case TOKnotidentity:
        case TOKnotequal:    n = (ofs1 != ofs2); break;
        default:
            assert(0);
        }
        return n;
    }
    bool null1 = (agg1->op == TOKnull);
    bool null2 = (agg2->op == TOKnull);

    int cmp;
    if (null1 || null2)
    {
        switch (op)
        {
        case TOKlt:   cmp =  null1 && !null2;   break;
        case TOKgt:   cmp = !null1 &&  null2;   break;
        case TOKle:   cmp = null1;              break;
        case TOKge:   cmp = null2;              break;
        case TOKidentity:
        case TOKequal:
        case TOKnotidentity: // 'cmp' gets inverted below
        case TOKnotequal:
            cmp = (null1 == null2);
            break;
        default:
            assert(0);
        }
    }
    else
    {
        switch (op)
        {
        case TOKidentity:
        case TOKequal:
        case TOKnotidentity: // 'cmp' gets inverted below
        case TOKnotequal:
            cmp = 0;
            break;
        default:
            return -1; // memory blocks are different
        }
    }
    if (op == TOKnotidentity || op == TOKnotequal)
        cmp ^= 1;
    return cmp;
}

// True if conversion from type 'from' to 'to' involves a reinterpret_cast
// floating point -> integer or integer -> floating point
bool isFloatIntPaint(Type *to, Type *from)
{
    return from->size() == to->size() &&
           ((from->isintegral() && to->isfloating()) ||
            (from->isfloating() && to->isintegral()));
}

// Reinterpret float/int value 'fromVal' as a float/integer of type 'to'.
Expression *paintFloatInt(UnionExp *pue, Expression *fromVal, Type *to)
{
    if (exceptionOrCantInterpret(fromVal))
        return fromVal;

    assert(to->size() == 4 || to->size() == 8);
    return Compiler::paintAsType(pue, fromVal, to);
}

/******** Constant folding, with support for CTFE ***************************/

/// Return true if non-pointer expression e can be compared
/// with >,is, ==, etc, using ctfeCmp, ctfeEqual, ctfeIdentity
bool isCtfeComparable(Expression *e)
{
    if (e->op == TOKslice)
        e = ((SliceExp *)e)->e1;

    if (e->isConst() != 1)
    {
        if (e->op == TOKnull ||
            e->op == TOKstring ||
            e->op == TOKfunction ||
            e->op == TOKdelegate ||
            e->op == TOKarrayliteral ||
            e->op == TOKstructliteral ||
            e->op == TOKassocarrayliteral ||
            e->op == TOKclassreference)
        {
            return true;
        }
        // Bugzilla 14123: TypeInfo object is comparable in CTFE
        if (e->op == TOKtypeid)
            return true;

        return false;
    }
    return true;
}

/// Map TOK comparison ops
template <typename N>
static bool numCmp(TOK op, N n1, N n2)
{
    switch (op)
    {
        case TOKlt:
            return n1 <  n2;
        case TOKle:
            return n1 <= n2;
        case TOKgt:
            return n1 >  n2;
        case TOKge:
            return n1 >= n2;

        default:
            assert(0);
    }
}

/// Returns cmp OP 0; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int specificCmp(TOK op, int rawCmp)
{
    return numCmp<int>(op, rawCmp, 0);
}

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int intUnsignedCmp(TOK op, dinteger_t n1, dinteger_t n2)
{
    return numCmp<dinteger_t>(op, n1, n2);
}

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int intSignedCmp(TOK op, sinteger_t n1, sinteger_t n2)
{
    return numCmp<sinteger_t>(op, n1, n2);
}

/// Returns e1 OP e2; where OP is ==, !=, <, >=, etc. Result is 0 or 1
int realCmp(TOK op, real_t r1, real_t r2)
{
    // Don't rely on compiler, handle NAN arguments separately
    if (CTFloat::isNaN(r1) || CTFloat::isNaN(r2)) // if unordered
    {
        switch (op)
        {
            case TOKlt:
            case TOKle:
            case TOKgt:
            case TOKge:
                return 0;

            default:
                assert(0);
        }
    }
    else
    {
        return numCmp<real_t>(op, r1, r2);
    }
}

int ctfeRawCmp(Loc loc, Expression *e1, Expression *e2);

/* Conceptually the same as memcmp(e1, e2).
 * e1 and e2 may be strings, arrayliterals, or slices.
 * For string types, return <0 if e1 < e2, 0 if e1==e2, >0 if e1 > e2.
 * For all other types, return 0 if e1 == e2, !=0 if e1 != e2.
 */
int ctfeCmpArrays(Loc loc, Expression *e1, Expression *e2, uinteger_t len)
{
    // Resolve slices, if necessary
    uinteger_t lo1 = 0;
    uinteger_t lo2 = 0;

    Expression *x = e1;
    if (x->op == TOKslice)
    {
        lo1 = ((SliceExp *)x)->lwr->toInteger();
        x = ((SliceExp *)x)->e1;
    }
    StringExp *se1 = (x->op == TOKstring) ? (StringExp *)x : NULL;
    ArrayLiteralExp *ae1 = (x->op == TOKarrayliteral) ? (ArrayLiteralExp *)x : NULL;

    x = e2;
    if (x->op == TOKslice)
    {
        lo2 = ((SliceExp *)x)->lwr->toInteger();
        x = ((SliceExp *)x)->e1;
    }
    StringExp *se2 = (x->op == TOKstring) ? (StringExp *)x : NULL;
    ArrayLiteralExp *ae2 = (x->op == TOKarrayliteral) ? (ArrayLiteralExp *)x : NULL;

    // Now both must be either TOKarrayliteral or TOKstring
    if (se1 && se2)
        return sliceCmpStringWithString(se1, se2, (size_t)lo1, (size_t)lo2, (size_t)len);
    if (se1 && ae2)
        return sliceCmpStringWithArray(se1, ae2, (size_t)lo1, (size_t)lo2, (size_t)len);
    if (se2 && ae1)
        return -sliceCmpStringWithArray(se2, ae1, (size_t)lo2, (size_t)lo1, (size_t)len);

    assert (ae1 && ae2);
    // Comparing two array literals. This case is potentially recursive.
    // If they aren't strings, we just need an equality check rather than
    // a full cmp.
    bool needCmp = ae1->type->nextOf()->isintegral();
    for (size_t i = 0; i < (size_t)len; i++)
    {
        Expression *ee1 = (*ae1->elements)[(size_t)(lo1 + i)];
        Expression *ee2 = (*ae2->elements)[(size_t)(lo2 + i)];
        if (needCmp)
        {
            sinteger_t c = ee1->toInteger() - ee2->toInteger();
            if (c > 0)
                return 1;
            if (c < 0)
                return -1;
        }
        else
        {
            if (ctfeRawCmp(loc, ee1, ee2))
                return 1;
        }
    }
    return 0;
}

/* Given a delegate expression e, return .funcptr.
 * If e is NullExp, return NULL.
 */
FuncDeclaration *funcptrOf(Expression *e)
{
    assert(e->type->ty == Tdelegate);

    if (e->op == TOKdelegate)
        return ((DelegateExp *)e)->func;
    if (e->op == TOKfunction)
        return ((FuncExp *)e)->fd;
    assert(e->op == TOKnull);
    return NULL;
}

bool isArray(Expression *e)
{
    return e->op == TOKarrayliteral || e->op == TOKstring ||
           e->op == TOKslice || e->op == TOKnull;
}

/* For strings, return <0 if e1 < e2, 0 if e1==e2, >0 if e1 > e2.
 * For all other types, return 0 if e1 == e2, !=0 if e1 != e2.
 */
int ctfeRawCmp(Loc loc, Expression *e1, Expression *e2)
{
    if (e1->op == TOKclassreference || e2->op == TOKclassreference)
    {
        if (e1->op == TOKclassreference && e2->op == TOKclassreference &&
            ((ClassReferenceExp *)e1)->value == ((ClassReferenceExp *)e2)->value)
            return 0;
        return 1;
    }
    if (e1->op == TOKtypeid && e2->op == TOKtypeid)
    {
        // printf("e1: %s\n", e1->toChars());
        // printf("e2: %s\n", e2->toChars());
        Type *t1 = isType(((TypeidExp *)e1)->obj);
        Type *t2 = isType(((TypeidExp *)e2)->obj);
        assert(t1);
        assert(t2);
        return t1 != t2;
    }

    // null == null, regardless of type

    if (e1->op == TOKnull && e2->op == TOKnull)
        return 0;

    if (e1->type->ty == Tpointer && e2->type->ty == Tpointer)
    {
        // Can only be an equality test.

        dinteger_t ofs1, ofs2;
        Expression *agg1 = getAggregateFromPointer(e1, &ofs1);
        Expression *agg2 = getAggregateFromPointer(e2, &ofs2);
        if ((agg1 == agg2) || (agg1->op == TOKvar && agg2->op == TOKvar &&
            ((VarExp *)agg1)->var == ((VarExp *)agg2)->var))
        {
            if (ofs1 == ofs2)
                return 0;
        }
        return 1;
    }
    if (e1->type->ty == Tdelegate && e2->type->ty == Tdelegate)
    {
        // If .funcptr isn't the same, they are not equal

        if (funcptrOf(e1) != funcptrOf(e2))
            return 1;

        // If both are delegate literals, assume they have the
        // same closure pointer. TODO: We don't support closures yet!
        if (e1->op == TOKfunction && e2->op == TOKfunction)
            return 0;
        assert(e1->op == TOKdelegate && e2->op == TOKdelegate);

        // Same .funcptr. Do they have the same .ptr?
        Expression * ptr1 = ((DelegateExp *)e1)->e1;
        Expression * ptr2 = ((DelegateExp *)e2)->e1;

        dinteger_t ofs1, ofs2;
        Expression *agg1 = getAggregateFromPointer(ptr1, &ofs1);
        Expression *agg2 = getAggregateFromPointer(ptr2, &ofs2);
        // If they are TOKvar, it means they are FuncDeclarations
        if ((agg1 == agg2 && ofs1 == ofs2) ||
            (agg1->op == TOKvar && agg2->op == TOKvar &&
             ((VarExp *)agg1)->var == ((VarExp *)agg2)->var))
        {
            return 0;
        }
        return 1;
    }
    if (isArray(e1) && isArray(e2))
    {
        uinteger_t len1 = resolveArrayLength(e1);
        uinteger_t len2 = resolveArrayLength(e2);
        // workaround for dmc optimizer bug calculating wrong len for
        // uinteger_t len = (len1 < len2 ? len1 : len2);
        // if (len == 0) ...
        if (len1 > 0 && len2 > 0)
        {
            uinteger_t len = (len1 < len2 ? len1 : len2);
            int res = ctfeCmpArrays(loc, e1, e2, len);
            if (res != 0)
                return res;
        }
        return (int)(len1 - len2);
    }
    if (e1->type->isintegral())
    {
        return e1->toInteger() != e2->toInteger();
    }
    real_t r1;
    real_t r2;
    if (e1->type->isreal())
    {
        r1 = e1->toReal();
        r2 = e2->toReal();
        goto L1;
    }
    else if (e1->type->isimaginary())
    {
        r1 = e1->toImaginary();
        r2 = e2->toImaginary();
     L1:
        if (CTFloat::isNaN(r1) || CTFloat::isNaN(r2)) // if unordered
        {
            return 1;
        }
        else
        {
            return (r1 != r2);
        }
    }
    else if (e1->type->iscomplex())
    {
        return e1->toComplex() != e2->toComplex();
    }

    if (e1->op == TOKstructliteral && e2->op == TOKstructliteral)
    {
        StructLiteralExp *es1 = (StructLiteralExp *)e1;
        StructLiteralExp *es2 = (StructLiteralExp *)e2;
        // For structs, we only need to return 0 or 1 (< and > aren't legal).

        if (es1->sd != es2->sd)
            return 1;
        else if ((!es1->elements || !es1->elements->dim) &&
            (!es2->elements || !es2->elements->dim))
            return 0;            // both arrays are empty
        else if (!es1->elements || !es2->elements)
            return 1;
        else if (es1->elements->dim != es2->elements->dim)
            return 1;
        else
        {
            for (size_t i = 0; i < es1->elements->dim; i++)
            {
                Expression *ee1 = (*es1->elements)[i];
                Expression *ee2 = (*es2->elements)[i];

                if (ee1 == ee2)
                    continue;
                if (!ee1 || !ee2)
                   return 1;
                int cmp = ctfeRawCmp(loc, ee1, ee2);
                if (cmp)
                    return 1;
            }
            return 0;   // All elements are equal
        }
    }
    if (e1->op == TOKassocarrayliteral && e2->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *es1 = (AssocArrayLiteralExp *)e1;
        AssocArrayLiteralExp *es2 = (AssocArrayLiteralExp *)e2;

        size_t dim = es1->keys->dim;
        if (es2->keys->dim != dim)
            return 1;

        bool *used = (bool *)mem.xmalloc(sizeof(bool) * dim);
        memset(used, 0, sizeof(bool) * dim);

        for (size_t i = 0; i < dim; ++i)
        {
            Expression *k1 = (*es1->keys)[i];
            Expression *v1 = (*es1->values)[i];
            Expression *v2 = NULL;
            for (size_t j = 0; j < dim; ++j)
            {
                if (used[j])
                    continue;
                Expression *k2 = (*es2->keys)[j];

                if (ctfeRawCmp(loc, k1, k2))
                    continue;
                used[j] = true;
                v2 = (*es2->values)[j];
                break;
            }
            if (!v2 || ctfeRawCmp(loc, v1, v2))
            {
                mem.xfree(used);
                return 1;
            }
        }
        mem.xfree(used);
        return 0;
    }
    error(loc, "CTFE internal error: bad compare of `%s` and `%s`", e1->toChars(), e2->toChars());
    assert(0);
    return 0;
}

/// Evaluate ==, !=.  Resolves slices before comparing. Returns 0 or 1
int ctfeEqual(Loc loc, TOK op, Expression *e1, Expression *e2)
{
    int cmp = !ctfeRawCmp(loc, e1, e2);
    if (op == TOKnotequal)
        cmp ^= 1;
    return cmp;
}

/// Evaluate is, !is.  Resolves slices before comparing. Returns 0 or 1
int ctfeIdentity(Loc loc, TOK op, Expression *e1, Expression *e2)
{
    //printf("ctfeIdentity op = '%s', e1 = %s %s, e2 = %s %s\n", Token::toChars(op),
    //    Token::toChars(e1->op), e1->toChars(), Token::toChars(e2->op), e1->toChars());
    int cmp;
    if (e1->op == TOKnull)
    {
        cmp = (e2->op == TOKnull);
    }
    else if (e2->op == TOKnull)
    {
        cmp = 0;
    }
    else if (e1->op == TOKsymoff && e2->op == TOKsymoff)
    {
        SymOffExp *es1 = (SymOffExp *)e1;
        SymOffExp *es2 = (SymOffExp *)e2;
        cmp = (es1->var == es2->var && es1->offset == es2->offset);
    }
    else if (e1->type->isreal())
        cmp = RealEquals(e1->toReal(), e2->toReal());
    else if (e1->type->isimaginary())
        cmp = RealEquals(e1->toImaginary(), e2->toImaginary());
    else if (e1->type->iscomplex())
    {
        complex_t v1 = e1->toComplex();
        complex_t v2 = e2->toComplex();
        cmp = RealEquals(creall(v1), creall(v2)) &&
                 RealEquals(cimagl(v1), cimagl(v1));
    }
    else
        cmp = !ctfeRawCmp(loc, e1, e2);

    if (op == TOKnotidentity || op == TOKnotequal)
        cmp ^= 1;
    return cmp;
}

/// Evaluate >,<=, etc. Resolves slices before comparing. Returns 0 or 1
int ctfeCmp(Loc loc, TOK op, Expression *e1, Expression *e2)
{
    Type *t1 = e1->type->toBasetype();
    Type *t2 = e2->type->toBasetype();

    if (t1->isString() && t2->isString())
        return specificCmp(op, ctfeRawCmp(loc, e1, e2));
    else if (t1->isreal())
        return realCmp(op, e1->toReal(), e2->toReal());
    else if (t1->isimaginary())
        return realCmp(op, e1->toImaginary(), e2->toImaginary());
    else if (t1->isunsigned() || t2->isunsigned())
        return intUnsignedCmp(op, e1->toInteger(), e2->toInteger());
    else
        return intSignedCmp(op, e1->toInteger(), e2->toInteger());
}

UnionExp ctfeCat(Loc loc, Type *type, Expression *e1, Expression *e2)
{
    Type *t1 = e1->type->toBasetype();
    Type *t2 = e2->type->toBasetype();
    UnionExp ue;
    if (e2->op == TOKstring && e1->op == TOKarrayliteral &&
        t1->nextOf()->isintegral())
    {
        // [chars] ~ string => string (only valid for CTFE)
        StringExp *es1 = (StringExp *)e2;
        ArrayLiteralExp *es2 = (ArrayLiteralExp *)e1;
        size_t len = es1->len + es2->elements->dim;
        unsigned char sz = es1->sz;

        void *s = mem.xmalloc((len + 1) * sz);
        memcpy((char *)s + sz * es2->elements->dim, es1->string, es1->len * sz);
        for (size_t i = 0; i < es2->elements->dim; i++)
        {
            Expression *es2e = (*es2->elements)[i];
            if (es2e->op != TOKint64)
            {
                new(&ue) CTFEExp(TOKcantexp);
                return ue;
            }
            dinteger_t v = es2e->toInteger();
            Port::valcpy((utf8_t *)s + i * sz, v, sz);
        }

        // Add terminating 0
        memset((utf8_t *)s + len * sz, 0, sz);

        new(&ue) StringExp(loc, s, len);
        StringExp *es = (StringExp *)ue.exp();
        es->sz = sz;
        es->committed = 0;
        es->type = type;
        return ue;
    }
    if (e1->op == TOKstring && e2->op == TOKarrayliteral &&
        t2->nextOf()->isintegral())
    {
        // string ~ [chars] => string (only valid for CTFE)
        // Concatenate the strings
        StringExp *es1 = (StringExp *)e1;
        ArrayLiteralExp *es2 = (ArrayLiteralExp *)e2;
        size_t len = es1->len + es2->elements->dim;
        unsigned char sz = es1->sz;

        void *s = mem.xmalloc((len + 1) * sz);
        memcpy(s, es1->string, es1->len * sz);
        for (size_t i = 0; i < es2->elements->dim; i++)
        {
            Expression *es2e = (*es2->elements)[i];
            if (es2e->op != TOKint64)
            {
                new(&ue) CTFEExp(TOKcantexp);
                return ue;
            }
            dinteger_t v = es2e->toInteger();
            Port::valcpy((utf8_t *)s + (es1->len + i) * sz, v, sz);
        }

        // Add terminating 0
        memset((utf8_t *)s + len * sz, 0, sz);

        new(&ue) StringExp(loc, s, len);
        StringExp *es = (StringExp *)ue.exp();
        es->sz = sz;
        es->committed = 0; //es1->committed;
        es->type = type;
        return ue;
    }
    if (e1->op == TOKarrayliteral && e2->op == TOKarrayliteral &&
        t1->nextOf()->equals(t2->nextOf()))
    {
        //  [ e1 ] ~ [ e2 ] ---> [ e1, e2 ]
        ArrayLiteralExp *es1 = (ArrayLiteralExp *)e1;
        ArrayLiteralExp *es2 = (ArrayLiteralExp *)e2;

        new(&ue) ArrayLiteralExp(es1->loc, type, copyLiteralArray(es1->elements));
        es1 = (ArrayLiteralExp *)ue.exp();
        es1->elements->insert(es1->elements->dim, copyLiteralArray(es2->elements));
        return ue;
    }
    if (e1->op == TOKarrayliteral && e2->op == TOKnull &&
        t1->nextOf()->equals(t2->nextOf()))
    {
        //  [ e1 ] ~ null ----> [ e1 ].dup
        ue = paintTypeOntoLiteralCopy(type, copyLiteral(e1).copy());
        return ue;
    }
    if (e1->op == TOKnull && e2->op == TOKarrayliteral &&
        t1->nextOf()->equals(t2->nextOf()))
    {
        //  null ~ [ e2 ] ----> [ e2 ].dup
        ue = paintTypeOntoLiteralCopy(type, copyLiteral(e2).copy());
        return ue;
    }
    ue = Cat(type, e1, e2);
    return ue;
}

/*  Given an AA literal 'ae', and a key 'e2':
 *  Return ae[e2] if present, or NULL if not found.
 */
Expression *findKeyInAA(Loc loc, AssocArrayLiteralExp *ae, Expression *e2)
{
    /* Search the keys backwards, in case there are duplicate keys
     */
    for (size_t i = ae->keys->dim; i;)
    {
        i--;
        Expression *ekey = (*ae->keys)[i];
        int eq = ctfeEqual(loc, TOKequal, ekey, e2);
        if (eq)
        {
            return (*ae->values)[i];
        }
    }
    return NULL;
}

/* Same as for constfold.Index, except that it only works for static arrays,
 * dynamic arrays, and strings. We know that e1 is an
 * interpreted CTFE expression, so it cannot have side-effects.
 */
Expression *ctfeIndex(Loc loc, Type *type, Expression *e1, uinteger_t indx)
{
    //printf("ctfeIndex(e1 = %s)\n", e1->toChars());
    assert(e1->type);
    if (e1->op == TOKstring)
    {
        StringExp *es1 = (StringExp *)e1;
        if (indx >= es1->len)
        {
            error(loc, "string index %llu is out of bounds [0 .. %llu]", (ulonglong)indx, (ulonglong)es1->len);
            return CTFEExp::cantexp;
        }
        return new IntegerExp(loc, es1->charAt(indx), type);
    }
    assert(e1->op == TOKarrayliteral);
    {
        ArrayLiteralExp *ale = (ArrayLiteralExp *)e1;
        if (indx >= ale->elements->dim)
        {
            error(loc, "array index %llu is out of bounds %s[0 .. %llu]", (ulonglong)indx, e1->toChars(), (ulonglong)ale->elements->dim);
            return CTFEExp::cantexp;
        }
        Expression *e = (*ale->elements)[(size_t)indx];
        return paintTypeOntoLiteral(type, e);
    }
}

Expression *ctfeCast(UnionExp *pue, Loc loc, Type *type, Type *to, Expression *e)
{
    if (e->op == TOKnull)
        return paintTypeOntoLiteral(pue, to, e);

    if (e->op == TOKclassreference)
    {
        // Disallow reinterpreting class casts. Do this by ensuring that
        // the original class can implicitly convert to the target class
        ClassDeclaration *originalClass = ((ClassReferenceExp *)e)->originalClass();
        if (originalClass->type->implicitConvTo(to->mutableOf()))
            return paintTypeOntoLiteral(pue, to, e);
        else
        {
            new(pue) NullExp(loc, to);
            return pue->exp();
        }
    }

    // Allow TypeInfo type painting
    if (isTypeInfo_Class(e->type) && e->type->implicitConvTo(to))
        return paintTypeOntoLiteral(pue, to, e);

    // Allow casting away const for struct literals
    if (e->op == TOKstructliteral &&
        e->type->toBasetype()->castMod(0) == to->toBasetype()->castMod(0))
        return paintTypeOntoLiteral(pue, to, e);

    Expression *r;
    if (e->type->equals(type) && type->equals(to))
    {
        // necessary not to change e's address for pointer comparisons
        r = e;
    }
    else if (to->toBasetype()->ty == Tarray &&
             type->toBasetype()->ty == Tarray &&
             to->toBasetype()->nextOf()->size() == type->toBasetype()->nextOf()->size())
    {
        // Bugzilla 12495: Array reinterpret casts: eg. string to immutable(ubyte)[]
        return paintTypeOntoLiteral(pue, to, e);
    }
    else
    {
        *pue = Cast(loc, type, to, e);
        r = pue->exp();
    }

    if (CTFEExp::isCantExp(r))
        error(loc, "cannot cast %s to %s at compile time", e->toChars(), to->toChars());

    if (e->op == TOKarrayliteral)
        ((ArrayLiteralExp *)e)->ownedByCtfe = OWNEDctfe;

    if (e->op == TOKstring)
        ((StringExp *)e)->ownedByCtfe = OWNEDctfe;

    return r;
}

/******** Assignment helper functions ***************************/

/* Set dest = src, where both dest and src are container value literals
 * (ie, struct literals, or static arrays (can be an array literal or a string))
 * Assignment is recursively in-place.
 * Purpose: any reference to a member of 'dest' will remain valid after the
 * assignment.
 */
void assignInPlace(Expression *dest, Expression *src)
{
    assert(dest->op == TOKstructliteral ||
           dest->op == TOKarrayliteral ||
           dest->op == TOKstring);
    Expressions *oldelems;
    Expressions *newelems;
    if (dest->op == TOKstructliteral)
    {
        assert(dest->op == src->op);
        oldelems = ((StructLiteralExp *)dest)->elements;
        newelems = ((StructLiteralExp *)src)->elements;
        if (((StructLiteralExp *)dest)->sd->isNested() && oldelems->dim == newelems->dim - 1)
            oldelems->push(NULL);
    }
    else if (dest->op == TOKarrayliteral && src->op==TOKarrayliteral)
    {
        oldelems = ((ArrayLiteralExp *)dest)->elements;
        newelems = ((ArrayLiteralExp *)src)->elements;
    }
    else if (dest->op == TOKstring && src->op == TOKstring)
    {
        sliceAssignStringFromString((StringExp *)dest, (StringExp *)src, 0);
        return;
    }
    else if (dest->op == TOKarrayliteral && src->op == TOKstring)
    {
        sliceAssignArrayLiteralFromString((ArrayLiteralExp *)dest, (StringExp *)src, 0);
        return;
    }
    else if (src->op == TOKarrayliteral && dest->op == TOKstring)
    {
        sliceAssignStringFromArrayLiteral((StringExp *)dest, (ArrayLiteralExp *)src, 0);
        return;
    }
    else
        assert(0);

    assert(oldelems->dim == newelems->dim);

    for (size_t i= 0; i < oldelems->dim; ++i)
    {
        Expression *e = (*newelems)[i];
        Expression *o = (*oldelems)[i];
        if (e->op == TOKstructliteral)
        {
            assert(o->op == e->op);
            assignInPlace(o, e);
        }
        else if (e->type->ty == Tsarray && e->op != TOKvoid &&
                 o->type->ty == Tsarray)
        {
            assignInPlace(o, e);
        }
        else
        {
            (*oldelems)[i] = (*newelems)[i];
        }
    }
}

// Duplicate the elements array, then set field 'indexToChange' = newelem.
Expressions *changeOneElement(Expressions *oldelems, size_t indexToChange, Expression *newelem)
{
    Expressions *expsx = new Expressions();
    ++CtfeStatus::numArrayAllocs;
    expsx->setDim(oldelems->dim);
    for (size_t j = 0; j < expsx->dim; j++)
    {
        if (j == indexToChange)
            (*expsx)[j] = newelem;
        else
            (*expsx)[j] = (*oldelems)[j];
    }
    return expsx;
}

// Given an AA literal aae,  set aae[index] = newval and return newval.
Expression *assignAssocArrayElement(Loc loc, AssocArrayLiteralExp *aae,
    Expression *index, Expression *newval)
{
    /* Create new associative array literal reflecting updated key/value
     */
    Expressions *keysx = aae->keys;
    Expressions *valuesx = aae->values;
    int updated = 0;
    for (size_t j = valuesx->dim; j; )
    {
        j--;
        Expression *ekey = (*aae->keys)[j];
        int eq = ctfeEqual(loc, TOKequal, ekey, index);
        if (eq)
        {
            (*valuesx)[j] = newval;
            updated = 1;
        }
    }
    if (!updated)
    {
        // Append index/newval to keysx[]/valuesx[]
        valuesx->push(newval);
        keysx->push(index);
    }
    return newval;
}

/// Given array literal oldval of type ArrayLiteralExp or StringExp, of length
/// oldlen, change its length to newlen. If the newlen is longer than oldlen,
/// all new elements will be set to the default initializer for the element type.
UnionExp changeArrayLiteralLength(Loc loc, TypeArray *arrayType,
    Expression *oldval,  size_t oldlen, size_t newlen)
{
    UnionExp ue;
    Type *elemType = arrayType->next;
    assert(elemType);
    Expression *defaultElem = elemType->defaultInitLiteral(loc);
    Expressions *elements = new Expressions();
    elements->setDim(newlen);

    // Resolve slices
    size_t indxlo = 0;
    if (oldval->op == TOKslice)
    {
        indxlo = (size_t)((SliceExp *)oldval)->lwr->toInteger();
        oldval = ((SliceExp *)oldval)->e1;
    }
    size_t copylen = oldlen < newlen ? oldlen : newlen;
    if (oldval->op == TOKstring)
    {
        StringExp *oldse = (StringExp *)oldval;
        void *s = mem.xcalloc(newlen + 1, oldse->sz);
        memcpy(s, oldse->string, copylen * oldse->sz);
        unsigned defaultValue = (unsigned)(defaultElem->toInteger());
        for (size_t elemi = copylen; elemi < newlen; ++elemi)
        {
            switch (oldse->sz)
            {
                case 1:     (( utf8_t *)s)[(size_t)(indxlo + elemi)] = ( utf8_t)defaultValue;  break;
                case 2:     ((utf16_t *)s)[(size_t)(indxlo + elemi)] = (utf16_t)defaultValue;  break;
                case 4:     ((utf32_t *)s)[(size_t)(indxlo + elemi)] = (utf32_t)defaultValue;  break;
                default:    assert(0);
            }
        }
        new(&ue) StringExp(loc, s, newlen);
        StringExp *se = (StringExp *)ue.exp();
        se->type = arrayType;
        se->sz = oldse->sz;
        se->committed = oldse->committed;
        se->ownedByCtfe = OWNEDctfe;
    }
    else
    {
        if (oldlen != 0)
        {
            assert(oldval->op == TOKarrayliteral);
            ArrayLiteralExp *ae = (ArrayLiteralExp *)oldval;
            for (size_t i = 0; i < copylen; i++)
                (*elements)[i] = (*ae->elements)[indxlo + i];
        }
        if (elemType->ty == Tstruct || elemType->ty == Tsarray)
        {
            /* If it is an aggregate literal representing a value type,
             * we need to create a unique copy for each element
             */
            for (size_t i = copylen; i < newlen; i++)
                (*elements)[i] = copyLiteral(defaultElem).copy();
        }
        else
        {
            for (size_t i = copylen; i < newlen; i++)
                (*elements)[i] = defaultElem;
        }
        new(&ue) ArrayLiteralExp(loc, arrayType, elements);
        ArrayLiteralExp *aae = (ArrayLiteralExp *)ue.exp();
        aae->ownedByCtfe = OWNEDctfe;
    }
    return ue;
}

/*************************** CTFE Sanity Checks ***************************/

bool isCtfeValueValid(Expression *newval)
{
    Type *tb = newval->type->toBasetype();

    if (newval->op == TOKint64 ||
        newval->op == TOKfloat64 ||
        newval->op == TOKchar ||
        newval->op == TOKcomplex80)
    {
        return tb->isscalar();
    }
    if (newval->op == TOKnull)
    {
        return tb->ty == Tnull ||
               tb->ty == Tpointer ||
               tb->ty == Tarray ||
               tb->ty == Taarray ||
               tb->ty == Tclass ||
               tb->ty == Tdelegate;
    }

    if (newval->op == TOKstring)
        return true;    // CTFE would directly use the StringExp in AST.
    if (newval->op == TOKarrayliteral)
        return true;    //((ArrayLiteralExp *)newval)->ownedByCtfe;
    if (newval->op == TOKassocarrayliteral)
        return true;    //((AssocArrayLiteralExp *)newval)->ownedByCtfe;
    if (newval->op == TOKstructliteral)
        return true;    //((StructLiteralExp *)newval)->ownedByCtfe;
    if (newval->op == TOKclassreference)
        return true;

    if (newval->op == TOKvector)
        return true;    // vector literal

    if (newval->op == TOKfunction)
        return true;    // function literal or delegate literal
    if (newval->op == TOKdelegate)
    {
        // &struct.func or &clasinst.func
        // &nestedfunc
        Expression *ethis = ((DelegateExp *)newval)->e1;
        return (ethis->op == TOKstructliteral ||
                ethis->op == TOKclassreference ||
                (ethis->op == TOKvar && ((VarExp *)ethis)->var == ((DelegateExp *)newval)->func));
    }
    if (newval->op == TOKsymoff)
    {
        // function pointer, or pointer to static variable
        Declaration *d = ((SymOffExp *)newval)->var;
        return d->isFuncDeclaration() || d->isDataseg();
    }
    if (newval->op == TOKtypeid)
    {
        // always valid
        return true;
    }
    if (newval->op == TOKaddress)
    {
        // e1 should be a CTFE reference
        Expression *e1 = ((AddrExp *)newval)->e1;
        return tb->ty == Tpointer &&
               ((e1->op == TOKstructliteral && isCtfeValueValid(e1)) ||
                (e1->op == TOKvar) ||
                (e1->op == TOKdotvar && isCtfeReferenceValid(e1)) ||
                (e1->op == TOKindex && isCtfeReferenceValid(e1)) ||
                (e1->op == TOKslice && e1->type->toBasetype()->ty == Tsarray));
    }
    if (newval->op == TOKslice)
    {
        // e1 should be an array aggregate
        SliceExp *se = (SliceExp *)newval;
        assert(se->lwr && se->lwr->op == TOKint64);
        assert(se->upr && se->upr->op == TOKint64);
        return (tb->ty == Tarray ||
                tb->ty == Tsarray) &&
               (se->e1->op == TOKstring ||
                se->e1->op == TOKarrayliteral);
    }

    if (newval->op == TOKvoid)
        return true;    // uninitialized value

    newval->error("CTFE internal error: illegal CTFE value %s", newval->toChars());
    return false;
}

bool isCtfeReferenceValid(Expression *newval)
{
    if (newval->op == TOKthis)
        return true;
    if (newval->op == TOKvar)
    {
        VarDeclaration *v = ((VarExp *)newval)->var->isVarDeclaration();
        assert(v);
        // Must not be a reference to a reference
        return true;
    }
    if (newval->op == TOKindex)
    {
        Expression *eagg = ((IndexExp *)newval)->e1;
        return eagg->op == TOKstring ||
               eagg->op == TOKarrayliteral ||
               eagg->op == TOKassocarrayliteral;
    }
    if (newval->op == TOKdotvar)
    {
        Expression *eagg = ((DotVarExp *)newval)->e1;
        return  (eagg->op == TOKstructliteral || eagg->op == TOKclassreference) &&
                isCtfeValueValid(eagg);
    }

    // Internally a ref variable may directly point a stack memory.
    // e.g. ref int v = 1;
    return isCtfeValueValid(newval);
}

// Used for debugging only
void showCtfeExpr(Expression *e, int level)
{
    for (int i = level; i > 0; --i) printf(" ");
    Expressions *elements = NULL;
    // We need the struct definition to detect block assignment
    StructDeclaration *sd = NULL;
    ClassDeclaration *cd = NULL;
    if (e->op == TOKstructliteral)
    {
        elements = ((StructLiteralExp *)e)->elements;
        sd = ((StructLiteralExp *)e)->sd;
        printf("STRUCT type = %s %p:\n", e->type->toChars(),
            e);
    }
    else if (e->op == TOKclassreference)
    {
        elements = ((ClassReferenceExp *)e)->value->elements;
        cd = ((ClassReferenceExp *)e)->originalClass();
        printf("CLASS type = %s %p:\n", e->type->toChars(),
            ((ClassReferenceExp *)e)->value);
    }
    else if (e->op == TOKarrayliteral)
    {
        elements = ((ArrayLiteralExp *)e)->elements;
        printf("ARRAY LITERAL type=%s %p:\n", e->type->toChars(),
            e);
    }
    else if (e->op == TOKassocarrayliteral)
    {
        printf("AA LITERAL type=%s %p:\n", e->type->toChars(),
            e);
    }
    else if (e->op == TOKstring)
    {
        printf("STRING %s %p\n", e->toChars(),
            ((StringExp *)e)->string);
    }
    else if (e->op == TOKslice)
    {
        printf("SLICE %p: %s\n", e, e->toChars());
        showCtfeExpr(((SliceExp *)e)->e1, level + 1);
    }
    else if (e->op == TOKvar)
    {
        printf("VAR %p %s\n", e, e->toChars());
        VarDeclaration *v = ((VarExp *)e)->var->isVarDeclaration();
        if (v && getValue(v))
            showCtfeExpr(getValue(v), level + 1);
    }
    else if (e->op == TOKaddress)
    {
        // This is potentially recursive. We mustn't try to print the thing we're pointing to.
        printf("POINTER %p to %p: %s\n", e, ((AddrExp *)e)->e1, e->toChars());
    }
    else
        printf("VALUE %p: %s\n", e, e->toChars());

    if (elements)
    {
        size_t fieldsSoFar = 0;
        for (size_t i = 0; i < elements->dim; i++)
        {
            Expression *z = NULL;
            VarDeclaration *v = NULL;
            if (i > 15)
            {
                printf("...(total %d elements)\n", (int)elements->dim);
                return;
            }
            if (sd)
            {
                v = sd->fields[i];
                z = (*elements)[i];
            }
            else if (cd)
            {
                while (i - fieldsSoFar >= cd->fields.dim)
                {
                    fieldsSoFar += cd->fields.dim;
                    cd = cd->baseClass;
                    for (int j = level; j > 0; --j) printf(" ");
                    printf(" BASE CLASS: %s\n", cd->toChars());
                }
                v = cd->fields[i - fieldsSoFar];
                assert((elements->dim + i) >= (fieldsSoFar + cd->fields.dim));
                size_t indx = (elements->dim - fieldsSoFar)- cd->fields.dim + i;
                assert(indx < elements->dim);
                z = (*elements)[indx];
            }
            if (!z)
            {
                for (int j = level; j > 0; --j) printf(" ");
                printf(" void\n");
                continue;
            }

            if (v)
            {
                // If it is a void assignment, use the default initializer
                if ((v->type->ty != z->type->ty) && v->type->ty == Tsarray)
                {
                    for (int j = level; --j; ) printf(" ");
                    printf(" field: block initalized static array\n");
                    continue;
                }
            }
            showCtfeExpr(z, level + 1);
        }
    }
}

/*************************** Void initialization ***************************/

UnionExp voidInitLiteral(Type *t, VarDeclaration *var)
{
    UnionExp ue;
    if (t->ty == Tsarray)
    {
        TypeSArray *tsa = (TypeSArray *)t;
        Expression *elem = voidInitLiteral(tsa->next, var).copy();

        // For aggregate value types (structs, static arrays) we must
        // create an a separate copy for each element.
        bool mustCopy = (elem->op == TOKarrayliteral || elem->op == TOKstructliteral);

        Expressions *elements = new Expressions();
        size_t d = (size_t)tsa->dim->toInteger();
        elements->setDim(d);
        for (size_t i = 0; i < d; i++)
        {
            if (mustCopy && i > 0)
                elem  = copyLiteral(elem).copy();
            (*elements)[i] = elem;
        }
        new(&ue) ArrayLiteralExp(var->loc, tsa, elements);
        ArrayLiteralExp *ae = (ArrayLiteralExp *)ue.exp();
        ae->ownedByCtfe = OWNEDctfe;
    }
    else if (t->ty == Tstruct)
    {
        TypeStruct *ts = (TypeStruct *)t;
        Expressions *exps = new Expressions();
        exps->setDim(ts->sym->fields.dim);
        for (size_t i = 0; i < ts->sym->fields.dim; i++)
        {
            (*exps)[i] = voidInitLiteral(ts->sym->fields[i]->type, ts->sym->fields[i]).copy();
        }
        new(&ue) StructLiteralExp(var->loc, ts->sym, exps);
        StructLiteralExp *se = (StructLiteralExp *)ue.exp();
        se->type = ts;
        se->ownedByCtfe = OWNEDctfe;
    }
    else
        new(&ue) VoidInitExp(var, t);
    return ue;
}
