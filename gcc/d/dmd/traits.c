
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/traits.c
 */

#include "root/dsystem.h"
#include "root/rmem.h"
#include "root/aav.h"
#include "root/checkedint.h"

#include "errors.h"
#include "mtype.h"
#include "init.h"
#include "expression.h"
#include "template.h"
#include "utf.h"
#include "enum.h"
#include "scope.h"
#include "hdrgen.h"
#include "statement.h"
#include "declaration.h"
#include "aggregate.h"
#include "import.h"
#include "id.h"
#include "dsymbol.h"
#include "module.h"
#include "attrib.h"
#include "parse.h"
#include "root/speller.h"

typedef int (*ForeachDg)(void *ctx, size_t idx, Dsymbol *s);
int ScopeDsymbol_foreach(Scope *sc, Dsymbols *members, ForeachDg dg, void *ctx, size_t *pn = NULL);
void freeFieldinit(Scope *sc);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);
Expression *trySemantic(Expression *e, Scope *sc);
Expression *semantic(Expression *e, Scope *sc);
Expression *typeToExpression(Type *t);


/************************************************
 * Delegate to be passed to overloadApply() that looks
 * for functions matching a trait.
 */

struct Ptrait
{
    Expression *e1;
    Expressions *exps;          // collected results
    Identifier *ident;          // which trait we're looking for
};

static int fptraits(void *param, Dsymbol *s)
{
    FuncDeclaration *f = s->isFuncDeclaration();
    if (!f)
        return 0;

    Ptrait *p = (Ptrait *)param;
    if (p->ident == Id::getVirtualFunctions && !f->isVirtual())
        return 0;

    if (p->ident == Id::getVirtualMethods && !f->isVirtualMethod())
        return 0;

    Expression *e;
    FuncAliasDeclaration* ad = new FuncAliasDeclaration(f->ident, f, false);
    ad->protection = f->protection;
    if (p->e1)
        e = new DotVarExp(Loc(), p->e1, ad, false);
    else
        e = new DsymbolExp(Loc(), ad, false);
    p->exps->push(e);
    return 0;
}

/**
 * Collects all unit test functions from the given array of symbols.
 *
 * This is a helper function used by the implementation of __traits(getUnitTests).
 *
 * Input:
 *      symbols             array of symbols to collect the functions from
 *      uniqueUnitTests     an associative array (should actually be a set) to
 *                          keep track of already collected functions. We're
 *                          using an AA here to avoid doing a linear search of unitTests
 *
 * Output:
 *      unitTests           array of DsymbolExp's of the collected unit test functions
 *      uniqueUnitTests     updated with symbols from unitTests[ ]
 */
static void collectUnitTests(Dsymbols *symbols, AA *uniqueUnitTests, Expressions *unitTests)
{
    if (!symbols)
        return;
    for (size_t i = 0; i < symbols->dim; i++)
    {
        Dsymbol *symbol = (*symbols)[i];
        UnitTestDeclaration *unitTest = symbol->isUnitTestDeclaration();
        if (unitTest)
        {
            if (!dmd_aaGetRvalue(uniqueUnitTests, (void *)unitTest))
            {
                FuncAliasDeclaration* ad = new FuncAliasDeclaration(unitTest->ident, unitTest, false);
                ad->protection = unitTest->protection;
                Expression* e = new DsymbolExp(Loc(), ad, false);
                unitTests->push(e);
                bool* value = (bool*) dmd_aaGet(&uniqueUnitTests, (void *)unitTest);
                *value = true;
            }
        }
        else
        {
            AttribDeclaration *attrDecl = symbol->isAttribDeclaration();

            if (attrDecl)
            {
                Dsymbols *decl = attrDecl->include(NULL, NULL);
                collectUnitTests(decl, uniqueUnitTests, unitTests);
            }
        }
    }
}

/************************ TraitsExp ************************************/

static Expression *True(TraitsExp *e)  { return new IntegerExp(e->loc, true, Type::tbool); }
static Expression *False(TraitsExp *e) { return new IntegerExp(e->loc, false, Type::tbool); }

bool isTypeArithmetic(Type *t)       { return t->isintegral() || t->isfloating(); }
bool isTypeFloating(Type *t)         { return t->isfloating(); }
bool isTypeIntegral(Type *t)         { return t->isintegral(); }
bool isTypeScalar(Type *t)           { return t->isscalar(); }
bool isTypeUnsigned(Type *t)         { return t->isunsigned(); }
bool isTypeAssociativeArray(Type *t) { return t->toBasetype()->ty == Taarray; }
bool isTypeStaticArray(Type *t)      { return t->toBasetype()->ty == Tsarray; }
bool isTypeAbstractClass(Type *t)    { return t->toBasetype()->ty == Tclass && ((TypeClass *)t->toBasetype())->sym->isAbstract(); }
bool isTypeFinalClass(Type *t)       { return t->toBasetype()->ty == Tclass && (((TypeClass *)t->toBasetype())->sym->storage_class & STCfinal) != 0; }

Expression *isTypeX(TraitsExp *e, bool (*fp)(Type *t))
{
    if (!e->args || !e->args->dim)
        return False(e);
    for (size_t i = 0; i < e->args->dim; i++)
    {
        Type *t = getType((*e->args)[i]);
        if (!t || !fp(t))
            return False(e);
    }
    return True(e);
}

bool isFuncAbstractFunction(FuncDeclaration *f) { return f->isAbstract(); }
bool isFuncVirtualFunction(FuncDeclaration *f) { return f->isVirtual(); }
bool isFuncVirtualMethod(FuncDeclaration *f) { return f->isVirtualMethod(); }
bool isFuncFinalFunction(FuncDeclaration *f) { return f->isFinalFunc(); }
bool isFuncStaticFunction(FuncDeclaration *f) { return !f->needThis() && !f->isNested(); }
bool isFuncOverrideFunction(FuncDeclaration *f) { return f->isOverride(); }

Expression *isFuncX(TraitsExp *e, bool (*fp)(FuncDeclaration *f))
{
    if (!e->args || !e->args->dim)
        return False(e);
    for (size_t i = 0; i < e->args->dim; i++)
    {
        Dsymbol *s = getDsymbol((*e->args)[i]);
        if (!s)
            return False(e);
        FuncDeclaration *f = s->isFuncDeclaration();
        if (!f || !fp(f))
            return False(e);
    }
    return True(e);
}

bool isDeclRef(Declaration *d) { return d->isRef(); }
bool isDeclOut(Declaration *d) { return d->isOut(); }
bool isDeclLazy(Declaration *d) { return (d->storage_class & STClazy) != 0; }

Expression *isDeclX(TraitsExp *e, bool (*fp)(Declaration *d))
{
    if (!e->args || !e->args->dim)
        return False(e);
    for (size_t i = 0; i < e->args->dim; i++)
    {
        Dsymbol *s = getDsymbol((*e->args)[i]);
        if (!s)
            return False(e);
        Declaration *d = s->isDeclaration();
        if (!d || !fp(d))
            return False(e);
    }
    return True(e);
}

// callback for TypeFunction::attributesApply
struct PushAttributes
{
    Expressions *mods;

    static int fp(void *param, const char *str)
    {
        PushAttributes *p = (PushAttributes *)param;
        p->mods->push(new StringExp(Loc(), const_cast<char *>(str)));
        return 0;
    }
};

StringTable traitsStringTable;

struct TraitsInitializer
{
    TraitsInitializer();
};

static TraitsInitializer traitsinitializer;

TraitsInitializer::TraitsInitializer()
{
    const char* traits[] = {
        "isAbstractClass",
        "isArithmetic",
        "isAssociativeArray",
        "isFinalClass",
        "isPOD",
        "isNested",
        "isFloating",
        "isIntegral",
        "isScalar",
        "isStaticArray",
        "isUnsigned",
        "isVirtualFunction",
        "isVirtualMethod",
        "isAbstractFunction",
        "isFinalFunction",
        "isOverrideFunction",
        "isStaticFunction",
        "isRef",
        "isOut",
        "isLazy",
        "hasMember",
        "identifier",
        "getProtection",
        "parent",
        "getLinkage",
        "getMember",
        "getOverloads",
        "getVirtualFunctions",
        "getVirtualMethods",
        "classInstanceSize",
        "allMembers",
        "derivedMembers",
        "isSame",
        "compiles",
        "parameters",
        "getAliasThis",
        "getAttributes",
        "getFunctionAttributes",
        "getFunctionVariadicStyle",
        "getParameterStorageClasses",
        "getUnitTests",
        "getVirtualIndex",
        "getPointerBitmap",
        NULL
    };

    traitsStringTable._init(40);

    for (size_t idx = 0;; idx++)
    {
        const char *s = traits[idx];
        if (!s) break;
        StringValue *sv = traitsStringTable.insert(s, strlen(s), const_cast<char *>(s));
        assert(sv);
    }
}

void *trait_search_fp(void *, const char *seed, int* cost)
{
    //printf("trait_search_fp('%s')\n", seed);
    size_t len = strlen(seed);
    if (!len)
        return NULL;

    *cost = 0;
    StringValue *sv = traitsStringTable.lookup(seed, len);
    return sv ? (void*)sv->ptrvalue : NULL;
}

static int fpisTemplate(void *, Dsymbol *s)
{
    if (s->isTemplateDeclaration())
        return 1;

    return 0;
}

bool isTemplate(Dsymbol *s)
{
    if (!s->toAlias()->isOverloadable())
        return false;

    return overloadApply(s, NULL, &fpisTemplate) != 0;
}

Expression *isSymbolX(TraitsExp *e, bool (*fp)(Dsymbol *s))
{
    if (!e->args || !e->args->dim)
        return False(e);
    for (size_t i = 0; i < e->args->dim; i++)
    {
        Dsymbol *s = getDsymbol((*e->args)[i]);
        if (!s || !fp(s))
            return False(e);
    }
    return True(e);
}

/**
 * get an array of size_t values that indicate possible pointer words in memory
 *  if interpreted as the type given as argument
 * the first array element is the size of the type for independent interpretation
 *  of the array
 * following elements bits represent one word (4/8 bytes depending on the target
 *  architecture). If set the corresponding memory might contain a pointer/reference.
 *
 *  [T.sizeof, pointerbit0-31/63, pointerbit32/64-63/128, ...]
 */
Expression *pointerBitmap(TraitsExp *e)
{
    if (!e->args || e->args->dim != 1)
    {
        error(e->loc, "a single type expected for trait pointerBitmap");
        return new ErrorExp();
    }
    Type *t = getType((*e->args)[0]);
    if (!t)
    {
        error(e->loc, "%s is not a type", (*e->args)[0]->toChars());
        return new ErrorExp();
    }
    d_uns64 sz;
    if (t->ty == Tclass && !((TypeClass*)t)->sym->isInterfaceDeclaration())
        sz = ((TypeClass*)t)->sym->AggregateDeclaration::size(e->loc);
    else
        sz = t->size(e->loc);
    if (sz == SIZE_INVALID)
        return new ErrorExp();

    const d_uns64 sz_size_t = Type::tsize_t->size(e->loc);
    if (sz > UINT64_MAX - sz_size_t)
    {
        error(e->loc, "size overflow for type %s", t->toChars());
        return new ErrorExp();
    }

    d_uns64 bitsPerWord = sz_size_t * 8;
    d_uns64 cntptr = (sz + sz_size_t - 1) / sz_size_t;
    d_uns64 cntdata = (cntptr + bitsPerWord - 1) / bitsPerWord;
    Array<d_uns64> data;
    data.setDim((size_t)cntdata);
    data.zero();

    class PointerBitmapVisitor : public Visitor
    {
    public:
        PointerBitmapVisitor(Array<d_uns64>* _data, d_uns64 _sz_size_t)
            : data(_data), offset(0), sz_size_t(_sz_size_t), error(false)
        {}

        void setpointer(d_uns64 off)
        {
            d_uns64 ptroff = off / sz_size_t;
            (*data)[(size_t)(ptroff / (8 * sz_size_t))] |= 1LL << (ptroff % (8 * sz_size_t));
        }
        virtual void visit(Type *t)
        {
            Type *tb = t->toBasetype();
            if (tb != t)
                tb->accept(this);
        }
        virtual void visit(TypeError *t) { visit((Type *)t); }
        virtual void visit(TypeNext *) { assert(0); }
        virtual void visit(TypeBasic *t)
        {
            if (t->ty == Tvoid)
                setpointer(offset);
        }
        virtual void visit(TypeVector *) { }
        virtual void visit(TypeArray *) { assert(0); }
        virtual void visit(TypeSArray *t)
        {
            d_uns64 arrayoff = offset;
            d_uns64 nextsize = t->next->size();
            if (nextsize == SIZE_INVALID)
                error = true;
            d_uns64 dim = t->dim->toInteger();
            for (d_uns64 i = 0; i < dim; i++)
            {
                offset = arrayoff + i * nextsize;
                t->next->accept(this);
            }
            offset = arrayoff;
        }
        virtual void visit(TypeDArray *) { setpointer(offset + sz_size_t); } // dynamic array is {length,ptr}
        virtual void visit(TypeAArray *) { setpointer(offset); }
        virtual void visit(TypePointer *t)
        {
            if (t->nextOf()->ty != Tfunction) // don't mark function pointers
                setpointer(offset);
        }
        virtual void visit(TypeReference *) { setpointer(offset); }
        virtual void visit(TypeClass *) { setpointer(offset); }
        virtual void visit(TypeFunction *) { }
        virtual void visit(TypeDelegate *) { setpointer(offset); } // delegate is {context, function}
        virtual void visit(TypeQualified *) { assert(0); } // assume resolved
        virtual void visit(TypeIdentifier *) { assert(0); }
        virtual void visit(TypeInstance *) { assert(0); }
        virtual void visit(TypeTypeof *) { assert(0); }
        virtual void visit(TypeReturn *) { assert(0); }
        virtual void visit(TypeEnum *t) { visit((Type *)t); }
        virtual void visit(TypeTuple *t) { visit((Type *)t); }
        virtual void visit(TypeSlice *) { assert(0); }
        virtual void visit(TypeNull *) { } // always a null pointer

        virtual void visit(TypeStruct *t)
        {
            d_uns64 structoff = offset;
            for (size_t i = 0; i < t->sym->fields.dim; i++)
            {
                VarDeclaration *v = t->sym->fields[i];
                offset = structoff + v->offset;
                if (v->type->ty == Tclass)
                    setpointer(offset);
                else
                    v->type->accept(this);
            }
            offset = structoff;
        }

        // a "toplevel" class is treated as an instance, while TypeClass fields are treated as references
        void visitClass(TypeClass* t)
        {
            d_uns64 classoff = offset;

            // skip vtable-ptr and monitor
            if (t->sym->baseClass)
                visitClass((TypeClass*)t->sym->baseClass->type);

            for (size_t i = 0; i < t->sym->fields.dim; i++)
            {
                VarDeclaration *v = t->sym->fields[i];
                offset = classoff + v->offset;
                v->type->accept(this);
            }
            offset = classoff;
        }

        Array<d_uns64>* data;
        d_uns64 offset;
        d_uns64 sz_size_t;
        bool error;
    };

    PointerBitmapVisitor pbv(&data, sz_size_t);
    if (t->ty == Tclass)
        pbv.visitClass((TypeClass*)t);
    else
        t->accept(&pbv);
    if (pbv.error)
        return new ErrorExp();

    Expressions* exps = new Expressions;
    exps->push(new IntegerExp(e->loc, sz, Type::tsize_t));
    for (d_uns64 i = 0; i < cntdata; i++)
        exps->push(new IntegerExp(e->loc, data[(size_t)i], Type::tsize_t));

    ArrayLiteralExp* ale = new ArrayLiteralExp(e->loc, Type::tsize_t->sarrayOf(cntdata + 1), exps);
    return ale;
}

static Expression *dimError(TraitsExp *e, int expected, int dim)
{
    e->error("expected %d arguments for `%s` but had %d", expected, e->ident->toChars(), dim);
    return new ErrorExp();
}

Expression *semanticTraits(TraitsExp *e, Scope *sc)
{
    if (e->ident != Id::compiles && e->ident != Id::isSame &&
        e->ident != Id::identifier && e->ident != Id::getProtection)
    {
        if (!TemplateInstance::semanticTiargs(e->loc, sc, e->args, 1))
            return new ErrorExp();
    }
    size_t dim = e->args ? e->args->dim : 0;

    if (e->ident == Id::isArithmetic)
    {
        return isTypeX(e, &isTypeArithmetic);
    }
    else if (e->ident == Id::isFloating)
    {
        return isTypeX(e, &isTypeFloating);
    }
    else if (e->ident == Id::isIntegral)
    {
        return isTypeX(e, &isTypeIntegral);
    }
    else if (e->ident == Id::isScalar)
    {
        return isTypeX(e, &isTypeScalar);
    }
    else if (e->ident == Id::isUnsigned)
    {
        return isTypeX(e, &isTypeUnsigned);
    }
    else if (e->ident == Id::isAssociativeArray)
    {
        return isTypeX(e, &isTypeAssociativeArray);
    }
    else if (e->ident == Id::isStaticArray)
    {
        return isTypeX(e, &isTypeStaticArray);
    }
    else if (e->ident == Id::isAbstractClass)
    {
        return isTypeX(e, &isTypeAbstractClass);
    }
    else if (e->ident == Id::isFinalClass)
    {
        return isTypeX(e, &isTypeFinalClass);
    }
    else if (e->ident == Id::isTemplate)
    {
        return isSymbolX(e, &isTemplate);
    }
    else if (e->ident == Id::isPOD)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Type *t = isType(o);
        if (!t)
        {
            e->error("type expected as second argument of __traits %s instead of %s",
                e->ident->toChars(), o->toChars());
            return new ErrorExp();
        }

        Type *tb = t->baseElemOf();
        if (StructDeclaration *sd = (tb->ty == Tstruct) ? ((TypeStruct *)tb)->sym : NULL)
        {
            return (sd->isPOD()) ? True(e) : False(e);
        }
        return True(e);
    }
    else if (e->ident == Id::isNested)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s)
        {
        }
        else if (AggregateDeclaration *a = s->isAggregateDeclaration())
        {
            return a->isNested() ? True(e) : False(e);
        }
        else if (FuncDeclaration *f = s->isFuncDeclaration())
        {
            return f->isNested() ? True(e) : False(e);
        }

        e->error("aggregate or function expected instead of '%s'", o->toChars());
        return new ErrorExp();
    }
    else if (e->ident == Id::isAbstractFunction)
    {
        return isFuncX(e, &isFuncAbstractFunction);
    }
    else if (e->ident == Id::isVirtualFunction)
    {
        return isFuncX(e, &isFuncVirtualFunction);
    }
    else if (e->ident == Id::isVirtualMethod)
    {
        return isFuncX(e, &isFuncVirtualMethod);
    }
    else if (e->ident == Id::isFinalFunction)
    {
        return isFuncX(e, &isFuncFinalFunction);
    }
    else if (e->ident == Id::isOverrideFunction)
    {
        return isFuncX(e, &isFuncOverrideFunction);
    }
    else if (e->ident == Id::isStaticFunction)
    {
        return isFuncX(e, &isFuncStaticFunction);
    }
    else if (e->ident == Id::isRef)
    {
        return isDeclX(e, &isDeclRef);
    }
    else if (e->ident == Id::isOut)
    {
        return isDeclX(e, &isDeclOut);
    }
    else if (e->ident == Id::isLazy)
    {
        return isDeclX(e, &isDeclLazy);
    }
    else if (e->ident == Id::identifier)
    {
        // Get identifier for symbol as a string literal
        /* Specify 0 for bit 0 of the flags argument to semanticTiargs() so that
         * a symbol should not be folded to a constant.
         * Bit 1 means don't convert Parameter to Type if Parameter has an identifier
         */
        if (!TemplateInstance::semanticTiargs(e->loc, sc, e->args, 2))
            return new ErrorExp();
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Identifier *id = NULL;
        if (Parameter *po = isParameter(o))
        {
            id = po->ident;
            assert(id);
        }
        else
        {
            Dsymbol *s = getDsymbol(o);
            if (!s || !s->ident)
            {
                e->error("argument %s has no identifier", o->toChars());
                return new ErrorExp();
            }
            id = s->ident;
        }

        StringExp *se = new StringExp(e->loc, const_cast<char *>(id->toChars()));
        return semantic(se, sc);
    }
    else if (e->ident == Id::getProtection)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        Scope *sc2 = sc->push();
        sc2->flags = sc->flags | SCOPEnoaccesscheck;
        bool ok = TemplateInstance::semanticTiargs(e->loc, sc2, e->args, 1);
        sc2->pop();
        if (!ok)
            return new ErrorExp();

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s)
        {
            if (!isError(o))
                e->error("argument %s has no protection", o->toChars());
            return new ErrorExp();
        }
        if (s->semanticRun == PASSinit)
            s->semantic(NULL);

        const char *protName = protectionToChars(s->prot().kind);   // TODO: How about package(names)
        assert(protName);
        StringExp *se = new StringExp(e->loc, const_cast<char *>(protName));
        return semantic(se, sc);
    }
    else if (e->ident == Id::parent)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (s)
        {
            if (FuncDeclaration *fd = s->isFuncDeclaration())   // Bugzilla 8943
                s = fd->toAliasFunc();
            if (!s->isImport())  // Bugzilla 8922
                s = s->toParent();
        }
        if (!s || s->isImport())
        {
            e->error("argument %s has no parent", o->toChars());
            return new ErrorExp();
        }

        if (FuncDeclaration *f = s->isFuncDeclaration())
        {
            if (TemplateDeclaration *td = getFuncTemplateDecl(f))
            {
                if (td->overroot)       // if not start of overloaded list of TemplateDeclaration's
                    td = td->overroot;  // then get the start
                Expression *ex = new TemplateExp(e->loc, td, f);
                ex = semantic(ex, sc);
                return ex;
            }

            if (FuncLiteralDeclaration *fld = f->isFuncLiteralDeclaration())
            {
                // Directly translate to VarExp instead of FuncExp
                Expression *ex = new VarExp(e->loc, fld, true);
                return semantic(ex, sc);
            }
        }

        return resolve(e->loc, sc, s, false);
    }
    else if (e->ident == Id::hasMember ||
             e->ident == Id::getMember ||
             e->ident == Id::getOverloads ||
             e->ident == Id::getVirtualMethods ||
             e->ident == Id::getVirtualFunctions)
    {
        if (dim != 2)
            return dimError(e, 2, dim);

        RootObject *o = (*e->args)[0];
        Expression *ex = isExpression((*e->args)[1]);
        if (!ex)
        {
            e->error("expression expected as second argument of __traits %s", e->ident->toChars());
            return new ErrorExp();
        }
        ex = ex->ctfeInterpret();

        StringExp *se = ex->toStringExp();
        if (!se || se->len == 0)
        {
            e->error("string expected as second argument of __traits %s instead of %s", e->ident->toChars(), ex->toChars());
            return new ErrorExp();
        }
        se = se->toUTF8(sc);

        if (se->sz != 1)
        {
            e->error("string must be chars");
            return new ErrorExp();
        }
        Identifier *id = Identifier::idPool((char *)se->string, se->len);

        /* Prefer dsymbol, because it might need some runtime contexts.
         */
        Dsymbol *sym = getDsymbol(o);
        if (sym)
        {
            ex = new DsymbolExp(e->loc, sym);
            ex = new DotIdExp(e->loc, ex, id);
        }
        else if (Type *t = isType(o))
            ex = typeDotIdExp(e->loc, t, id);
        else if (Expression *ex2 = isExpression(o))
            ex = new DotIdExp(e->loc, ex2, id);
        else
        {
            e->error("invalid first argument");
            return new ErrorExp();
        }

        if (e->ident == Id::hasMember)
        {
            if (sym)
            {
                if (sym->search(e->loc, id))
                    return True(e);
            }

            /* Take any errors as meaning it wasn't found
             */
            Scope *scx = sc->push();
            scx->flags |= SCOPEignoresymbolvisibility;
            ex = trySemantic(ex, scx);
            scx->pop();
            return ex ? True(e) : False(e);
        }
        else if (e->ident == Id::getMember)
        {
            if (ex->op == TOKdotid)
                // Prevent semantic() from replacing Symbol with its initializer
                ((DotIdExp *)ex)->wantsym = true;
            Scope *scx = sc->push();
            scx->flags |= SCOPEignoresymbolvisibility;
            ex = semantic(ex, scx);
            scx->pop();
            return ex;
        }
        else if (e->ident == Id::getVirtualFunctions ||
                 e->ident == Id::getVirtualMethods ||
                 e->ident == Id::getOverloads)
        {
            unsigned errors = global.errors;
            Expression *eorig = ex;
            Scope *scx = sc->push();
            scx->flags |= SCOPEignoresymbolvisibility;
            ex = semantic(ex, scx);
            if (errors < global.errors)
                e->error("%s cannot be resolved", eorig->toChars());
            //ex->print();

            /* Create tuple of functions of ex
             */
            Expressions *exps = new Expressions();
            FuncDeclaration *f;
            if (ex->op == TOKvar)
            {
                VarExp *ve = (VarExp *)ex;
                f = ve->var->isFuncDeclaration();
                ex = NULL;
            }
            else if (ex->op == TOKdotvar)
            {
                DotVarExp *dve = (DotVarExp *)ex;
                f = dve->var->isFuncDeclaration();
                if (dve->e1->op == TOKdottype || dve->e1->op == TOKthis)
                    ex = NULL;
                else
                    ex = dve->e1;
            }
            else
                f = NULL;
            Ptrait p;
            p.exps = exps;
            p.e1 = ex;
            p.ident = e->ident;
            overloadApply(f, &p, &fptraits);

            ex = new TupleExp(e->loc, exps);
            ex = semantic(ex, scx);
            scx->pop();
            return ex;
        }
        else
            assert(0);
    }
    else if (e->ident == Id::classInstanceSize)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        ClassDeclaration *cd = s ? s->isClassDeclaration() : NULL;
        if (!cd)
        {
            e->error("first argument is not a class");
            return new ErrorExp();
        }
        if (cd->sizeok != SIZEOKdone)
        {
            cd->size(cd->loc);
        }
        if (cd->sizeok != SIZEOKdone)
        {
            e->error("%s %s is forward referenced", cd->kind(), cd->toChars());
            return new ErrorExp();
        }

        return new IntegerExp(e->loc, cd->structsize, Type::tsize_t);
    }
    else if (e->ident == Id::getAliasThis)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        AggregateDeclaration *ad = s ? s->isAggregateDeclaration() : NULL;
        if (!ad)
        {
            e->error("argument is not an aggregate type");
            return new ErrorExp();
        }

        Expressions *exps = new Expressions();
        if (ad->aliasthis)
            exps->push(new StringExp(e->loc, const_cast<char *>(ad->aliasthis->ident->toChars())));
        Expression *ex = new TupleExp(e->loc, exps);
        ex = semantic(ex, sc);
        return ex;
    }
    else if (e->ident == Id::getAttributes)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s)
        {
            e->error("first argument is not a symbol");
            return new ErrorExp();
        }
        if (Import *imp = s->isImport())
        {
            s = imp->mod;
        }

        //printf("getAttributes %s, attrs = %p, scope = %p\n", s->toChars(), s->userAttribDecl, s->_scope);
        UserAttributeDeclaration *udad = s->userAttribDecl;
        Expressions *exps = udad ? udad->getAttributes() : new Expressions();
        TupleExp *tup = new TupleExp(e->loc, exps);
        return semantic(tup, sc);
    }
    else if (e->ident == Id::getFunctionAttributes)
    {
        /// extract all function attributes as a tuple (const/shared/inout/pure/nothrow/etc) except UDAs.
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        Type *t = isType(o);
        TypeFunction *tf = NULL;
        if (s)
        {
            if (FuncDeclaration *f = s->isFuncDeclaration())
                t = f->type;
            else if (VarDeclaration *v = s->isVarDeclaration())
                t = v->type;
        }
        if (t)
        {
            if (t->ty == Tfunction)
                tf = (TypeFunction *)t;
            else if (t->ty == Tdelegate)
                tf = (TypeFunction *)t->nextOf();
            else if (t->ty == Tpointer && t->nextOf()->ty == Tfunction)
                tf = (TypeFunction *)t->nextOf();
        }
        if (!tf)
        {
            e->error("first argument is not a function");
            return new ErrorExp();
        }

        Expressions *mods = new Expressions();
        PushAttributes pa;
        pa.mods = mods;
        tf->modifiersApply(&pa, &PushAttributes::fp);
        tf->attributesApply(&pa, &PushAttributes::fp, TRUSTformatSystem);

        TupleExp *tup = new TupleExp(e->loc, mods);
        return semantic(tup, sc);
    }
    else if (e->ident == Id::getFunctionVariadicStyle)
    {
        /* Accept a symbol or a type. Returns one of the following:
         *  "none"      not a variadic function
         *  "argptr"    extern(D) void dstyle(...), use `__argptr` and `__arguments`
         *  "stdarg"    extern(C) void cstyle(int, ...), use core.stdc.stdarg
         *  "typesafe"  void typesafe(T[] ...)
         */
        // get symbol linkage as a string
        if (dim != 1)
            return dimError(e, 1, dim);

        LINK link;
        int varargs;
        RootObject *o = (*e->args)[0];
        Type *t = isType(o);
        TypeFunction *tf = NULL;
        if (t)
        {
            if (t->ty == Tfunction)
                tf = (TypeFunction *)t;
            else if (t->ty == Tdelegate)
                tf = (TypeFunction *)t->nextOf();
            else if (t->ty == Tpointer && t->nextOf()->ty == Tfunction)
                tf = (TypeFunction *)t->nextOf();
        }
        if (tf)
        {
            link = tf->linkage;
            varargs = tf->varargs;
        }
        else
        {
            Dsymbol *s = getDsymbol(o);
            FuncDeclaration *fd = NULL;
            if (!s || (fd = s->isFuncDeclaration()) == NULL)
            {
                e->error("argument to `__traits(getFunctionVariadicStyle, %s)` is not a function", o->toChars());
                return new ErrorExp();
            }
            link = fd->linkage;
            fd->getParameters(&varargs);
        }
        const char *style;
        switch (varargs)
        {
            case 0: style = "none";                      break;
            case 1: style = (link == LINKd) ? "argptr"
                                            : "stdarg";  break;
            case 2:     style = "typesafe";              break;
            default:
                assert(0);
        }
        StringExp *se = new StringExp(e->loc, const_cast<char*>(style));
        return semantic(se, sc);
    }
    else if (e->ident == Id::getParameterStorageClasses)
    {
        /* Accept a function symbol or a type, followed by a parameter index.
         * Returns a tuple of strings of the parameter's storage classes.
         */
        // get symbol linkage as a string
        if (dim != 2)
            return dimError(e, 2, dim);

        RootObject *o1 = (*e->args)[1];
        RootObject *o = (*e->args)[0];
        Type *t = isType(o);
        TypeFunction *tf = NULL;
        if (t)
        {
            if (t->ty == Tfunction)
                tf = (TypeFunction *)t;
            else if (t->ty == Tdelegate)
                tf = (TypeFunction *)t->nextOf();
            else if (t->ty == Tpointer && t->nextOf()->ty == Tfunction)
                tf = (TypeFunction *)t->nextOf();
        }
        Parameters* fparams;
        if (tf)
        {
            fparams = tf->parameters;
        }
        else
        {
            Dsymbol *s = getDsymbol(o);
            FuncDeclaration *fd = NULL;
            if (!s || (fd = s->isFuncDeclaration()) == NULL)
            {
                e->error("first argument to `__traits(getParameterStorageClasses, %s, %s)` is not a function",
                    o->toChars(), o1->toChars());
                return new ErrorExp();
            }
            fparams = fd->getParameters(NULL);
        }

        StorageClass stc;

        // Set stc to storage class of the ith parameter
        Expression *ex = isExpression((*e->args)[1]);
        if (!ex)
        {
            e->error("expression expected as second argument of `__traits(getParameterStorageClasses, %s, %s)`",
                o->toChars(), o1->toChars());
            return new ErrorExp();
        }
        ex = ex->ctfeInterpret();
        uinteger_t ii = ex->toUInteger();
        if (ii >= Parameter::dim(fparams))
        {
            e->error("parameter index must be in range 0..%u not %s", (unsigned)Parameter::dim(fparams), ex->toChars());
            return new ErrorExp();
        }

        unsigned n = (unsigned)ii;
        Parameter *p = Parameter::getNth(fparams, n);
        stc = p->storageClass;

        // This mirrors hdrgen.visit(Parameter p)
        if (p->type && p->type->mod & MODshared)
            stc &= ~STCshared;

        Expressions *exps = new Expressions;

        if (stc & STCauto)
            exps->push(new StringExp(e->loc, const_cast<char *>("auto")));
        if (stc & STCreturn)
            exps->push(new StringExp(e->loc, const_cast<char *>("return")));

        if (stc & STCout)
            exps->push(new StringExp(e->loc, const_cast<char *>("out")));
        else if (stc & STCref)
            exps->push(new StringExp(e->loc, const_cast<char *>("ref")));
        else if (stc & STCin)
            exps->push(new StringExp(e->loc, const_cast<char *>("in")));
        else if (stc & STClazy)
            exps->push(new StringExp(e->loc, const_cast<char *>("lazy")));
        else if (stc & STCalias)
            exps->push(new StringExp(e->loc, const_cast<char *>("alias")));

        if (stc & STCconst)
            exps->push(new StringExp(e->loc, const_cast<char *>("const")));
        if (stc & STCimmutable)
            exps->push(new StringExp(e->loc, const_cast<char *>("immutable")));
        if (stc & STCwild)
            exps->push(new StringExp(e->loc, const_cast<char *>("inout")));
        if (stc & STCshared)
            exps->push(new StringExp(e->loc, const_cast<char *>("shared")));
        if (stc & STCscope && !(stc & STCscopeinferred))
            exps->push(new StringExp(e->loc, const_cast<char *>("scope")));

        TupleExp *tup = new TupleExp(e->loc, exps);
        return semantic(tup, sc);
    }
    else if (e->ident == Id::getLinkage)
    {
        // get symbol linkage as a string
        if (dim != 1)
            return dimError(e, 1, dim);

        LINK link;
        RootObject *o = (*e->args)[0];
        Type *t = isType(o);
        TypeFunction *tf = NULL;
        if (t)
        {
            if (t->ty == Tfunction)
                tf = (TypeFunction *)t;
            else if (t->ty == Tdelegate)
                tf = (TypeFunction *)t->nextOf();
            else if (t->ty == Tpointer && t->nextOf()->ty == Tfunction)
                tf = (TypeFunction *)t->nextOf();
        }
        if (tf)
            link = tf->linkage;
        else
        {
            Dsymbol *s = getDsymbol(o);
            Declaration *d = NULL;
            if (!s || (d = s->isDeclaration()) == NULL)
            {
                e->error("argument to `__traits(getLinkage, %s)` is not a declaration", o->toChars());
                return new ErrorExp();
            }
            link = d->linkage;
        }
        const char *linkage = linkageToChars(link);
        StringExp *se = new StringExp(e->loc, const_cast<char *>(linkage));
        return semantic(se, sc);
    }
    else if (e->ident == Id::allMembers ||
             e->ident == Id::derivedMembers)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s)
        {
            e->error("argument has no members");
            return new ErrorExp();
        }
        if (Import *imp = s->isImport())
        {
            // Bugzilla 9692
            s = imp->mod;
        }

        ScopeDsymbol *sds = s->isScopeDsymbol();
        if (!sds || sds->isTemplateDeclaration())
        {
            e->error("%s %s has no members", s->kind(), s->toChars());
            return new ErrorExp();
        }

        // use a struct as local function
        struct PushIdentsDg
        {
            ScopeDsymbol *sds;
            Identifiers *idents;

            static int dg(void *ctx, size_t, Dsymbol *sm)
            {
                if (!sm)
                    return 1;
                //printf("\t[%i] %s %s\n", i, sm->kind(), sm->toChars());
                if (sm->ident)
                {
                    const char *idx = sm->ident->toChars();
                    if (idx[0] == '_' && idx[1] == '_' &&
                        sm->ident != Id::ctor &&
                        sm->ident != Id::dtor &&
                        sm->ident != Id::__xdtor &&
                        sm->ident != Id::postblit &&
                        sm->ident != Id::__xpostblit)
                    {
                        return 0;
                    }

                    if (sm->ident == Id::empty)
                    {
                        return 0;
                    }
                    if (sm->isTypeInfoDeclaration()) // Bugzilla 15177
                        return 0;
                    PushIdentsDg *pid = (PushIdentsDg *)ctx;
                    if (!pid->sds->isModule() && sm->isImport()) // Bugzilla 17057
                        return 0;

                    //printf("\t%s\n", sm->ident->toChars());
                    Identifiers *idents = pid->idents;

                    /* Skip if already present in idents[]
                     */
                    for (size_t j = 0; j < idents->dim; j++)
                    {
                        Identifier *id = (*idents)[j];
                        if (id == sm->ident)
                            return 0;
                    }

                    idents->push(sm->ident);
                }
                else
                {
                    EnumDeclaration *ed = sm->isEnumDeclaration();
                    if (ed)
                    {
                        ScopeDsymbol_foreach(NULL, ed->members, &PushIdentsDg::dg, ctx);
                    }
                }
                return 0;
            }
        };

        Identifiers *idents = new Identifiers;
        PushIdentsDg ctx;
        ctx.sds = sds;
        ctx.idents = idents;
        ScopeDsymbol_foreach(sc, sds->members, &PushIdentsDg::dg, &ctx);
        ClassDeclaration *cd = sds->isClassDeclaration();
        if (cd && e->ident == Id::allMembers)
        {
            if (cd->semanticRun < PASSsemanticdone)
                cd->semantic(NULL);    // Bugzilla 13668: Try to resolve forward reference

            struct PushBaseMembers
            {
                static void dg(ClassDeclaration *cd, PushIdentsDg *ctx)
                {
                    for (size_t i = 0; i < cd->baseclasses->dim; i++)
                    {
                        ClassDeclaration *cb = (*cd->baseclasses)[i]->sym;
                        assert(cb);
                        ScopeDsymbol_foreach(NULL, cb->members, &PushIdentsDg::dg, ctx);
                        if (cb->baseclasses->dim)
                            dg(cb, ctx);
                    }
                }
            };
            PushBaseMembers::dg(cd, &ctx);
        }

        // Turn Identifiers into StringExps reusing the allocated array
        assert(sizeof(Expressions) == sizeof(Identifiers));
        Expressions *exps = (Expressions *)idents;
        for (size_t i = 0; i < idents->dim; i++)
        {
            Identifier *id = (*idents)[i];
            StringExp *se = new StringExp(e->loc, const_cast<char *>(id->toChars()));
            (*exps)[i] = se;
        }

        /* Making this a tuple is more flexible, as it can be statically unrolled.
         * To make an array literal, enclose __traits in [ ]:
         *   [ __traits(allMembers, ...) ]
         */
        Expression *ex = new TupleExp(e->loc, exps);
        ex = semantic(ex, sc);
        return ex;
    }
    else if (e->ident == Id::compiles)
    {
        /* Determine if all the objects - types, expressions, or symbols -
         * compile without error
         */
        if (!dim)
            return False(e);

        for (size_t i = 0; i < dim; i++)
        {
            unsigned errors = global.startGagging();
            Scope *sc2 = sc->push();
            sc2->tinst = NULL;
            sc2->minst = NULL;
            sc2->flags = (sc->flags & ~(SCOPEctfe | SCOPEcondition)) | SCOPEcompile | SCOPEfullinst;
            bool err = false;

            RootObject *o = (*e->args)[i];
            Type *t = isType(o);
            Expression *ex = t ? typeToExpression(t) : isExpression(o);
            if (!ex && t)
            {
                Dsymbol *s;
                t->resolve(e->loc, sc2, &ex, &t, &s);
                if (t)
                {
                    t->semantic(e->loc, sc2);
                    if (t->ty == Terror)
                        err = true;
                }
                else if (s && s->errors)
                    err = true;
            }
            if (ex)
            {
                ex = semantic(ex, sc2);
                ex = resolvePropertiesOnly(sc2, ex);
                ex = ex->optimize(WANTvalue);
                if (sc2->func && sc2->func->type->ty == Tfunction)
                {
                    TypeFunction *tf = (TypeFunction *)sc2->func->type;
                    canThrow(ex, sc2->func, tf->isnothrow);
                }
                ex = checkGC(sc2, ex);
                if (ex->op == TOKerror)
                    err = true;
            }

            // Carefully detach the scope from the parent and throw it away as
            // we only need it to evaluate the expression
            // https://issues.dlang.org/show_bug.cgi?id=15428
            freeFieldinit(sc2);
            sc2->enclosing = NULL;
            sc2->pop();

            if (global.endGagging(errors) || err)
            {
                return False(e);
            }
        }
        return True(e);
    }
    else if (e->ident == Id::isSame)
    {
        /* Determine if two symbols are the same
         */
        if (dim != 2)
            return dimError(e, 2, dim);

        if (!TemplateInstance::semanticTiargs(e->loc, sc, e->args, 0))
            return new ErrorExp();

        RootObject *o1 = (*e->args)[0];
        RootObject *o2 = (*e->args)[1];
        Dsymbol *s1 = getDsymbol(o1);
        Dsymbol *s2 = getDsymbol(o2);
        //printf("isSame: %s, %s\n", o1->toChars(), o2->toChars());
        if (!s1 && !s2)
        {
            Expression *ea1 = isExpression(o1);
            Expression *ea2 = isExpression(o2);
            if (ea1 && ea2)
            {
                if (ea1->equals(ea2))
                    return True(e);
            }
        }
        if (!s1 || !s2)
            return False(e);
        s1 = s1->toAlias();
        s2 = s2->toAlias();

        if (s1->isFuncAliasDeclaration())
            s1 = ((FuncAliasDeclaration *)s1)->toAliasFunc();
        if (s2->isFuncAliasDeclaration())
            s2 = ((FuncAliasDeclaration *)s2)->toAliasFunc();

        return (s1 == s2) ? True(e) : False(e);
    }
    else if (e->ident == Id::getUnitTests)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s)
        {
            e->error("argument %s to __traits(getUnitTests) must be a module or aggregate",
                o->toChars());
            return new ErrorExp();
        }
        if (Import *imp = s->isImport())  // Bugzilla 10990
            s = imp->mod;

        ScopeDsymbol* sds = s->isScopeDsymbol();
        if (!sds)
        {
            e->error("argument %s to __traits(getUnitTests) must be a module or aggregate, not a %s",
                s->toChars(), s->kind());
            return new ErrorExp();
        }

        Expressions *exps = new Expressions();
        if (global.params.useUnitTests)
        {
            // Should actually be a set
            AA* uniqueUnitTests = NULL;
            collectUnitTests(sds->members, uniqueUnitTests, exps);
        }
        TupleExp *te= new TupleExp(e->loc, exps);
        return semantic(te, sc);
    }
    else if(e->ident == Id::getVirtualIndex)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);

        FuncDeclaration *fd = s ? s->isFuncDeclaration() : NULL;
        if (!fd)
        {
            e->error("first argument to __traits(getVirtualIndex) must be a function");
            return new ErrorExp();
        }

        fd = fd->toAliasFunc(); // Neccessary to support multiple overloads.
        return new IntegerExp(e->loc, fd->vtblIndex, Type::tptrdiff_t);
    }
    else if (e->ident == Id::getPointerBitmap)
    {
        return pointerBitmap(e);
    }

    if (const char *sub = (const char *)speller(e->ident->toChars(), &trait_search_fp, NULL, idchars))
        e->error("unrecognized trait '%s', did you mean '%s'?", e->ident->toChars(), sub);
    else
        e->error("unrecognized trait '%s'", e->ident->toChars());
    return new ErrorExp();

    e->error("wrong number of arguments %d", (int)dim);
    return new ErrorExp();
}
