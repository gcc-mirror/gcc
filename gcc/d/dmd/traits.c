
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
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
#include "target.h"

typedef int (*ForeachDg)(void *ctx, size_t idx, Dsymbol *s);
int ScopeDsymbol_foreach(Scope *sc, Dsymbols *members, ForeachDg dg, void *ctx, size_t *pn = NULL);
void freeFieldinit(Scope *sc);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);
Package *resolveIsPackage(Dsymbol *sym);
Expression *typeToExpression(Type *t);
Type *decoToType(const char *deco);
bool expressionsToString(OutBuffer &buf, Scope *sc, Expressions *exps);


/************************************************
 * Delegate to be passed to overloadApply() that looks
 * for functions matching a trait.
 */

struct Ptrait
{
    Dsymbol *sym;
    Expression *e1;
    Expressions *exps;          // collected results
    Identifier *ident;          // which trait we're looking for
    bool includeTemplates;
    AA **funcTypeHash;
};

/* Compute the function signature and insert it in the
 * hashtable, if not present. This is needed so that
 * traits(getOverlods, F3, "visit") does not count `int visit(int)`
 * twice in the following example:
 *
 * =============================================
 * interface F1 { int visit(int);}
 * interface F2 { int visit(int); void visit(); }
 * interface F3 : F2, F1 {}
 *==============================================
 */
static void insertInterfaceInheritedFunction(Ptrait *p, FuncDeclaration *fd, Expression *e)
{
    Identifier *signature = Identifier::idPool(fd->type->toChars());
    //printf("%s - %s\n", fd->toChars, signature);
    if (!dmd_aaGetRvalue(*p->funcTypeHash, (void *)signature))
    {
        bool* value = (bool*) dmd_aaGet(p->funcTypeHash, (void *)signature);
        *value = true;
        p->exps->push(e);
    }
}

static int fptraits(void *param, Dsymbol *s)
{
    Ptrait *p = (Ptrait *)param;
    if (p->includeTemplates)
    {
        p->exps->push(new DsymbolExp(Loc(),s, false));
        return 0;
    }
    FuncDeclaration *fd = s->isFuncDeclaration();
    if (!fd)
        return 0;

    if (p->ident == Id::getVirtualFunctions && !fd->isVirtual())
        return 0;

    if (p->ident == Id::getVirtualMethods && !fd->isVirtualMethod())
        return 0;

    Expression *e;
    FuncAliasDeclaration* ad = new FuncAliasDeclaration(fd->ident, fd, false);
    ad->protection = fd->protection;
    if (p->e1)
        e = new DotVarExp(Loc(), p->e1, ad, false);
    else
        e = new DsymbolExp(Loc(), ad, false);
     // if the parent is an interface declaration
     // we must check for functions with the same signature
     // in different inherited interfaces
     if (p->sym && p->sym->isInterfaceDeclaration())
         insertInterfaceInheritedFunction(p, fd, e);
     else
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
    for (size_t i = 0; i < symbols->length; i++)
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
                Dsymbols *decl = attrDecl->include(NULL);
                collectUnitTests(decl, uniqueUnitTests, unitTests);
            }
        }
    }
}

/***************************************************
 * Determine if type t is copyable.
 * Params:
 *      t = type to check
 * Returns:
 *      true if we can copy it
 */
static bool isCopyable(Type *t)
{
    //printf("isCopyable() %s\n", t->toChars());
    if (TypeStruct *ts = t->isTypeStruct())
    {
        if (ts->sym->postblit &&
            (ts->sym->postblit->storage_class & STCdisable))
            return false;
    }
    return true;
}

/************************ TraitsExp ************************************/

static Expression *True(TraitsExp *e)  { return new IntegerExp(e->loc, true, Type::tbool); }
static Expression *False(TraitsExp *e) { return new IntegerExp(e->loc, false, Type::tbool); }

/**************************************
 * Convert `Expression` or `Type` to corresponding `Dsymbol`,
 * additionally strip off expression contexts.
 *
 * Some symbol related `__traits` ignore arguments expression contexts.
 * For example:
 *  struct S { void f() {} }
 *  S s;
 *  pragma(msg, __traits(isNested, s.f));
 *  // s.f is DotVarExp, but __traits(isNested) needs a FuncDeclaration.
 *
 * This is used for that common `__traits` behavior.
 */
static Dsymbol *getDsymbolWithoutExpCtx(RootObject *oarg)
{
    if (Expression *e = isExpression(oarg))
    {
        if (e->op == TOKdotvar)
            return ((DotVarExp *)e)->var;
        if (e->op == TOKdottd)
            return ((DotTemplateExp *)e)->td;
    }
    return getDsymbol(oarg);
}

/**
   Gets the function type from a given AST node
   if the node is a function of some sort.

 Params:
    o = an AST node to check for a `TypeFunction`
    fdp = optional pointer to a function declararion, to be set
      if `o` is a function declarartion.

 Returns:
    a type node if `o` is a declaration of
        a delegate, function, function-pointer
      or a variable of the former.  Otherwise, `null`.
*/
static TypeFunction *toTypeFunction(RootObject *o, FuncDeclaration **fdp = NULL)
{
    Dsymbol *s = getDsymbolWithoutExpCtx(o);
    Type *t = isType(o);
    TypeFunction *tf = NULL;

    if (s)
    {
        FuncDeclaration *fd = s->isFuncDeclaration();
        if (fd)
        {
            t = fd->type;
            if (fdp)
                *fdp = fd;
        }
        else if (VarDeclaration *vd = s->isVarDeclaration())
            t = vd->type;
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

    return tf;
}

static bool isTypeArithmetic(Type *t)       { return t->isintegral() || t->isfloating(); }
static bool isTypeFloating(Type *t)         { return t->isfloating(); }
static bool isTypeIntegral(Type *t)         { return t->isintegral(); }
static bool isTypeScalar(Type *t)           { return t->isscalar(); }
static bool isTypeUnsigned(Type *t)         { return t->isunsigned(); }
static bool isTypeAssociativeArray(Type *t) { return t->toBasetype()->ty == Taarray; }
static bool isTypeStaticArray(Type *t)      { return t->toBasetype()->ty == Tsarray; }
static bool isTypeAbstractClass(Type *t)    { return t->toBasetype()->ty == Tclass && ((TypeClass *)t->toBasetype())->sym->isAbstract(); }
static bool isTypeFinalClass(Type *t)       { return t->toBasetype()->ty == Tclass && (((TypeClass *)t->toBasetype())->sym->storage_class & STCfinal) != 0; }

static Expression *isTypeX(TraitsExp *e, bool (*fp)(Type *t))
{
    if (!e->args || !e->args->length)
        return False(e);
    for (size_t i = 0; i < e->args->length; i++)
    {
        Type *t = getType((*e->args)[i]);
        if (!t || !fp(t))
            return False(e);
    }
    return True(e);
}

static bool isDsymDeprecated(Dsymbol *s) { return s->isDeprecated(); }

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

static Expression *isDsymX(TraitsExp *e, bool (*fp)(Dsymbol *s))
{
    if (!e->args || !e->args->length)
        return False(e);
    for (size_t i = 0; i < e->args->length; i++)
    {
        Dsymbol *s = getDsymbolWithoutExpCtx((*e->args)[i]);
        if (!s || !fp(s))
            return False(e);
    }
    return True(e);
}

static bool isFuncAbstractFunction(FuncDeclaration *f) { return f->isAbstract(); }
static bool isFuncVirtualFunction(FuncDeclaration *f) { return f->isVirtual(); }
static bool isFuncVirtualMethod(FuncDeclaration *f) { return f->isVirtualMethod(); }
static bool isFuncFinalFunction(FuncDeclaration *f) { return f->isFinalFunc(); }
static bool isFuncStaticFunction(FuncDeclaration *f) { return !f->needThis() && !f->isNested(); }
static bool isFuncOverrideFunction(FuncDeclaration *f) { return f->isOverride(); }

static Expression *isFuncX(TraitsExp *e, bool (*fp)(FuncDeclaration *f))
{
    if (!e->args || !e->args->length)
        return False(e);
    for (size_t i = 0; i < e->args->length; i++)
    {
        Dsymbol *s = getDsymbolWithoutExpCtx((*e->args)[i]);
        if (!s)
            return False(e);
        FuncDeclaration *f = s->isFuncDeclaration();
        if (!f || !fp(f))
            return False(e);
    }
    return True(e);
}

static bool isDeclDisabled(Declaration *d) { return d->isDisabled(); }
static bool isDeclFuture(Declaration *d) { return d->isFuture(); }
static bool isDeclRef(Declaration *d) { return d->isRef(); }
static bool isDeclOut(Declaration *d) { return d->isOut(); }
static bool isDeclLazy(Declaration *d) { return (d->storage_class & STClazy) != 0; }

static Expression *isDeclX(TraitsExp *e, bool (*fp)(Declaration *d))
{
    if (!e->args || !e->args->length)
        return False(e);
    for (size_t i = 0; i < e->args->length; i++)
    {
        Dsymbol *s = getDsymbolWithoutExpCtx((*e->args)[i]);
        if (!s)
            return False(e);
        Declaration *d = s->isDeclaration();
        if (!d || !fp(d))
            return False(e);
    }
    return True(e);
}

static bool isPkgModule(Package *p) { return p->isModule() || p->isPackageMod(); }
static bool isPkgPackage(Package *p) { return p->isModule() == NULL; }

static Expression *isPkgX(TraitsExp *e, bool (*fp)(Package *p))
{
    if (!e->args || !e->args->length)
        return False(e);
    for (size_t i = 0; i < e->args->length; i++)
    {
        Dsymbol *s = getDsymbolWithoutExpCtx((*e->args)[i]);
        if (!s)
            return False(e);
        Package *p = resolveIsPackage(s);
        if (!p || !fp(p))
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
        "isDisabled",
        "isDeprecated",
        "isFuture",
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
        "isModule",
        "isPackage",
        "isRef",
        "isOut",
        "isLazy",
        "isReturnOnStack",
        "hasMember",
        "identifier",
        "getProtection",
        "getVisibility",
        "parent",
        "child",
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
        "getAliasThis",
        "getAttributes",
        "getFunctionAttributes",
        "getFunctionVariadicStyle",
        "getParameterStorageClasses",
        "getUnitTests",
        "getVirtualIndex",
        "getPointerBitmap",
        "isZeroInit",
        "getTargetInfo",
        "getLocation",
        "hasPostblit",
        "isCopyable",
        NULL
    };

    traitsStringTable._init(56);

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
    if (!e->args || e->args->length != 1)
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
            for (size_t i = 0; i < t->sym->fields.length; i++)
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

            for (size_t i = 0; i < t->sym->fields.length; i++)
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
    if (e->ident != Id::compiles &&
        e->ident != Id::isSame &&
        e->ident != Id::identifier &&
        e->ident != Id::getProtection && e->ident != Id::getVisibility &&
        e->ident != Id::getAttributes)
    {
        // Pretend we're in a deprecated scope so that deprecation messages
        // aren't triggered when checking if a symbol is deprecated
        const StorageClass save = sc->stc;
        if (e->ident == Id::isDeprecated)
            sc->stc |= STCdeprecated;
        if (!TemplateInstance::semanticTiargs(e->loc, sc, e->args, 1))
        {
            sc->stc = save;
            return new ErrorExp();
        }
        sc->stc = save;
    }
    size_t dim = e->args ? e->args->length : 0;

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
    else if (e->ident == Id::isDeprecated)
    {
        return isDsymX(e, &isDsymDeprecated);
    }
    else if (e->ident == Id::isFuture)
    {
        return isDeclX(e, &isDeclFuture);
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
        if (dim != 1)
            return dimError(e, 1, dim);

        return isDsymX(e, &isTemplate);
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
    else if (e->ident == Id::hasPostblit)
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
            return sd->postblit ? True(e) : False(e);
        }
        return False(e);
    }
    else if (e->ident == Id::isCopyable)
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

        return isCopyable(t) ? True(e) : False(e);
    }
    else if (e->ident == Id::isNested)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbolWithoutExpCtx(o);
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

        e->error("aggregate or function expected instead of `%s`", o->toChars());
        return new ErrorExp();
    }
    else if (e->ident == Id::isDisabled)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isDeclX(e, &isDeclDisabled);
    }
    else if (e->ident == Id::isAbstractFunction)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncAbstractFunction);
    }
    else if (e->ident == Id::isVirtualFunction)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncVirtualFunction);
    }
    else if (e->ident == Id::isVirtualMethod)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncVirtualMethod);
    }
    else if (e->ident == Id::isFinalFunction)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncFinalFunction);
    }
    else if (e->ident == Id::isOverrideFunction)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncOverrideFunction);
    }
    else if (e->ident == Id::isStaticFunction)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isFuncX(e, &isFuncStaticFunction);
    }
    else if (e->ident == Id::isModule)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isPkgX(e, &isPkgModule);
    }
    else if (e->ident == Id::isPackage)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isPkgX(e, &isPkgPackage);
    }
    else if (e->ident == Id::isRef)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isDeclX(e, &isDeclRef);
    }
    else if (e->ident == Id::isOut)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        return isDeclX(e, &isDeclOut);
    }
    else if (e->ident == Id::isLazy)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

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
            if (!po->ident)
            {
                e->error("argument `%s` has no identifier", po->type->toChars());
                return new ErrorExp();
            }
            id = po->ident;
        }
        else
        {
            Dsymbol *s = getDsymbolWithoutExpCtx(o);
            if (!s || !s->ident)
            {
                e->error("argument %s has no identifier", o->toChars());
                return new ErrorExp();
            }
            id = s->ident;
        }

        StringExp *se = new StringExp(e->loc, const_cast<char *>(id->toChars()));
        return expressionSemantic(se, sc);
    }
    else if (e->ident == Id::getProtection || e->ident == Id::getVisibility)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        Scope *sc2 = sc->push();
        sc2->flags = sc->flags | SCOPEnoaccesscheck | SCOPEignoresymbolvisibility;
        bool ok = TemplateInstance::semanticTiargs(e->loc, sc2, e->args, 1);
        sc2->pop();
        if (!ok)
            return new ErrorExp();

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbolWithoutExpCtx(o);
        if (!s)
        {
            if (!isError(o))
                e->error("argument %s has no protection", o->toChars());
            return new ErrorExp();
        }
        if (s->semanticRun == PASSinit)
            dsymbolSemantic(s, NULL);

        const char *protName = protectionToChars(s->prot().kind);   // TODO: How about package(names)
        assert(protName);
        StringExp *se = new StringExp(e->loc, const_cast<char *>(protName));
        return expressionSemantic(se, sc);
    }
    else if (e->ident == Id::parent)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbolWithoutExpCtx(o);
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
                ex = expressionSemantic(ex, sc);
                return ex;
            }

            if (FuncLiteralDeclaration *fld = f->isFuncLiteralDeclaration())
            {
                // Directly translate to VarExp instead of FuncExp
                Expression *ex = new VarExp(e->loc, fld, true);
                return expressionSemantic(ex, sc);
            }
        }

        return resolve(e->loc, sc, s, false);
    }
    else if (e->ident == Id::child)
    {
        if (dim != 2)
            return dimError(e, 2, dim);

        Expression *ex;
        RootObject *op = (*e->args)[0];
        if (Dsymbol *symp = getDsymbol(op))
            ex = new DsymbolExp(e->loc, symp);
        else if (Expression *exp = isExpression(op))
            ex = exp;
        else
        {
            e->error("symbol or expression expected as first argument of __traits `child` instead of `%s`", op->toChars());
            return new ErrorExp();
        }

        ex = expressionSemantic(ex, sc);
        RootObject *oc = (*e->args)[1];
        Dsymbol *symc = getDsymbol(oc);
        if (!symc)
        {
            e->error("symbol expected as second argument of __traits `child` instead of `%s`", oc->toChars());
            return new ErrorExp();
        }

        if (Declaration *d = symc->isDeclaration())
            ex = new DotVarExp(e->loc, ex, d);
        else if (TemplateDeclaration *td = symc->isTemplateDeclaration())
            ex = new DotExp(e->loc, ex, new TemplateExp(e->loc, td));
        else if (ScopeDsymbol *ti = symc->isScopeDsymbol())
            ex = new DotExp(e->loc, ex, new ScopeExp(e->loc, ti));
        else
            assert(0);

        ex = expressionSemantic(ex, sc);
        return ex;
    }
    else if (e->ident == Id::toType)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        Expression *ex = isExpression((*e->args)[0]);
        if (!ex)
        {
            e->error("expression expected as second argument of __traits `%s`", e->ident->toChars());
            return new ErrorExp();
        }
        ex = ex->ctfeInterpret();

        StringExp *se = semanticString(sc, ex, "__traits(toType, string)");
        if (!se)
        {
            return new ErrorExp();
        }
        Type *t = decoToType(se->toUTF8(sc)->toPtr());
        if (!t)
        {
            e->error("cannot determine `%s`", e->toChars());
            return new ErrorExp();
        }
        ex = new TypeExp(e->loc, t);
        ex = expressionSemantic(ex, sc);
        return ex;
    }
    else if (e->ident == Id::hasMember ||
             e->ident == Id::getMember ||
             e->ident == Id::getOverloads ||
             e->ident == Id::getVirtualMethods ||
             e->ident == Id::getVirtualFunctions)
    {
        if (dim != 2 && !(dim == 3 && e->ident == Id::getOverloads))
            return dimError(e, 2, dim);

        RootObject *o = (*e->args)[0];
        Expression *ex = isExpression((*e->args)[1]);
        if (!ex)
        {
            e->error("expression expected as second argument of __traits %s", e->ident->toChars());
            return new ErrorExp();
        }
        ex = ex->ctfeInterpret();

        bool includeTemplates = false;
        if (dim == 3 && e->ident == Id::getOverloads)
        {
            Expression *b = isExpression((*e->args)[2]);
            b = b->ctfeInterpret();
            if (!b->type->equals(Type::tbool))
            {
                e->error("`bool` expected as third argument of `__traits(getOverloads)`, not `%s` of type `%s`", b->toChars(), b->type->toChars());
                return new ErrorExp();
            }
            includeTemplates = b->isBool(true);
        }

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
            if (e->ident == Id::hasMember)
            {
                if (sym->search(e->loc, id) != NULL)
                    return True(e);
            }
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

        // ignore symbol visibility and disable access checks for these traits
        Scope *scx = sc->push();
        scx->flags |= SCOPEignoresymbolvisibility | SCOPEnoaccesscheck;

        if (e->ident == Id::hasMember)
        {
            /* Take any errors as meaning it wasn't found
             */
            ex = trySemantic(ex, scx);
            scx->pop();
            return ex ? True(e) : False(e);
        }
        else if (e->ident == Id::getMember)
        {
            if (ex->op == TOKdotid)
                // Prevent semantic() from replacing Symbol with its initializer
                ((DotIdExp *)ex)->wantsym = true;
            ex = expressionSemantic(ex, scx);
            scx->pop();
            return ex;
        }
        else if (e->ident == Id::getVirtualFunctions ||
                 e->ident == Id::getVirtualMethods ||
                 e->ident == Id::getOverloads)
        {
            unsigned errors = global.errors;
            Expression *eorig = ex;
            ex = expressionSemantic(ex, scx);
            if (errors < global.errors)
                e->error("%s cannot be resolved", eorig->toChars());
            //ex->print();

            /* Create tuple of functions of ex
             */
            Expressions *exps = new Expressions();
            Dsymbol *f;
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
            else if (ex->op == TOKtemplate)
            {
                TemplateExp *te = (TemplateExp *)ex;
                TemplateDeclaration *td = te->td;
                f = td;
                if (td && td->funcroot)
                    f = td->funcroot;
                ex = NULL;
            }
            else
                f = NULL;
            Ptrait p;
            p.sym = sym;
            p.exps = exps;
            p.e1 = ex;
            p.ident = e->ident;
            p.includeTemplates = includeTemplates;
            AA *funcTypeHash = NULL;
            p.funcTypeHash = &funcTypeHash;

            InterfaceDeclaration *ifd = NULL;
            if (sym)
                ifd = sym->isInterfaceDeclaration();
            // If the symbol passed as a parameter is an
            // interface that inherits other interfaces
            if (ifd && ifd->interfaces.length)
            {
                // check the overloads of each inherited interface individually
                for (size_t i = 0; i < ifd->interfaces.length; i++)
                {
                    BaseClass *bc = ifd->interfaces.ptr[i];
                    if (Dsymbol *fd = bc->sym->search(e->loc, f->ident))
                        overloadApply(fd, &p, &fptraits);
                }
            }
            else
                overloadApply(f, &p, &fptraits);

            ex = new TupleExp(e->loc, exps);
            ex = expressionSemantic(ex, scx);
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
        ex = expressionSemantic(ex, sc);
        return ex;
    }
    else if (e->ident == Id::getAttributes)
    {
        /* Specify 0 for bit 0 of the flags argument to semanticTiargs() so that
         * a symbol should not be folded to a constant.
         * Bit 1 means don't convert Parameter to Type if Parameter has an identifier
         */
        if (!TemplateInstance::semanticTiargs(e->loc, sc, e->args, 3))
            return new ErrorExp();

        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Parameter *po = isParameter(o);
        Dsymbol *s = getDsymbolWithoutExpCtx(o);
        UserAttributeDeclaration *udad = NULL;
        if (po)
        {
            udad = po->userAttribDecl;
        }
        else if (s)
        {
            if (Import *imp = s->isImport())
            {
                s = imp->mod;
            }
            //printf("getAttributes %s, attrs = %p, scope = %p\n", s->toChars(), s->userAttribDecl, s->_scope);
            udad = s->userAttribDecl;
        }
        else
        {
            e->error("first argument is not a symbol");
            return new ErrorExp();
        }

        Expressions *exps = udad ? udad->getAttributes() : new Expressions();
        TupleExp *tup = new TupleExp(e->loc, exps);
        return expressionSemantic(tup, sc);
    }
    else if (e->ident == Id::getFunctionAttributes)
    {
        /* extract all function attributes as a tuple (const/shared/inout/pure/nothrow/etc) except UDAs.
         * https://dlang.org/spec/traits.html#getFunctionAttributes
         */
        if (dim != 1)
            return dimError(e, 1, dim);

        TypeFunction *tf = toTypeFunction((*e->args)[0]);

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
        return expressionSemantic(tup, sc);
    }
    else if (e->ident == Id::isReturnOnStack)
    {
        /* Extract as a boolean if function return value is on the stack
         * https://dlang.org/spec/traits.html#isReturnOnStack
         */
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        FuncDeclaration *fd = NULL;
        TypeFunction *tf = toTypeFunction(o, &fd);

        if (!tf)
        {
            e->error("argument to `__traits(isReturnOnStack, %s)` is not a function", o->toChars());
            return new ErrorExp();
        }

        bool value = target.isReturnOnStack(tf, fd && fd->needThis());
        return new IntegerExp(e->loc, value, Type::tbool);
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
        VarArg varargs;
        RootObject *o = (*e->args)[0];
        FuncDeclaration *fd = NULL;
        TypeFunction *tf = toTypeFunction(o, &fd);

        if (tf)
        {
            link = tf->linkage;
            varargs = tf->parameterList.varargs;
        }
        else
        {
            if (!fd)
            {
                e->error("argument to `__traits(getFunctionVariadicStyle, %s)` is not a function", o->toChars());
                return new ErrorExp();
            }
            link = fd->linkage;
            varargs = fd->getParameterList().varargs;
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
        return expressionSemantic(se, sc);
    }
    else if (e->ident == Id::getParameterStorageClasses)
    {
        /* Accept a function symbol or a type, followed by a parameter index.
         * Returns a tuple of strings of the parameter's storage classes.
         */
        // get symbol linkage as a string
        if (dim != 2)
            return dimError(e, 2, dim);

        RootObject *o = (*e->args)[0];
        RootObject *o1 = (*e->args)[1];

        FuncDeclaration *fd = NULL;
        TypeFunction *tf = toTypeFunction(o, &fd);

        ParameterList fparams;
        if (tf)
        {
            fparams = tf->parameterList;
        }
        else
        {
            if (!fd)
            {
                e->error("first argument to `__traits(getParameterStorageClasses, %s, %s)` is not a function",
                    o->toChars(), o1->toChars());
                return new ErrorExp();
            }
            fparams = fd->getParameterList();
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
        if (ii >= fparams.length())
        {
            e->error("parameter index must be in range 0..%u not %s", (unsigned)fparams.length(), ex->toChars());
            return new ErrorExp();
        }

        unsigned n = (unsigned)ii;
        Parameter *p = fparams[n];
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
        return expressionSemantic(tup, sc);
    }
    else if (e->ident == Id::getLinkage)
    {
        // get symbol linkage as a string
        if (dim != 1)
            return dimError(e, 1, dim);

        LINK link;
        RootObject *o = (*e->args)[0];

        TypeFunction *tf = toTypeFunction(o);

        if (tf)
            link = tf->linkage;
        else
        {
            Dsymbol *s = getDsymbol(o);
            Declaration *d = NULL;
            AggregateDeclaration *ad = NULL;
            if (!s || ((d = s->isDeclaration()) == NULL
                       && (ad = s->isAggregateDeclaration()) == NULL))
            {
                e->error("argument to `__traits(getLinkage, %s)` is not a declaration", o->toChars());
                return new ErrorExp();
            }
            if (d != NULL)
                link = d->linkage;
            else
            {
                switch (ad->classKind)
                {
                    case ClassKind::d:
                        link = LINKd;
                        break;
                    case ClassKind::cpp:
                        link = LINKcpp;
                        break;
                    case ClassKind::objc:
                        link = LINKobjc;
                        break;
                    default:
                        assert(0);
                }
            }
        }
        const char *linkage = linkageToChars(link);
        StringExp *se = new StringExp(e->loc, const_cast<char *>(linkage));
        return expressionSemantic(se, sc);
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

        // https://issues.dlang.org/show_bug.cgi?id=16044
        if (Package *p = s->isPackage())
        {
            if (Module *pm = p->isPackageMod())
                s = pm;
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

                // skip local symbols, such as static foreach loop variables
                if (Declaration *decl = sm->isDeclaration())
                {
                    if (decl->storage_class & STClocal)
                    {
                        return 0;
                    }
                }

                // https://issues.dlang.org/show_bug.cgi?id=20915
                // skip version and debug identifiers
                if (sm->isVersionSymbol() || sm->isDebugSymbol())
                    return 0;

                //printf("\t[%i] %s %s\n", i, sm->kind(), sm->toChars());
                if (sm->ident)
                {
                    // https://issues.dlang.org/show_bug.cgi?id=10096
                    // https://issues.dlang.org/show_bug.cgi?id=10100
                    // Skip over internal members in __traits(allMembers)
                    if ((sm->isCtorDeclaration() && sm->ident != Id::ctor) ||
                        (sm->isDtorDeclaration() && sm->ident != Id::dtor) ||
                        (sm->isPostBlitDeclaration() && sm->ident != Id::postblit) ||
                        sm->isInvariantDeclaration() ||
                        sm->isUnitTestDeclaration())
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
                    for (size_t j = 0; j < idents->length; j++)
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
                dsymbolSemantic(cd, NULL);    // Bugzilla 13668: Try to resolve forward reference

            struct PushBaseMembers
            {
                static void dg(ClassDeclaration *cd, PushIdentsDg *ctx)
                {
                    for (size_t i = 0; i < cd->baseclasses->length; i++)
                    {
                        ClassDeclaration *cb = (*cd->baseclasses)[i]->sym;
                        assert(cb);
                        ScopeDsymbol_foreach(NULL, cb->members, &PushIdentsDg::dg, ctx);
                        if (cb->baseclasses->length)
                            dg(cb, ctx);
                    }
                }
            };
            PushBaseMembers::dg(cd, &ctx);
        }

        // Turn Identifiers into StringExps reusing the allocated array
        assert(sizeof(Expressions) == sizeof(Identifiers));
        Expressions *exps = (Expressions *)idents;
        for (size_t i = 0; i < idents->length; i++)
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
        ex = expressionSemantic(ex, sc);
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
            while (t)
            {
                if (TypeMixin *tm = t->isTypeMixin())
                {
                    /* The mixin string could be a type or an expression.
                     * Have to try compiling it to see.
                     */
                    OutBuffer buf;
                    if (expressionsToString(buf, sc, tm->exps))
                    {
                        err = true;
                        break;
                    }
                    const size_t len = buf.length();
                    const char *str = buf.extractChars();
                    Parser p(e->loc, sc->_module, (const utf8_t *)str, len, false);
                    p.nextToken();
                    //printf("p.loc.linnum = %d\n", p.loc.linnum);

                    o = p.parseTypeOrAssignExp(TOKeof);
                    if (p.errors || p.token.value != TOKeof)
                    {
                        err = true;
                        break;
                    }
                    t = isType(o);
                }
                else
                    break;
            }

            if (!err)
            {
                Expression *ex = t ? typeToExpression(t) : isExpression(o);
                if (!ex && t)
                {
                    Dsymbol *s;
                    t->resolve(e->loc, sc2, &ex, &t, &s);
                    if (t)
                    {
                        typeSemantic(t, e->loc, sc2);
                        if (t->ty == Terror)
                            err = true;
                    }
                    else if (s && s->errors)
                        err = true;
                }
                if (ex)
                {
                    ex = expressionSemantic(ex, sc2);
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

        // issue 12001, allow isSame, <BasicType>, <BasicType>
        Type *t1 = isType(o1);
        Type *t2 = isType(o2);
        if (t1 && t2 && t1->equals(t2))
            return True(e);

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
        Dsymbol *s = getDsymbolWithoutExpCtx(o);
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
        return expressionSemantic(te, sc);
    }
    else if (e->ident == Id::getVirtualIndex)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbolWithoutExpCtx(o);

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
    else if (e->ident == Id::isZeroInit)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        RootObject *o = (*e->args)[0];
        Type *t = isType(o);
        if (!t)
        {
            e->error("type expected as second argument of __traits `%s` instead of `%s`",
                e->ident->toChars(), o->toChars());
            return new ErrorExp();
        }

        Type *tb = t->baseElemOf();
        return tb->isZeroInit(e->loc) ? True(e) : False(e);
    }
    else if (e->ident == Id::getTargetInfo)
    {
        if (dim != 1)
            return dimError(e, 1, dim);

        Expression *ex = isExpression((*e->args)[0]);
        StringExp *se = ex ? ex->ctfeInterpret()->toStringExp() : NULL;
        if (!ex || !se || se->len == 0)
        {
            e->error("string expected as argument of __traits `%s` instead of `%s`", e->ident->toChars(), ex->toChars());
            return new ErrorExp();
        }
        se = se->toUTF8(sc);

        Expression *r = target.getTargetInfo(se->toPtr(), e->loc);
        if (!r)
        {
            e->error("`getTargetInfo` key `\"%s\"` not supported by this implementation", se->toPtr());
            return new ErrorExp();
        }
        return expressionSemantic(r, sc);
    }
    else if (e->ident == Id::getLocation)
    {
        if (dim != 1)
            return dimError(e, 1, dim);
        RootObject *arg0 = (*e->args)[0];
        Dsymbol *s = getDsymbolWithoutExpCtx(arg0);
        if (!s || !s->loc.filename)
        {
            e->error("can only get the location of a symbol, not `%s`", arg0->toChars());
            return new ErrorExp();
        }

        const FuncDeclaration *fd = s->isFuncDeclaration();
        if (fd && fd->overnext)
        {
            e->error("cannot get location of an overload set, "
                     "use `__traits(getOverloads, ..., \"%s\"%s)[N]` "
                     "to get the Nth overload",
                     arg0->toChars(), "");
            return new ErrorExp();
        }

        Expressions *exps = new Expressions();
        exps->setDim(3);
        (*exps)[0] = new StringExp(e->loc, const_cast<char *>(s->loc.filename), strlen(s->loc.filename));
        (*exps)[1] = new IntegerExp(e->loc, s->loc.linnum, Type::tint32);
        (*exps)[2] = new IntegerExp(e->loc, s->loc.charnum, Type::tint32);
        TupleExp *tup = new TupleExp(e->loc, exps);
        return expressionSemantic(tup, sc);
    }

    if (const char *sub = (const char *)speller(e->ident->toChars(), &trait_search_fp, NULL, idchars))
        e->error("unrecognized trait `%s`, did you mean `%s`?", e->ident->toChars(), sub);
    else
        e->error("unrecognized trait `%s`", e->ident->toChars());
    return new ErrorExp();

    e->error("wrong number of arguments %d", (int)dim);
    return new ErrorExp();
}
