
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/mtype.c
 */

#include "root/dsystem.h"
#include "root/checkedint.h"
#include "root/rmem.h"

#include "mars.h"
#include "mangle.h"
#include "dsymbol.h"
#include "mtype.h"
#include "scope.h"
#include "init.h"
#include "expression.h"
#include "statement.h"
#include "attrib.h"
#include "declaration.h"
#include "template.h"
#include "id.h"
#include "enum.h"
#include "module.h"
#include "import.h"
#include "aggregate.h"
#include "hdrgen.h"
#include "target.h"

bool symbolIsVisible(Scope *sc, Dsymbol *s);
typedef int (*ForeachDg)(void *ctx, size_t paramidx, Parameter *param);
int Parameter_foreach(Parameters *parameters, ForeachDg dg, void *ctx, size_t *pn = NULL);
FuncDeclaration *isFuncAddress(Expression *e, bool *hasOverloads = NULL);
Expression *extractSideEffect(Scope *sc, const char *name, Expression **e0, Expression *e, bool alwaysCopy = false);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);
Expression *typeToExpression(Type *t);
Expression *typeToExpressionHelper(TypeQualified *t, Expression *e, size_t i = 0);
RootObject *compileTypeMixin(TypeMixin *tm, Loc loc, Scope *sc);

/***************************** Type *****************************/

ClassDeclaration *Type::dtypeinfo;
ClassDeclaration *Type::typeinfoclass;
ClassDeclaration *Type::typeinfointerface;
ClassDeclaration *Type::typeinfostruct;
ClassDeclaration *Type::typeinfopointer;
ClassDeclaration *Type::typeinfoarray;
ClassDeclaration *Type::typeinfostaticarray;
ClassDeclaration *Type::typeinfoassociativearray;
ClassDeclaration *Type::typeinfovector;
ClassDeclaration *Type::typeinfoenum;
ClassDeclaration *Type::typeinfofunction;
ClassDeclaration *Type::typeinfodelegate;
ClassDeclaration *Type::typeinfotypelist;
ClassDeclaration *Type::typeinfoconst;
ClassDeclaration *Type::typeinfoinvariant;
ClassDeclaration *Type::typeinfoshared;
ClassDeclaration *Type::typeinfowild;

TemplateDeclaration *Type::rtinfo;

Type *Type::tvoid;
Type *Type::tint8;
Type *Type::tuns8;
Type *Type::tint16;
Type *Type::tuns16;
Type *Type::tint32;
Type *Type::tuns32;
Type *Type::tint64;
Type *Type::tuns64;
Type *Type::tint128;
Type *Type::tuns128;
Type *Type::tfloat32;
Type *Type::tfloat64;
Type *Type::tfloat80;

Type *Type::timaginary32;
Type *Type::timaginary64;
Type *Type::timaginary80;

Type *Type::tcomplex32;
Type *Type::tcomplex64;
Type *Type::tcomplex80;

Type *Type::tbool;
Type *Type::tchar;
Type *Type::twchar;
Type *Type::tdchar;

Type *Type::tshiftcnt;
Type *Type::terror;
Type *Type::tnull;
Type *Type::tnoreturn;

Type *Type::tsize_t;
Type *Type::tptrdiff_t;
Type *Type::thash_t;

Type *Type::tvoidptr;
Type *Type::tstring;
Type *Type::twstring;
Type *Type::tdstring;
Type *Type::basic[TMAX];
unsigned char Type::sizeTy[TMAX];
StringTable Type::stringtable;

void initTypeMangle();

Type::Type(TY ty)
{
    this->ty = ty;
    this->mod = 0;
    this->deco = NULL;
    this->cto = NULL;
    this->ito = NULL;
    this->sto = NULL;
    this->scto = NULL;
    this->wto = NULL;
    this->wcto = NULL;
    this->swto = NULL;
    this->swcto = NULL;
    this->pto = NULL;
    this->rto = NULL;
    this->arrayof = NULL;
    this->vtinfo = NULL;
    this->ctype = NULL;
}

const char *Type::kind()
{
    assert(false); // should be overridden
    return NULL;
}

Type *Type::copy()
{
    void *pt = mem.xmalloc(sizeTy[ty]);
    Type *t = (Type *)memcpy(pt, (void *)this, sizeTy[ty]);
    return t;
}

Type *Type::syntaxCopy()
{
    print();
    fprintf(stderr, "ty = %d\n", ty);
    assert(0);
    return this;
}

bool Type::equals(RootObject *o)
{
    Type *t = (Type *)o;
    //printf("Type::equals(%s, %s)\n", toChars(), t->toChars());
    // deco strings are unique
    // and semantic() has been run
    if (this == o || ((t && deco == t->deco) && deco != NULL))
    {
        //printf("deco = '%s', t->deco = '%s'\n", deco, t->deco);
        return true;
    }
    //if (deco && t && t->deco) printf("deco = '%s', t->deco = '%s'\n", deco, t->deco);
    return false;
}

bool Type::equivalent(Type *t)
{
    return immutableOf()->equals(t->immutableOf());
}

void Type::_init()
{
    stringtable._init(14000);

    for (size_t i = 0; i < TMAX; i++)
        sizeTy[i] = sizeof(TypeBasic);
    sizeTy[Tsarray] = sizeof(TypeSArray);
    sizeTy[Tarray] = sizeof(TypeDArray);
    sizeTy[Taarray] = sizeof(TypeAArray);
    sizeTy[Tpointer] = sizeof(TypePointer);
    sizeTy[Treference] = sizeof(TypeReference);
    sizeTy[Tfunction] = sizeof(TypeFunction);
    sizeTy[Tdelegate] = sizeof(TypeDelegate);
    sizeTy[Tident] = sizeof(TypeIdentifier);
    sizeTy[Tinstance] = sizeof(TypeInstance);
    sizeTy[Ttypeof] = sizeof(TypeTypeof);
    sizeTy[Tenum] = sizeof(TypeEnum);
    sizeTy[Tstruct] = sizeof(TypeStruct);
    sizeTy[Tclass] = sizeof(TypeClass);
    sizeTy[Ttuple] = sizeof(TypeTuple);
    sizeTy[Tslice] = sizeof(TypeSlice);
    sizeTy[Treturn] = sizeof(TypeReturn);
    sizeTy[Terror] = sizeof(TypeError);
    sizeTy[Tnull] = sizeof(TypeNull);
    sizeTy[Tvector] = sizeof(TypeVector);
    sizeTy[Ttraits] = sizeof(TypeTraits);
    sizeTy[Tmixin] = sizeof(TypeMixin);
    sizeTy[Tnoreturn] = sizeof(TypeNoreturn);

    initTypeMangle();

    // Set basic types
    static TY basetab[] =
        { Tvoid, Tint8, Tuns8, Tint16, Tuns16, Tint32, Tuns32, Tint64, Tuns64,
          Tint128, Tuns128,
          Tfloat32, Tfloat64, Tfloat80,
          Timaginary32, Timaginary64, Timaginary80,
          Tcomplex32, Tcomplex64, Tcomplex80,
          Tbool,
          Tchar, Twchar, Tdchar, Terror };

    for (size_t i = 0; basetab[i] != Terror; i++)
    {
        Type *t = new TypeBasic(basetab[i]);
        t = t->merge();
        basic[basetab[i]] = t;
    }
    basic[Terror] = new TypeError();

    tnoreturn = new TypeNoreturn();
    tnoreturn->deco = tnoreturn->merge()->deco;
    basic[Tnoreturn] = tnoreturn;

    tvoid = basic[Tvoid];
    tint8 = basic[Tint8];
    tuns8 = basic[Tuns8];
    tint16 = basic[Tint16];
    tuns16 = basic[Tuns16];
    tint32 = basic[Tint32];
    tuns32 = basic[Tuns32];
    tint64 = basic[Tint64];
    tuns64 = basic[Tuns64];
    tint128 = basic[Tint128];
    tuns128 = basic[Tuns128];
    tfloat32 = basic[Tfloat32];
    tfloat64 = basic[Tfloat64];
    tfloat80 = basic[Tfloat80];

    timaginary32 = basic[Timaginary32];
    timaginary64 = basic[Timaginary64];
    timaginary80 = basic[Timaginary80];

    tcomplex32 = basic[Tcomplex32];
    tcomplex64 = basic[Tcomplex64];
    tcomplex80 = basic[Tcomplex80];

    tbool = basic[Tbool];
    tchar = basic[Tchar];
    twchar = basic[Twchar];
    tdchar = basic[Tdchar];

    tshiftcnt = tint32;
    terror = basic[Terror];
    tnoreturn = basic[Tnoreturn];
    tnull = new TypeNull();
    tnull->deco = tnull->merge()->deco;

    tvoidptr = tvoid->pointerTo();
    tstring = tchar->immutableOf()->arrayOf();
    twstring = twchar->immutableOf()->arrayOf();
    tdstring = tdchar->immutableOf()->arrayOf();

    const bool isLP64 = global.params.isLP64;

    tsize_t = basic[isLP64 ? Tuns64 : Tuns32];
    tptrdiff_t = basic[isLP64 ? Tint64 : Tint32];
    thash_t = tsize_t;
}

d_uns64 Type::size()
{
    return size(Loc());
}

d_uns64 Type::size(Loc loc)
{
    error(loc, "no size for type %s", toChars());
    return SIZE_INVALID;
}

unsigned Type::alignsize()
{
    return (unsigned)size(Loc());
}

Type *Type::trySemantic(Loc loc, Scope *sc)
{
    //printf("+trySemantic(%s) %d\n", toChars(), global.errors);
    unsigned errors = global.startGagging();
    Type *t = typeSemantic(this, loc, sc);
    if (global.endGagging(errors) || t->ty == Terror)        // if any errors happened
    {
        t = NULL;
    }
    //printf("-trySemantic(%s) %d\n", toChars(), global.errors);
    return t;
}

/********************************
 * Return a copy of this type with all attributes null-initialized.
 * Useful for creating a type with different modifiers.
 */

Type *Type::nullAttributes()
{
    unsigned sz = sizeTy[ty];
    void *pt = mem.xmalloc(sz);
    Type *t = (Type *)memcpy(pt, (void *)this, sz);
    t->deco = NULL;
    t->arrayof = NULL;
    t->pto = NULL;
    t->rto = NULL;
    t->cto = NULL;
    t->ito = NULL;
    t->sto = NULL;
    t->scto = NULL;
    t->wto = NULL;
    t->wcto = NULL;
    t->swto = NULL;
    t->swcto = NULL;
    t->vtinfo = NULL;
    t->ctype = NULL;
    if (t->ty == Tstruct) ((TypeStruct *)t)->att = RECfwdref;
    if (t->ty == Tclass) ((TypeClass *)t)->att = RECfwdref;
    return t;
}

/********************************
 * Convert to 'const'.
 */

Type *Type::constOf()
{
    //printf("Type::constOf() %p %s\n", this, toChars());
    if (mod == MODconst)
        return this;
    if (cto)
    {
        assert(cto->mod == MODconst);
        return cto;
    }
    Type *t = makeConst();
    t = t->merge();
    t->fixTo(this);
    //printf("-Type::constOf() %p %s\n", t, t->toChars());
    return t;
}

/********************************
 * Convert to 'immutable'.
 */

Type *Type::immutableOf()
{
    //printf("Type::immutableOf() %p %s\n", this, toChars());
    if (isImmutable())
        return this;
    if (ito)
    {
        assert(ito->isImmutable());
        return ito;
    }
    Type *t = makeImmutable();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p\n", t);
    return t;
}

/********************************
 * Make type mutable.
 */

Type *Type::mutableOf()
{
    //printf("Type::mutableOf() %p, %s\n", this, toChars());
    Type *t = this;
    if (isImmutable())
    {
        t = ito;                // immutable => naked
        assert(!t || (t->isMutable() && !t->isShared()));
    }
    else if (isConst())
    {
        if (isShared())
        {
            if (isWild())
                t = swcto;      // shared wild const -> shared
            else
                t = sto;        // shared const => shared
        }
        else
        {
            if (isWild())
                t = wcto;       // wild const -> naked
            else
                t = cto;        // const => naked
        }
        assert(!t || t->isMutable());
    }
    else if (isWild())
    {
        if (isShared())
            t = sto;            // shared wild => shared
        else
            t = wto;            // wild => naked
        assert(!t || t->isMutable());
    }
    if (!t)
    {
        t = makeMutable();
        t = t->merge();
        t->fixTo(this);
    }
    else
        t = t->merge();
    assert(t->isMutable());
    return t;
}

Type *Type::sharedOf()
{
    //printf("Type::sharedOf() %p, %s\n", this, toChars());
    if (mod == MODshared)
        return this;
    if (sto)
    {
        assert(sto->mod == MODshared);
        return sto;
    }
    Type *t = makeShared();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p\n", t);
    return t;
}

Type *Type::sharedConstOf()
{
    //printf("Type::sharedConstOf() %p, %s\n", this, toChars());
    if (mod == (MODshared | MODconst))
        return this;
    if (scto)
    {
        assert(scto->mod == (MODshared | MODconst));
        return scto;
    }
    Type *t = makeSharedConst();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p\n", t);
    return t;
}


/********************************
 * Make type unshared.
 *      0            => 0
 *      const        => const
 *      immutable    => immutable
 *      shared       => 0
 *      shared const => const
 *      wild         => wild
 *      wild const   => wild const
 *      shared wild  => wild
 *      shared wild const => wild const
 */

Type *Type::unSharedOf()
{
    //printf("Type::unSharedOf() %p, %s\n", this, toChars());
    Type *t = this;

    if (isShared())
    {
        if (isWild())
        {
            if (isConst())
                t = wcto;   // shared wild const => wild const
            else
                t = wto;    // shared wild => wild
        }
        else
        {
            if (isConst())
                t = cto;    // shared const => const
            else
                t = sto;    // shared => naked
        }
        assert(!t || !t->isShared());
    }

    if (!t)
    {
        t = this->nullAttributes();
        t->mod = mod & ~MODshared;
        t->ctype = ctype;
        t = t->merge();

        t->fixTo(this);
    }
    else
        t = t->merge();
    assert(!t->isShared());
    return t;
}

/********************************
 * Convert to 'wild'.
 */

Type *Type::wildOf()
{
    //printf("Type::wildOf() %p %s\n", this, toChars());
    if (mod == MODwild)
        return this;
    if (wto)
    {
        assert(wto->mod == MODwild);
        return wto;
    }
    Type *t = makeWild();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p %s\n", t, t->toChars());
    return t;
}

Type *Type::wildConstOf()
{
    //printf("Type::wildConstOf() %p %s\n", this, toChars());
    if (mod == MODwildconst)
        return this;
    if (wcto)
    {
        assert(wcto->mod == MODwildconst);
        return wcto;
    }
    Type *t = makeWildConst();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p %s\n", t, t->toChars());
    return t;
}

Type *Type::sharedWildOf()
{
    //printf("Type::sharedWildOf() %p, %s\n", this, toChars());
    if (mod == (MODshared | MODwild))
        return this;
    if (swto)
    {
        assert(swto->mod == (MODshared | MODwild));
        return swto;
    }
    Type *t = makeSharedWild();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p %s\n", t, t->toChars());
    return t;
}

Type *Type::sharedWildConstOf()
{
    //printf("Type::sharedWildConstOf() %p, %s\n", this, toChars());
    if (mod == (MODshared | MODwildconst))
        return this;
    if (swcto)
    {
        assert(swcto->mod == (MODshared | MODwildconst));
        return swcto;
    }
    Type *t = makeSharedWildConst();
    t = t->merge();
    t->fixTo(this);
    //printf("\t%p %s\n", t, t->toChars());
    return t;
}

/**********************************
 * For our new type 'this', which is type-constructed from t,
 * fill in the cto, ito, sto, scto, wto shortcuts.
 */

void Type::fixTo(Type *t)
{
    // If fixing this: immutable(T*) by t: immutable(T)*,
    // cache t to this->xto won't break transitivity.
    Type *mto = NULL;
    Type *tn = nextOf();
    if (!tn || (ty != Tsarray && tn->mod == t->nextOf()->mod))
    {
        switch (t->mod)
        {
            case 0:                           mto = t;  break;
            case MODconst:                    cto = t;  break;
            case MODwild:                     wto = t;  break;
            case MODwildconst:               wcto = t;  break;
            case MODshared:                   sto = t;  break;
            case MODshared | MODconst:       scto = t;  break;
            case MODshared | MODwild:        swto = t;  break;
            case MODshared | MODwildconst:  swcto = t;  break;
            case MODimmutable:                ito = t;  break;
        }
    }

    assert(mod != t->mod);
#define X(m, n) (((m) << 4) | (n))
    switch (mod)
    {
        case 0:
            break;

        case MODconst:
            cto = mto;
            t->cto = this;
            break;

        case MODwild:
            wto = mto;
            t->wto = this;
            break;

        case MODwildconst:
            wcto = mto;
            t->wcto = this;
            break;

        case MODshared:
            sto = mto;
            t->sto = this;
            break;

        case MODshared | MODconst:
            scto = mto;
            t->scto = this;
            break;

        case MODshared | MODwild:
            swto = mto;
            t->swto = this;
            break;

        case MODshared | MODwildconst:
            swcto = mto;
            t->swcto = this;
            break;

        case MODimmutable:
            t->ito = this;
            if (t->  cto) t->  cto->ito = this;
            if (t->  sto) t->  sto->ito = this;
            if (t-> scto) t-> scto->ito = this;
            if (t->  wto) t->  wto->ito = this;
            if (t-> wcto) t-> wcto->ito = this;
            if (t-> swto) t-> swto->ito = this;
            if (t->swcto) t->swcto->ito = this;
            break;

        default:
            assert(0);
    }
#undef X

    check();
    t->check();
    //printf("fixTo: %s, %s\n", toChars(), t->toChars());
}

/***************************
 * Look for bugs in constructing types.
 */

void Type::check()
{
    switch (mod)
    {
        case 0:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODconst:
            if (cto) assert(cto->mod == 0);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODwild:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == 0);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODwildconst:
            assert(!  cto ||   cto->mod == MODconst);
            assert(!  ito ||   ito->mod == MODimmutable);
            assert(!  sto ||   sto->mod == MODshared);
            assert(! scto ||  scto->mod == (MODshared | MODconst));
            assert(!  wto ||   wto->mod == MODwild);
            assert(! wcto ||  wcto->mod == 0);
            assert(! swto ||  swto->mod == (MODshared | MODwild));
            assert(!swcto || swcto->mod == (MODshared | MODwildconst));
            break;

        case MODshared:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == 0);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODshared | MODconst:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == 0);
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODshared | MODwild:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == MODimmutable);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == 0);
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        case MODshared | MODwildconst:
            assert(!  cto ||   cto->mod == MODconst);
            assert(!  ito ||   ito->mod == MODimmutable);
            assert(!  sto ||   sto->mod == MODshared);
            assert(! scto ||  scto->mod == (MODshared | MODconst));
            assert(!  wto ||   wto->mod == MODwild);
            assert(! wcto ||  wcto->mod == MODwildconst);
            assert(! swto ||  swto->mod == (MODshared | MODwild));
            assert(!swcto || swcto->mod == 0);
            break;

        case MODimmutable:
            if (cto) assert(cto->mod == MODconst);
            if (ito) assert(ito->mod == 0);
            if (sto) assert(sto->mod == MODshared);
            if (scto) assert(scto->mod == (MODshared | MODconst));
            if (wto) assert(wto->mod == MODwild);
            if (wcto) assert(wcto->mod == MODwildconst);
            if (swto) assert(swto->mod == (MODshared | MODwild));
            if (swcto) assert(swcto->mod == (MODshared | MODwildconst));
            break;

        default:
            assert(0);
    }

    Type *tn = nextOf();
    if (tn && ty != Tfunction && tn->ty != Tfunction && ty != Tenum)
    {
        // Verify transitivity
        switch (mod)
        {
            case 0:
            case MODconst:
            case MODwild:
            case MODwildconst:
            case MODshared:
            case MODshared | MODconst:
            case MODshared | MODwild:
            case MODshared | MODwildconst:
            case MODimmutable:
                assert(tn->mod == MODimmutable || (tn->mod & mod) == mod);
                break;

            default:
                assert(0);
        }
        tn->check();
    }
}

Type *Type::makeConst()
{
    //printf("Type::makeConst() %p, %s\n", this, toChars());
    if (cto) return cto;
    Type *t = this->nullAttributes();
    t->mod = MODconst;
    //printf("-Type::makeConst() %p, %s\n", t, toChars());
    return t;
}

Type *Type::makeImmutable()
{
    if (ito) return ito;
    Type *t = this->nullAttributes();
    t->mod = MODimmutable;
    return t;
}

Type *Type::makeShared()
{
    if (sto) return sto;
    Type *t = this->nullAttributes();
    t->mod = MODshared;
    return t;
}

Type *Type::makeSharedConst()
{
    if (scto) return scto;
    Type *t = this->nullAttributes();
    t->mod = MODshared | MODconst;
    return t;
}

Type *Type::makeWild()
{
    if (wto) return wto;
    Type *t = this->nullAttributes();
    t->mod = MODwild;
    return t;
}

Type *Type::makeWildConst()
{
    if (wcto) return wcto;
    Type *t = this->nullAttributes();
    t->mod = MODwildconst;
    return t;
}

Type *Type::makeSharedWild()
{
    if (swto) return swto;
    Type *t = this->nullAttributes();
    t->mod = MODshared | MODwild;
    return t;
}

Type *Type::makeSharedWildConst()
{
    if (swcto) return swcto;
    Type *t = this->nullAttributes();
    t->mod = MODshared | MODwildconst;
    return t;
}

Type *Type::makeMutable()
{
    Type *t = this->nullAttributes();
    t->mod = mod & MODshared;
    return t;
}

/*************************************
 * Apply STCxxxx bits to existing type.
 * Use *before* semantic analysis is run.
 */

Type *Type::addSTC(StorageClass stc)
{
    Type *t = this;
    if (t->isImmutable())
        ;
    else if (stc & STCimmutable)
    {
        t = t->makeImmutable();
    }
    else
    {
        if ((stc & STCshared) && !t->isShared())
        {
            if (t->isWild())
            {
                if (t->isConst())
                    t = t->makeSharedWildConst();
                else
                    t = t->makeSharedWild();
            }
            else
            {
                if (t->isConst())
                    t = t->makeSharedConst();
                else
                    t = t->makeShared();
            }
        }
        if ((stc & STCconst) && !t->isConst())
        {
            if (t->isShared())
            {
                if (t->isWild())
                    t = t->makeSharedWildConst();
                else
                    t = t->makeSharedConst();
            }
            else
            {
                if (t->isWild())
                    t = t->makeWildConst();
                else
                    t = t->makeConst();
            }
        }
        if ((stc & STCwild) && !t->isWild())
        {
            if (t->isShared())
            {
                if (t->isConst())
                    t = t->makeSharedWildConst();
                else
                    t = t->makeSharedWild();
            }
            else
            {
                if (t->isConst())
                    t = t->makeWildConst();
                else
                    t = t->makeWild();
            }
        }
    }
    return t;
}

/************************************
 * Convert MODxxxx to STCxxx
 */

StorageClass ModToStc(unsigned mod)
{
    StorageClass stc = 0;
    if (mod & MODimmutable) stc |= STCimmutable;
    if (mod & MODconst)     stc |= STCconst;
    if (mod & MODwild)      stc |= STCwild;
    if (mod & MODshared)    stc |= STCshared;
    return stc;
}

/************************************
 * Apply MODxxxx bits to existing type.
 */

Type *Type::castMod(MOD mod)
{   Type *t;

    switch (mod)
    {
        case 0:
            t = unSharedOf()->mutableOf();
            break;

        case MODconst:
            t = unSharedOf()->constOf();
            break;

        case MODwild:
            t = unSharedOf()->wildOf();
            break;

        case MODwildconst:
            t = unSharedOf()->wildConstOf();
            break;

        case MODshared:
            t = mutableOf()->sharedOf();
            break;

        case MODshared | MODconst:
            t = sharedConstOf();
            break;

        case MODshared | MODwild:
            t = sharedWildOf();
            break;

        case MODshared | MODwildconst:
            t = sharedWildConstOf();
            break;

        case MODimmutable:
            t = immutableOf();
            break;

        default:
            assert(0);
    }
    return t;
}

/************************************
 * Add MODxxxx bits to existing type.
 * We're adding, not replacing, so adding const to
 * a shared type => "shared const"
 */

Type *Type::addMod(MOD mod)
{
    /* Add anything to immutable, and it remains immutable
     */
    Type *t = this;
    if (!t->isImmutable())
    {
        //printf("addMod(%x) %s\n", mod, toChars());
        switch (mod)
        {
            case 0:
                break;

            case MODconst:
                if (isShared())
                {
                    if (isWild())
                        t = sharedWildConstOf();
                    else
                        t = sharedConstOf();
                }
                else
                {
                    if (isWild())
                        t = wildConstOf();
                    else
                        t = constOf();
                }
                break;

            case MODwild:
                if (isShared())
                {
                    if (isConst())
                        t = sharedWildConstOf();
                    else
                        t = sharedWildOf();
                }
                else
                {
                    if (isConst())
                        t = wildConstOf();
                    else
                        t = wildOf();
                }
                break;

            case MODwildconst:
                if (isShared())
                    t = sharedWildConstOf();
                else
                    t = wildConstOf();
                break;

            case MODshared:
                if (isWild())
                {
                    if (isConst())
                        t = sharedWildConstOf();
                    else
                        t = sharedWildOf();
                }
                else
                {
                    if (isConst())
                        t = sharedConstOf();
                    else
                        t = sharedOf();
                }
                break;

            case MODshared | MODconst:
                if (isWild())
                    t = sharedWildConstOf();
                else
                    t = sharedConstOf();
                break;

            case MODshared | MODwild:
                if (isConst())
                    t = sharedWildConstOf();
                else
                    t = sharedWildOf();
                break;

            case MODshared | MODwildconst:
                t = sharedWildConstOf();
                break;

            case MODimmutable:
                t = immutableOf();
                break;

            default:
                assert(0);
        }
    }
    return t;
}

/************************************
 * Add storage class modifiers to type.
 */

Type *Type::addStorageClass(StorageClass stc)
{
    /* Just translate to MOD bits and let addMod() do the work
     */
    MOD mod = 0;

    if (stc & STCimmutable)
        mod = MODimmutable;
    else
    {
        if (stc & (STCconst | STCin))
            mod |= MODconst;
        if (stc & STCwild)
            mod |= MODwild;
        if (stc & STCshared)
            mod |= MODshared;
    }
    return addMod(mod);
}

Type *Type::pointerTo()
{
    if (ty == Terror)
        return this;
    if (!pto)
    {
        Type *t = new TypePointer(this);
        if (ty == Tfunction)
        {
            t->deco = t->merge()->deco;
            pto = t;
        }
        else
            pto = t->merge();
    }
    return pto;
}

Type *Type::referenceTo()
{
    if (ty == Terror)
        return this;
    if (!rto)
    {
        Type *t = new TypeReference(this);
        rto = t->merge();
    }
    return rto;
}

Type *Type::arrayOf()
{
    if (ty == Terror)
        return this;
    if (!arrayof)
    {
        Type *t = new TypeDArray(this);
        arrayof = t->merge();
    }
    return arrayof;
}

// Make corresponding static array type without semantic
Type *Type::sarrayOf(dinteger_t dim)
{
    assert(deco);
    Type *t = new TypeSArray(this, new IntegerExp(Loc(), dim, Type::tsize_t));

    // according to TypeSArray::semantic()
    t = t->addMod(mod);
    t = t->merge();

    return t;
}

Type *Type::aliasthisOf()
{
    AggregateDeclaration *ad = isAggregate(this);
    if (ad && ad->aliasthis)
    {
        Dsymbol *s = ad->aliasthis;
        if (s->isAliasDeclaration())
            s = s->toAlias();
        Declaration *d = s->isDeclaration();
        if (d && !d->isTupleDeclaration())
        {
            assert(d->type);
            Type *t = d->type;
            if (d->isVarDeclaration() && d->needThis())
            {
                t = t->addMod(this->mod);
            }
            else if (d->isFuncDeclaration())
            {
                FuncDeclaration *fd = resolveFuncCall(Loc(), NULL, d, NULL, this, NULL, 1);
                if (fd && fd->errors)
                    return Type::terror;
                if (fd && !fd->type->nextOf() && !fd->functionSemantic())
                    fd = NULL;
                if (fd)
                {
                    t = fd->type->nextOf();
                    if (!t) // issue 14185
                        return Type::terror;
                    t = t->substWildTo(mod == 0 ? MODmutable : (MODFlags)mod);
                }
                else
                    return Type::terror;
            }
            return t;
        }
        EnumDeclaration *ed = s->isEnumDeclaration();
        if (ed)
        {
            Type *t = ed->type;
            return t;
        }
        TemplateDeclaration *td = s->isTemplateDeclaration();
        if (td)
        {
            assert(td->_scope);
            FuncDeclaration *fd = resolveFuncCall(Loc(), NULL, td, NULL, this, NULL, 1);
            if (fd && fd->errors)
                return Type::terror;
            if (fd && fd->functionSemantic())
            {
                Type *t = fd->type->nextOf();
                t = t->substWildTo(mod == 0 ? MODmutable : (MODFlags)mod);
                return t;
            }
            else
                return Type::terror;
        }
        //printf("%s\n", s->kind());
    }
    return NULL;
}

bool Type::checkAliasThisRec()
{
    Type *tb = toBasetype();
    AliasThisRec* pflag;
    if (tb->ty == Tstruct)
        pflag = &((TypeStruct *)tb)->att;
    else if (tb->ty == Tclass)
        pflag = &((TypeClass *)tb)->att;
    else
        return false;

    AliasThisRec flag = (AliasThisRec)(*pflag & RECtypeMask);
    if (flag == RECfwdref)
    {
        Type *att = aliasthisOf();
        flag = att && att->implicitConvTo(this) ? RECyes : RECno;
    }
    *pflag = (AliasThisRec)(flag | (*pflag & ~RECtypeMask));
    return flag == RECyes;
}

Dsymbol *Type::toDsymbol(Scope *)
{
    return NULL;
}

/*******************************
 * If this is a shell around another type,
 * get that other type.
 */

Type *Type::toBasetype()
{
    return this;
}

/***************************
 * Return !=0 if modfrom can be implicitly converted to modto
 */
bool MODimplicitConv(MOD modfrom, MOD modto)
{
    if (modfrom == modto)
        return true;

    //printf("MODimplicitConv(from = %x, to = %x)\n", modfrom, modto);
    #define X(m, n) (((m) << 4) | (n))
    switch (X(modfrom & ~MODshared, modto & ~MODshared))
    {
        case X(0,            MODconst):
        case X(MODwild,      MODconst):
        case X(MODwild,      MODwildconst):
        case X(MODwildconst, MODconst):
            return (modfrom & MODshared) == (modto & MODshared);

        case X(MODimmutable, MODconst):
        case X(MODimmutable, MODwildconst):
            return true;

        default:
            return false;
    }
    #undef X
}

/***************************
 * Return MATCHexact or MATCHconst if a method of type '() modfrom' can call a method of type '() modto'.
 */
MATCH MODmethodConv(MOD modfrom, MOD modto)
{
    if (modfrom == modto)
        return MATCHexact;
    if (MODimplicitConv(modfrom, modto))
        return MATCHconst;

    #define X(m, n) (((m) << 4) | (n))
    switch (X(modfrom, modto))
    {
        case X(0,            MODwild):
        case X(MODimmutable, MODwild):
        case X(MODconst,     MODwild):
        case X(MODwildconst, MODwild):
        case X(MODshared,              MODshared|MODwild):
        case X(MODshared|MODimmutable, MODshared|MODwild):
        case X(MODshared|MODconst,     MODshared|MODwild):
        case X(MODshared|MODwildconst, MODshared|MODwild):
            return MATCHconst;

        default:
            return MATCHnomatch;
    }
    #undef X
}

/***************************
 * Merge mod bits to form common mod.
 */
MOD MODmerge(MOD mod1, MOD mod2)
{
    if (mod1 == mod2)
        return mod1;

    //printf("MODmerge(1 = %x, 2 = %x)\n", mod1, mod2);
    MOD result = 0;
    if ((mod1 | mod2) & MODshared)
    {
        // If either type is shared, the result will be shared
        result |= MODshared;
        mod1 &= ~MODshared;
        mod2 &= ~MODshared;
    }
    if (mod1 == 0 || mod1 == MODmutable || mod1 == MODconst ||
        mod2 == 0 || mod2 == MODmutable || mod2 == MODconst)
    {
        // If either type is mutable or const, the result will be const.
        result |= MODconst;
    }
    else
    {
        // MODimmutable vs MODwild
        // MODimmutable vs MODwildconst
        //      MODwild vs MODwildconst
        assert(mod1 & MODwild || mod2 & MODwild);
        result |= MODwildconst;
    }
    return result;
}

/*********************************
 * Store modifier name into buf.
 */
void MODtoBuffer(OutBuffer *buf, MOD mod)
{
    switch (mod)
    {
        case 0:
            break;

        case MODimmutable:
            buf->writestring(Token::tochars[TOKimmutable]);
            break;

        case MODshared:
            buf->writestring(Token::tochars[TOKshared]);
            break;

        case MODshared | MODconst:
            buf->writestring(Token::tochars[TOKshared]);
            buf->writeByte(' ');
            /* fall through */
        case MODconst:
            buf->writestring(Token::tochars[TOKconst]);
            break;

        case MODshared | MODwild:
            buf->writestring(Token::tochars[TOKshared]);
            buf->writeByte(' ');
            /* fall through */
        case MODwild:
            buf->writestring(Token::tochars[TOKwild]);
            break;

        case MODshared | MODwildconst:
            buf->writestring(Token::tochars[TOKshared]);
            buf->writeByte(' ');
            /* fall through */
        case MODwildconst:
            buf->writestring(Token::tochars[TOKwild]);
            buf->writeByte(' ');
            buf->writestring(Token::tochars[TOKconst]);
            break;

        default:
            assert(0);
    }
}


/*********************************
 * Return modifier name.
 */
char *MODtoChars(MOD mod)
{
    OutBuffer buf;
    buf.reserve(16);
    MODtoBuffer(&buf, mod);
    return buf.extractChars();
}

/********************************
 * For pretty-printing a type.
 */

const char *Type::toChars()
{
    OutBuffer buf;
    buf.reserve(16);
    HdrGenState hgs;
    hgs.fullQual = (ty == Tclass && !mod);

    ::toCBuffer(this, &buf, NULL, &hgs);
    return buf.extractChars();
}

char *Type::toPrettyChars(bool QualifyTypes)
{
    OutBuffer buf;
    buf.reserve(16);
    HdrGenState hgs;
    hgs.fullQual = QualifyTypes;

    ::toCBuffer(this, &buf, NULL, &hgs);
    return buf.extractChars();
}

/*********************************
 * Store this type's modifier name into buf.
 */
void Type::modToBuffer(OutBuffer *buf)
{
    if (mod)
    {
        buf->writeByte(' ');
        MODtoBuffer(buf, mod);
    }
}

/*********************************
 * Return this type's modifier name.
 */
char *Type::modToChars()
{
    OutBuffer buf;
    buf.reserve(16);
    modToBuffer(&buf);
    return buf.extractChars();
}

/** For each active modifier (MODconst, MODimmutable, etc) call fp with a
void* for the work param and a string representation of the attribute. */
int Type::modifiersApply(void *param, int (*fp)(void *, const char *))
{
    static unsigned char modsArr[] = { MODconst, MODimmutable, MODwild, MODshared };

    for (size_t idx = 0; idx < 4; ++idx)
    {
        if (mod & modsArr[idx])
        {
            if (int res = fp(param, MODtoChars(modsArr[idx])))
                return res;
        }
    }
    return 0;
}

/************************************
 * Strip all parameter's idenfiers and their default arguments for merging types.
 * If some of parameter types or return type are function pointer, delegate, or
 * the types which contains either, then strip also from them.
 */

Type *stripDefaultArgs(Type *t)
{
  struct N
  {
    static Parameters *stripParams(Parameters *parameters)
    {
        Parameters *params = parameters;
        if (params && params->length > 0)
        {
            for (size_t i = 0; i < params->length; i++)
            {
                Parameter *p = (*params)[i];
                Type *ta = stripDefaultArgs(p->type);
                if (ta != p->type || p->defaultArg || p->ident || p->userAttribDecl)
                {
                    if (params == parameters)
                    {
                        params = new Parameters();
                        params->setDim(parameters->length);
                        for (size_t j = 0; j < params->length; j++)
                            (*params)[j] = (*parameters)[j];
                    }
                    (*params)[i] = new Parameter(p->storageClass, ta, NULL, NULL, NULL);
                }
            }
        }
        return params;
    }
  };

    if (t == NULL)
        return t;

    if (t->ty == Tfunction)
    {
        TypeFunction *tf = (TypeFunction *)t;
        Type *tret = stripDefaultArgs(tf->next);
        Parameters *params = N::stripParams(tf->parameterList.parameters);
        if (tret == tf->next && params == tf->parameterList.parameters)
            goto Lnot;
        tf = (TypeFunction *)tf->copy();
        tf->parameterList.parameters = params;
        tf->next = tret;
        //printf("strip %s\n   <- %s\n", tf->toChars(), t->toChars());
        t = tf;
    }
    else if (t->ty == Ttuple)
    {
        TypeTuple *tt = (TypeTuple *)t;
        Parameters *args = N::stripParams(tt->arguments);
        if (args == tt->arguments)
            goto Lnot;
        t = t->copy();
        ((TypeTuple *)t)->arguments = args;
    }
    else if (t->ty == Tenum)
    {
        // TypeEnum::nextOf() may be != NULL, but it's not necessary here.
        goto Lnot;
    }
    else
    {
        Type *tn = t->nextOf();
        Type *n = stripDefaultArgs(tn);
        if (n == tn)
            goto Lnot;
        t = t->copy();
        ((TypeNext *)t)->next = n;
    }
    //printf("strip %s\n", t->toChars());
Lnot:
    return t;
}

/************************************
 */

Type *Type::merge()
{
    if (ty == Terror) return this;
    if (ty == Ttypeof) return this;
    if (ty == Tident) return this;
    if (ty == Tinstance) return this;
    if (ty == Taarray && !((TypeAArray *)this)->index->merge()->deco)
        return this;
    if (ty != Tenum && nextOf() && !nextOf()->deco)
        return this;

    //printf("merge(%s)\n", toChars());
    Type *t = this;
    assert(t);
    if (!deco)
    {
        OutBuffer buf;
        buf.reserve(32);

        mangleToBuffer(this, &buf);

        StringValue *sv = stringtable.update((char *)buf.slice().ptr, buf.length());
        if (sv->ptrvalue)
        {
            t = (Type *) sv->ptrvalue;
            assert(t->deco);
            //printf("old value, deco = '%s' %p\n", t->deco, t->deco);
        }
        else
        {
            sv->ptrvalue = (char *)(t = stripDefaultArgs(t));
            deco = t->deco = const_cast<char *>(sv->toDchars());
            //printf("new value, deco = '%s' %p\n", t->deco, t->deco);
        }
    }
    return t;
}

/*************************************
 * This version does a merge even if the deco is already computed.
 * Necessary for types that have a deco, but are not merged.
 */
Type *Type::merge2()
{
    //printf("merge2(%s)\n", toChars());
    Type *t = this;
    assert(t);
    if (!t->deco)
        return t->merge();

    StringValue *sv = stringtable.lookup((char *)t->deco, strlen(t->deco));
    if (sv && sv->ptrvalue)
    {   t = (Type *) sv->ptrvalue;
        assert(t->deco);
    }
    else
        assert(0);
    return t;
}

bool Type::isintegral()
{
    return false;
}

bool Type::isfloating()
{
    return false;
}

bool Type::isreal()
{
    return false;
}

bool Type::isimaginary()
{
    return false;
}

bool Type::iscomplex()
{
    return false;
}

bool Type::isscalar()
{
    return false;
}

bool Type::isunsigned()
{
    return false;
}

ClassDeclaration *Type::isClassHandle()
{
    return NULL;
}

bool Type::isscope()
{
    return false;
}

bool Type::isString()
{
    return false;
}

/**************************
 * When T is mutable,
 * Given:
 *      T a, b;
 * Can we bitwise assign:
 *      a = b;
 * ?
 */
bool Type::isAssignable()
{
    return true;
}

/**************************
 * Returns true if T can be converted to boolean value.
 */
bool Type::isBoolean()
{
    return isscalar();
}

/********************************
 * true if when type goes out of scope, it needs a destructor applied.
 * Only applies to value types, not ref types.
 */
bool Type::needsDestruction()
{
    return false;
}

/*********************************
 *
 */

bool Type::needsNested()
{
    return false;
}

/*********************************
 * Check type to see if it is based on a deprecated symbol.
 */

void Type::checkDeprecated(Loc loc, Scope *sc)
{
    if (Dsymbol *s = toDsymbol(sc))
    {
        s->checkDeprecated(loc, sc);
    }
}


Expression *Type::defaultInit(Loc)
{
    return NULL;
}

/***************************************
 * Use when we prefer the default initializer to be a literal,
 * rather than a global immutable variable.
 */
Expression *Type::defaultInitLiteral(Loc loc)
{
    return defaultInit(loc);
}

bool Type::isZeroInit(Loc)
{
    return false;           // assume not
}

bool Type::isBaseOf(Type *, int *)
{
    return 0;           // assume not
}

/********************************
 * Determine if 'this' can be implicitly converted
 * to type 'to'.
 * Returns:
 *      MATCHnomatch, MATCHconvert, MATCHconst, MATCHexact
 */

MATCH Type::implicitConvTo(Type *to)
{
    //printf("Type::implicitConvTo(this=%p, to=%p)\n", this, to);
    //printf("from: %s\n", toChars());
    //printf("to  : %s\n", to->toChars());
    if (this->equals(to))
        return MATCHexact;
    return MATCHnomatch;
}

/*******************************
 * Determine if converting 'this' to 'to' is an identity operation,
 * a conversion to const operation, or the types aren't the same.
 * Returns:
 *      MATCHexact      'this' == 'to'
 *      MATCHconst      'to' is const
 *      MATCHnomatch    conversion to mutable or invariant
 */

MATCH Type::constConv(Type *to)
{
    //printf("Type::constConv(this = %s, to = %s)\n", toChars(), to->toChars());
    if (equals(to))
        return MATCHexact;
    if (ty == to->ty && MODimplicitConv(mod, to->mod))
        return MATCHconst;
    return MATCHnomatch;
}

/***************************************
 * Return MOD bits matching this type to wild parameter type (tprm).
 */

unsigned char Type::deduceWild(Type *t, bool)
{
    //printf("Type::deduceWild this = '%s', tprm = '%s'\n", toChars(), tprm->toChars());

    if (t->isWild())
    {
        if (isImmutable())
            return MODimmutable;
        else if (isWildConst())
        {
            if (t->isWildConst())
                return MODwild;
            else
                return MODwildconst;
        }
        else if (isWild())
            return MODwild;
        else if (isConst())
            return MODconst;
        else if (isMutable())
            return MODmutable;
        else
            assert(0);
    }
    return 0;
}

Type *Type::unqualify(unsigned m)
{
    Type *t = mutableOf()->unSharedOf();

    Type *tn = ty == Tenum ? NULL : nextOf();
    if (tn && tn->ty != Tfunction)
    {
        Type *utn = tn->unqualify(m);
        if (utn != tn)
        {
            if (ty == Tpointer)
                t = utn->pointerTo();
            else if (ty == Tarray)
                t = utn->arrayOf();
            else if (ty == Tsarray)
                t = new TypeSArray(utn, ((TypeSArray *)this)->dim);
            else if (ty == Taarray)
            {
                t = new TypeAArray(utn, ((TypeAArray *)this)->index);
                ((TypeAArray *)t)->sc = ((TypeAArray *)this)->sc;   // duplicate scope
            }
            else
                assert(0);

            t = t->merge();
        }
    }
    t = t->addMod(mod & ~m);
    return t;
}

Type *Type::substWildTo(unsigned mod)
{
    //printf("+Type::substWildTo this = %s, mod = x%x\n", toChars(), mod);
    Type *t;

    if (Type *tn = nextOf())
    {
        // substitution has no effect on function pointer type.
        if (ty == Tpointer && tn->ty == Tfunction)
        {
            t = this;
            goto L1;
        }

        t = tn->substWildTo(mod);
        if (t == tn)
            t = this;
        else
        {
            if (ty == Tpointer)
                t = t->pointerTo();
            else if (ty == Tarray)
                t = t->arrayOf();
            else if (ty == Tsarray)
                t = new TypeSArray(t, ((TypeSArray *)this)->dim->syntaxCopy());
            else if (ty == Taarray)
            {
                t = new TypeAArray(t, ((TypeAArray *)this)->index->syntaxCopy());
                ((TypeAArray *)t)->sc = ((TypeAArray *)this)->sc;   // duplicate scope
            }
            else if (ty == Tdelegate)
            {
                t = new TypeDelegate(t);
            }
            else
                assert(0);

            t = t->merge();
        }
    }
    else
        t = this;

L1:
    if (isWild())
    {
        if (mod == MODimmutable)
        {
            t = t->immutableOf();
        }
        else if (mod == MODwildconst)
        {
            t = t->wildConstOf();
        }
        else if (mod == MODwild)
        {
            if (isWildConst())
                t = t->wildConstOf();
            else
                t = t->wildOf();
        }
        else if (mod == MODconst)
        {
            t = t->constOf();
        }
        else
        {
            if (isWildConst())
                t = t->constOf();
            else
                t = t->mutableOf();
        }
    }
    if (isConst())
        t = t->addMod(MODconst);
    if (isShared())
        t = t->addMod(MODshared);

    //printf("-Type::substWildTo t = %s\n", t->toChars());
    return t;
}

Type *TypeFunction::substWildTo(unsigned)
{
    if (!iswild && !(mod & MODwild))
        return this;

    // Substitude inout qualifier of function type to mutable or immutable
    // would break type system. Instead substitude inout to the most weak
    // qualifer - const.
    unsigned m = MODconst;

    assert(next);
    Type *tret = next->substWildTo(m);
    Parameters *params = parameterList.parameters;
    if (mod & MODwild)
        params = parameterList.parameters->copy();
    for (size_t i = 0; i < params->length; i++)
    {
        Parameter *p = (*params)[i];
        Type *t = p->type->substWildTo(m);
        if (t == p->type)
            continue;
        if (params == parameterList.parameters)
            params = parameterList.parameters->copy();
        (*params)[i] = new Parameter(p->storageClass, t, NULL, NULL, NULL);
    }
    if (next == tret && params == parameterList.parameters)
        return this;

    // Similar to TypeFunction::syntaxCopy;
    TypeFunction *t = new TypeFunction(ParameterList(params, parameterList.varargs),
                                       tret, linkage);
    t->mod = ((mod & MODwild) ? (mod & ~MODwild) | MODconst : mod);
    t->isnothrow = isnothrow;
    t->isnogc = isnogc;
    t->purity = purity;
    t->isproperty = isproperty;
    t->isref = isref;
    t->isreturn = isreturn;
    t->isscope = isscope;
    t->isscopeinferred = isscopeinferred;
    t->iswild = 0;
    t->trust = trust;
    t->fargs = fargs;
    return t->merge();
}

/**************************
 * Return type with the top level of it being mutable.
 */
Type *Type::toHeadMutable()
{
    if (!mod)
        return this;
    return mutableOf();
}

/***************************************
 * Calculate built-in properties which just the type is necessary.
 *
 * If flag & 1, don't report "not a property" error and just return NULL.
 */
Expression *Type::getProperty(Loc loc, Identifier *ident, int flag)
{
    Expression *e;

    if (ident == Id::__sizeof)
    {
        d_uns64 sz = size(loc);
        if (sz == SIZE_INVALID)
            return new ErrorExp();
        e = new IntegerExp(loc, sz, Type::tsize_t);
    }
    else if (ident == Id::__xalignof)
    {
        unsigned explicitAlignment = alignment();
        unsigned naturalAlignment = alignsize();
        unsigned actualAlignment = (explicitAlignment == STRUCTALIGN_DEFAULT ? naturalAlignment : explicitAlignment);
        e = new IntegerExp(loc, actualAlignment, Type::tsize_t);
    }
    else if (ident == Id::_init)
    {
        Type *tb = toBasetype();
        e = defaultInitLiteral(loc);
        if (tb->ty == Tstruct && tb->needsNested())
        {
            StructLiteralExp *se = (StructLiteralExp *)e;
            se->useStaticInit = true;
        }
    }
    else if (ident == Id::_mangleof)
    {
        if (!deco)
        {
            error(loc, "forward reference of type %s.mangleof", toChars());
            e = new ErrorExp();
        }
        else
        {
            e = new StringExp(loc, (char *)deco, strlen(deco));
            Scope sc;
            e = expressionSemantic(e, &sc);
        }
    }
    else if (ident == Id::stringof)
    {
        const char *s = toChars();
        e = new StringExp(loc, const_cast<char *>(s), strlen(s));
        Scope sc;
        e = expressionSemantic(e, &sc);
    }
    else if (flag && this != Type::terror)
    {
        return NULL;
    }
    else
    {
        Dsymbol *s = NULL;
        if (ty == Tstruct || ty == Tclass || ty == Tenum)
            s = toDsymbol(NULL);
        if (s)
            s = s->search_correct(ident);
        if (this != Type::terror)
        {
            if (s)
                error(loc, "no property `%s` for type `%s`, did you mean `%s`?", ident->toChars(), toChars(), s->toPrettyChars());
            else
                error(loc, "no property `%s` for type `%s`", ident->toChars(), toChars());
        }
        e = new ErrorExp();
    }
    return e;
}

/***************************************
 * Access the members of the object e. This type is same as e->type.
 *
 * If flag & 1, don't report "not a property" error and just return NULL.
 */
Expression *Type::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    VarDeclaration *v = NULL;

    Expression *ex = e;
    while (ex->op == TOKcomma)
        ex = ((CommaExp *)ex)->e2;
    if (ex->op == TOKdotvar)
    {
        DotVarExp *dv = (DotVarExp *)ex;
        v = dv->var->isVarDeclaration();
    }
    else if (ex->op == TOKvar)
    {
        VarExp *ve = (VarExp *)ex;
        v = ve->var->isVarDeclaration();
    }
    if (v)
    {
        if (ident == Id::offsetof)
        {
            if (v->isField())
            {
                AggregateDeclaration *ad = v->toParent()->isAggregateDeclaration();
                ad->size(e->loc);
                if (ad->sizeok != SIZEOKdone)
                    return new ErrorExp();
                e = new IntegerExp(e->loc, v->offset, Type::tsize_t);
                return e;
            }
        }
        else if (ident == Id::_init)
        {
            Type *tb = toBasetype();
            e = defaultInitLiteral(e->loc);
            if (tb->ty == Tstruct && tb->needsNested())
            {
                StructLiteralExp *se = (StructLiteralExp *)e;
                se->useStaticInit = true;
            }
            goto Lreturn;
        }
    }
    if (ident == Id::stringof)
    {
        /* Bugzilla 3796: this should demangle e->type->deco rather than
         * pretty-printing the type.
         */
        const char *s = e->toChars();
        e = new StringExp(e->loc, const_cast<char *>(s), strlen(s));
    }
    else
        e = getProperty(e->loc, ident, flag & 1);

Lreturn:
    if (e)
        e = expressionSemantic(e, sc);
    return e;
}

/************************************
 * Return alignment to use for this type.
 */

structalign_t Type::alignment()
{
    return STRUCTALIGN_DEFAULT;
}

/***************************************
 * Figures out what to do with an undefined member reference
 * for classes and structs.
 *
 * If flag & 1, don't report "not a property" error and just return NULL.
 */
Expression *Type::noMember(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    //printf("Type::noMember(e: %s ident: %s flag: %d)\n", e->toChars(), ident->toChars(), flag);

    static int nest;      // https://issues.dlang.org/show_bug.cgi?id=17380

    if (++nest > global.recursionLimit)
    {
      ::error(e->loc, "cannot resolve identifier `%s`", ident->toChars());
      --nest;
      return (flag & 1) ? NULL : new ErrorExp();
    }

    assert(ty == Tstruct || ty == Tclass);
    AggregateDeclaration *sym = toDsymbol(sc)->isAggregateDeclaration();
    assert(sym);

    if (ident != Id::__sizeof &&
        ident != Id::__xalignof &&
        ident != Id::_init &&
        ident != Id::_mangleof &&
        ident != Id::stringof &&
        ident != Id::offsetof &&
        // Bugzilla 15045: Don't forward special built-in member functions.
        ident != Id::ctor &&
        ident != Id::dtor &&
        ident != Id::__xdtor &&
        ident != Id::postblit &&
        ident != Id::__xpostblit)
    {
        /* Look for overloaded opDot() to see if we should forward request
         * to it.
         */
        if (Dsymbol *fd = search_function(sym, Id::opDot))
        {
            /* Rewrite e.ident as:
             *  e.opDot().ident
             */
            e = build_overload(e->loc, sc, e, NULL, fd);
            e = new DotIdExp(e->loc, e, ident);
            e = expressionSemantic(e, sc);
            --nest;
            return e;
        }

        /* Look for overloaded opDispatch to see if we should forward request
         * to it.
         */
        if (Dsymbol *fd = search_function(sym, Id::opDispatch))
        {
            /* Rewrite e.ident as:
             *  e.opDispatch!("ident")
             */
            TemplateDeclaration *td = fd->isTemplateDeclaration();
            if (!td)
            {
                fd->error("must be a template opDispatch(string s), not a %s", fd->kind());
                --nest;
                return new ErrorExp();
            }
            StringExp *se = new StringExp(e->loc, const_cast<char *>(ident->toChars()));
            Objects *tiargs = new Objects();
            tiargs->push(se);
            DotTemplateInstanceExp *dti = new DotTemplateInstanceExp(e->loc, e, Id::opDispatch, tiargs);
            dti->ti->tempdecl = td;

            /* opDispatch, which doesn't need IFTI,  may occur instantiate error.
             * It should be gagged if flag & 1.
             * e.g.
             *  template opDispatch(name) if (isValid!name) { ... }
             */
            unsigned errors = flag & 1 ? global.startGagging() : 0;
            e = semanticY(dti, sc, 0);
            if (flag & 1 && global.endGagging(errors))
                e = NULL;
            --nest;
            return e;
        }

        /* See if we should forward to the alias this.
         */
        if (sym->aliasthis)
        {   /* Rewrite e.ident as:
             *  e.aliasthis.ident
             */
            e = resolveAliasThis(sc, e);
            DotIdExp *die = new DotIdExp(e->loc, e, ident);
            e = semanticY(die, sc, flag & 1);
            --nest;
            return e;
        }
    }

    e = Type::dotExp(sc, e, ident, flag);
    --nest;
    return e;
}

void Type::error(Loc loc, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::verror(loc, format, ap);
    va_end( ap );
}

void Type::warning(Loc loc, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::vwarning(loc, format, ap);
    va_end( ap );
}

Identifier *Type::getTypeInfoIdent()
{
    // _init_10TypeInfo_%s
    OutBuffer buf;
    buf.reserve(32);
    mangleToBuffer(this, &buf);

    size_t len = buf.length();
    buf.writeByte(0);

    // Allocate buffer on stack, fail over to using malloc()
    char namebuf[128];
    size_t namelen = 19 + sizeof(len) * 3 + len + 1;
    char *name = namelen <= sizeof(namebuf) ? namebuf : (char *)mem.xmalloc(namelen);

    int length = sprintf(name, "_D%lluTypeInfo_%s6__initZ", (unsigned long long) 9 + len, buf.slice().ptr);
    //printf("%p, deco = %s, name = %s\n", this, deco, name);
    assert(0 < length && (size_t)length < namelen);     // don't overflow the buffer

    Identifier *id = Identifier::idPool(name, length);

    if (name != namebuf)
        free(name);
    return id;
}

TypeBasic *Type::isTypeBasic()
{
    return NULL;
}

TypeError *Type::isTypeError()
{
    return ty == Terror ? (TypeError *)this : NULL;
}

TypeVector *Type::isTypeVector()
{
    return ty == Tvector ? (TypeVector *)this : NULL;
}

TypeSArray *Type::isTypeSArray()
{
    return ty == Tsarray ? (TypeSArray *)this : NULL;
}

TypeDArray *Type::isTypeDArray()
{
    return ty == Tarray ? (TypeDArray *)this : NULL;
}

TypeAArray *Type::isTypeAArray()
{
    return ty == Taarray ? (TypeAArray *)this : NULL;
}

TypePointer *Type::isTypePointer()
{
    return ty == Tpointer ? (TypePointer *)this : NULL;
}

TypeReference *Type::isTypeReference()
{
    return ty == Treference ? (TypeReference *)this : NULL;
}

TypeFunction *Type::isTypeFunction()
{
    return ty == Tfunction ? (TypeFunction *)this : NULL;
}

TypeDelegate *Type::isTypeDelegate()
{
    return ty == Tdelegate ? (TypeDelegate *)this : NULL;
}

TypeIdentifier *Type::isTypeIdentifier()
{
    return ty == Tident ? (TypeIdentifier *)this : NULL;
}

TypeInstance *Type::isTypeInstance()
{
    return ty == Tinstance ? (TypeInstance *)this : NULL;
}

TypeTypeof *Type::isTypeTypeof()
{
    return ty == Ttypeof ? (TypeTypeof *)this : NULL;
}

TypeReturn *Type::isTypeReturn()
{
    return ty == Treturn ? (TypeReturn *)this : NULL;
}

TypeStruct *Type::isTypeStruct()
{
    return ty == Tstruct ? (TypeStruct *)this : NULL;
}

TypeEnum *Type::isTypeEnum()
{
    return ty == Tenum ? (TypeEnum *)this : NULL;
}

TypeClass *Type::isTypeClass()
{
    return ty == Tclass ? (TypeClass *)this : NULL;
}

TypeTuple *Type::isTypeTuple()
{
    return ty == Ttuple ? (TypeTuple *)this : NULL;
}

TypeSlice *Type::isTypeSlice()
{
    return ty == Tslice ? (TypeSlice *)this : NULL;
}

TypeNull *Type::isTypeNull()
{
    return ty == Tnull ? (TypeNull *)this : NULL;
}

TypeTraits *Type::isTypeTraits()
{
    return ty == Ttraits ? (TypeTraits *)this : NULL;
}

TypeMixin *Type::isTypeMixin()
{
    return ty == Tmixin ? (TypeMixin *)this : NULL;
}

TypeNoreturn *Type::isTypeNoreturn()
{
    return ty == Tnoreturn ? (TypeNoreturn *)this : NULL;
}

TypeFunction *Type::toTypeFunction()
{
    if (ty != Tfunction)
        assert(0);
    return (TypeFunction *)this;
}

/***************************************
 * Resolve 'this' type to either type, symbol, or expression.
 * If errors happened, resolved to Type.terror.
 */
void Type::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool)
{
    //printf("Type::resolve() %s, %d\n", toChars(), ty);
    Type *t = typeSemantic(this, loc, sc);
    *pt = t;
    *pe = NULL;
    *ps = NULL;
}

/***************************************
 * Normalize `e` as the result of Type::resolve() process.
 */
void Type::resolveExp(Expression *e, Type **pt, Expression **pe, Dsymbol **ps)
{
    *pt = NULL;
    *pe = NULL;
    *ps = NULL;

    Dsymbol *s;
    switch (e->op)
    {
        case TOKerror:
            *pt = Type::terror;
            return;

        case TOKtype:
            *pt = e->type;
            return;

        case TOKvar:
            s = ((VarExp *)e)->var;
            if (s->isVarDeclaration())
                goto Ldefault;
            //if (s->isOverDeclaration())
            //    todo;
            break;

        case TOKtemplate:
            // TemplateDeclaration
            s = ((TemplateExp *)e)->td;
            break;

        case TOKimport:
            s = ((ScopeExp *)e)->sds;
            // TemplateDeclaration, TemplateInstance, Import, Package, Module
            break;

        case TOKfunction:
            s = getDsymbol(e);
            break;

        //case TOKthis:
        //case TOKsuper:

        //case TOKtuple:

        //case TOKoverloadset:

        //case TOKdotvar:
        //case TOKdottd:
        //case TOKdotti:
        //case TOKdottype:
        //case TOKdot:

        default:
    Ldefault:
            *pe = e;
            return;
    }

    *ps = s;
}

/***************************************
 * Return !=0 if the type or any of its subtypes is wild.
 */

int Type::hasWild() const
{
    return mod & MODwild;
}

/***************************************
 * Return !=0 if type has pointers that need to
 * be scanned by the GC during a collection cycle.
 */
bool Type::hasPointers()
{
    //printf("Type::hasPointers() %s, %d\n", toChars(), ty);
    return false;
}

/*************************************
 * Detect if type has pointer fields that are initialized to void.
 * Local stack variables with such void fields can remain uninitialized,
 * leading to pointer bugs.
 * Returns:
 *  true if so
 */
bool Type::hasVoidInitPointers()
{
    return false;
}

/*************************************
 * If this is a type of something, return that something.
 */

Type *Type::nextOf()
{
    return NULL;
}

/*************************************
 * If this is a type of static array, return its base element type.
 */

Type *Type::baseElemOf()
{
    Type *t = toBasetype();
    while (t->ty == Tsarray)
        t = ((TypeSArray *)t)->next->toBasetype();
    return t;
}

/*************************************
 * Bugzilla 14488: Check if the inner most base type is complex or imaginary.
 * Should only give alerts when set to emit transitional messages.
 */

void Type::checkComplexTransition(Loc loc)
{
    Type *t = baseElemOf();
    while (t->ty == Tpointer || t->ty == Tarray)
        t = t->nextOf()->baseElemOf();

    if (t->isimaginary() || t->iscomplex())
    {
        Type *rt;
        switch (t->ty)
        {
            case Tcomplex32:
            case Timaginary32:
                rt = Type::tfloat32; break;
            case Tcomplex64:
            case Timaginary64:
                rt = Type::tfloat64; break;
            case Tcomplex80:
            case Timaginary80:
                rt = Type::tfloat80; break;
            default:
                assert(0);
        }
        if (t->iscomplex())
        {
            message(loc, "use of complex type `%s` is scheduled for deprecation, "
                    "use `std.complex.Complex!(%s)` instead", toChars(), rt->toChars());
        }
        else
        {
            message(loc, "use of imaginary type `%s` is scheduled for deprecation, "
                    "use `%s` instead\n", toChars(), rt->toChars());
        }
    }
}

/*******************************************
 * Compute number of elements for a (possibly multidimensional) static array,
 * or 1 for other types.
 * Params:
 *  loc = for error message
 * Returns:
 *  number of elements, uint.max on overflow
 */
unsigned Type::numberOfElems(const Loc &loc)
{
  //printf("Type::numberOfElems()\n");
  uinteger_t n = 1;
  Type *tb = this;
  while ((tb = tb->toBasetype())->ty == Tsarray)
    {
      bool overflow = false;
      n = mulu(n, ((TypeSArray *)tb)->dim->toUInteger(), overflow);
      if (overflow || n >= UINT32_MAX)
      {
          error(loc, "static array `%s` size overflowed to %llu", toChars(), (unsigned long long)n);
          return UINT32_MAX;
      }
      tb = ((TypeSArray *)tb)->next;
    }
  return (unsigned)n;
}

/****************************************
 * Return the mask that an integral type will
 * fit into.
 */
uinteger_t Type::sizemask()
{   uinteger_t m;

    switch (toBasetype()->ty)
    {
        case Tbool:     m = 1;                          break;
        case Tchar:
        case Tint8:
        case Tuns8:     m = 0xFF;                       break;
        case Twchar:
        case Tint16:
        case Tuns16:    m = 0xFFFFUL;                   break;
        case Tdchar:
        case Tint32:
        case Tuns32:    m = 0xFFFFFFFFUL;               break;
        case Tint64:
        case Tuns64:    m = 0xFFFFFFFFFFFFFFFFULL;      break;
        default:
                assert(0);
    }
    return m;
}

/* ============================= TypeError =========================== */

TypeError::TypeError()
        : Type(Terror)
{
}

Type *TypeError::syntaxCopy()
{
    // No semantic analysis done, no need to copy
    return this;
}

d_uns64 TypeError::size(Loc) { return SIZE_INVALID; }
Expression *TypeError::getProperty(Loc, Identifier *, int) { return new ErrorExp(); }
Expression *TypeError::dotExp(Scope *, Expression *, Identifier *, int) { return new ErrorExp(); }
Expression *TypeError::defaultInit(Loc) { return new ErrorExp(); }
Expression *TypeError::defaultInitLiteral(Loc) { return new ErrorExp(); }

/* ============================= TypeNext =========================== */

TypeNext::TypeNext(TY ty, Type *next)
        : Type(ty)
{
    this->next = next;
}

void TypeNext::checkDeprecated(Loc loc, Scope *sc)
{
    Type::checkDeprecated(loc, sc);
    if (next)   // next can be NULL if TypeFunction and auto return type
        next->checkDeprecated(loc, sc);
}

int TypeNext::hasWild() const
{
    if (ty == Tfunction)
        return 0;
    if (ty == Tdelegate)
        return Type::hasWild();
    return mod & MODwild || (next && next->hasWild());
}


/*******************************
 * For TypeFunction, nextOf() can return NULL if the function return
 * type is meant to be inferred, and semantic() hasn't yet ben run
 * on the function. After semantic(), it must no longer be NULL.
 */

Type *TypeNext::nextOf()
{
    return next;
}

Type *TypeNext::makeConst()
{
    //printf("TypeNext::makeConst() %p, %s\n", this, toChars());
    if (cto)
    {
        assert(cto->mod == MODconst);
        return cto;
    }
    TypeNext *t = (TypeNext *)Type::makeConst();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isShared())
        {
            if (next->isWild())
                t->next = next->sharedWildConstOf();
            else
                t->next = next->sharedConstOf();
        }
        else
        {
            if (next->isWild())
                t->next = next->wildConstOf();
            else
                t->next = next->constOf();
        }
    }
    //printf("TypeNext::makeConst() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeImmutable()
{
    //printf("TypeNext::makeImmutable() %s\n", toChars());
    if (ito)
    {
        assert(ito->isImmutable());
        return ito;
    }
    TypeNext *t = (TypeNext *)Type::makeImmutable();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        t->next = next->immutableOf();
    }
    return t;
}

Type *TypeNext::makeShared()
{
    //printf("TypeNext::makeShared() %s\n", toChars());
    if (sto)
    {
        assert(sto->mod == MODshared);
        return sto;
    }
    TypeNext *t = (TypeNext *)Type::makeShared();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isWild())
        {
            if (next->isConst())
                t->next = next->sharedWildConstOf();
            else
                t->next = next->sharedWildOf();
        }
        else
        {
            if (next->isConst())
                t->next = next->sharedConstOf();
            else
                t->next = next->sharedOf();
        }
    }
    //printf("TypeNext::makeShared() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeSharedConst()
{
    //printf("TypeNext::makeSharedConst() %s\n", toChars());
    if (scto)
    {
        assert(scto->mod == (MODshared | MODconst));
        return scto;
    }
    TypeNext *t = (TypeNext *)Type::makeSharedConst();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isWild())
            t->next = next->sharedWildConstOf();
        else
            t->next = next->sharedConstOf();
    }
    //printf("TypeNext::makeSharedConst() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeWild()
{
    //printf("TypeNext::makeWild() %s\n", toChars());
    if (wto)
    {
        assert(wto->mod == MODwild);
        return wto;
    }
    TypeNext *t = (TypeNext *)Type::makeWild();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isShared())
        {
            if (next->isConst())
                t->next = next->sharedWildConstOf();
            else
                t->next = next->sharedWildOf();
        }
        else
        {
            if (next->isConst())
                t->next = next->wildConstOf();
            else
                t->next = next->wildOf();
        }
    }
    //printf("TypeNext::makeWild() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeWildConst()
{
    //printf("TypeNext::makeWildConst() %s\n", toChars());
    if (wcto)
    {
        assert(wcto->mod == MODwildconst);
        return wcto;
    }
    TypeNext *t = (TypeNext *)Type::makeWildConst();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isShared())
            t->next = next->sharedWildConstOf();
        else
            t->next = next->wildConstOf();
    }
    //printf("TypeNext::makeWildConst() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeSharedWild()
{
    //printf("TypeNext::makeSharedWild() %s\n", toChars());
    if (swto)
    {
        assert(swto->isSharedWild());
        return swto;
    }
    TypeNext *t = (TypeNext *)Type::makeSharedWild();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        if (next->isConst())
            t->next = next->sharedWildConstOf();
        else
            t->next = next->sharedWildOf();
    }
    //printf("TypeNext::makeSharedWild() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeSharedWildConst()
{
    //printf("TypeNext::makeSharedWildConst() %s\n", toChars());
    if (swcto)
    {
        assert(swcto->mod == (MODshared | MODwildconst));
        return swcto;
    }
    TypeNext *t = (TypeNext *)Type::makeSharedWildConst();
    if (ty != Tfunction && next->ty != Tfunction &&
        !next->isImmutable())
    {
        t->next = next->sharedWildConstOf();
    }
    //printf("TypeNext::makeSharedWildConst() returns %p, %s\n", t, t->toChars());
    return t;
}

Type *TypeNext::makeMutable()
{
    //printf("TypeNext::makeMutable() %p, %s\n", this, toChars());
    TypeNext *t = (TypeNext *)Type::makeMutable();
    if (ty == Tsarray)
    {
        t->next = next->mutableOf();
    }
    //printf("TypeNext::makeMutable() returns %p, %s\n", t, t->toChars());
    return t;
}

MATCH TypeNext::constConv(Type *to)
{
    //printf("TypeNext::constConv from = %s, to = %s\n", toChars(), to->toChars());
    if (equals(to))
        return MATCHexact;

    if (!(ty == to->ty && MODimplicitConv(mod, to->mod)))
        return MATCHnomatch;

    Type *tn = to->nextOf();
    if (!(tn && next->ty == tn->ty))
        return MATCHnomatch;

    MATCH m;
    if (to->isConst())  // whole tail const conversion
    {   // Recursive shared level check
        m = next->constConv(tn);
        if (m == MATCHexact)
            m = MATCHconst;
    }
    else
    {   //printf("\tnext => %s, to->next => %s\n", next->toChars(), tn->toChars());
        m = next->equals(tn) ? MATCHconst : MATCHnomatch;
    }
    return m;
}

unsigned char TypeNext::deduceWild(Type *t, bool isRef)
{
    if (ty == Tfunction)
        return 0;

    unsigned char wm;

    Type *tn = t->nextOf();
    if (!isRef && (ty == Tarray || ty == Tpointer) && tn)
    {
        wm = next->deduceWild(tn, true);
        if (!wm)
            wm = Type::deduceWild(t, true);
    }
    else
    {
        wm = Type::deduceWild(t, isRef);
        if (!wm && tn)
            wm = next->deduceWild(tn, true);
    }

    return wm;
}


void TypeNext::transitive()
{
    /* Invoke transitivity of type attributes
     */
    next = next->addMod(mod);
}

/* ============================= TypeBasic =========================== */

#define TFLAGSintegral  1
#define TFLAGSfloating  2
#define TFLAGSunsigned  4
#define TFLAGSreal      8
#define TFLAGSimaginary 0x10
#define TFLAGScomplex   0x20

TypeBasic::TypeBasic(TY ty)
        : Type(ty)
{   const char *d;
    unsigned flags;

    flags = 0;
    switch (ty)
    {
        case Tvoid:     d = Token::toChars(TOKvoid);
                        break;

        case Tint8:     d = Token::toChars(TOKint8);
                        flags |= TFLAGSintegral;
                        break;

        case Tuns8:     d = Token::toChars(TOKuns8);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tint16:    d = Token::toChars(TOKint16);
                        flags |= TFLAGSintegral;
                        break;

        case Tuns16:    d = Token::toChars(TOKuns16);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tint32:    d = Token::toChars(TOKint32);
                        flags |= TFLAGSintegral;
                        break;

        case Tuns32:    d = Token::toChars(TOKuns32);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tfloat32:  d = Token::toChars(TOKfloat32);
                        flags |= TFLAGSfloating | TFLAGSreal;
                        break;

        case Tint64:    d = Token::toChars(TOKint64);
                        flags |= TFLAGSintegral;
                        break;

        case Tuns64:    d = Token::toChars(TOKuns64);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tint128:   d = Token::toChars(TOKint128);
                        flags |= TFLAGSintegral;
                        break;

        case Tuns128:   d = Token::toChars(TOKuns128);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tfloat64:  d = Token::toChars(TOKfloat64);
                        flags |= TFLAGSfloating | TFLAGSreal;
                        break;

        case Tfloat80:  d = Token::toChars(TOKfloat80);
                        flags |= TFLAGSfloating | TFLAGSreal;
                        break;

        case Timaginary32: d = Token::toChars(TOKimaginary32);
                        flags |= TFLAGSfloating | TFLAGSimaginary;
                        break;

        case Timaginary64: d = Token::toChars(TOKimaginary64);
                        flags |= TFLAGSfloating | TFLAGSimaginary;
                        break;

        case Timaginary80: d = Token::toChars(TOKimaginary80);
                        flags |= TFLAGSfloating | TFLAGSimaginary;
                        break;

        case Tcomplex32: d = Token::toChars(TOKcomplex32);
                        flags |= TFLAGSfloating | TFLAGScomplex;
                        break;

        case Tcomplex64: d = Token::toChars(TOKcomplex64);
                        flags |= TFLAGSfloating | TFLAGScomplex;
                        break;

        case Tcomplex80: d = Token::toChars(TOKcomplex80);
                        flags |= TFLAGSfloating | TFLAGScomplex;
                        break;

        case Tbool:     d = "bool";
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tchar:     d = Token::toChars(TOKchar);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Twchar:    d = Token::toChars(TOKwchar);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        case Tdchar:    d = Token::toChars(TOKdchar);
                        flags |= TFLAGSintegral | TFLAGSunsigned;
                        break;

        default:        assert(0);
    }
    this->dstring = d;
    this->flags = flags;
    merge();
}

const char *TypeBasic::kind()
{
    return dstring;
}

Type *TypeBasic::syntaxCopy()
{
    // No semantic analysis done on basic types, no need to copy
    return this;
}

d_uns64 TypeBasic::size(Loc)
{   unsigned size;

    //printf("TypeBasic::size()\n");
    switch (ty)
    {
        case Tint8:
        case Tuns8:     size = 1;       break;
        case Tint16:
        case Tuns16:    size = 2;       break;
        case Tint32:
        case Tuns32:
        case Tfloat32:
        case Timaginary32:
                        size = 4;       break;
        case Tint64:
        case Tuns64:
        case Tfloat64:
        case Timaginary64:
                        size = 8;       break;
        case Tfloat80:
        case Timaginary80:
                        size = target.realsize; break;
        case Tcomplex32:
                        size = 8;               break;
        case Tcomplex64:
        case Tint128:
        case Tuns128:
                        size = 16;              break;
        case Tcomplex80:
                        size = target.realsize * 2; break;

        case Tvoid:
            //size = Type::size();      // error message
            size = 1;
            break;

        case Tbool:     size = 1;               break;
        case Tchar:     size = 1;               break;
        case Twchar:    size = 2;               break;
        case Tdchar:    size = 4;               break;

        default:
            assert(0);
            break;
    }
    //printf("TypeBasic::size() = %d\n", size);
    return size;
}

unsigned TypeBasic::alignsize()
{
    return target.alignsize(this);
}


Expression *TypeBasic::getProperty(Loc loc, Identifier *ident, int flag)
{
    Expression *e;
    dinteger_t ivalue;
    real_t fvalue;

    //printf("TypeBasic::getProperty('%s')\n", ident->toChars());
    if (ident == Id::max)
    {
        switch (ty)
        {
        case Tint8:
            ivalue = 0x7F;
            goto Livalue;
        case Tuns8:
            ivalue = 0xFF;
            goto Livalue;
        case Tint16:
            ivalue = 0x7FFFUL;
            goto Livalue;
        case Tuns16:
            ivalue = 0xFFFFUL;
            goto Livalue;
        case Tint32:
            ivalue = 0x7FFFFFFFUL;
            goto Livalue;
        case Tuns32:
            ivalue = 0xFFFFFFFFUL;
            goto Livalue;
        case Tint64:
            ivalue = 0x7FFFFFFFFFFFFFFFLL;
            goto Livalue;
        case Tuns64:
            ivalue = 0xFFFFFFFFFFFFFFFFULL;
            goto Livalue;
        case Tbool:
            ivalue = 1;
            goto Livalue;
        case Tchar:
            ivalue = 0xFF;
            goto Livalue;
        case Twchar:
            ivalue = 0xFFFFUL;
            goto Livalue;
        case Tdchar:
            ivalue = 0x10FFFFUL;
            goto Livalue;
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            fvalue = target.FloatProperties.max;
            goto Lfvalue;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            fvalue = target.DoubleProperties.max;
            goto Lfvalue;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            fvalue = target.RealProperties.max;
            goto Lfvalue;
        }
    }
    else if (ident == Id::min)
    {
        switch (ty)
        {
        case Tint8:
            ivalue = -128;
            goto Livalue;
        case Tuns8:
            ivalue = 0;
            goto Livalue;
        case Tint16:
            ivalue = -32768;
            goto Livalue;
        case Tuns16:
            ivalue = 0;
            goto Livalue;
        case Tint32:
            ivalue = -2147483647L - 1;
            goto Livalue;
        case Tuns32:
            ivalue = 0;
            goto Livalue;
        case Tint64:
            ivalue = (-9223372036854775807LL-1LL);
            goto Livalue;
        case Tuns64:
            ivalue = 0;
            goto Livalue;
        case Tbool:
            ivalue = 0;
            goto Livalue;
        case Tchar:
            ivalue = 0;
            goto Livalue;
        case Twchar:
            ivalue = 0;
            goto Livalue;
        case Tdchar:
            ivalue = 0;
            goto Livalue;

        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            error(loc, "use .min_normal property instead of .min");
            return new ErrorExp();
        }
    }
    else if (ident == Id::min_normal)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            fvalue = target.FloatProperties.min_normal;
            goto Lfvalue;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            fvalue = target.DoubleProperties.min_normal;
            goto Lfvalue;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            fvalue = target.RealProperties.min_normal;
            goto Lfvalue;
        }
    }
    else if (ident == Id::nan)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Tcomplex64:
        case Tcomplex80:
        case Timaginary32:
        case Timaginary64:
        case Timaginary80:
        case Tfloat32:
        case Tfloat64:
        case Tfloat80:
            fvalue = target.RealProperties.nan;
            goto Lfvalue;
        }
    }
    else if (ident == Id::infinity)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Tcomplex64:
        case Tcomplex80:
        case Timaginary32:
        case Timaginary64:
        case Timaginary80:
        case Tfloat32:
        case Tfloat64:
        case Tfloat80:
            fvalue = target.RealProperties.infinity;
            goto Lfvalue;
        }
    }
    else if (ident == Id::dig)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.dig;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.dig;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.dig;
            goto Lint;
        }
    }
    else if (ident == Id::epsilon)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            fvalue = target.FloatProperties.epsilon;
            goto Lfvalue;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            fvalue = target.DoubleProperties.epsilon;
            goto Lfvalue;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            fvalue = target.RealProperties.epsilon;
            goto Lfvalue;
        }
    }
    else if (ident == Id::mant_dig)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.mant_dig;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.mant_dig;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.mant_dig;
            goto Lint;
        }
    }
    else if (ident == Id::max_10_exp)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.max_10_exp;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.max_10_exp;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.max_10_exp;
            goto Lint;
        }
    }
    else if (ident == Id::max_exp)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.max_exp;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.max_exp;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.max_exp;
            goto Lint;
        }
    }
    else if (ident == Id::min_10_exp)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.min_10_exp;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.min_10_exp;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.min_10_exp;
            goto Lint;
        }
    }
    else if (ident == Id::min_exp)
    {
        switch (ty)
        {
        case Tcomplex32:
        case Timaginary32:
        case Tfloat32:
            ivalue = target.FloatProperties.min_exp;
            goto Lint;
        case Tcomplex64:
        case Timaginary64:
        case Tfloat64:
            ivalue = target.DoubleProperties.min_exp;
            goto Lint;
        case Tcomplex80:
        case Timaginary80:
        case Tfloat80:
            ivalue = target.RealProperties.min_exp;
            goto Lint;
        }
    }

    return Type::getProperty(loc, ident, flag);

Livalue:
    e = new IntegerExp(loc, ivalue, this);
    return e;

Lfvalue:
    if (isreal() || isimaginary())
        e = new RealExp(loc, fvalue, this);
    else
    {
        complex_t cvalue = complex_t(fvalue, fvalue);
        //for (int i = 0; i < 20; i++)
        //    printf("%02x ", ((unsigned char *)&cvalue)[i]);
        //printf("\n");
        e = new ComplexExp(loc, cvalue, this);
    }
    return e;

Lint:
    e = new IntegerExp(loc, ivalue, Type::tint32);
    return e;
}

Expression *TypeBasic::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    Type *t;

    if (ident == Id::re)
    {
        switch (ty)
        {
            case Tcomplex32:    t = tfloat32;           goto L1;
            case Tcomplex64:    t = tfloat64;           goto L1;
            case Tcomplex80:    t = tfloat80;           goto L1;
            L1:
                e = e->castTo(sc, t);
                break;

            case Tfloat32:
            case Tfloat64:
            case Tfloat80:
                break;

            case Timaginary32:  t = tfloat32;           goto L2;
            case Timaginary64:  t = tfloat64;           goto L2;
            case Timaginary80:  t = tfloat80;           goto L2;
            L2:
                e = new RealExp(e->loc, CTFloat::zero, t);
                break;

            default:
                e = Type::getProperty(e->loc, ident, flag);
                break;
        }
    }
    else if (ident == Id::im)
    {   Type *t2;

        switch (ty)
        {
            case Tcomplex32:    t = timaginary32;       t2 = tfloat32;  goto L3;
            case Tcomplex64:    t = timaginary64;       t2 = tfloat64;  goto L3;
            case Tcomplex80:    t = timaginary80;       t2 = tfloat80;  goto L3;
            L3:
                e = e->castTo(sc, t);
                e->type = t2;
                break;

            case Timaginary32:  t = tfloat32;   goto L4;
            case Timaginary64:  t = tfloat64;   goto L4;
            case Timaginary80:  t = tfloat80;   goto L4;
            L4:
                e = e->copy();
                e->type = t;
                break;

            case Tfloat32:
            case Tfloat64:
            case Tfloat80:
                e = new RealExp(e->loc, CTFloat::zero, this);
                break;

            default:
                e = Type::getProperty(e->loc, ident, flag);
                break;
        }
    }
    else
    {
        return Type::dotExp(sc, e, ident, flag);
    }
    if (!(flag & 1) || e)
        e = expressionSemantic(e, sc);
    return e;
}

Expression *TypeBasic::defaultInit(Loc loc)
{
    dinteger_t value = 0;

    switch (ty)
    {
        case Tchar:
            value = 0xFF;
            break;

        case Twchar:
        case Tdchar:
            value = 0xFFFF;
            break;

        case Timaginary32:
        case Timaginary64:
        case Timaginary80:
        case Tfloat32:
        case Tfloat64:
        case Tfloat80:
            return new RealExp(loc, target.RealProperties.snan, this);

        case Tcomplex32:
        case Tcomplex64:
        case Tcomplex80:
        {   // Can't use fvalue + I*fvalue (the im part becomes a quiet NaN).
            complex_t cvalue = complex_t(target.RealProperties.snan, target.RealProperties.snan);
            return new ComplexExp(loc, cvalue, this);
        }

        case Tvoid:
            error(loc, "void does not have a default initializer");
            return new ErrorExp();
    }
    return new IntegerExp(loc, value, this);
}

bool TypeBasic::isZeroInit(Loc)
{
    switch (ty)
    {
        case Tchar:
        case Twchar:
        case Tdchar:
        case Timaginary32:
        case Timaginary64:
        case Timaginary80:
        case Tfloat32:
        case Tfloat64:
        case Tfloat80:
        case Tcomplex32:
        case Tcomplex64:
        case Tcomplex80:
            return false;       // no
        default:
            return true;        // yes
    }
}

bool TypeBasic::isintegral()
{
    //printf("TypeBasic::isintegral('%s') x%x\n", toChars(), flags);
    return (flags & TFLAGSintegral) != 0;
}

bool TypeBasic::isfloating()
{
    return (flags & TFLAGSfloating) != 0;
}

bool TypeBasic::isreal()
{
    return (flags & TFLAGSreal) != 0;
}

bool TypeBasic::isimaginary()
{
    return (flags & TFLAGSimaginary) != 0;
}

bool TypeBasic::iscomplex()
{
    return (flags & TFLAGScomplex) != 0;
}

bool TypeBasic::isunsigned()
{
    return (flags & TFLAGSunsigned) != 0;
}

bool TypeBasic::isscalar()
{
    return (flags & (TFLAGSintegral | TFLAGSfloating)) != 0;
}

MATCH TypeBasic::implicitConvTo(Type *to)
{
    //printf("TypeBasic::implicitConvTo(%s) from %s\n", to->toChars(), toChars());
    if (this == to)
        return MATCHexact;

    if (ty == to->ty)
    {
        if (mod == to->mod)
            return MATCHexact;
        else if (MODimplicitConv(mod, to->mod))
            return MATCHconst;
        else if (!((mod ^ to->mod) & MODshared))    // for wild matching
            return MATCHconst;
        else
            return MATCHconvert;
    }

    if (ty == Tvoid || to->ty == Tvoid)
        return MATCHnomatch;
    if (to->ty == Tbool)
        return MATCHnomatch;

    TypeBasic *tob;
    if (to->ty == Tvector && to->deco)
    {
        TypeVector *tv = (TypeVector *)to;
        tob = tv->elementType();
    }
    else if (to->ty == Tenum)
    {
        EnumDeclaration *ed = ((TypeEnum *)to)->sym;
        if (ed->isSpecial())
        {
            /* Special enums that allow implicit conversions to them.  */
            tob = to->toBasetype()->isTypeBasic();
            if (tob)
                return implicitConvTo(tob);
        }
        else
            return MATCHnomatch;
    }
    else
        tob = to->isTypeBasic();
    if (!tob)
        return MATCHnomatch;

    if (flags & TFLAGSintegral)
    {
        // Disallow implicit conversion of integers to imaginary or complex
        if (tob->flags & (TFLAGSimaginary | TFLAGScomplex))
            return MATCHnomatch;

        // If converting from integral to integral
        if (tob->flags & TFLAGSintegral)
        {   d_uns64 sz = size(Loc());
            d_uns64 tosz = tob->size(Loc());

            /* Can't convert to smaller size
             */
            if (sz > tosz)
                return MATCHnomatch;

            /* Can't change sign if same size
             */
            /*if (sz == tosz && (flags ^ tob->flags) & TFLAGSunsigned)
                return MATCHnomatch;*/
        }
    }
    else if (flags & TFLAGSfloating)
    {
        // Disallow implicit conversion of floating point to integer
        if (tob->flags & TFLAGSintegral)
            return MATCHnomatch;

        assert(tob->flags & TFLAGSfloating || to->ty == Tvector);

        // Disallow implicit conversion from complex to non-complex
        if (flags & TFLAGScomplex && !(tob->flags & TFLAGScomplex))
            return MATCHnomatch;

        // Disallow implicit conversion of real or imaginary to complex
        if (flags & (TFLAGSreal | TFLAGSimaginary) &&
            tob->flags & TFLAGScomplex)
            return MATCHnomatch;

        // Disallow implicit conversion to-from real and imaginary
        if ((flags & (TFLAGSreal | TFLAGSimaginary)) !=
            (tob->flags & (TFLAGSreal | TFLAGSimaginary)))
            return MATCHnomatch;
    }
    return MATCHconvert;
}

TypeBasic *TypeBasic::isTypeBasic()
{
    return (TypeBasic *)this;
}

/* ============================= TypeVector =========================== */

/* The basetype must be one of:
 *   byte[16],ubyte[16],short[8],ushort[8],int[4],uint[4],long[2],ulong[2],float[4],double[2]
 * For AVX:
 *   byte[32],ubyte[32],short[16],ushort[16],int[8],uint[8],long[4],ulong[4],float[8],double[4]
 */
TypeVector::TypeVector(Type *basetype)
        : Type(Tvector)
{
    this->basetype = basetype;
}

TypeVector *TypeVector::create(Type *basetype)
{
    return new TypeVector(basetype);
}

const char *TypeVector::kind()
{
    return "vector";
}

Type *TypeVector::syntaxCopy()
{
    return new TypeVector(basetype->syntaxCopy());
}

TypeBasic *TypeVector::elementType()
{
    assert(basetype->ty == Tsarray);
    TypeSArray *t = (TypeSArray *)basetype;
    TypeBasic *tb = t->nextOf()->isTypeBasic();
    assert(tb);
    return tb;
}

bool TypeVector::isBoolean()
{
    return false;
}

d_uns64 TypeVector::size(Loc)
{
    return basetype->size();
}

unsigned TypeVector::alignsize()
{
    return (unsigned)basetype->size();
}

Expression *TypeVector::getProperty(Loc loc, Identifier *ident, int flag)
{
    return Type::getProperty(loc, ident, flag);
}

Expression *TypeVector::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    if (ident == Id::ptr && e->op == TOKcall)
    {
        /* The trouble with TOKcall is the return ABI for float[4] is different from
         * __vector(float[4]), and a type paint won't do.
         */
        e = new AddrExp(e->loc, e);
        e = expressionSemantic(e, sc);
        e = e->castTo(sc, basetype->nextOf()->pointerTo());
        return e;
    }
    if (ident == Id::array)
    {
        //e = e->castTo(sc, basetype);
        // Keep lvalue-ness
        e = new VectorArrayExp(e->loc, e);
        e = expressionSemantic(e, sc);
        return e;
    }
    if (ident == Id::_init || ident == Id::offsetof || ident == Id::stringof || ident == Id::__xalignof)
    {
        // init should return a new VectorExp (Bugzilla 12776)
        // offsetof does not work on a cast expression, so use e directly
        // stringof should not add a cast to the output
        return Type::dotExp(sc, e, ident, flag);
    }
    return basetype->dotExp(sc, e->castTo(sc, basetype), ident, flag);
}

Expression *TypeVector::defaultInit(Loc loc)
{
    //printf("TypeVector::defaultInit()\n");
    assert(basetype->ty == Tsarray);
    Expression *e = basetype->defaultInit(loc);
    VectorExp *ve = new VectorExp(loc, e, this);
    ve->type = this;
    ve->dim = (int)(basetype->size(loc) / elementType()->size(loc));
    return ve;
}

Expression *TypeVector::defaultInitLiteral(Loc loc)
{
    //printf("TypeVector::defaultInitLiteral()\n");
    assert(basetype->ty == Tsarray);
    Expression *e = basetype->defaultInitLiteral(loc);
    VectorExp *ve = new VectorExp(loc, e, this);
    ve->type = this;
    ve->dim = (int)(basetype->size(loc) / elementType()->size(loc));
    return ve;
}

bool TypeVector::isZeroInit(Loc loc)
{
    return basetype->isZeroInit(loc);
}

bool TypeVector::isintegral()
{
    //printf("TypeVector::isintegral('%s') x%x\n", toChars(), flags);
    return basetype->nextOf()->isintegral();
}

bool TypeVector::isfloating()
{
    return basetype->nextOf()->isfloating();
}

bool TypeVector::isunsigned()
{
    return basetype->nextOf()->isunsigned();
}

bool TypeVector::isscalar()
{
    return basetype->nextOf()->isscalar();
}

MATCH TypeVector::implicitConvTo(Type *to)
{
    //printf("TypeVector::implicitConvTo(%s) from %s\n", to->toChars(), toChars());
    if (this == to)
        return MATCHexact;
#ifdef IN_GCC
    if (to->ty == Tvector)
    {
        TypeVector *tv = (TypeVector *)to;
        assert(basetype->ty == Tsarray && tv->basetype->ty == Tsarray);

        // Can't convert to a vector which has different size.
        if (basetype->size() != tv->basetype->size())
            return MATCHnomatch;

        // Allow conversion to void[]
        if (tv->basetype->nextOf()->ty == Tvoid)
            return MATCHconvert;

        // Otherwise implicitly convertible only if basetypes are.
        return basetype->implicitConvTo(tv->basetype);
    }
#else
    if (ty == to->ty)
        return MATCHconvert;
#endif
    return MATCHnomatch;
}

/***************************** TypeArray *****************************/

TypeArray::TypeArray(TY ty, Type *next)
    : TypeNext(ty, next)
{
}

Expression *TypeArray::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    e = Type::dotExp(sc, e, ident, flag);

    if (!(flag & 1) || e)
        e = expressionSemantic(e, sc);
    return e;
}


/***************************** TypeSArray *****************************/

TypeSArray::TypeSArray(Type *t, Expression *dim)
    : TypeArray(Tsarray, t)
{
    //printf("TypeSArray(%s)\n", dim->toChars());
    this->dim = dim;
}

const char *TypeSArray::kind()
{
    return "sarray";
}

Type *TypeSArray::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    Expression *e = dim->syntaxCopy();
    t = new TypeSArray(t, e);
    t->mod = mod;
    return t;
}

d_uns64 TypeSArray::size(Loc loc)
{
    //printf("TypeSArray::size()\n");
    uinteger_t n = numberOfElems(loc);
    uinteger_t elemsize = baseElemOf()->size();
    bool overflow = false;
    uinteger_t sz = mulu(n, elemsize, overflow);
    if (overflow || sz >= UINT32_MAX)
    {
        if (elemsize != SIZE_INVALID && n != UINT32_MAX)
            error(loc, "static array `%s` size overflowed to %lld", toChars(), (long long)sz);
        return SIZE_INVALID;
    }
    return sz;
}

unsigned TypeSArray::alignsize()
{
    return next->alignsize();
}

void TypeSArray::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    //printf("TypeSArray::resolve() %s\n", toChars());
    next->resolve(loc, sc, pe, pt, ps, intypeid);
    //printf("s = %p, e = %p, t = %p\n", *ps, *pe, *pt);
    if (*pe)
    {
        // It's really an index expression
        if (Dsymbol *s = getDsymbol(*pe))
            *pe = new DsymbolExp(loc, s);
        *pe = new ArrayExp(loc, *pe, dim);
    }
    else if (*ps)
    {
        Dsymbol *s = *ps;
        TupleDeclaration *td = s->isTupleDeclaration();
        if (td)
        {
            ScopeDsymbol *sym = new ArrayScopeSymbol(sc, td);
            sym->parent = sc->scopesym;
            sc = sc->push(sym);
            sc = sc->startCTFE();
            dim = expressionSemantic(dim, sc);
            sc = sc->endCTFE();
            sc = sc->pop();

            dim = dim->ctfeInterpret();
            uinteger_t d = dim->toUInteger();

            if (d >= td->objects->length)
            {
                error(loc, "tuple index %llu exceeds length %u", d, td->objects->length);
                *ps = NULL;
                *pt = Type::terror;
                return;
            }
            RootObject *o = (*td->objects)[(size_t)d];
            if (o->dyncast() == DYNCAST_DSYMBOL)
            {
                *ps = (Dsymbol *)o;
                return;
            }
            if (o->dyncast() == DYNCAST_EXPRESSION)
            {
                Expression *e = (Expression *)o;
                if (e->op == TOKdsymbol)
                {
                    *ps = ((DsymbolExp *)e)->s;
                    *pe = NULL;
                }
                else
                {
                    *ps = NULL;
                    *pe = e;
                }
                return;
            }
            if (o->dyncast() == DYNCAST_TYPE)
            {
                *ps = NULL;
                *pt = ((Type *)o)->addMod(this->mod);
                return;
            }

            /* Create a new TupleDeclaration which
             * is a slice [d..d+1] out of the old one.
             * Do it this way because TemplateInstance::semanticTiargs()
             * can handle unresolved Objects this way.
             */
            Objects *objects = new Objects;
            objects->setDim(1);
            (*objects)[0] = o;

            TupleDeclaration *tds = new TupleDeclaration(loc, td->ident, objects);
            *ps = tds;
        }
        else
            goto Ldefault;
    }
    else
    {
        if ((*pt)->ty != Terror)
            next = *pt;     // prevent re-running semantic() on 'next'
     Ldefault:
        Type::resolve(loc, sc, pe, pt, ps, intypeid);
    }
}

Expression *TypeSArray::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    if (ident == Id::length)
    {
        Loc oldLoc = e->loc;
        e = dim->copy();
        e->loc = oldLoc;
    }
    else if (ident == Id::ptr)
    {
        if (e->op == TOKtype)
        {
            e->error("%s is not an expression", e->toChars());
            return new ErrorExp();
        }
        else if (!(flag & 2) && sc->func && !sc->intypeof && sc->func->setUnsafe())
        {
            e->deprecation("%s.ptr cannot be used in @safe code, use &%s[0] instead", e->toChars(), e->toChars());
            // return new ErrorExp();
        }
        e = e->castTo(sc, e->type->nextOf()->pointerTo());
    }
    else
    {
        e = TypeArray::dotExp(sc, e, ident, flag);
    }
    if (!(flag & 1) || e)
        e = expressionSemantic(e, sc);
    return e;
}

structalign_t TypeSArray::alignment()
{
    return next->alignment();
}

bool TypeSArray::isString()
{
    TY nty = next->toBasetype()->ty;
    return nty == Tchar || nty == Twchar || nty == Tdchar;
}

MATCH TypeSArray::constConv(Type *to)
{
    if (to->ty == Tsarray)
    {
        TypeSArray *tsa = (TypeSArray *)to;
        if (!dim->equals(tsa->dim))
            return MATCHnomatch;
    }
    return TypeNext::constConv(to);
}

MATCH TypeSArray::implicitConvTo(Type *to)
{
    //printf("TypeSArray::implicitConvTo(to = %s) this = %s\n", to->toChars(), toChars());

    if (to->ty == Tarray)
    {
        TypeDArray *ta = (TypeDArray *)to;

        if (!MODimplicitConv(next->mod, ta->next->mod))
            return MATCHnomatch;

        /* Allow conversion to void[]
         */
        if (ta->next->ty == Tvoid)
        {
            return MATCHconvert;
        }

        MATCH m = next->constConv(ta->next);
        if (m > MATCHnomatch)
        {
            return MATCHconvert;
        }
        return MATCHnomatch;
    }

    if (to->ty == Tsarray)
    {
        if (this == to)
            return MATCHexact;

        TypeSArray *tsa = (TypeSArray *)to;

        if (dim->equals(tsa->dim))
        {
            /* Since static arrays are value types, allow
             * conversions from const elements to non-const
             * ones, just like we allow conversion from const int
             * to int.
             */
            MATCH m = next->implicitConvTo(tsa->next);
            if (m >= MATCHconst)
            {
                if (mod != to->mod)
                    m = MATCHconst;
                return m;
            }
        }
    }
    return MATCHnomatch;
}

Expression *TypeSArray::defaultInit(Loc loc)
{
    if (next->ty == Tvoid)
        return tuns8->defaultInit(loc);
    else
        return next->defaultInit(loc);
}

bool TypeSArray::isZeroInit(Loc loc)
{
    return next->isZeroInit(loc);
}

bool TypeSArray::needsDestruction()
{
    return next->needsDestruction();
}

/*********************************
 *
 */

bool TypeSArray::needsNested()
{
    return next->needsNested();
}

Expression *TypeSArray::defaultInitLiteral(Loc loc)
{
    size_t d = (size_t)dim->toInteger();
    Expression *elementinit;
    if (next->ty == Tvoid)
        elementinit = tuns8->defaultInitLiteral(loc);
    else
        elementinit = next->defaultInitLiteral(loc);
    Expressions *elements = new Expressions();
    elements->setDim(d);
    for (size_t i = 0; i < d; i++)
        (*elements)[i] = NULL;
    ArrayLiteralExp *ae = new ArrayLiteralExp(Loc(), this, elementinit, elements);
    return ae;
}

bool TypeSArray::hasPointers()
{
    /* Don't want to do this, because:
     *    struct S { T* array[0]; }
     * may be a variable length struct.
     */
    //if (dim->toInteger() == 0)
    //    return false;

    if (next->ty == Tvoid)
    {
        // Arrays of void contain arbitrary data, which may include pointers
        return true;
    }
    else
        return next->hasPointers();
}

/***************************** TypeDArray *****************************/

TypeDArray::TypeDArray(Type *t)
    : TypeArray(Tarray, t)
{
    //printf("TypeDArray(t = %p)\n", t);
}

const char *TypeDArray::kind()
{
    return "darray";
}

Type *TypeDArray::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    if (t == next)
        t = this;
    else
    {
        t = new TypeDArray(t);
        t->mod = mod;
    }
    return t;
}

d_uns64 TypeDArray::size(Loc)
{
    //printf("TypeDArray::size()\n");
    return target.ptrsize * 2;
}

unsigned TypeDArray::alignsize()
{
    // A DArray consists of two ptr-sized values, so align it on pointer size
    // boundary
    return target.ptrsize;
}

void TypeDArray::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    //printf("TypeDArray::resolve() %s\n", toChars());
    next->resolve(loc, sc, pe, pt, ps, intypeid);
    //printf("s = %p, e = %p, t = %p\n", *ps, *pe, *pt);
    if (*pe)
    {
        // It's really a slice expression
        if (Dsymbol *s = getDsymbol(*pe))
            *pe = new DsymbolExp(loc, s);
        *pe = new ArrayExp(loc, *pe);
    }
    else if (*ps)
    {
        TupleDeclaration *td = (*ps)->isTupleDeclaration();
        if (td)
            ;   // keep *ps
        else
            goto Ldefault;
    }
    else
    {
        if ((*pt)->ty != Terror)
            next = *pt;     // prevent re-running semantic() on 'next'
     Ldefault:
        Type::resolve(loc, sc, pe, pt, ps, intypeid);
    }
}

Expression *TypeDArray::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    if (e->op == TOKtype &&
        (ident == Id::length || ident == Id::ptr))
    {
        e->error("%s is not an expression", e->toChars());
        return new ErrorExp();
    }
    if (ident == Id::length)
    {
        if (e->op == TOKstring)
        {
            StringExp *se = (StringExp *)e;
            return new IntegerExp(se->loc, se->len, Type::tsize_t);
        }
        if (e->op == TOKnull)
            return new IntegerExp(e->loc, 0, Type::tsize_t);
        if (checkNonAssignmentArrayOp(e))
            return new ErrorExp();
        e = new ArrayLengthExp(e->loc, e);
        e->type = Type::tsize_t;
        return e;
    }
    else if (ident == Id::ptr)
    {
        if (!(flag & 2) && sc->func && !sc->intypeof && sc->func->setUnsafe())
        {
            e->deprecation("%s.ptr cannot be used in @safe code, use &%s[0] instead", e->toChars(), e->toChars());
            // return new ErrorExp();
        }
        e = e->castTo(sc, next->pointerTo());
        return e;
    }
    else
    {
        e = TypeArray::dotExp(sc, e, ident, flag);
    }
    return e;
}

bool TypeDArray::isString()
{
    TY nty = next->toBasetype()->ty;
    return nty == Tchar || nty == Twchar || nty == Tdchar;
}

MATCH TypeDArray::implicitConvTo(Type *to)
{
    //printf("TypeDArray::implicitConvTo(to = %s) this = %s\n", to->toChars(), toChars());
    if (equals(to))
        return MATCHexact;

    if (to->ty == Tarray)
    {
        TypeDArray *ta = (TypeDArray *)to;

        if (!MODimplicitConv(next->mod, ta->next->mod))
            return MATCHnomatch;        // not const-compatible

        /* Allow conversion to void[]
         */
        if (next->ty != Tvoid && ta->next->ty == Tvoid)
        {
            return MATCHconvert;
        }

        MATCH m = next->constConv(ta->next);
        if (m > MATCHnomatch)
        {
            if (m == MATCHexact && mod != to->mod)
                m = MATCHconst;
            return m;
        }
    }
    return Type::implicitConvTo(to);
}

Expression *TypeDArray::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypeDArray::isZeroInit(Loc)
{
    return true;
}

bool TypeDArray::isBoolean()
{
    return true;
}

bool TypeDArray::hasPointers()
{
    return true;
}


/***************************** TypeAArray *****************************/

TypeAArray::TypeAArray(Type *t, Type *index)
    : TypeArray(Taarray, t)
{
    this->index = index;
    this->loc = Loc();
    this->sc = NULL;
}

TypeAArray *TypeAArray::create(Type *t, Type *index)
{
    return new TypeAArray(t, index);
}

const char *TypeAArray::kind()
{
    return "aarray";
}

Type *TypeAArray::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    Type *ti = index->syntaxCopy();
    if (t == next && ti == index)
        t = this;
    else
    {
        t = new TypeAArray(t, ti);
        t->mod = mod;
    }
    return t;
}

d_uns64 TypeAArray::size(Loc)
{
    return target.ptrsize;
}

void TypeAArray::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    //printf("TypeAArray::resolve() %s\n", toChars());

    // Deal with the case where we thought the index was a type, but
    // in reality it was an expression.
    if (index->ty == Tident || index->ty == Tinstance || index->ty == Tsarray)
    {
        Expression *e;
        Type *t;
        Dsymbol *s;

        index->resolve(loc, sc, &e, &t, &s, intypeid);
        if (e)
        {
            // It was an expression -
            // Rewrite as a static array
            TypeSArray *tsa = new TypeSArray(next, e);
            tsa->mod = this->mod;   // just copy mod field so tsa's semantic is not yet done
            return tsa->resolve(loc, sc, pe, pt, ps, intypeid);
        }
        else if (t)
            index = t;
        else
            index->error(loc, "index is not a type or an expression");
    }
    Type::resolve(loc, sc, pe, pt, ps, intypeid);
}


Expression *TypeAArray::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    if (ident == Id::length)
    {
        static FuncDeclaration *fd_aaLen = NULL;
        if (fd_aaLen == NULL)
        {
            Parameters *fparams = new Parameters();
            fparams->push(new Parameter(STCin, this, NULL, NULL, NULL));
            fd_aaLen = FuncDeclaration::genCfunc(fparams, Type::tsize_t, Id::aaLen);
            TypeFunction *tf = fd_aaLen->type->toTypeFunction();
            tf->purity = PUREconst;
            tf->isnothrow = true;
            tf->isnogc = false;
        }
        Expression *ev = new VarExp(e->loc, fd_aaLen, false);
        e = new CallExp(e->loc, ev, e);
        e->type = fd_aaLen->type->toTypeFunction()->next;
    }
    else
        e = Type::dotExp(sc, e, ident, flag);
    return e;
}

Expression *TypeAArray::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypeAArray::isZeroInit(Loc)
{
    return true;
}

bool TypeAArray::isBoolean()
{
    return true;
}

bool TypeAArray::hasPointers()
{
    return true;
}

MATCH TypeAArray::implicitConvTo(Type *to)
{
    //printf("TypeAArray::implicitConvTo(to = %s) this = %s\n", to->toChars(), toChars());
    if (equals(to))
        return MATCHexact;

    if (to->ty == Taarray)
    {   TypeAArray *ta = (TypeAArray *)to;

        if (!MODimplicitConv(next->mod, ta->next->mod))
            return MATCHnomatch;        // not const-compatible

        if (!MODimplicitConv(index->mod, ta->index->mod))
            return MATCHnomatch;        // not const-compatible

        MATCH m = next->constConv(ta->next);
        MATCH mi = index->constConv(ta->index);
        if (m > MATCHnomatch && mi > MATCHnomatch)
        {
            return MODimplicitConv(mod, to->mod) ? MATCHconst : MATCHnomatch;
        }
    }
    return Type::implicitConvTo(to);
}

MATCH TypeAArray::constConv(Type *to)
{
    if (to->ty == Taarray)
    {
        TypeAArray *taa = (TypeAArray *)to;
        MATCH mindex = index->constConv(taa->index);
        MATCH mkey = next->constConv(taa->next);
        // Pick the worst match
        return mkey < mindex ? mkey : mindex;
    }
    return Type::constConv(to);
}

/***************************** TypePointer *****************************/

TypePointer::TypePointer(Type *t)
    : TypeNext(Tpointer, t)
{
}

TypePointer *TypePointer::create(Type *t)
{
    return new TypePointer(t);
}

const char *TypePointer::kind()
{
    return "pointer";
}

Type *TypePointer::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    if (t == next)
        t = this;
    else
    {
        t = new TypePointer(t);
        t->mod = mod;
    }
    return t;
}

d_uns64 TypePointer::size(Loc)
{
    return target.ptrsize;
}

MATCH TypePointer::implicitConvTo(Type *to)
{
    //printf("TypePointer::implicitConvTo(to = %s) %s\n", to->toChars(), toChars());

    if (equals(to))
        return MATCHexact;
    if (next->ty == Tfunction)
    {
        if (to->ty == Tpointer)
        {
            TypePointer *tp = (TypePointer *)to;
            if (tp->next->ty == Tfunction)
            {
                if (next->equals(tp->next))
                    return MATCHconst;

                if (next->covariant(tp->next) == 1)
                {
                    Type *tret = this->next->nextOf();
                    Type *toret = tp->next->nextOf();
                    if (tret->ty == Tclass && toret->ty == Tclass)
                    {
                        /* Bugzilla 10219: Check covariant interface return with offset tweaking.
                         * interface I {}
                         * class C : Object, I {}
                         * I function() dg = function C() {}    // should be error
                         */
                        int offset = 0;
                        if (toret->isBaseOf(tret, &offset) && offset != 0)
                            return MATCHnomatch;
                    }
                    return MATCHconvert;
                }
            }
            else if (tp->next->ty == Tvoid)
            {
                // Allow conversions to void*
                return MATCHconvert;
            }
        }
        return MATCHnomatch;
    }
    else if (to->ty == Tpointer)
    {
        TypePointer *tp = (TypePointer *)to;
        assert(tp->next);

        if (!MODimplicitConv(next->mod, tp->next->mod))
            return MATCHnomatch;        // not const-compatible

        /* Alloc conversion to void*
         */
        if (next->ty != Tvoid && tp->next->ty == Tvoid)
        {
            return MATCHconvert;
        }

        MATCH m = next->constConv(tp->next);
        if (m > MATCHnomatch)
        {
            if (m == MATCHexact && mod != to->mod)
                m = MATCHconst;
            return m;
        }
    }
    return MATCHnomatch;
}

MATCH TypePointer::constConv(Type *to)
{
    if (next->ty == Tfunction)
    {
        if (to->nextOf() && next->equals(((TypeNext *)to)->next))
            return Type::constConv(to);
        else
            return MATCHnomatch;
    }
    return TypeNext::constConv(to);
}

bool TypePointer::isscalar()
{
    return true;
}

Expression *TypePointer::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypePointer::isZeroInit(Loc)
{
    return true;
}

bool TypePointer::hasPointers()
{
    return true;
}


/***************************** TypeReference *****************************/

TypeReference::TypeReference(Type *t)
    : TypeNext(Treference, t)
{
    // BUG: what about references to static arrays?
}

const char *TypeReference::kind()
{
    return "reference";
}

Type *TypeReference::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    if (t == next)
        t = this;
    else
    {
        t = new TypeReference(t);
        t->mod = mod;
    }
    return t;
}

d_uns64 TypeReference::size(Loc)
{
    return target.ptrsize;
}

Expression *TypeReference::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    // References just forward things along
    return next->dotExp(sc, e, ident, flag);
}

Expression *TypeReference::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypeReference::isZeroInit(Loc)
{
    return true;
}


/***************************** TypeFunction *****************************/

TypeFunction::TypeFunction(const ParameterList &pl, Type *treturn, LINK linkage, StorageClass stc)
    : TypeNext(Tfunction, treturn)
{
//if (!treturn) *(char*)0=0;
//    assert(treturn);
    assert(VARARGnone <= pl.varargs && pl.varargs <= VARARGtypesafe);
    this->parameterList = pl;
    this->linkage = linkage;
    this->inuse = 0;
    this->isnothrow = false;
    this->isnogc = false;
    this->purity = PUREimpure;
    this->isproperty = false;
    this->isref = false;
    this->isreturn = false;
    this->isscope = false;
    this->isscopeinferred = false;
    this->iswild = 0;
    this->fargs = NULL;

    if (stc & STCpure)
        this->purity = PUREfwdref;
    if (stc & STCnothrow)
        this->isnothrow = true;
    if (stc & STCnogc)
        this->isnogc = true;
    if (stc & STCproperty)
        this->isproperty = true;

    if (stc & STCref)
        this->isref = true;
    if (stc & STCreturn)
        this->isreturn = true;
    if (stc & STCscope)
        this->isscope = true;
    if (stc & STCscopeinferred)
        this->isscopeinferred = true;

    this->trust = TRUSTdefault;
    if (stc & STCsafe)
        this->trust = TRUSTsafe;
    if (stc & STCsystem)
        this->trust = TRUSTsystem;
    if (stc & STCtrusted)
        this->trust = TRUSTtrusted;
}

TypeFunction *TypeFunction::create(Parameters *parameters, Type *treturn, VarArg varargs, LINK linkage, StorageClass stc)
{
    return new TypeFunction(ParameterList(parameters, varargs), treturn, linkage, stc);
}

const char *TypeFunction::kind()
{
    return "function";
}

Type *TypeFunction::syntaxCopy()
{
    Type *treturn = next ? next->syntaxCopy() : NULL;
    Parameters *parameters = Parameter::arraySyntaxCopy(parameterList.parameters);
    TypeFunction *t = new TypeFunction(ParameterList(parameters, parameterList.varargs),
                                       treturn, linkage);
    t->mod = mod;
    t->isnothrow = isnothrow;
    t->isnogc = isnogc;
    t->purity = purity;
    t->isproperty = isproperty;
    t->isref = isref;
    t->isreturn = isreturn;
    t->isscope = isscope;
    t->isscopeinferred = isscopeinferred;
    t->iswild = iswild;
    t->trust = trust;
    t->fargs = fargs;
    return t;
}

/*******************************
 * Covariant means that 'this' can substitute for 't',
 * i.e. a pure function is a match for an impure type.
 * Params:
 *      t = type 'this' is covariant with
 *      pstc = if not null, store STCxxxx which would make it covariant
 *      fix17349 = enable fix https://issues.dlang.org/show_bug.cgi?id=17349
 * Returns:
 *      0       types are distinct
 *      1       this is covariant with t
 *      2       arguments match as far as overloading goes,
 *              but types are not covariant
 *      3       cannot determine covariance because of forward references
 *      *pstc   STCxxxx which would make it covariant
 */

int Type::covariant(Type *t, StorageClass *pstc, bool fix17349)
{
    if (pstc)
        *pstc = 0;
    StorageClass stc = 0;

    bool notcovariant = false;

    TypeFunction *t1;
    TypeFunction *t2;

    if (equals(t))
        return 1;                       // covariant

    if (ty != Tfunction || t->ty != Tfunction)
        goto Ldistinct;

    t1 = (TypeFunction *)this;
    t2 = (TypeFunction *)t;

    if (t1->parameterList.varargs != t2->parameterList.varargs)
        goto Ldistinct;

    if (t1->parameterList.parameters && t2->parameterList.parameters)
    {
        size_t dim = t1->parameterList.length();
        if (dim != t2->parameterList.length())
            goto Ldistinct;

        for (size_t i = 0; i < dim; i++)
        {
            Parameter *fparam1 = t1->parameterList[i];
            Parameter *fparam2 = t2->parameterList[i];

            if (!fparam1->type->equals(fparam2->type))
            {
                if (!fix17349)
                    goto Ldistinct;
                Type *tp1 = fparam1->type;
                Type *tp2 = fparam2->type;
                if (tp1->ty == tp2->ty)
                {
                    if (tp1->ty == Tclass)
                    {
                        if (((TypeClass *)tp1)->sym == ((TypeClass *)tp2)->sym && MODimplicitConv(tp2->mod, tp1->mod))
                            goto Lcov;
                    }
                    else if (tp1->ty == Tstruct)
                    {
                        if (((TypeStruct *)tp1)->sym == ((TypeStruct *)tp2)->sym && MODimplicitConv(tp2->mod, tp1->mod))
                            goto Lcov;
                    }
                    else if (tp1->ty == Tpointer)
                    {
                        if (tp2->implicitConvTo(tp1))
                            goto Lcov;
                    }
                    else if (tp1->ty == Tarray)
                    {
                        if (tp2->implicitConvTo(tp1))
                            goto Lcov;
                    }
                    else if (tp1->ty == Tdelegate)
                    {
                        if (tp1->implicitConvTo(tp2))
                            goto Lcov;
                    }
                }
                goto Ldistinct;
            }
        Lcov:
            notcovariant |= !fparam1->isCovariant(t1->isref, fparam2);
        }
    }
    else if (t1->parameterList.parameters != t2->parameterList.parameters)
    {
        size_t dim1 = t1->parameterList.length();
        size_t dim2 = t2->parameterList.length();
        if (dim1 || dim2)
            goto Ldistinct;
    }

    // The argument lists match
    if (notcovariant)
        goto Lnotcovariant;
    if (t1->linkage != t2->linkage)
        goto Lnotcovariant;

  {
    // Return types
    Type *t1n = t1->next;
    Type *t2n = t2->next;

    if (!t1n || !t2n)           // happens with return type inference
        goto Lnotcovariant;

    if (t1n->equals(t2n))
        goto Lcovariant;
    if (t1n->ty == Tclass && t2n->ty == Tclass)
    {
        /* If same class type, but t2n is const, then it's
         * covariant. Do this test first because it can work on
         * forward references.
         */
        if (((TypeClass *)t1n)->sym == ((TypeClass *)t2n)->sym &&
            MODimplicitConv(t1n->mod, t2n->mod))
            goto Lcovariant;

        // If t1n is forward referenced:
        ClassDeclaration *cd = ((TypeClass *)t1n)->sym;
        if (cd->semanticRun < PASSsemanticdone && !cd->isBaseInfoComplete())
            dsymbolSemantic(cd, NULL);
        if (!cd->isBaseInfoComplete())
        {
            return 3;   // forward references
        }
    }
    if (t1n->ty == Tstruct && t2n->ty == Tstruct)
    {
        if (((TypeStruct *)t1n)->sym == ((TypeStruct *)t2n)->sym &&
            MODimplicitConv(t1n->mod, t2n->mod))
            goto Lcovariant;
    }
    else if (t1n->ty == t2n->ty && t1n->implicitConvTo(t2n))
        goto Lcovariant;
    else if (t1n->ty == Tnull)
    {
        // NULL is covariant with any pointer type, but not with any
        // dynamic arrays, associative arrays or delegates.
        // https://issues.dlang.org/show_bug.cgi?id=8589
        // https://issues.dlang.org/show_bug.cgi?id=19618
        Type *t2bn = t2n->toBasetype();
        if (t2bn->ty == Tnull || t2bn->ty == Tpointer || t2bn->ty == Tclass)
            goto Lcovariant;
    }
  }
    goto Lnotcovariant;

Lcovariant:
    if (t1->isref != t2->isref)
        goto Lnotcovariant;

    if (!t1->isref && (t1->isscope || t2->isscope))
    {
        StorageClass stc1 = t1->isscope ? STCscope : 0;
        StorageClass stc2 = t2->isscope ? STCscope : 0;
        if (t1->isreturn)
        {
            stc1 |= STCreturn;
            if (!t1->isscope)
                stc1 |= STCref;
        }
        if (t2->isreturn)
        {
            stc2 |= STCreturn;
            if (!t2->isscope)
                stc2 |= STCref;
        }
        if (!Parameter::isCovariantScope(t1->isref, stc1, stc2))
            goto Lnotcovariant;
    }

    // We can subtract 'return ref' from 'this', but cannot add it
    else if (t1->isreturn && !t2->isreturn)
        goto Lnotcovariant;

    /* Can convert mutable to const
     */
    if (!MODimplicitConv(t2->mod, t1->mod))
    {
        goto Ldistinct;
    }

    /* Can convert pure to impure, nothrow to throw, and nogc to gc
     */
    if (!t1->purity && t2->purity)
        stc |= STCpure;

    if (!t1->isnothrow && t2->isnothrow)
        stc |= STCnothrow;

    if (!t1->isnogc && t2->isnogc)
        stc |= STCnogc;

    /* Can convert safe/trusted to system
     */
    if (t1->trust <= TRUSTsystem && t2->trust >= TRUSTtrusted)
    {
        // Should we infer trusted or safe? Go with safe.
        stc |= STCsafe;
    }

    if (stc)
    {   if (pstc)
            *pstc = stc;
        goto Lnotcovariant;
    }

    //printf("\tcovaraint: 1\n");
    return 1;

Ldistinct:
    //printf("\tcovaraint: 0\n");
    return 0;

Lnotcovariant:
    //printf("\tcovaraint: 2\n");
    return 2;
}

bool TypeFunction::checkRetType(Loc loc)
{
    Type *tb = next->toBasetype();
    if (tb->ty == Tfunction)
    {
        error(loc, "functions cannot return a function");
        next = Type::terror;
    }
    if (tb->ty == Ttuple)
    {
        error(loc, "functions cannot return a tuple");
        next = Type::terror;
    }
    if (!isref && (tb->ty == Tstruct || tb->ty == Tsarray))
    {
        Type *tb2 = tb->baseElemOf();
        if (tb2->ty == Tstruct && !((TypeStruct *)tb2)->sym->members)
        {
            error(loc, "functions cannot return opaque type %s by value", tb->toChars());
            next = Type::terror;
        }
    }
    if (tb->ty == Terror)
        return true;

    return false;
}

/* Determine purity level based on mutability of t
 * and whether it is a 'ref' type or not.
 */
static PURE purityOfType(bool isref, Type *t)
{
    if (isref)
    {
        if (t->mod & MODimmutable)
            return PUREstrong;
        if (t->mod & (MODconst | MODwild))
            return PUREconst;
        return PUREweak;
    }

    t = t->baseElemOf();

    if (!t->hasPointers() || t->mod & MODimmutable)
        return PUREstrong;

    /* Accept immutable(T)[] and immutable(T)* as being strongly pure
     */
    if (t->ty == Tarray || t->ty == Tpointer)
    {
        Type *tn = t->nextOf()->toBasetype();
        if (tn->mod & MODimmutable)
            return PUREstrong;
        if (tn->mod & (MODconst | MODwild))
            return PUREconst;
    }

    /* The rest of this is too strict; fix later.
     * For example, the only pointer members of a struct may be immutable,
     * which would maintain strong purity.
     * (Just like for dynamic arrays and pointers above.)
     */
    if (t->mod & (MODconst | MODwild))
        return PUREconst;

    /* Should catch delegates and function pointers, and fold in their purity
     */
    return PUREweak;
}

/********************************************
 * Set 'purity' field of 'this'.
 * Do this lazily, as the parameter types might be forward referenced.
 */
void TypeFunction::purityLevel()
{
    TypeFunction *tf = this;
    if (tf->purity != PUREfwdref)
        return;

    purity = PUREstrong; // assume strong until something weakens it

    /* Evaluate what kind of purity based on the modifiers for the parameters
     */
    const size_t dim = tf->parameterList.length();
    for (size_t i = 0; i < dim; i++)
    {
        Parameter *fparam = tf->parameterList[i];
        Type *t = fparam->type;
        if (!t)
            continue;

        if (fparam->storageClass & (STClazy | STCout))
        {
            purity = PUREweak;
            break;
        }
        switch (purityOfType((fparam->storageClass & STCref) != 0, t))
        {
            case PUREweak:
                purity = PUREweak;
                break;

            case PUREconst:
                purity = PUREconst;
                continue;

            case PUREstrong:
                continue;

            default:
                assert(0);
        }
        break;              // since PUREweak, no need to check further
    }

    if (purity > PUREweak && tf->nextOf())
    {
        /* Adjust purity based on mutability of return type.
         * https://issues.dlang.org/show_bug.cgi?id=15862
         */
        const PURE purity2 = purityOfType(tf->isref, tf->nextOf());
        if (purity2 < purity)
            purity = purity2;
    }
    tf->purity = purity;
}

// arguments get specially formatted
static const char *getParamError(TypeFunction *tf, Expression *arg, Parameter *par)
{
    if (global.gag && !global.params.showGaggedErrors)
        return NULL;
    // show qualification when toChars() is the same but types are different
    const char *at = arg->type->toChars();
    bool qual = !arg->type->equals(par->type) && strcmp(at, par->type->toChars()) == 0;
    if (qual)
        at = arg->type->toPrettyChars(true);
    OutBuffer buf;
    // only mention rvalue if it's relevant
    const bool rv = !arg->isLvalue() && (par->storageClass & (STCref | STCout)) != 0;
    buf.printf("cannot pass %sargument `%s` of type `%s` to parameter `%s`",
        rv ? "rvalue " : "", arg->toChars(), at,
        parameterToChars(par, tf, qual));
    return buf.extractChars();
}

static const char *getMatchError(const char *format, ...)
{
    if (global.gag && !global.params.showGaggedErrors)
        return NULL;
    OutBuffer buf;
    va_list ap;
    va_start(ap, format);
    buf.vprintf(format, ap);
    va_end(ap);
    return buf.extractChars();
}

/********************************
 * 'args' are being matched to function 'this'
 * Determine match level.
 * Input:
 *      flag    1       performing a partial ordering match
 *      pMessage        address to store error message, or null
 * Returns:
 *      MATCHxxxx
 */

MATCH TypeFunction::callMatch(Type *tthis, Expressions *args, int flag, const char **pMessage)
{
    //printf("TypeFunction::callMatch() %s\n", toChars());
    MATCH match = MATCHexact;           // assume exact match
    unsigned char wildmatch = 0;

    if (tthis)
    {
        Type *t = tthis;
        if (t->toBasetype()->ty == Tpointer)
            t = t->toBasetype()->nextOf();      // change struct* to struct
        if (t->mod != mod)
        {
            if (MODimplicitConv(t->mod, mod))
                match = MATCHconst;
            else if ((mod & MODwild) && MODimplicitConv(t->mod, (mod & ~MODwild) | MODconst))
            {
                match = MATCHconst;
            }
            else
                return MATCHnomatch;
        }
        if (isWild())
        {
            if (t->isWild())
                wildmatch |= MODwild;
            else if (t->isConst())
                wildmatch |= MODconst;
            else if (t->isImmutable())
                wildmatch |= MODimmutable;
            else
                wildmatch |= MODmutable;
        }
    }

    size_t nparams = parameterList.length();
    size_t nargs = args ? args->length : 0;
    if (nargs > nparams)
    {
        if (parameterList.varargs == VARARGnone)
        {
            // suppress early exit if an error message is wanted,
            // so we can check any matching args are valid
            if (!pMessage)
                goto Nomatch;           // too many args; no match
        }
        match = MATCHconvert;           // match ... with a "conversion" match level
    }

    for (size_t u = 0; u < nargs; u++)
    {
        if (u >= nparams)
            break;
        Parameter *p = parameterList[u];
        Expression *arg = (*args)[u];
        assert(arg);
        Type *tprm = p->type;
        Type *targ = arg->type;

        if (!(p->storageClass & STClazy && tprm->ty == Tvoid && targ->ty != Tvoid))
        {
            bool isRef = (p->storageClass & (STCref | STCout)) != 0;
            wildmatch |= targ->deduceWild(tprm, isRef);
        }
    }
    if (wildmatch)
    {
        /* Calculate wild matching modifier
         */
        if (wildmatch & MODconst || wildmatch & (wildmatch - 1))
            wildmatch = MODconst;
        else if (wildmatch & MODimmutable)
            wildmatch = MODimmutable;
        else if (wildmatch & MODwild)
            wildmatch = MODwild;
        else
        {
            assert(wildmatch & MODmutable);
            wildmatch = MODmutable;
        }
    }

    for (size_t u = 0; u < nparams; u++)
    {
        MATCH m;

        Parameter *p = parameterList[u];
        assert(p);
        if (u >= nargs)
        {
            if (p->defaultArg)
                continue;
            goto L1;        // try typesafe variadics
        }
        {
            Expression *arg = (*args)[u];
            assert(arg);
            //printf("arg: %s, type: %s\n", arg->toChars(), arg->type->toChars());

            Type *targ = arg->type;
            Type *tprm = wildmatch ? p->type->substWildTo(wildmatch) : p->type;

            if (p->storageClass & STClazy && tprm->ty == Tvoid && targ->ty != Tvoid)
                m = MATCHconvert;
            else
            {
                //printf("%s of type %s implicitConvTo %s\n", arg->toChars(), targ->toChars(), tprm->toChars());
                if (flag)
                {
                    // for partial ordering, value is an irrelevant mockup, just look at the type
                    m = targ->implicitConvTo(tprm);
                }
                else
                    m = arg->implicitConvTo(tprm);
                //printf("match %d\n", m);
            }

            // Non-lvalues do not match ref or out parameters
            if (p->storageClass & (STCref | STCout))
            {
                // Bugzilla 13783: Don't use toBasetype() to handle enum types.
                Type *ta = targ;
                Type *tp = tprm;
                //printf("fparam[%d] ta = %s, tp = %s\n", u, ta->toChars(), tp->toChars());

                if (m && !arg->isLvalue())
                {
                    if (p->storageClass & STCout)
                    {
                        if (pMessage) *pMessage = getParamError(this, arg, p);
                        goto Nomatch;
                    }

                    if (arg->op == TOKstring && tp->ty == Tsarray)
                    {
                        if (ta->ty != Tsarray)
                        {
                            Type *tn = tp->nextOf()->castMod(ta->nextOf()->mod);
                            dinteger_t dim = ((StringExp *)arg)->len;
                            ta = tn->sarrayOf(dim);
                        }
                    }
                    else if (arg->op == TOKslice && tp->ty == Tsarray)
                    {
                        // Allow conversion from T[lwr .. upr] to ref T[upr-lwr]
                        if (ta->ty != Tsarray)
                        {
                            Type *tn = ta->nextOf();
                            dinteger_t dim = ((TypeSArray *)tp)->dim->toUInteger();
                            ta = tn->sarrayOf(dim);
                        }
                    }
                    else
                    {
                        if (pMessage) *pMessage = getParamError(this, arg, p);
                        goto Nomatch;
                    }
                }

                /* Find most derived alias this type being matched.
                 * Bugzilla 15674: Allow on both ref and out parameters.
                 */
                while (1)
                {
                    Type *tat = ta->toBasetype()->aliasthisOf();
                    if (!tat || !tat->implicitConvTo(tprm))
                        break;
                    ta = tat;
                }

                /* A ref variable should work like a head-const reference.
                 * e.g. disallows:
                 *  ref T      <- an lvalue of const(T) argument
                 *  ref T[dim] <- an lvalue of const(T[dim]) argument
                 */
                if (!ta->constConv(tp))
                {
                    if (pMessage) *pMessage = getParamError(this, arg, p);
                    goto Nomatch;
                }
            }
        }

        /* prefer matching the element type rather than the array
         * type when more arguments are present with T[]...
         */
        if (parameterList.varargs == VARARGtypesafe && u + 1 == nparams && nargs > nparams)
            goto L1;

        //printf("\tm = %d\n", m);
        if (m == MATCHnomatch)                  // if no match
        {
          L1:
            if (parameterList.varargs == VARARGtypesafe && u + 1 == nparams)       // if last varargs param
            {
                Type *tb = p->type->toBasetype();
                TypeSArray *tsa;
                dinteger_t sz;

                switch (tb->ty)
                {
                case Tsarray:
                    tsa = (TypeSArray *)tb;
                    sz = tsa->dim->toInteger();
                    if (sz != nargs - u)
                    {
                        if (pMessage)
                            *pMessage = getMatchError("expected %llu variadic argument(s), not %zu", sz, nargs - u);
                        goto Nomatch;
                    }
                    /* fall through */
                case Tarray:
                    {
                        TypeArray *ta = (TypeArray *)tb;
                        for (; u < nargs; u++)
                        {
                            Expression *arg = (*args)[u];
                            assert(arg);

                            /* If lazy array of delegates,
                             * convert arg(s) to delegate(s)
                             */
                            Type *tret = p->isLazyArray();
                            if (tret)
                            {
                                if (ta->next->equals(arg->type))
                                    m = MATCHexact;
                                else if (tret->toBasetype()->ty == Tvoid)
                                    m = MATCHconvert;
                                else
                                {
                                    m = arg->implicitConvTo(tret);
                                    if (m == MATCHnomatch)
                                        m = arg->implicitConvTo(ta->next);
                                }
                            }
                            else
                                m = arg->implicitConvTo(ta->next);

                            if (m == MATCHnomatch)
                            {
                                if (pMessage) *pMessage = getParamError(this, arg, p);
                                goto Nomatch;
                            }
                            if (m < match)
                                match = m;
                        }
                        goto Ldone;
                    }
                case Tclass:
                    // Should see if there's a constructor match?
                    // Or just leave it ambiguous?
                    goto Ldone;

                default:
                    break;
                }
            }
            if (pMessage && u < nargs)
                *pMessage = getParamError(this, (*args)[u], p);
            else if (pMessage)
                *pMessage = getMatchError("missing argument for parameter #%d: `%s`",
                    u + 1, parameterToChars(p, this, false));
            goto Nomatch;
        }
        if (m < match)
            match = m;                  // pick worst match
    }

Ldone:
    if (pMessage && !parameterList.varargs && nargs > nparams)
    {
        // all parameters had a match, but there are surplus args
        *pMessage = getMatchError("expected %d argument(s), not %d", nparams, nargs);
        goto Nomatch;
    }
    //printf("match = %d\n", match);
    return match;

Nomatch:
    //printf("no match\n");
    return MATCHnomatch;
}

/********************************************
 * Return true if there are lazy parameters.
 */
bool TypeFunction::hasLazyParameters()
{
    size_t dim = parameterList.length();
    for (size_t i = 0; i < dim; i++)
    {
        Parameter *fparam = parameterList[i];
        if (fparam->storageClass & STClazy)
            return true;
    }
    return false;
}

/*******************************
 * Check for `extern (D) U func(T t, ...)` variadic function type,
 * which has `_arguments[]` added as the first argument.
 * Returns:
 *  true if D-style variadic
 */
bool TypeFunction::isDstyleVariadic() const
{
    return linkage == LINKd && parameterList.varargs == VARARGvariadic;
}

/***************************
 * Examine function signature for parameter p and see if
 * the value of p can 'escape' the scope of the function.
 * This is useful to minimize the needed annotations for the parameters.
 * Params:
 *  p = parameter to this function
 * Returns:
 *  true if escapes via assignment to global or through a parameter
 */

bool TypeFunction::parameterEscapes(Parameter *p)
{
    /* Scope parameters do not escape.
     * Allow 'lazy' to imply 'scope' -
     * lazy parameters can be passed along
     * as lazy parameters to the next function, but that isn't
     * escaping.
     */
    if (parameterStorageClass(p) & (STCscope | STClazy))
        return false;
    return true;
}

/************************************
 * Take the specified storage class for p,
 * and use the function signature to infer whether
 * STCscope and STCreturn should be OR'd in.
 * (This will not affect the name mangling.)
 * Params:
 *  p = one of the parameters to 'this'
 * Returns:
 *  storage class with STCscope or STCreturn OR'd in
 */
StorageClass TypeFunction::parameterStorageClass(Parameter *p)
{
    StorageClass stc = p->storageClass;
    if (!global.params.vsafe)
        return stc;

    if (stc & (STCscope | STCreturn | STClazy) || purity == PUREimpure)
        return stc;

    /* If haven't inferred the return type yet, can't infer storage classes
     */
    if (!nextOf())
        return stc;

    purityLevel();

    // See if p can escape via any of the other parameters
    if (purity == PUREweak)
    {
        const size_t dim = parameterList.length();
        for (size_t i = 0; i < dim; i++)
        {
            Parameter *fparam = parameterList[i];
            Type *t = fparam->type;
            if (!t)
                continue;
            t = t->baseElemOf();
            if (t->isMutable() && t->hasPointers())
            {
                if (fparam->storageClass & (STCref | STCout))
                {
                }
                else if (t->ty == Tarray || t->ty == Tpointer)
                {
                    Type *tn = t->nextOf()->toBasetype();
                    if (!(tn->isMutable() && tn->hasPointers()))
                        continue;
                }
                return stc;
            }
        }
    }

    stc |= STCscope;

    /* Inferring STCreturn here has false positives
     * for pure functions, producing spurious error messages
     * about escaping references.
     * Give up on it for now.
     */
    return stc;
}

Expression *TypeFunction::defaultInit(Loc loc)
{
    error(loc, "function does not have a default initializer");
    return new ErrorExp();
}

Type *TypeFunction::addStorageClass(StorageClass stc)
{
    //printf("addStorageClass(%llx) %d\n", stc, (stc & STCscope) != 0);
    TypeFunction *t = Type::addStorageClass(stc)->toTypeFunction();
    if ((stc & STCpure && !t->purity) ||
        (stc & STCnothrow && !t->isnothrow) ||
        (stc & STCnogc && !t->isnogc) ||
        (stc & STCscope && !t->isscope) ||
        (stc & STCsafe && t->trust < TRUSTtrusted))
    {
        // Klunky to change these
        TypeFunction *tf = new TypeFunction(t->parameterList, t->next, t->linkage, 0);
        tf->mod = t->mod;
        tf->fargs = fargs;
        tf->purity = t->purity;
        tf->isnothrow = t->isnothrow;
        tf->isnogc = t->isnogc;
        tf->isproperty = t->isproperty;
        tf->isref = t->isref;
        tf->isreturn = t->isreturn;
        tf->isscope = t->isscope;
        tf->isscopeinferred = t->isscopeinferred;
        tf->trust = t->trust;
        tf->iswild = t->iswild;

        if (stc & STCpure)
            tf->purity = PUREfwdref;
        if (stc & STCnothrow)
            tf->isnothrow = true;
        if (stc & STCnogc)
            tf->isnogc = true;
        if (stc & STCsafe)
            tf->trust = TRUSTsafe;
        if (stc & STCscope)
        {
            tf->isscope = true;
            if (stc & STCscopeinferred)
                tf->isscopeinferred = true;
        }

        tf->deco = tf->merge()->deco;
        t = tf;
    }
    return t;
}

/** For each active attribute (ref/const/nogc/etc) call fp with a void* for the
work param and a string representation of the attribute. */
int TypeFunction::attributesApply(void *param, int (*fp)(void *, const char *), TRUSTformat trustFormat)
{
    int res = 0;

    if (purity) res = fp(param, "pure");
    if (res) return res;

    if (isnothrow) res = fp(param, "nothrow");
    if (res) return res;

    if (isnogc) res = fp(param, "@nogc");
    if (res) return res;

    if (isproperty) res = fp(param, "@property");
    if (res) return res;

    if (isref) res = fp(param, "ref");
    if (res) return res;

    if (isreturn) res = fp(param, "return");
    if (res) return res;

    if (isscope && !isscopeinferred) res = fp(param, "scope");
    if (res) return res;

    TRUST trustAttrib = trust;

    if (trustAttrib == TRUSTdefault)
    {
        // Print out "@system" when trust equals TRUSTdefault (if desired).
        if (trustFormat == TRUSTformatSystem)
            trustAttrib = TRUSTsystem;
        else
            return res;  // avoid calling with an empty string
    }

    return fp(param, trustToChars(trustAttrib));
}

/***************************** TypeDelegate *****************************/

TypeDelegate::TypeDelegate(Type *t)
    : TypeNext(Tfunction, t)
{
    ty = Tdelegate;
}

TypeDelegate *TypeDelegate::create(Type *t)
{
    return new TypeDelegate(t);
}

const char *TypeDelegate::kind()
{
    return "delegate";
}

Type *TypeDelegate::syntaxCopy()
{
    Type *t = next->syntaxCopy();
    if (t == next)
        t = this;
    else
    {
        t = new TypeDelegate(t);
        t->mod = mod;
    }
    return t;
}

Type *TypeDelegate::addStorageClass(StorageClass stc)
{
    TypeDelegate *t = (TypeDelegate*)Type::addStorageClass(stc);
    if (!global.params.vsafe)
        return t;

    /* The rest is meant to add 'scope' to a delegate declaration if it is of the form:
     *  alias dg_t = void* delegate();
     *  scope dg_t dg = ...;
     */
    if (stc & STCscope)
    {
        Type *n = t->next->addStorageClass(STCscope | STCscopeinferred);
        if (n != t->next)
        {
            t->next = n;
            t->deco = t->merge()->deco; // mangling supposed to not be changed due to STCscopeinferrred
        }
    }
    return t;
}

d_uns64 TypeDelegate::size(Loc)
{
    return target.ptrsize * 2;
}

unsigned TypeDelegate::alignsize()
{
    return target.ptrsize;
}

MATCH TypeDelegate::implicitConvTo(Type *to)
{
    //printf("TypeDelegate::implicitConvTo(this=%p, to=%p)\n", this, to);
    //printf("from: %s\n", toChars());
    //printf("to  : %s\n", to->toChars());
    if (this == to)
        return MATCHexact;
#if 1 // not allowing covariant conversions because it interferes with overriding
    if (to->ty == Tdelegate && this->nextOf()->covariant(to->nextOf()) == 1)
    {
        Type *tret = this->next->nextOf();
        Type *toret = ((TypeDelegate *)to)->next->nextOf();
        if (tret->ty == Tclass && toret->ty == Tclass)
        {
            /* Bugzilla 10219: Check covariant interface return with offset tweaking.
             * interface I {}
             * class C : Object, I {}
             * I delegate() dg = delegate C() {}    // should be error
             */
            int offset = 0;
            if (toret->isBaseOf(tret, &offset) && offset != 0)
                return MATCHnomatch;
        }
        return MATCHconvert;
    }
#endif
    return MATCHnomatch;
}

Expression *TypeDelegate::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypeDelegate::isZeroInit(Loc)
{
    return true;
}

bool TypeDelegate::isBoolean()
{
    return true;
}

Expression *TypeDelegate::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    if (ident == Id::ptr)
    {
        e = new DelegatePtrExp(e->loc, e);
        e = expressionSemantic(e, sc);
    }
    else if (ident == Id::funcptr)
    {
        if (!(flag & 2) && sc->func && !sc->intypeof && sc->func->setUnsafe())
        {
            e->error("%s.funcptr cannot be used in @safe code", e->toChars());
            return new ErrorExp();
        }
        e = new DelegateFuncptrExp(e->loc, e);
        e = expressionSemantic(e, sc);
    }
    else
    {
        e = Type::dotExp(sc, e, ident, flag);
    }
    return e;
}

bool TypeDelegate::hasPointers()
{
    return true;
}

/***************************** TypeTraits ********************************/

TypeTraits::TypeTraits(const Loc &loc, TraitsExp *exp)
     : Type(Ttraits)
{
    this->loc = loc;
    this->exp = exp;
    this->sym = NULL;
}

Type *TypeTraits::syntaxCopy()
{
    TraitsExp *te = (TraitsExp *) exp->syntaxCopy();
    TypeTraits *tt = new TypeTraits(loc, te);
    tt->mod = mod;
    return tt;
}

Dsymbol *TypeTraits::toDsymbol(Scope *sc)
{
    Type *t = NULL;
    Expression *e = NULL;
    Dsymbol *s = NULL;
    resolve(loc, sc, &e, &t, &s);
    if (t && t->ty != Terror)
        s = t->toDsymbol(sc);
    else if (e)
        s = getDsymbol(e);

    return s;
}

void TypeTraits::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool)
{
    *pt = NULL;
    *pe = NULL;
    *ps = NULL;

    if (Type *t = typeSemantic(this, loc, sc))
        *pt = t;
    else if (sym)
        *ps = sym;
    else
        *pt = Type::terror;
}

d_uns64 TypeTraits::size(Loc)
{
    return SIZE_INVALID;
}

/***************************** TypeMixin *****************************/

/******
 * Implements mixin types.
 *
 * Semantic analysis will convert it to a real type.
 */
TypeMixin::TypeMixin(const Loc &loc, Expressions *exps)
    : Type(Tmixin)
{
    this->loc = loc;
    this->exps = exps;
    this->obj = NULL; // cached result of semantic analysis.
}

const char *TypeMixin::kind()
{
    return "mixin";
}

Type *TypeMixin::syntaxCopy()
{
    return new TypeMixin(loc, Expression::arraySyntaxCopy(exps));
}

Dsymbol *TypeMixin::toDsymbol(Scope *sc)
{
    Type *t = NULL;
    Expression *e = NULL;
    Dsymbol *s = NULL;
    resolve(loc, sc, &e, &t, &s);
    if (t)
        s = t->toDsymbol(sc);
    else if (e)
        s = getDsymbol(e);

    return s;
}

void TypeMixin::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    // if already resolved just set pe/pt/ps and return.
    if (obj)
    {
        *pe = isExpression(obj);
        *pt = isType(obj);
        *ps = isDsymbol(obj);
        return;
    }

    RootObject *o = compileTypeMixin(this, loc, sc);
    if (Type *t = isType(o))
    {
        t->resolve(loc, sc, pe, pt, ps, intypeid);
        if (*pt)
            (*pt) = (*pt)->addMod(mod);
    }
    else if (Expression *e = isExpression(o))
    {
        e = expressionSemantic(e, sc);
        if (TypeExp *et = e->isTypeExp())
        {
            *pe = NULL;
            *pt = et->type->addMod(mod);
            *ps = NULL;
        }
        else
        {
            *pe = e;
            *pt = NULL;
            *ps = NULL;
        }
    }
    else
    {
        *pe = NULL;
        *pt = Type::terror;
        *ps = NULL;
    }

    // save the result
    obj = *pe ? (RootObject *)*pe : (*pt ? (RootObject *)*pt : (RootObject *)*ps);
}

/***************************** TypeQualified *****************************/

TypeQualified::TypeQualified(TY ty, Loc loc)
    : Type(ty)
{
    this->loc = loc;
}

void TypeQualified::syntaxCopyHelper(TypeQualified *t)
{
    //printf("TypeQualified::syntaxCopyHelper(%s) %s\n", t->toChars(), toChars());
    idents.setDim(t->idents.length);
    for (size_t i = 0; i < idents.length; i++)
    {
        RootObject *id = t->idents[i];
        if (id->dyncast() == DYNCAST_DSYMBOL)
        {
            TemplateInstance *ti = (TemplateInstance *)id;

            ti = (TemplateInstance *)ti->syntaxCopy(NULL);
            id = ti;
        }
        else if (id->dyncast() == DYNCAST_EXPRESSION)
        {
            Expression *e = (Expression *)id;
            e = e->syntaxCopy();
            id = e;
        }
        else if (id->dyncast() == DYNCAST_TYPE)
        {
            Type *tx = (Type *)id;
            tx = tx->syntaxCopy();
            id = tx;
        }
        idents[i] = id;
    }
}

void TypeQualified::addIdent(Identifier *ident)
{
    idents.push(ident);
}

void TypeQualified::addInst(TemplateInstance *inst)
{
    idents.push(inst);
}

void TypeQualified::addIndex(RootObject *e)
{
    idents.push(e);
}

d_uns64 TypeQualified::size(Loc)
{
    error(this->loc, "size of type %s is not known", toChars());
    return SIZE_INVALID;
}

/*************************************
 * Resolve a tuple index.
 */
void TypeQualified::resolveTupleIndex(Loc loc, Scope *sc, Dsymbol *s,
        Expression **pe, Type **pt, Dsymbol **ps, RootObject *oindex)
{
    *pt = NULL;
    *ps = NULL;
    *pe = NULL;

    TupleDeclaration *td = s->isTupleDeclaration();

    Expression *eindex = isExpression(oindex);
    Type *tindex = isType(oindex);
    Dsymbol *sindex = isDsymbol(oindex);

    if (!td)
    {
        // It's really an index expression
        if (tindex)
            eindex = new TypeExp(loc, tindex);
        else if (sindex)
            eindex = ::resolve(loc, sc, sindex, false);
        Expression *e = new IndexExp(loc, ::resolve(loc, sc, s, false), eindex);
        e = expressionSemantic(e, sc);
        resolveExp(e, pt, pe, ps);
        return;
    }

    // Convert oindex to Expression, then try to resolve to constant.
    if (tindex)
        tindex->resolve(loc, sc, &eindex, &tindex, &sindex);
    if (sindex)
        eindex = ::resolve(loc, sc, sindex, false);
    if (!eindex)
    {
        ::error(loc, "index is %s not an expression", oindex->toChars());
        *pt = Type::terror;
        return;
    }
    sc = sc->startCTFE();
    eindex = expressionSemantic(eindex, sc);
    sc = sc->endCTFE();

    eindex = eindex->ctfeInterpret();
    if (eindex->op == TOKerror)
    {
        *pt = Type::terror;
        return;
    }

    const uinteger_t d = eindex->toUInteger();
    if (d >= td->objects->length)
    {
        ::error(loc, "tuple index %llu exceeds length %u", (ulonglong)d, (unsigned)td->objects->length);
        *pt = Type::terror;
        return;
    }

    RootObject *o = (*td->objects)[(size_t)d];
    *pt = isType(o);
    *ps = isDsymbol(o);
    *pe = isExpression(o);

    if (*pt)
        *pt = typeSemantic(*pt, loc, sc);
    if (*pe)
        resolveExp(*pe, pt, pe, ps);
}

/*************************************
 * Takes an array of Identifiers and figures out if
 * it represents a Type or an Expression.
 * Output:
 *      if expression, *pe is set
 *      if type, *pt is set
 */
void TypeQualified::resolveHelper(Loc loc, Scope *sc,
        Dsymbol *s, Dsymbol *,
        Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    *pe = NULL;
    *pt = NULL;
    *ps = NULL;
    if (s)
    {
        //printf("\t1: s = '%s' %p, kind = '%s'\n",s->toChars(), s, s->kind());
        Declaration *d = s->isDeclaration();
        if (d && (d->storage_class & STCtemplateparameter))
            s = s->toAlias();
        else
        {
            // check for deprecated aliases
            s->checkDeprecated(loc, sc);
            if (d)
                d->checkDisabled(loc, sc, true);
        }

        s = s->toAlias();
        //printf("\t2: s = '%s' %p, kind = '%s'\n",s->toChars(), s, s->kind());
        for (size_t i = 0; i < idents.length; i++)
        {
            RootObject *id = idents[i];

            if (id->dyncast() == DYNCAST_EXPRESSION ||
                id->dyncast() == DYNCAST_TYPE)
            {
                Type *tx;
                Expression *ex;
                Dsymbol *sx;
                resolveTupleIndex(loc, sc, s, &ex, &tx, &sx, id);
                if (sx)
                {
                    s = sx->toAlias();
                    continue;
                }
                if (tx)
                    ex = new TypeExp(loc, tx);
                assert(ex);

                ex = typeToExpressionHelper(this, ex, i + 1);
                ex = expressionSemantic(ex, sc);
                resolveExp(ex, pt, pe, ps);
                return;
            }

            Type *t = s->getType();     // type symbol, type alias, or type tuple?
            unsigned errorsave = global.errors;
            int flags = t == NULL ? SearchLocalsOnly : IgnorePrivateImports;
            Dsymbol *sm = s->searchX(loc, sc, id, flags);
            if (sm)
            {
                if (!(sc->flags & SCOPEignoresymbolvisibility) && !symbolIsVisible(sc, sm))
                {
                    ::error(loc, "`%s` is not visible from module `%s`", sm->toPrettyChars(), sc->_module->toChars());
                    sm = NULL;
                }
                // Same check as in Expression::semanticY(DotIdExp)
                else if (sm->isPackage() && checkAccess(sc, (Package *)sm))
                {
                    // @@@DEPRECATED_2.096@@@
                    // Should be an error in 2.106. Just remove the deprecation call
                    // and uncomment the null assignment
                    ::deprecation(loc, "%s %s is not accessible here, perhaps add 'static import %s;'",
                         sm->kind(), sm->toPrettyChars(), sm->toPrettyChars());
                    //sm = null;
                }
            }
            if (global.errors != errorsave)
            {
                *pt = Type::terror;
                return;
            }
            //printf("\t3: s = %p %s %s, sm = %p\n", s, s->kind(), s->toChars(), sm);
            if (intypeid && !t && sm && sm->needThis())
                goto L3;
            if (VarDeclaration *v = s->isVarDeclaration())
            {
                // https://issues.dlang.org/show_bug.cgi?id=19913
                // v->type would be null if it is a forward referenced member.
                if (v->type == NULL)
                    dsymbolSemantic(v, sc);
                if (v->storage_class & (STCconst | STCimmutable | STCmanifest) ||
                    v->type->isConst() || v->type->isImmutable())
                {
                    // Bugzilla 13087: this.field is not constant always
                    if (!v->isThisDeclaration())
                        goto L3;
                }
            }
            if (!sm)
            {
                if (!t)
                {
                    if (s->isDeclaration())         // var, func, or tuple declaration?
                    {
                        t = s->isDeclaration()->type;
                        if (!t && s->isTupleDeclaration())  // expression tuple?
                            goto L3;
                    }
                    else if (s->isTemplateInstance() ||
                             s->isImport() || s->isPackage() || s->isModule())
                    {
                        goto L3;
                    }
                }
                if (t)
                {
                    sm = t->toDsymbol(sc);
                    if (sm && id->dyncast() == DYNCAST_IDENTIFIER)
                    {
                        sm = sm->search(loc, (Identifier *)id, IgnorePrivateImports);
                        if (sm)
                            goto L2;
                    }
                L3:
                    Expression *e;
                    VarDeclaration *v = s->isVarDeclaration();
                    FuncDeclaration *f = s->isFuncDeclaration();
                    if (intypeid || (!v && !f))
                        e = ::resolve(loc, sc, s, true);
                    else
                        e = new VarExp(loc, s->isDeclaration(), true);

                    e = typeToExpressionHelper(this, e, i);
                    e = expressionSemantic(e, sc);
                    resolveExp(e, pt, pe, ps);
                    return;
                }
                else
                {
                    if (id->dyncast() == DYNCAST_DSYMBOL)
                    {
                        // searchX already handles errors for template instances
                        assert(global.errors);
                    }
                    else
                    {
                        assert(id->dyncast() == DYNCAST_IDENTIFIER);
                        sm = s->search_correct((Identifier *)id);
                        if (sm)
                            error(loc, "identifier `%s` of `%s` is not defined, did you mean %s `%s`?",
                                  id->toChars(), toChars(), sm->kind(), sm->toChars());
                        else
                            error(loc, "identifier `%s` of `%s` is not defined", id->toChars(), toChars());
                    }
                    *pe = new ErrorExp();
                }
                return;
            }
        L2:
            s = sm->toAlias();
        }

        if (EnumMember *em = s->isEnumMember())
        {
            // It's not a type, it's an expression
            *pe = em->getVarExp(loc, sc);
            return;
        }
        if (VarDeclaration *v = s->isVarDeclaration())
        {
            /* This is mostly same with DsymbolExp::semantic(), but we cannot use it
             * because some variables used in type context need to prevent lowering
             * to a literal or contextful expression. For example:
             *
             *  enum a = 1; alias b = a;
             *  template X(alias e){ alias v = e; }  alias x = X!(1);
             *  struct S { int v; alias w = v; }
             *      // TypeIdentifier 'a', 'e', and 'v' should be TOKvar,
             *      // because getDsymbol() need to work in AliasDeclaration::semantic().
             */
            if (!v->type ||
                (!v->type->deco && v->inuse))
            {
                if (v->inuse)   // Bugzilla 9494
                    error(loc, "circular reference to %s `%s`", v->kind(), v->toPrettyChars());
                else
                    error(loc, "forward reference to %s `%s`", v->kind(), v->toPrettyChars());
                *pt = Type::terror;
                return;
            }
            if (v->type->ty == Terror)
                *pt = Type::terror;
            else
                *pe = new VarExp(loc, v);
            return;
        }
        if (FuncLiteralDeclaration *fld = s->isFuncLiteralDeclaration())
        {
            //printf("'%s' is a function literal\n", fld->toChars());
            *pe = new FuncExp(loc, fld);
            *pe = expressionSemantic(*pe, sc);
            return;
        }
L1:
        Type *t = s->getType();
        if (!t)
        {
            // If the symbol is an import, try looking inside the import
            if (Import *si = s->isImport())
            {
                s = si->search(loc, s->ident);
                if (s && s != si)
                    goto L1;
                s = si;
            }
            *ps = s;
            return;
        }
        if (t->ty == Tinstance && t != this && !t->deco)
        {
            if (!((TypeInstance *)t)->tempinst->errors)
                error(loc, "forward reference to `%s`", t->toChars());
            *pt = Type::terror;
            return;
        }

        if (t->ty == Ttuple)
            *pt = t;
        else
            *pt = t->merge();
    }
    if (!s)
    {
        /* Look for what user might have intended
         */
        const char *p = mutableOf()->unSharedOf()->toChars();
        Identifier *id = Identifier::idPool(p, strlen(p));
        if (const char *n = importHint(p))
            error(loc, "`%s` is not defined, perhaps `import %s;` ?", p, n);
        else if (Dsymbol *s2 = sc->search_correct(id))
            error(loc, "undefined identifier `%s`, did you mean %s `%s`?", p, s2->kind(), s2->toChars());
        else if (const char *q = Scope::search_correct_C(id))
            error(loc, "undefined identifier `%s`, did you mean `%s`?", p, q);
        else
            error(loc, "undefined identifier `%s`", p);

        *pt = Type::terror;
    }
}

/***************************** TypeIdentifier *****************************/

TypeIdentifier::TypeIdentifier(Loc loc, Identifier *ident)
    : TypeQualified(Tident, loc)
{
    this->ident = ident;
}

const char *TypeIdentifier::kind()
{
    return "identifier";
}

Type *TypeIdentifier::syntaxCopy()
{
    TypeIdentifier *t = new TypeIdentifier(loc, ident);
    t->syntaxCopyHelper(this);
    t->mod = mod;
    return t;
}

/*************************************
 * Takes an array of Identifiers and figures out if
 * it represents a Type or an Expression.
 * Output:
 *      if expression, *pe is set
 *      if type, *pt is set
 */

void TypeIdentifier::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    //printf("TypeIdentifier::resolve(sc = %p, idents = '%s')\n", sc, toChars());

    if ((ident->equals(Id::_super) || ident->equals(Id::This)) && !hasThis(sc))
    {
        AggregateDeclaration *ad = sc->getStructClassScope();
        if (ad)
        {
            ClassDeclaration *cd = ad->isClassDeclaration();
            if (cd)
            {
                if (ident->equals(Id::This))
                    ident = cd->ident;
                else if (cd->baseClass && ident->equals(Id::_super))
                    ident = cd->baseClass->ident;
            }
            else
            {
                StructDeclaration *sd = ad->isStructDeclaration();
                if (sd && ident->equals(Id::This))
                    ident = sd->ident;
            }
        }
    }
    if (ident == Id::ctfe)
    {
        error(loc, "variable __ctfe cannot be read at compile time");
        *pe = NULL;
        *ps = NULL;
        *pt = Type::terror;
        return;
    }

    Dsymbol *scopesym;
    Dsymbol *s = sc->search(loc, ident, &scopesym);
    resolveHelper(loc, sc, s, scopesym, pe, pt, ps, intypeid);
    if (*pt)
        (*pt) = (*pt)->addMod(mod);
}

/*****************************************
 * See if type resolves to a symbol, if so,
 * return that symbol.
 */

Dsymbol *TypeIdentifier::toDsymbol(Scope *sc)
{
    //printf("TypeIdentifier::toDsymbol('%s')\n", toChars());
    if (!sc)
        return NULL;

    Type *t;
    Expression *e;
    Dsymbol *s;

    resolve(loc, sc, &e, &t, &s);
    if (t && t->ty != Tident)
        s = t->toDsymbol(sc);
    if (e)
        s = getDsymbol(e);

    return s;
}

/***************************** TypeInstance *****************************/

TypeInstance::TypeInstance(Loc loc, TemplateInstance *tempinst)
    : TypeQualified(Tinstance, loc)
{
    this->tempinst = tempinst;
}

const char *TypeInstance::kind()
{
    return "instance";
}

Type *TypeInstance::syntaxCopy()
{
    //printf("TypeInstance::syntaxCopy() %s, %d\n", toChars(), idents.length);
    TypeInstance *t = new TypeInstance(loc, (TemplateInstance *)tempinst->syntaxCopy(NULL));
    t->syntaxCopyHelper(this);
    t->mod = mod;
    return t;
}

void TypeInstance::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    // Note close similarity to TypeIdentifier::resolve()
    *pe = NULL;
    *pt = NULL;
    *ps = NULL;
    //printf("TypeInstance::resolve(sc = %p, tempinst = '%s')\n", sc, tempinst->toChars());
    dsymbolSemantic(tempinst, sc);
    if (!global.gag && tempinst->errors)
    {
        *pt = terror;
        return;
    }

    resolveHelper(loc, sc, tempinst, NULL, pe, pt, ps, intypeid);
    if (*pt)
        *pt = (*pt)->addMod(mod);
    //if (*pt) printf("pt = '%s'\n", (*pt)->toChars());
}

Dsymbol *TypeInstance::toDsymbol(Scope *sc)
{
    Type *t;
    Expression *e;
    Dsymbol *s;

    //printf("TypeInstance::semantic(%s)\n", toChars());
    resolve(loc, sc, &e, &t, &s);
    if (t && t->ty != Tinstance)
        s = t->toDsymbol(sc);

    return s;
}


/***************************** TypeTypeof *****************************/

TypeTypeof::TypeTypeof(Loc loc, Expression *exp)
        : TypeQualified(Ttypeof, loc)
{
    this->exp = exp;
    inuse = 0;
}

const char *TypeTypeof::kind()
{
    return "typeof";
}

Type *TypeTypeof::syntaxCopy()
{
    //printf("TypeTypeof::syntaxCopy() %s\n", toChars());
    TypeTypeof *t = new TypeTypeof(loc, exp->syntaxCopy());
    t->syntaxCopyHelper(this);
    t->mod = mod;
    return t;
}

Dsymbol *TypeTypeof::toDsymbol(Scope *sc)
{
    //printf("TypeTypeof::toDsymbol('%s')\n", toChars());
    Expression *e;
    Type *t;
    Dsymbol *s;
    resolve(loc, sc, &e, &t, &s);

    return s;
}

void TypeTypeof::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    *pe = NULL;
    *pt = NULL;
    *ps = NULL;

    //printf("TypeTypeof::resolve(sc = %p, idents = '%s')\n", sc, toChars());
    //static int nest; if (++nest == 50) *(char*)0=0;
    if (sc == NULL)
    {
        *pt = Type::terror;
        error(loc, "Invalid scope.");
        return;
    }
    if (inuse)
    {
        inuse = 2;
        error(loc, "circular typeof definition");
    Lerr:
        *pt = Type::terror;
        inuse--;
        return;
    }
    inuse++;

    Type *t;
    {
        /* Currently we cannot evalute 'exp' in speculative context, because
         * the type implementation may leak to the final execution. Consider:
         *
         * struct S(T) {
         *   string toString() const { return "x"; }
         * }
         * void main() {
         *   alias X = typeof(S!int());
         *   assert(typeid(X).xtoString(null) == "x");
         * }
         */
        Scope *sc2 = sc->push();
        sc2->intypeof = 1;
        Expression *exp2 = expressionSemantic(exp, sc2);
        exp2 = resolvePropertiesOnly(sc2, exp2);
        sc2->pop();

        if (exp2->op == TOKerror)
        {
            if (!global.gag)
                exp = exp2;
            goto Lerr;
        }
        exp = exp2;

        if (exp->op == TOKtype ||
            exp->op == TOKscope)
        {
            if (exp->checkType())
                goto Lerr;

            /* Today, 'typeof(func)' returns void if func is a
             * function template (TemplateExp), or
             * template lambda (FuncExp).
             * It's actually used in Phobos as an idiom, to branch code for
             * template functions.
             */
        }
        if (FuncDeclaration *f = exp->op == TOKvar    ? ((   VarExp *)exp)->var->isFuncDeclaration()
                               : exp->op == TOKdotvar ? ((DotVarExp *)exp)->var->isFuncDeclaration() : NULL)
        {
            if (f->checkForwardRef(loc))
                goto Lerr;
        }
        if (FuncDeclaration *f = isFuncAddress(exp))
        {
            if (f->checkForwardRef(loc))
                goto Lerr;
        }

        t = exp->type;
        if (!t)
        {
            error(loc, "expression (%s) has no type", exp->toChars());
            goto Lerr;
        }
        if (t->ty == Ttypeof)
        {
            error(loc, "forward reference to %s", toChars());
            goto Lerr;
        }
    }
    if (idents.length == 0)
        *pt = t;
    else
    {
        if (Dsymbol *s = t->toDsymbol(sc))
            resolveHelper(loc, sc, s, NULL, pe, pt, ps, intypeid);
        else
        {
            Expression *e = typeToExpressionHelper(this, new TypeExp(loc, t));
            e = expressionSemantic(e, sc);
            resolveExp(e, pt, pe, ps);
        }
    }
    if (*pt)
        (*pt) = (*pt)->addMod(mod);
    inuse--;
    return;
}

d_uns64 TypeTypeof::size(Loc loc)
{
    if (exp->type)
        return exp->type->size(loc);
    else
        return TypeQualified::size(loc);
}



/***************************** TypeReturn *****************************/

TypeReturn::TypeReturn(Loc loc)
        : TypeQualified(Treturn, loc)
{
}

const char *TypeReturn::kind()
{
    return "return";
}

Type *TypeReturn::syntaxCopy()
{
    TypeReturn *t = new TypeReturn(loc);
    t->syntaxCopyHelper(this);
    t->mod = mod;
    return t;
}

Dsymbol *TypeReturn::toDsymbol(Scope *sc)
{
    Expression *e;
    Type *t;
    Dsymbol *s;
    resolve(loc, sc, &e, &t, &s);

    return s;
}

void TypeReturn::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    *pe = NULL;
    *pt = NULL;
    *ps = NULL;

    //printf("TypeReturn::resolve(sc = %p, idents = '%s')\n", sc, toChars());
    Type *t;
    {
        FuncDeclaration *func = sc->func;
        if (!func)
        {
            error(loc, "typeof(return) must be inside function");
            goto Lerr;
        }
        if (func->fes)
            func = func->fes->func;

        t = func->type->nextOf();
        if (!t)
        {
            error(loc, "cannot use typeof(return) inside function %s with inferred return type", sc->func->toChars());
            goto Lerr;
        }
    }
    if (idents.length == 0)
        *pt = t;
    else
    {
        if (Dsymbol *s = t->toDsymbol(sc))
            resolveHelper(loc, sc, s, NULL, pe, pt, ps, intypeid);
        else
        {
            Expression *e = typeToExpressionHelper(this, new TypeExp(loc, t));
            e = expressionSemantic(e, sc);
            resolveExp(e, pt, pe, ps);
        }
    }
    if (*pt)
        (*pt) = (*pt)->addMod(mod);
    return;

Lerr:
    *pt = Type::terror;
    return;
}

/***************************** TypeEnum *****************************/

TypeEnum::TypeEnum(EnumDeclaration *sym)
        : Type(Tenum)
{
    this->sym = sym;
}

const char *TypeEnum::kind()
{
    return "enum";
}

Type *TypeEnum::syntaxCopy()
{
    return this;
}

d_uns64 TypeEnum::size(Loc loc)
{
    return sym->getMemtype(loc)->size(loc);
}

unsigned TypeEnum::alignsize()
{
    Type *t = sym->getMemtype(Loc());
    if (t->ty == Terror)
        return 4;
    return t->alignsize();
}

Dsymbol *TypeEnum::toDsymbol(Scope *)
{
    return sym;
}

Type *TypeEnum::toBasetype()
{
    if (!sym->members && !sym->memtype)
        return this;
    Type *tb = sym->getMemtype(Loc())->toBasetype();
    return tb->castMod(mod);         // retain modifier bits from 'this'
}

Expression *TypeEnum::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    // Bugzilla 14010
    if (ident == Id::_mangleof)
        return getProperty(e->loc, ident, flag & 1);

    if (sym->semanticRun < PASSsemanticdone)
        dsymbolSemantic(sym, NULL);

    Dsymbol *s = sym->search(e->loc, ident);
    if (!s)
    {
        if (ident == Id::max ||
            ident == Id::min ||
            ident == Id::_init)
        {
            return getProperty(e->loc, ident, flag & 1);
        }
        Expression *res = sym->getMemtype(Loc())->dotExp(sc, e, ident, 1);
        if (!(flag & 1) && !res)
        {
            if (Dsymbol *ns = sym->search_correct(ident))
                e->error("no property `%s` for type `%s`. Did you mean `%s.%s` ?",
                    ident->toChars(), toChars(), toChars(), ns->toChars());
            else
                e->error("no property `%s` for type `%s`",
                    ident->toChars(), toChars());

            return new ErrorExp();
        }
        return res;
    }
    EnumMember *m = s->isEnumMember();
    return m->getVarExp(e->loc, sc);
}

Expression *TypeEnum::getProperty(Loc loc, Identifier *ident, int flag)
{
    Expression *e;
    if (ident == Id::max || ident == Id::min)
    {
        return sym->getMaxMinValue(loc, ident);
    }
    else if (ident == Id::_init)
    {
        e = defaultInitLiteral(loc);
    }
    else if (ident == Id::stringof)
    {
        const char *s = toChars();
        e = new StringExp(loc, const_cast<char *>(s), strlen(s));
        Scope sc;
        e = expressionSemantic(e, &sc);
    }
    else if (ident == Id::_mangleof)
    {
        e = Type::getProperty(loc, ident, flag);
    }
    else
    {
        e = toBasetype()->getProperty(loc, ident, flag);
    }
    return e;
}

bool TypeEnum::isintegral()
{
    return sym->getMemtype(Loc())->isintegral();
}

bool TypeEnum::isfloating()
{
    return sym->getMemtype(Loc())->isfloating();
}

bool TypeEnum::isreal()
{
    return sym->getMemtype(Loc())->isreal();
}

bool TypeEnum::isimaginary()
{
    return sym->getMemtype(Loc())->isimaginary();
}

bool TypeEnum::iscomplex()
{
    return sym->getMemtype(Loc())->iscomplex();
}

bool TypeEnum::isunsigned()
{
    return sym->getMemtype(Loc())->isunsigned();
}

bool TypeEnum::isscalar()
{
    return sym->getMemtype(Loc())->isscalar();
}

bool TypeEnum::isString()
{
    return sym->getMemtype(Loc())->isString();
}

bool TypeEnum::isAssignable()
{
    return sym->getMemtype(Loc())->isAssignable();
}

bool TypeEnum::isBoolean()
{
    return sym->getMemtype(Loc())->isBoolean();
}

bool TypeEnum::needsDestruction()
{
    return sym->getMemtype(Loc())->needsDestruction();
}

bool TypeEnum::needsNested()
{
    return sym->getMemtype(Loc())->needsNested();
}

MATCH TypeEnum::implicitConvTo(Type *to)
{
    MATCH m;

    //printf("TypeEnum::implicitConvTo()\n");
    if (ty == to->ty && sym == ((TypeEnum *)to)->sym)
        m = (mod == to->mod) ? MATCHexact : MATCHconst;
    else if (sym->getMemtype(Loc())->implicitConvTo(to))
        m = MATCHconvert;       // match with conversions
    else
        m = MATCHnomatch;       // no match
    return m;
}

MATCH TypeEnum::constConv(Type *to)
{
    if (equals(to))
        return MATCHexact;
    if (ty == to->ty && sym == ((TypeEnum *)to)->sym &&
        MODimplicitConv(mod, to->mod))
        return MATCHconst;
    return MATCHnomatch;
}


Expression *TypeEnum::defaultInit(Loc loc)
{
    // Initialize to first member of enum
    Expression *e = sym->getDefaultValue(loc);
    e = e->copy();
    e->loc = loc;
    e->type = this;     // to deal with const, immutable, etc., variants
    return e;
}

bool TypeEnum::isZeroInit(Loc loc)
{
    return sym->getDefaultValue(loc)->isBool(false);
}

bool TypeEnum::hasPointers()
{
    return sym->getMemtype(Loc())->hasPointers();
}

bool TypeEnum::hasVoidInitPointers()
{
    return sym->getMemtype(Loc())->hasVoidInitPointers();
}

Type *TypeEnum::nextOf()
{
    return sym->getMemtype(Loc())->nextOf();
}

/***************************** TypeStruct *****************************/

TypeStruct::TypeStruct(StructDeclaration *sym)
        : Type(Tstruct)
{
    this->sym = sym;
    this->att = RECfwdref;
    this->cppmangle = CPPMANGLEdefault;
}

TypeStruct *TypeStruct::create(StructDeclaration *sym)
{
    return new TypeStruct(sym);
}

const char *TypeStruct::kind()
{
    return "struct";
}

Type *TypeStruct::syntaxCopy()
{
    return this;
}

d_uns64 TypeStruct::size(Loc loc)
{
    return sym->size(loc);
}

unsigned TypeStruct::alignsize()
{
    sym->size(Loc());               // give error for forward references
    return sym->alignsize;
}

Dsymbol *TypeStruct::toDsymbol(Scope *)
{
    return sym;
}

Expression *TypeStruct::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    Dsymbol *s;

    assert(e->op != TOKdot);

    // Bugzilla 14010
    if (ident == Id::_mangleof)
        return getProperty(e->loc, ident, flag & 1);

    /* If e.tupleof
     */
    if (ident == Id::_tupleof)
    {
        /* Create a TupleExp out of the fields of the struct e:
         * (e.field0, e.field1, e.field2, ...)
         */
        e = expressionSemantic(e, sc);  // do this before turning on noaccesscheck

        if (!sym->determineFields())
        {
            error(e->loc, "unable to determine fields of `%s` because of forward references", toChars());
        }

        Expression *e0 = NULL;
        Expression *ev = e->op == TOKtype ? NULL : e;
        if (ev)
            ev = extractSideEffect(sc, "__tup", &e0, ev);

        Expressions *exps = new Expressions;
        exps->reserve(sym->fields.length);
        for (size_t i = 0; i < sym->fields.length; i++)
        {
            VarDeclaration *v = sym->fields[i];
            Expression *ex;
            if (ev)
                ex = new DotVarExp(e->loc, ev, v);
            else
            {
                ex = new VarExp(e->loc, v);
                ex->type = ex->type->addMod(e->type->mod);
            }
            exps->push(ex);
        }

        e = new TupleExp(e->loc, e0, exps);
        Scope *sc2 = sc->push();
        sc2->flags = sc->flags | SCOPEnoaccesscheck;
        e = expressionSemantic(e, sc2);
        sc2->pop();
        return e;
    }

    const int flags = sc->flags & SCOPEignoresymbolvisibility ? IgnoreSymbolVisibility : 0;
    s = sym->search(e->loc, ident, flags | IgnorePrivateImports);
L1:
    if (!s)
    {
        return noMember(sc, e, ident, flag);
    }
    if (!(sc->flags & SCOPEignoresymbolvisibility) && !symbolIsVisible(sc, s))
    {
        return noMember(sc, e, ident, flag);
    }
    if (!s->isFuncDeclaration())        // because of overloading
    {
        s->checkDeprecated(e->loc, sc);
        if (Declaration *d = s->isDeclaration())
            d->checkDisabled(e->loc, sc);
    }
    s = s->toAlias();

    EnumMember *em = s->isEnumMember();
    if (em)
    {
        return em->getVarExp(e->loc, sc);
    }

    if (VarDeclaration *v = s->isVarDeclaration())
    {
        if (!v->type ||
            (!v->type->deco && v->inuse))
        {
            if (v->inuse) // Bugzilla 9494
                e->error("circular reference to %s `%s`", v->kind(), v->toPrettyChars());
            else
                e->error("forward reference to %s `%s`", v->kind(), v->toPrettyChars());
            return new ErrorExp();
        }
        if (v->type->ty == Terror)
            return new ErrorExp();

        if ((v->storage_class & STCmanifest) && v->_init)
        {
            if (v->inuse)
            {
                e->error("circular initialization of %s `%s`", v->kind(), v->toPrettyChars());
                return new ErrorExp();
            }
            checkAccess(e->loc, sc, NULL, v);
            Expression *ve = new VarExp(e->loc, v);
            ve = expressionSemantic(ve, sc);
            return ve;
        }
    }

    if (Type *t = s->getType())
    {
        return expressionSemantic(new TypeExp(e->loc, t), sc);
    }

    TemplateMixin *tm = s->isTemplateMixin();
    if (tm)
    {
        Expression *de = new DotExp(e->loc, e, new ScopeExp(e->loc, tm));
        de->type = e->type;
        return de;
    }

    TemplateDeclaration *td = s->isTemplateDeclaration();
    if (td)
    {
        if (e->op == TOKtype)
            e = new TemplateExp(e->loc, td);
        else
            e = new DotTemplateExp(e->loc, e, td);
        e = expressionSemantic(e, sc);
        return e;
    }

    TemplateInstance *ti = s->isTemplateInstance();
    if (ti)
    {
        if (!ti->semanticRun)
        {
            dsymbolSemantic(ti, sc);
            if (!ti->inst || ti->errors)    // if template failed to expand
                return new ErrorExp();
        }
        s = ti->inst->toAlias();
        if (!s->isTemplateInstance())
            goto L1;
        if (e->op == TOKtype)
            e = new ScopeExp(e->loc, ti);
        else
            e = new DotExp(e->loc, e, new ScopeExp(e->loc, ti));
        return expressionSemantic(e, sc);
    }

    if (s->isImport() || s->isModule() || s->isPackage())
    {
        e = ::resolve(e->loc, sc, s, false);
        return e;
    }

    OverloadSet *o = s->isOverloadSet();
    if (o)
    {
        OverExp *oe = new OverExp(e->loc, o);
        if (e->op == TOKtype)
            return oe;
        return new DotExp(e->loc, e, oe);
    }

    Declaration *d = s->isDeclaration();
    if (!d)
    {
        e->error("%s.%s is not a declaration", e->toChars(), ident->toChars());
        return new ErrorExp();
    }

    if (e->op == TOKtype)
    {
        /* It's:
         *    Struct.d
         */
        if (TupleDeclaration *tup = d->isTupleDeclaration())
        {
            e = new TupleExp(e->loc, tup);
            e = expressionSemantic(e, sc);
            return e;
        }
        if (d->needThis() && sc->intypeof != 1)
        {
            /* Rewrite as:
             *  this.d
             */
            if (hasThis(sc))
            {
                e = new DotVarExp(e->loc, new ThisExp(e->loc), d);
                e = expressionSemantic(e, sc);
                return e;
            }
        }
        if (d->semanticRun == PASSinit)
            dsymbolSemantic(d, NULL);
        checkAccess(e->loc, sc, e, d);
        VarExp *ve = new VarExp(e->loc, d);
        if (d->isVarDeclaration() && d->needThis())
            ve->type = d->type->addMod(e->type->mod);
        return ve;
    }

    bool unreal = e->op == TOKvar && ((VarExp *)e)->var->isField();
    if (d->isDataseg() || (unreal && d->isField()))
    {
        // (e, d)
        checkAccess(e->loc, sc, e, d);
        Expression *ve = new VarExp(e->loc, d);
        e = unreal ? ve : new CommaExp(e->loc, e, ve);
        e = expressionSemantic(e, sc);
        return e;
    }

    e = new DotVarExp(e->loc, e, d);
    e = expressionSemantic(e, sc);
    return e;
}

structalign_t TypeStruct::alignment()
{
    if (sym->alignment == 0)
        sym->size(sym->loc);
    return sym->alignment;
}

Expression *TypeStruct::defaultInit(Loc)
{
    Declaration *d = new SymbolDeclaration(sym->loc, sym);
    assert(d);
    d->type = this;
    d->storage_class |= STCrvalue;      // Bugzilla 14398
    return new VarExp(sym->loc, d);
}

/***************************************
 * Use when we prefer the default initializer to be a literal,
 * rather than a global immutable variable.
 */
Expression *TypeStruct::defaultInitLiteral(Loc loc)
{
    sym->size(loc);
    if (sym->sizeok != SIZEOKdone)
        return new ErrorExp();
    Expressions *structelems = new Expressions();
    structelems->setDim(sym->fields.length - sym->isNested());
    unsigned offset = 0;
    for (size_t j = 0; j < structelems->length; j++)
    {
        VarDeclaration *vd = sym->fields[j];
        Expression *e;
        if (vd->inuse)
        {
            error(loc, "circular reference to `%s`", vd->toPrettyChars());
            return new ErrorExp();
        }
        if (vd->offset < offset || vd->type->size() == 0)
            e = NULL;
        else if (vd->_init)
        {
            if (vd->_init->isVoidInitializer())
                e = NULL;
            else
                e = vd->getConstInitializer(false);
        }
        else
            e = vd->type->defaultInitLiteral(loc);
        if (e && e->op == TOKerror)
            return e;
        if (e)
            offset = vd->offset + (unsigned)vd->type->size();
        (*structelems)[j] = e;
    }
    StructLiteralExp *structinit = new StructLiteralExp(loc, (StructDeclaration *)sym, structelems);

    /* Copy from the initializer symbol for larger symbols,
     * otherwise the literals expressed as code get excessively large.
     */
    if (size(loc) > target.ptrsize * 4U && !needsNested())
        structinit->useStaticInit = true;

    structinit->type = this;
    return structinit;
}


bool TypeStruct::isZeroInit(Loc)
{
    return sym->zeroInit != 0;
}

bool TypeStruct::isBoolean()
{
    return false;
}

bool TypeStruct::needsDestruction()
{
    return sym->dtor != NULL;
}

bool TypeStruct::needsNested()
{
    if (sym->isNested())
        return true;

    for (size_t i = 0; i < sym->fields.length; i++)
    {
        VarDeclaration *v = sym->fields[i];
        if (!v->isDataseg() && v->type->needsNested())
            return true;
    }
    return false;
}

bool TypeStruct::isAssignable()
{
    bool assignable = true;
    unsigned offset = ~0;  // dead-store initialize to prevent spurious warning

    /* If any of the fields are const or immutable,
     * then one cannot assign this struct.
     */
    for (size_t i = 0; i < sym->fields.length; i++)
    {
        VarDeclaration *v = sym->fields[i];
        //printf("%s [%d] v = (%s) %s, v->offset = %d, v->parent = %s", sym->toChars(), i, v->kind(), v->toChars(), v->offset, v->parent->kind());
        if (i == 0)
            ;
        else if (v->offset == offset)
        {
            /* If any fields of anonymous union are assignable,
             * then regard union as assignable.
             * This is to support unsafe things like Rebindable templates.
             */
            if (assignable)
                continue;
        }
        else
        {
            if (!assignable)
                return false;
        }
        assignable = v->type->isMutable() && v->type->isAssignable();
        offset = v->offset;
        //printf(" -> assignable = %d\n", assignable);
    }

    return assignable;
}

bool TypeStruct::hasPointers()
{
    // Probably should cache this information in sym rather than recompute
    StructDeclaration *s = sym;

    if (sym->members && !sym->determineFields() && sym->type != Type::terror)
        error(sym->loc, "no size because of forward references");

    for (size_t i = 0; i < s->fields.length; i++)
    {
        Declaration *d = s->fields[i];
        if (d->storage_class & STCref || d->hasPointers())
            return true;
    }
    return false;
}

bool TypeStruct::hasVoidInitPointers()
{
    // Probably should cache this information in sym rather than recompute
    StructDeclaration *s = sym;

    sym->size(Loc()); // give error for forward references
    for (size_t i = 0; i < s->fields.length; i++)
    {
        VarDeclaration *v = s->fields[i];
        if (v->_init && v->_init->isVoidInitializer() && v->type->hasPointers())
            return true;
        if (!v->_init && v->type->hasVoidInitPointers())
            return true;
    }
    return false;
}

MATCH TypeStruct::implicitConvTo(Type *to)
{   MATCH m;

    //printf("TypeStruct::implicitConvTo(%s => %s)\n", toChars(), to->toChars());

    if (ty == to->ty && sym == ((TypeStruct *)to)->sym)
    {
        m = MATCHexact;         // exact match
        if (mod != to->mod)
        {
            m = MATCHconst;
            if (MODimplicitConv(mod, to->mod))
                ;
            else
            {
                /* Check all the fields. If they can all be converted,
                 * allow the conversion.
                 */
                unsigned offset = ~0;   // dead-store to prevent spurious warning
                for (size_t i = 0; i < sym->fields.length; i++)
                {
                    VarDeclaration *v = sym->fields[i];
                    if (i == 0)
                        ;
                    else if (v->offset == offset)
                    {
                        if (m > MATCHnomatch)
                            continue;
                    }
                    else
                    {
                        if (m <= MATCHnomatch)
                            return m;
                    }

                    // 'from' type
                    Type *tvf = v->type->addMod(mod);

                    // 'to' type
                    Type *tv = v->type->addMod(to->mod);

                    // field match
                    MATCH mf = tvf->implicitConvTo(tv);
                    //printf("\t%s => %s, match = %d\n", v->type->toChars(), tv->toChars(), mf);

                    if (mf <= MATCHnomatch)
                        return mf;
                    if (mf < m)         // if field match is worse
                        m = mf;
                    offset = v->offset;
                }
            }
        }
    }
    else if (sym->aliasthis && !(att & RECtracing))
    {
        att = (AliasThisRec)(att | RECtracing);
        m = aliasthisOf()->implicitConvTo(to);
        att = (AliasThisRec)(att & ~RECtracing);
    }
    else
        m = MATCHnomatch;       // no match
    return m;
}

MATCH TypeStruct::constConv(Type *to)
{
    if (equals(to))
        return MATCHexact;
    if (ty == to->ty && sym == ((TypeStruct *)to)->sym &&
        MODimplicitConv(mod, to->mod))
        return MATCHconst;
    return MATCHnomatch;
}

unsigned char TypeStruct::deduceWild(Type *t, bool isRef)
{
    if (ty == t->ty && sym == ((TypeStruct *)t)->sym)
        return Type::deduceWild(t, isRef);

    unsigned char wm = 0;

    if (t->hasWild() && sym->aliasthis && !(att & RECtracing))
    {
        att = (AliasThisRec)(att | RECtracing);
        wm = aliasthisOf()->deduceWild(t, isRef);
        att = (AliasThisRec)(att & ~RECtracing);
    }

    return wm;
}

Type *TypeStruct::toHeadMutable()
{
    return this;
}


/***************************** TypeClass *****************************/

TypeClass::TypeClass(ClassDeclaration *sym)
        : Type(Tclass)
{
    this->sym = sym;
    this->att = RECfwdref;
    this->cppmangle = CPPMANGLEdefault;
}

const char *TypeClass::kind()
{
    return "class";
}

Type *TypeClass::syntaxCopy()
{
    return this;
}

d_uns64 TypeClass::size(Loc)
{
    return target.ptrsize;
}

Dsymbol *TypeClass::toDsymbol(Scope *)
{
    return sym;
}

Expression *TypeClass::dotExp(Scope *sc, Expression *e, Identifier *ident, int flag)
{
    Dsymbol *s;
    assert(e->op != TOKdot);

    // Bugzilla 12543
    if (ident == Id::__sizeof || ident == Id::__xalignof || ident == Id::_mangleof)
    {
        return Type::getProperty(e->loc, ident, 0);
    }

    /* If e.tupleof
     */
    if (ident == Id::_tupleof)
    {
        /* Create a TupleExp
         */
        e = expressionSemantic(e, sc);  // do this before turning on noaccesscheck

        sym->size(e->loc); // do semantic of type

        Expression *e0 = NULL;
        Expression *ev = e->op == TOKtype ? NULL : e;
        if (ev)
            ev = extractSideEffect(sc, "__tup", &e0, ev);

        Expressions *exps = new Expressions;
        exps->reserve(sym->fields.length);
        for (size_t i = 0; i < sym->fields.length; i++)
        {
            VarDeclaration *v = sym->fields[i];
            // Don't include hidden 'this' pointer
            if (v->isThisDeclaration())
                continue;
            Expression *ex;
            if (ev)
                ex = new DotVarExp(e->loc, ev, v);
            else
            {
                ex = new VarExp(e->loc, v);
                ex->type = ex->type->addMod(e->type->mod);
            }
            exps->push(ex);
        }

        e = new TupleExp(e->loc, e0, exps);
        Scope *sc2 = sc->push();
        sc2->flags = sc->flags | SCOPEnoaccesscheck;
        e = expressionSemantic(e, sc2);
        sc2->pop();
        return e;
    }

    int flags = sc->flags & SCOPEignoresymbolvisibility ? IgnoreSymbolVisibility : 0;
    s = sym->search(e->loc, ident, flags | IgnorePrivateImports);

L1:
    if (!s)
    {
        // See if it's 'this' class or a base class
        if (sym->ident == ident)
        {
            if (e->op == TOKtype)
                return Type::getProperty(e->loc, ident, 0);
            e = new DotTypeExp(e->loc, e, sym);
            e = expressionSemantic(e, sc);
            return e;
        }
        if (ClassDeclaration *cbase = sym->searchBase(ident))
        {
            if (e->op == TOKtype)
                return Type::getProperty(e->loc, ident, 0);
            if (InterfaceDeclaration *ifbase = cbase->isInterfaceDeclaration())
                e = new CastExp(e->loc, e, ifbase->type);
            else
                e = new DotTypeExp(e->loc, e, cbase);
            e = expressionSemantic(e, sc);
            return e;
        }

        if (ident == Id::classinfo)
        {
            if (!Type::typeinfoclass)
            {
                error(e->loc, "`object.TypeInfo_Class` could not be found, but is implicitly used");
                return new ErrorExp();
            }

            Type *t = Type::typeinfoclass->type;
            if (e->op == TOKtype || e->op == TOKdottype)
            {
                /* For type.classinfo, we know the classinfo
                 * at compile time.
                 */
                if (!sym->vclassinfo)
                    sym->vclassinfo = new TypeInfoClassDeclaration(sym->type);
                e = new VarExp(e->loc, sym->vclassinfo);
                e = e->addressOf();
                e->type = t;    // do this so we don't get redundant dereference
            }
            else
            {
                /* For class objects, the classinfo reference is the first
                 * entry in the vtbl[]
                 */
                e = new PtrExp(e->loc, e);
                e->type = t->pointerTo();
                if (sym->isInterfaceDeclaration())
                {
                    if (sym->isCPPinterface())
                    {
                        /* C++ interface vtbl[]s are different in that the
                         * first entry is always pointer to the first virtual
                         * function, not classinfo.
                         * We can't get a .classinfo for it.
                         */
                        error(e->loc, "no .classinfo for C++ interface objects");
                    }
                    /* For an interface, the first entry in the vtbl[]
                     * is actually a pointer to an instance of struct Interface.
                     * The first member of Interface is the .classinfo,
                     * so add an extra pointer indirection.
                     */
                    e->type = e->type->pointerTo();
                    e = new PtrExp(e->loc, e);
                    e->type = t->pointerTo();
                }
                e = new PtrExp(e->loc, e, t);
            }
            return e;
        }

        if (ident == Id::__vptr)
        {
            /* The pointer to the vtbl[]
             * *cast(immutable(void*)**)e
             */
            e = e->castTo(sc, tvoidptr->immutableOf()->pointerTo()->pointerTo());
            e = new PtrExp(e->loc, e);
            e = expressionSemantic(e, sc);
            return e;
        }

        if (ident == Id::__monitor && sym->hasMonitor())
        {
            /* The handle to the monitor (call it a void*)
             * *(cast(void**)e + 1)
             */
            e = e->castTo(sc, tvoidptr->pointerTo());
            e = new AddExp(e->loc, e, new IntegerExp(1));
            e = new PtrExp(e->loc, e);
            e = expressionSemantic(e, sc);
            return e;
        }

        if (ident == Id::outer && sym->vthis)
        {
            if (sym->vthis->semanticRun == PASSinit)
                dsymbolSemantic(sym->vthis, NULL);

            if (ClassDeclaration *cdp = sym->toParent2()->isClassDeclaration())
            {
                DotVarExp *dve = new DotVarExp(e->loc, e, sym->vthis);
                dve->type = cdp->type->addMod(e->type->mod);
                return dve;
            }

            /* Bugzilla 15839: Find closest parent class through nested functions.
             */
            for (Dsymbol *p = sym->toParent2(); p; p = p->toParent2())
            {
                FuncDeclaration *fd = p->isFuncDeclaration();
                if (!fd)
                    break;
                if (fd->isNested())
                    continue;
                AggregateDeclaration *ad = fd->isThis();
                if (!ad)
                    break;
                if (ad->isClassDeclaration())
                {
                    ThisExp *ve = new ThisExp(e->loc);

                    ve->var = fd->vthis;
                    const bool nestedError = fd->vthis->checkNestedReference(sc, e->loc);
                    assert(!nestedError);

                    ve->type = fd->vthis->type->addMod(e->type->mod);
                    return ve;
                }
                break;
            }

            // Continue to show enclosing function's frame (stack or closure).
            DotVarExp *dve = new DotVarExp(e->loc, e, sym->vthis);
            dve->type = sym->vthis->type->addMod(e->type->mod);
            return dve;
        }

        return noMember(sc, e, ident, flag & 1);
    }
    if (!(sc->flags & SCOPEignoresymbolvisibility) && !symbolIsVisible(sc, s))
    {
        return noMember(sc, e, ident, flag);
    }
    if (!s->isFuncDeclaration())        // because of overloading
    {
        s->checkDeprecated(e->loc, sc);
        if (Declaration *d = s->isDeclaration())
            d->checkDisabled(e->loc, sc);
    }
    s = s->toAlias();

    EnumMember *em = s->isEnumMember();
    if (em)
    {
        return em->getVarExp(e->loc, sc);
    }

    if (VarDeclaration *v = s->isVarDeclaration())
    {
        if (!v->type ||
            (!v->type->deco && v->inuse))
        {
            if (v->inuse) // Bugzilla 9494
                e->error("circular reference to %s `%s`", v->kind(), v->toPrettyChars());
            else
                e->error("forward reference to %s `%s`", v->kind(), v->toPrettyChars());
            return new ErrorExp();
        }
        if (v->type->ty == Terror)
            return new ErrorExp();

        if ((v->storage_class & STCmanifest) && v->_init)
        {
            if (v->inuse)
            {
                e->error("circular initialization of %s `%s`", v->kind(), v->toPrettyChars());
                return new ErrorExp();
            }
            checkAccess(e->loc, sc, NULL, v);
            Expression *ve = new VarExp(e->loc, v);
            ve = expressionSemantic(ve, sc);
            return ve;
        }
    }

    if (Type *t = s->getType())
    {
        return expressionSemantic(new TypeExp(e->loc, t), sc);
    }

    TemplateMixin *tm = s->isTemplateMixin();
    if (tm)
    {
        Expression *de = new DotExp(e->loc, e, new ScopeExp(e->loc, tm));
        de->type = e->type;
        return de;
    }

    TemplateDeclaration *td = s->isTemplateDeclaration();
    if (td)
    {
        if (e->op == TOKtype)
            e = new TemplateExp(e->loc, td);
        else
            e = new DotTemplateExp(e->loc, e, td);
        e = expressionSemantic(e, sc);
        return e;
    }

    TemplateInstance *ti = s->isTemplateInstance();
    if (ti)
    {
        if (!ti->semanticRun)
        {
            dsymbolSemantic(ti, sc);
            if (!ti->inst || ti->errors)    // if template failed to expand
                return new ErrorExp();
        }
        s = ti->inst->toAlias();
        if (!s->isTemplateInstance())
            goto L1;
        if (e->op == TOKtype)
            e = new ScopeExp(e->loc, ti);
        else
            e = new DotExp(e->loc, e, new ScopeExp(e->loc, ti));
        return expressionSemantic(e, sc);
    }

    if (s->isImport() || s->isModule() || s->isPackage())
    {
        e = ::resolve(e->loc, sc, s, false);
        return e;
    }

    OverloadSet *o = s->isOverloadSet();
    if (o)
    {
        OverExp *oe = new OverExp(e->loc, o);
        if (e->op == TOKtype)
            return oe;
        return new DotExp(e->loc, e, oe);
    }

    Declaration *d = s->isDeclaration();
    if (!d)
    {
        e->error("%s.%s is not a declaration", e->toChars(), ident->toChars());
        return new ErrorExp();
    }

    if (e->op == TOKtype)
    {
        /* It's:
         *    Class.d
         */
        if (TupleDeclaration *tup = d->isTupleDeclaration())
        {
            e = new TupleExp(e->loc, tup);
            e = expressionSemantic(e, sc);
            return e;
        }
        if (d->needThis() && sc->intypeof != 1)
        {
            /* Rewrite as:
             *  this.d
             */
            if (hasThis(sc))
            {
                // This is almost same as getRightThis() in expression.c
                Expression *e1 = new ThisExp(e->loc);
                e1 = expressionSemantic(e1, sc);
            L2:
                Type *t = e1->type->toBasetype();
                ClassDeclaration *cd = e->type->isClassHandle();
                ClassDeclaration *tcd = t->isClassHandle();
                if (cd && tcd && (tcd == cd || cd->isBaseOf(tcd, NULL)))
                {
                    e = new DotTypeExp(e1->loc, e1, cd);
                    e = new DotVarExp(e->loc, e, d);
                    e = expressionSemantic(e, sc);
                    return e;
                }
                if (tcd && tcd->isNested())
                {   /* e1 is the 'this' pointer for an inner class: tcd.
                     * Rewrite it as the 'this' pointer for the outer class.
                     */

                    e1 = new DotVarExp(e->loc, e1, tcd->vthis);
                    e1->type = tcd->vthis->type;
                    e1->type = e1->type->addMod(t->mod);
                    // Do not call checkNestedRef()
                    //e1 = expressionSemantic(e1, sc);

                    // Skip up over nested functions, and get the enclosing
                    // class type.
                    int n = 0;
                    for (s = tcd->toParent();
                         s && s->isFuncDeclaration();
                         s = s->toParent())
                    {   FuncDeclaration *f = s->isFuncDeclaration();
                        if (f->vthis)
                        {
                            //printf("rewriting e1 to %s's this\n", f->toChars());
                            n++;
                            e1 = new VarExp(e->loc, f->vthis);
                        }
                        else
                        {
                            e = new VarExp(e->loc, d);
                            return e;
                        }
                    }
                    if (s && s->isClassDeclaration())
                    {   e1->type = s->isClassDeclaration()->type;
                        e1->type = e1->type->addMod(t->mod);
                        if (n > 1)
                            e1 = expressionSemantic(e1, sc);
                    }
                    else
                        e1 = expressionSemantic(e1, sc);
                    goto L2;
                }
            }
        }
        //printf("e = %s, d = %s\n", e->toChars(), d->toChars());
        if (d->semanticRun == PASSinit)
            dsymbolSemantic(d, NULL);
        checkAccess(e->loc, sc, e, d);
        VarExp *ve = new VarExp(e->loc, d);
        if (d->isVarDeclaration() && d->needThis())
            ve->type = d->type->addMod(e->type->mod);
        return ve;
    }

    bool unreal = e->op == TOKvar && ((VarExp *)e)->var->isField();
    if (d->isDataseg() || (unreal && d->isField()))
    {
        // (e, d)
        checkAccess(e->loc, sc, e, d);
        Expression *ve = new VarExp(e->loc, d);
        e = unreal ? ve : new CommaExp(e->loc, e, ve);
        e = expressionSemantic(e, sc);
        return e;
    }

    e = new DotVarExp(e->loc, e, d);
    e = expressionSemantic(e, sc);
    return e;
}

ClassDeclaration *TypeClass::isClassHandle()
{
    return sym;
}

bool TypeClass::isscope()
{
    return sym->isscope;
}

bool TypeClass::isBaseOf(Type *t, int *poffset)
{
    if (t && t->ty == Tclass)
    {
        ClassDeclaration *cd = ((TypeClass *)t)->sym;
        if (sym->isBaseOf(cd, poffset))
            return true;
    }
    return false;
}

MATCH TypeClass::implicitConvTo(Type *to)
{
    //printf("TypeClass::implicitConvTo(to = '%s') %s\n", to->toChars(), toChars());
    MATCH m = constConv(to);
    if (m > MATCHnomatch)
        return m;

    ClassDeclaration *cdto = to->isClassHandle();
    if (cdto)
    {
        //printf("TypeClass::implicitConvTo(to = '%s') %s, isbase = %d %d\n", to->toChars(), toChars(), cdto->isBaseInfoComplete(), sym->isBaseInfoComplete());
        if (cdto->semanticRun < PASSsemanticdone && !cdto->isBaseInfoComplete())
            dsymbolSemantic(cdto, NULL);
        if (sym->semanticRun < PASSsemanticdone && !sym->isBaseInfoComplete())
            dsymbolSemantic(sym, NULL);
        if (cdto->isBaseOf(sym, NULL) && MODimplicitConv(mod, to->mod))
        {
            //printf("'to' is base\n");
            return MATCHconvert;
        }
    }

    m = MATCHnomatch;
    if (sym->aliasthis && !(att & RECtracing))
    {
        att = (AliasThisRec)(att | RECtracing);
        m = aliasthisOf()->implicitConvTo(to);
        att = (AliasThisRec)(att & ~RECtracing);
    }

    return m;
}

MATCH TypeClass::constConv(Type *to)
{
    if (equals(to))
        return MATCHexact;
    if (ty == to->ty && sym == ((TypeClass *)to)->sym &&
        MODimplicitConv(mod, to->mod))
        return MATCHconst;

    /* Conversion derived to const(base)
     */
    int offset = 0;
    if (to->isBaseOf(this, &offset) && offset == 0 &&
        MODimplicitConv(mod, to->mod))
    {
        // Disallow:
        //  derived to base
        //  inout(derived) to inout(base)
        if (!to->isMutable() && !to->isWild())
            return MATCHconvert;
    }

    return MATCHnomatch;
}

unsigned char TypeClass::deduceWild(Type *t, bool isRef)
{
    ClassDeclaration *cd = t->isClassHandle();
    if (cd && (sym == cd || cd->isBaseOf(sym, NULL)))
        return Type::deduceWild(t, isRef);

    unsigned char wm = 0;

    if (t->hasWild() && sym->aliasthis && !(att & RECtracing))
    {
        att = (AliasThisRec)(att | RECtracing);
        wm = aliasthisOf()->deduceWild(t, isRef);
        att = (AliasThisRec)(att & ~RECtracing);
    }

    return wm;
}

Type *TypeClass::toHeadMutable()
{
    return this;
}

Expression *TypeClass::defaultInit(Loc loc)
{
    return new NullExp(loc, this);
}

bool TypeClass::isZeroInit(Loc)
{
    return true;
}

bool TypeClass::isBoolean()
{
    return true;
}

bool TypeClass::hasPointers()
{
    return true;
}

/***************************** TypeTuple *****************************/

TypeTuple::TypeTuple(Parameters *arguments)
    : Type(Ttuple)
{
    //printf("TypeTuple(this = %p)\n", this);
    this->arguments = arguments;
    //printf("TypeTuple() %p, %s\n", this, toChars());
}

/****************
 * Form TypeTuple from the types of the expressions.
 * Assume exps[] is already tuple expanded.
 */

TypeTuple::TypeTuple(Expressions *exps)
    : Type(Ttuple)
{
    Parameters *arguments = new Parameters;
    if (exps)
    {
        arguments->setDim(exps->length);
        for (size_t i = 0; i < exps->length; i++)
        {   Expression *e = (*exps)[i];
            if (e->type->ty == Ttuple)
                e->error("cannot form tuple of tuples");
            Parameter *arg = new Parameter(STCundefined, e->type, NULL, NULL, NULL);
            (*arguments)[i] = arg;
        }
    }
    this->arguments = arguments;
    //printf("TypeTuple() %p, %s\n", this, toChars());
}

TypeTuple *TypeTuple::create(Parameters *arguments)
{
    return new TypeTuple(arguments);
}

/*******************************************
 * Type tuple with 0, 1 or 2 types in it.
 */
TypeTuple::TypeTuple()
    : Type(Ttuple)
{
    arguments = new Parameters();
}

TypeTuple::TypeTuple(Type *t1)
    : Type(Ttuple)
{
    arguments = new Parameters();
    arguments->push(new Parameter(0, t1, NULL, NULL, NULL));
}

TypeTuple::TypeTuple(Type *t1, Type *t2)
    : Type(Ttuple)
{
    arguments = new Parameters();
    arguments->push(new Parameter(0, t1, NULL, NULL, NULL));
    arguments->push(new Parameter(0, t2, NULL, NULL, NULL));
}

const char *TypeTuple::kind()
{
    return "tuple";
}

Type *TypeTuple::syntaxCopy()
{
    Parameters *args = Parameter::arraySyntaxCopy(arguments);
    Type *t = new TypeTuple(args);
    t->mod = mod;
    return t;
}

bool TypeTuple::equals(RootObject *o)
{
    Type *t = (Type *)o;
    //printf("TypeTuple::equals(%s, %s)\n", toChars(), t->toChars());
    if (this == t)
        return true;
    if (t->ty == Ttuple)
    {
        TypeTuple *tt = (TypeTuple *)t;
        if (arguments->length == tt->arguments->length)
        {
            for (size_t i = 0; i < tt->arguments->length; i++)
            {
                Parameter *arg1 = (*arguments)[i];
                Parameter *arg2 = (*tt->arguments)[i];
                if (!arg1->type->equals(arg2->type))
                    return false;
            }
            return true;
        }
    }
    return false;
}

Expression *TypeTuple::getProperty(Loc loc, Identifier *ident, int flag)
{
    Expression *e;

    if (ident == Id::length)
    {
        e = new IntegerExp(loc, arguments->length, Type::tsize_t);
    }
    else if (ident == Id::_init)
    {
        e = defaultInitLiteral(loc);
    }
    else if (flag)
    {
        e = NULL;
    }
    else
    {
        error(loc, "no property `%s` for tuple `%s`", ident->toChars(), toChars());
        e = new ErrorExp();
    }
    return e;
}

Expression *TypeTuple::defaultInit(Loc loc)
{
    Expressions *exps = new Expressions();
    exps->setDim(arguments->length);
    for (size_t i = 0; i < arguments->length; i++)
    {
        Parameter *p = (*arguments)[i];
        assert(p->type);
        Expression *e = p->type->defaultInitLiteral(loc);
        if (e->op == TOKerror)
            return e;
        (*exps)[i] = e;
    }
    return new TupleExp(loc, exps);
}

/***************************** TypeSlice *****************************/

/* This is so we can slice a TypeTuple */

TypeSlice::TypeSlice(Type *next, Expression *lwr, Expression *upr)
    : TypeNext(Tslice, next)
{
    //printf("TypeSlice[%s .. %s]\n", lwr->toChars(), upr->toChars());
    this->lwr = lwr;
    this->upr = upr;
}

const char *TypeSlice::kind()
{
    return "slice";
}

Type *TypeSlice::syntaxCopy()
{
    Type *t = new TypeSlice(next->syntaxCopy(), lwr->syntaxCopy(), upr->syntaxCopy());
    t->mod = mod;
    return t;
}

void TypeSlice::resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid)
{
    next->resolve(loc, sc, pe, pt, ps, intypeid);
    if (*pe)
    {
        // It's really a slice expression
        if (Dsymbol *s = getDsymbol(*pe))
            *pe = new DsymbolExp(loc, s);
        *pe = new ArrayExp(loc, *pe, new IntervalExp(loc, lwr, upr));
    }
    else if (*ps)
    {
        Dsymbol *s = *ps;
        TupleDeclaration *td = s->isTupleDeclaration();
        if (td)
        {
            /* It's a slice of a TupleDeclaration
             */
            ScopeDsymbol *sym = new ArrayScopeSymbol(sc, td);
            sym->parent = sc->scopesym;
            sc = sc->push(sym);
            sc = sc->startCTFE();
            lwr = expressionSemantic(lwr, sc);
            upr = expressionSemantic(upr, sc);
            sc = sc->endCTFE();
            sc = sc->pop();

            lwr = lwr->ctfeInterpret();
            upr = upr->ctfeInterpret();
            uinteger_t i1 = lwr->toUInteger();
            uinteger_t i2 = upr->toUInteger();

            if (!(i1 <= i2 && i2 <= td->objects->length))
            {
                error(loc, "slice [%llu..%llu] is out of range of [0..%u]", i1, i2, td->objects->length);
                *ps = NULL;
                *pt = Type::terror;
                return;
            }

            if (i1 == 0 && i2 == td->objects->length)
            {
                *ps = td;
                return;
            }

            /* Create a new TupleDeclaration which
             * is a slice [i1..i2] out of the old one.
             */
            Objects *objects = new Objects;
            objects->setDim((size_t)(i2 - i1));
            for (size_t i = 0; i < objects->length; i++)
            {
                (*objects)[i] = (*td->objects)[(size_t)i1 + i];
            }

            TupleDeclaration *tds = new TupleDeclaration(loc, td->ident, objects);
            *ps = tds;
        }
        else
            goto Ldefault;
    }
    else
    {
        if ((*pt)->ty != Terror)
            next = *pt;     // prevent re-running semantic() on 'next'
     Ldefault:
        Type::resolve(loc, sc, pe, pt, ps, intypeid);
    }
}

/***************************** TypeNull *****************************/

TypeNull::TypeNull()
        : Type(Tnull)
{
}

const char *TypeNull::kind()
{
    return "null";
}

Type *TypeNull::syntaxCopy()
{
    // No semantic analysis done, no need to copy
    return this;
}

MATCH TypeNull::implicitConvTo(Type *to)
{
    //printf("TypeNull::implicitConvTo(this=%p, to=%p)\n", this, to);
    //printf("from: %s\n", toChars());
    //printf("to  : %s\n", to->toChars());
    MATCH m = Type::implicitConvTo(to);
    if (m != MATCHnomatch)
        return m;

    // NULL implicitly converts to any pointer type or dynamic array
    //if (type->ty == Tpointer && type->nextOf()->ty == Tvoid)
    {
        Type *tb = to->toBasetype();
        if (tb->ty == Tnull ||
            tb->ty == Tpointer || tb->ty == Tarray ||
            tb->ty == Taarray  || tb->ty == Tclass ||
            tb->ty == Tdelegate)
            return MATCHconst;
    }

    return MATCHnomatch;
}

bool TypeNull::isBoolean()
{
    return true;
}

d_uns64 TypeNull::size(Loc loc)
{
    return tvoidptr->size(loc);
}

Expression *TypeNull::defaultInit(Loc)
{
    return new NullExp(Loc(), Type::tnull);
}

/***************************** TypeNoreturn *****************************/

TypeNoreturn::TypeNoreturn()
    : Type(Tnoreturn)
{
    //printf("TypeNoreturn %p\n", this);
}

const char *TypeNoreturn::kind()
{
    return "noreturn";
}

Type *TypeNoreturn::syntaxCopy()
{
    // No semantic analysis done, no need to copy
    return this;
}

MATCH TypeNoreturn::implicitConvTo(Type *to)
{
    //printf("TypeNoreturn::implicitConvTo(this=%p, to=%p)\n", this, to);
    //printf("from: %s\n", toChars());
    //printf("to  : %s\n", to.toChars());
    MATCH m = Type::implicitConvTo(to);
    return (m == MATCHexact) ? MATCHexact : MATCHconvert;
}

bool TypeNoreturn::isBoolean()
{
    return true;  // bottom type can be implicitly converted to any other type
}

d_uns64 TypeNoreturn::size(Loc)
{
    return 0;
}

unsigned TypeNoreturn::alignsize()
{
    return 0;
}

/***********************************************************
 * Encapsulate Parameters* so .length and [i] can be used on it.
 * https://dlang.org/spec/function.html#ParameterList
 */

ParameterList::ParameterList(Parameters *parameters, VarArg varargs)
{
    this->parameters = parameters;
    this->varargs = varargs;
}

size_t ParameterList::length()
{
    return Parameter::dim(parameters);
}

/***************************** Parameter *****************************/

Parameter::Parameter(StorageClass storageClass, Type *type, Identifier *ident,
                     Expression *defaultArg, UserAttributeDeclaration *userAttribDecl)
{
    this->type = type;
    this->ident = ident;
    this->storageClass = storageClass;
    this->defaultArg = defaultArg;
    this->userAttribDecl = userAttribDecl;
}

Parameter *Parameter::create(StorageClass storageClass, Type *type, Identifier *ident,
                             Expression *defaultArg, UserAttributeDeclaration *userAttribDecl)
{
    return new Parameter(storageClass, type, ident, defaultArg, userAttribDecl);
}

Parameter *Parameter::syntaxCopy()
{
    return new Parameter(storageClass,
        type ? type->syntaxCopy() : NULL,
        ident,
        defaultArg ? defaultArg->syntaxCopy() : NULL,
        userAttribDecl ? (UserAttributeDeclaration *) userAttribDecl->syntaxCopy(NULL) : NULL);
}

Parameters *Parameter::arraySyntaxCopy(Parameters *parameters)
{
    Parameters *params = NULL;
    if (parameters)
    {
        params = new Parameters();
        params->setDim(parameters->length);
        for (size_t i = 0; i < params->length; i++)
            (*params)[i] = (*parameters)[i]->syntaxCopy();
    }
    return params;
}

/****************************************************
 * Determine if parameter is a lazy array of delegates.
 * If so, return the return type of those delegates.
 * If not, return NULL.
 *
 * Returns T if the type is one of the following forms:
 *      T delegate()[]
 *      T delegate()[dim]
 */

Type *Parameter::isLazyArray()
{
    Type *tb = type->toBasetype();
    if (tb->ty == Tsarray || tb->ty == Tarray)
    {
        Type *tel = ((TypeArray *)tb)->next->toBasetype();
        if (tel->ty == Tdelegate)
        {
            TypeDelegate *td = (TypeDelegate *)tel;
            TypeFunction *tf = td->next->toTypeFunction();

            if (tf->parameterList.varargs == VARARGnone && tf->parameterList.length() == 0)
            {
                return tf->next;    // return type of delegate
            }
        }
    }
    return NULL;
}

/***************************************
 * Determine number of arguments, folding in tuples.
 */

static int dimDg(void *ctx, size_t, Parameter *)
{
    ++*(size_t *)ctx;
    return 0;
}

size_t Parameter::dim(Parameters *parameters)
{
    size_t n = 0;
    Parameter_foreach(parameters, &dimDg, &n);
    return n;
}

/***************************************
 * Get nth Parameter, folding in tuples.
 * Returns:
 *      Parameter*      nth Parameter
 *      NULL            not found, *pn gets incremented by the number
 *                      of Parameters
 */

struct GetNthParamCtx
{
    size_t nth;
    Parameter *param;
};

static int getNthParamDg(void *ctx, size_t n, Parameter *p)
{
    GetNthParamCtx *c = (GetNthParamCtx *)ctx;
    if (n == c->nth)
    {
        c->param = p;
        return 1;
    }
    return 0;
}

Parameter *Parameter::getNth(Parameters *parameters, size_t nth, size_t *)
{
    GetNthParamCtx ctx = { nth, NULL };
    int res = Parameter_foreach(parameters, &getNthParamDg, &ctx);
    return res ? ctx.param : NULL;
}

/***************************************
 * Expands tuples in args in depth first order. Calls
 * dg(void *ctx, size_t argidx, Parameter *arg) for each Parameter.
 * If dg returns !=0, stops and returns that value else returns 0.
 * Use this function to avoid the O(N + N^2/2) complexity of
 * calculating dim and calling N times getNth.
 */

int Parameter_foreach(Parameters *parameters, ForeachDg dg, void *ctx, size_t *pn)
{
    assert(dg);
    if (!parameters)
        return 0;

    size_t n = pn ? *pn : 0; // take over index
    int result = 0;
    for (size_t i = 0; i < parameters->length; i++)
    {
        Parameter *p = (*parameters)[i];
        Type *t = p->type->toBasetype();

        if (t->ty == Ttuple)
        {
            TypeTuple *tu = (TypeTuple *)t;
            result = Parameter_foreach(tu->arguments, dg, ctx, &n);
        }
        else
            result = dg(ctx, n++, p);

        if (result)
            break;
    }

    if (pn)
        *pn = n; // update index
    return result;
}


const char *Parameter::toChars()
{
    return ident ? ident->toChars() : "__anonymous_param";
}

/*********************************
 * Compute covariance of parameters `this` and `p`
 * as determined by the storage classes of both.
 * Params:
 *  p = Parameter to compare with
 * Returns:
 *  true = `this` can be used in place of `p`
 *  false = nope
 */
bool Parameter::isCovariant(bool returnByRef, const Parameter *p) const
{
    const StorageClass stc = STCref | STCin | STCout | STClazy;
    if ((this->storageClass & stc) != (p->storageClass & stc))
        return false;

    return isCovariantScope(returnByRef, this->storageClass, p->storageClass);
}

bool Parameter::isCovariantScope(bool returnByRef, StorageClass from, StorageClass to)
{
    if (from == to)
        return true;

    struct SR
    {
        /* Classification of 'scope-return-ref' possibilities
        */
        enum
        {
            SRNone,
            SRScope,
            SRReturnScope,
            SRRef,
            SRReturnRef,
            SRRefScope,
            SRReturnRef_Scope,
            SRRef_ReturnScope,
            SRMAX,
        };

        /* Shrinking the representation is necessary because StorageClass is so wide
         * Params:
         *   returnByRef = true if the function returns by ref
         *   stc = storage class of parameter
         */
        static unsigned buildSR(bool returnByRef, StorageClass stc)
        {
            unsigned result;
            StorageClass stc2 = stc & (STCref | STCscope | STCreturn);
            if (stc2 == 0)
                result = SRNone;
            else if (stc2 == STCref)
                result = SRRef;
            else if (stc2 == STCscope)
                result = SRScope;
            else if (stc2 == (STCscope | STCreturn))
                result = SRReturnScope;
            else if (stc2 == (STCref | STCreturn))
                result = SRReturnRef;
            else if (stc2 == (STCscope | STCref))
                result = SRRefScope;
            else if (stc2 == (STCscope | STCref | STCreturn))
                result = returnByRef ? SRReturnRef_Scope : SRRef_ReturnScope;
            else
                assert(0);
            return result;
        }

        static void covariantInit(bool covariant[SRMAX][SRMAX])
        {
            /* Initialize covariant[][] with this:

                 From\To           n   rs  s
                 None              X
                 ReturnScope       X   X
                 Scope             X   X   X

                 From\To           r   rr  rs  rr-s r-rs
                 Ref               X   X
                 ReturnRef             X
                 RefScope          X   X   X   X    X
                 ReturnRef-Scope       X       X
                 Ref-ReturnScope   X   X            X
            */
            for (int i = 0; i < SRMAX; i++)
            {
                covariant[i][i] = true;
                covariant[SRRefScope][i] = true;
            }
            covariant[SRReturnScope][SRNone]        = true;
            covariant[SRScope      ][SRNone]        = true;
            covariant[SRScope      ][SRReturnScope] = true;

            covariant[SRRef            ][SRReturnRef] = true;
            covariant[SRReturnRef_Scope][SRReturnRef] = true;
            covariant[SRRef_ReturnScope][SRRef      ] = true;
            covariant[SRRef_ReturnScope][SRReturnRef] = true;
        }
    };

    /* result is true if the 'from' can be used as a 'to'
     */

    if ((from ^ to) & STCref)               // differing in 'ref' means no covariance
        return false;

    static bool covariant[SR::SRMAX][SR::SRMAX];
    static bool init = false;
    if (!init)
    {
        SR::covariantInit(covariant);
        init = true;
    }

    return covariant[SR::buildSR(returnByRef, from)][SR::buildSR(returnByRef, to)];
}

/**
 * For printing two types with qualification when necessary.
 * Params:
 *    t1 = The first type to receive the type name for
 *    t2 = The second type to receive the type name for
 * Returns:
 *    The fully-qualified names of both types if the two type names are not the same,
 *    or the unqualified names of both types if the two type names are the same.
 */
void toAutoQualChars(const char **result, Type *t1, Type *t2)
{
    const char *s1 = t1->toChars();
    const char *s2 = t2->toChars();
    if (strcmp(s1, s2) == 0)
    {
        s1 = t1->toPrettyChars(true);
        s2 = t2->toPrettyChars(true);
    }
    result[0] = s1;
    result[1] = s2;
}
