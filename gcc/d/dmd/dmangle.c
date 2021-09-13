
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/mangle.c
 */

#include "root/dsystem.h"
#include "root/root.h"
#include "root/aav.h"

#include "mangle.h"
#include "init.h"
#include "declaration.h"
#include "aggregate.h"
#include "mtype.h"
#include "attrib.h"
#include "target.h"
#include "template.h"
#include "id.h"
#include "module.h"
#include "enum.h"
#include "expression.h"
#include "utf.h"

typedef int (*ForeachDg)(void *ctx, size_t paramidx, Parameter *param);
int Parameter_foreach(Parameters *parameters, ForeachDg dg, void *ctx, size_t *pn = NULL);

static const char *mangleChar[TMAX];

void initTypeMangle()
{
    mangleChar[Tarray] = "A";
    mangleChar[Tsarray] = "G";
    mangleChar[Taarray] = "H";
    mangleChar[Tpointer] = "P";
    mangleChar[Treference] = "R";
    mangleChar[Tfunction] = "F";
    mangleChar[Tident] = "I";
    mangleChar[Tclass] = "C";
    mangleChar[Tstruct] = "S";
    mangleChar[Tenum] = "E";
    mangleChar[Tdelegate] = "D";

    mangleChar[Tnone] = "n";
    mangleChar[Tvoid] = "v";
    mangleChar[Tint8] = "g";
    mangleChar[Tuns8] = "h";
    mangleChar[Tint16] = "s";
    mangleChar[Tuns16] = "t";
    mangleChar[Tint32] = "i";
    mangleChar[Tuns32] = "k";
    mangleChar[Tint64] = "l";
    mangleChar[Tuns64] = "m";
    mangleChar[Tint128] = "zi";
    mangleChar[Tuns128] = "zk";
    mangleChar[Tfloat32] = "f";
    mangleChar[Tfloat64] = "d";
    mangleChar[Tfloat80] = "e";

    mangleChar[Timaginary32] = "o";
    mangleChar[Timaginary64] = "p";
    mangleChar[Timaginary80] = "j";
    mangleChar[Tcomplex32] = "q";
    mangleChar[Tcomplex64] = "r";
    mangleChar[Tcomplex80] = "c";

    mangleChar[Tbool] = "b";
    mangleChar[Tchar] = "a";
    mangleChar[Twchar] = "u";
    mangleChar[Tdchar] = "w";

    // '@' shouldn't appear anywhere in the deco'd names
    mangleChar[Tinstance] = "@";
    mangleChar[Terror] = "@";
    mangleChar[Ttypeof] = "@";
    mangleChar[Ttuple] = "B";
    mangleChar[Tslice] = "@";
    mangleChar[Treturn] = "@";
    mangleChar[Tvector] = "@";
    mangleChar[Ttraits] = "@";
    mangleChar[Tmixin] = "@";
    mangleChar[Tnoreturn] = "@";    // becomes 'Nn'

    mangleChar[Tnull] = "n";    // same as TypeNone

    for (size_t i = 0; i < TMAX; i++)
    {
        if (!mangleChar[i])
            fprintf(stderr, "ty = %llu\n", (ulonglong)i);
        assert(mangleChar[i]);
    }
}

/*********************************
 * Mangling for mod.
 */
void MODtoDecoBuffer(OutBuffer *buf, MOD mod)
{
    switch (mod)
    {
        case 0:
            break;
        case MODconst:
            buf->writeByte('x');
            break;
        case MODimmutable:
            buf->writeByte('y');
            break;
        case MODshared:
            buf->writeByte('O');
            break;
        case MODshared | MODconst:
            buf->writestring("Ox");
            break;
        case MODwild:
            buf->writestring("Ng");
            break;
        case MODwildconst:
            buf->writestring("Ngx");
            break;
        case MODshared | MODwild:
            buf->writestring("ONg");
            break;
        case MODshared | MODwildconst:
            buf->writestring("ONgx");
            break;
        default:
            assert(0);
    }
}

class Mangler : public Visitor
{
public:
    AA *types;
    AA *idents;
    OutBuffer *buf;

    Mangler(OutBuffer *buf)
    {
        this->types = NULL;
        this->idents = NULL;
        this->buf = buf;
    }

    /**
    * writes a back reference with the relative position encoded with base 26
    *  using upper case letters for all digits but the last digit which uses
    *  a lower case letter.
    * The decoder has to look up the referenced position to determine
    *  whether the back reference is an identifier (starts with a digit)
    *  or a type (starts with a letter).
    *
    * Params:
    *  pos           = relative position to encode
    */
    void writeBackRef(size_t pos)
    {
        buf->writeByte('Q');
        const size_t base = 26;
        size_t mul = 1;
        while (pos >= mul * base)
            mul *= base;
        while (mul >= base)
        {
            unsigned char dig = (unsigned char)(pos / mul);
            buf->writeByte('A' + dig);
            pos -= dig * mul;
            mul /= base;
        }
        buf->writeByte('a' + (unsigned char)pos);
    }

    /**
    * Back references a non-basic type
    *
    * The encoded mangling is
    *       'Q' <relative position of first occurrence of type>
    *
    * Params:
    *  t = the type to encode via back referencing
    *
    * Returns:
    *  true if the type was found. A back reference has been encoded.
    *  false if the type was not found. The current position is saved for later back references.
    */
    bool backrefType(Type *t)
    {
        if (!t->isTypeBasic())
        {
            size_t *p = (size_t *)dmd_aaGet(&types, (void *)t);
            if (*p)
            {
                writeBackRef(buf->length() - *p);
                return true;
            }
            *p = buf->length();
        }
        return false;
    }

    /**
    * Back references a single identifier
    *
    * The encoded mangling is
    *       'Q' <relative position of first occurrence of type>
    *
    * Params:
    *  id = the identifier to encode via back referencing
    *
    * Returns:
    *  true if the identifier was found. A back reference has been encoded.
    *  false if the identifier was not found. The current position is saved for later back references.
    */
    bool backrefIdentifier(Identifier *id)
    {
        size_t *p = (size_t *)dmd_aaGet(&idents, (void *)id);
        if (*p)
        {
            writeBackRef(buf->length() - *p);
            return true;
        }
        *p = buf->length();
        return false;
    }

    void mangleSymbol(Dsymbol *s)
    {
        s->accept(this);
    }

    void mangleType(Type *t)
    {
        if (!backrefType(t))
            t->accept(this);
    }

    void mangleIdentifier(Identifier *id, Dsymbol *s)
    {
        if (!backrefIdentifier(id))
            toBuffer(id->toChars(), s);
    }

    ////////////////////////////////////////////////////////////////////////////

    /**************************************************
     * Type mangling
     */

    void visitWithMask(Type *t, unsigned char modMask)
    {
        if (modMask != t->mod)
        {
            MODtoDecoBuffer(buf, t->mod);
        }
        mangleType(t);
    }

    void visit(Type *t)
    {
        buf->writestring(mangleChar[t->ty]);
    }

    void visit(TypeNext *t)
    {
        visit((Type *)t);
        visitWithMask(t->next, t->mod);
    }

    void visit(TypeVector *t)
    {
        buf->writestring("Nh");
        visitWithMask(t->basetype, t->mod);
    }

    void visit(TypeSArray *t)
    {
        visit((Type *)t);
        if (t->dim)
            buf->print(t->dim->toInteger());
        if (t->next)
            visitWithMask(t->next, t->mod);
    }

    void visit(TypeDArray *t)
    {
        visit((Type *)t);
        if (t->next)
            visitWithMask(t->next, t->mod);
    }

    void visit(TypeAArray *t)
    {
        visit((Type *)t);
        visitWithMask(t->index, 0);
        visitWithMask(t->next, t->mod);
    }

    void visit(TypeFunction *t)
    {
        //printf("TypeFunction::toDecoBuffer() t = %p %s\n", t, t->toChars());
        //static int nest; if (++nest == 50) *(char*)0=0;

        mangleFuncType(t, t, t->mod, t->next);
    }

    void mangleFuncType(TypeFunction *t, TypeFunction *ta, unsigned char modMask, Type *tret)
    {
        //printf("mangleFuncType() %s\n", t->toChars());
        if (t->inuse && tret)
        {
            // printf("TypeFunction.mangleFuncType() t = %s inuse\n", t->toChars());
            t->inuse = 2;       // flag error to caller
            return;
        }
        t->inuse++;

        if (modMask != t->mod)
            MODtoDecoBuffer(buf, t->mod);

        unsigned char mc;
        switch (t->linkage)
        {
            case LINKd:             mc = 'F';       break;
            case LINKc:             mc = 'U';       break;
            case LINKwindows:       mc = 'W';       break;
            case LINKcpp:           mc = 'R';       break;
            case LINKobjc:          mc = 'Y';       break;
            default:
                assert(0);
        }
        buf->writeByte(mc);

        if (ta->purity || ta->isnothrow || ta->isnogc || ta->isproperty || ta->isref || ta->trust || ta->isreturn || ta->isscope)
        {
            if (ta->purity)
                buf->writestring("Na");
            if (ta->isnothrow)
                buf->writestring("Nb");
            if (ta->isref)
                buf->writestring("Nc");
            if (ta->isproperty)
                buf->writestring("Nd");
            if (ta->isnogc)
                buf->writestring("Ni");
            if (ta->isreturn)
                buf->writestring("Nj");
            if (ta->isscope && !ta->isreturn && !ta->isscopeinferred)
                buf->writestring("Nl");
            switch (ta->trust)
            {
                case TRUSTtrusted:
                    buf->writestring("Ne");
                    break;
                case TRUSTsafe:
                    buf->writestring("Nf");
                    break;
                default:
                    break;
            }
        }

        // Write argument types
        paramsToDecoBuffer(t->parameterList.parameters);
        //if (buf->slice().ptr[buf->length() - 1] == '@') halt();
        buf->writeByte('Z' - t->parameterList.varargs);   // mark end of arg list
        if (tret != NULL)
            visitWithMask(tret, 0);

        t->inuse--;
    }

    void visit(TypeIdentifier *t)
    {
        visit((Type *)t);
        const char *name = t->ident->toChars();
        size_t len = strlen(name);
        buf->print(len);
        buf->writestring(name);
    }

    void visit(TypeEnum *t)
    {
        visit((Type *)t);
        mangleSymbol(t->sym);
    }

    void visit(TypeStruct *t)
    {
        //printf("TypeStruct::toDecoBuffer('%s') = '%s'\n", t->toChars(), name);
        visit((Type *)t);
        mangleSymbol(t->sym);
    }

    void visit(TypeClass *t)
    {
        //printf("TypeClass::toDecoBuffer('%s' mod=%x) = '%s'\n", t->toChars(), mod, name);
        visit((Type *)t);
        mangleSymbol(t->sym);
    }

    void visit(TypeTuple *t)
    {
        //printf("TypeTuple::toDecoBuffer() t = %p, %s\n", t, t->toChars());
        visit((Type *)t);
        paramsToDecoBuffer(t->arguments);
        buf->writeByte('Z');
    }

    void visit(TypeNull *t)
    {
        visit((Type *)t);
    }

    void visit(TypeNoreturn *)
    {
        buf->writestring("Nn");
    }

    ////////////////////////////////////////////////////////////////////////////

    void mangleDecl(Declaration *sthis)
    {
        mangleParent(sthis);

        assert(sthis->ident);
        mangleIdentifier(sthis->ident, sthis);
        if (FuncDeclaration *fd = sthis->isFuncDeclaration())
        {
            mangleFunc(fd, false);
        }
        else if (sthis->type)
        {
            visitWithMask(sthis->type, 0);
        }
        else
            assert(0);
    }

    void mangleParent(Dsymbol *s)
    {
        Dsymbol *p;
        if (TemplateInstance *ti = s->isTemplateInstance())
            p = ti->isTemplateMixin() ? ti->parent : ti->tempdecl->parent;
        else
            p = s->parent;

        if (p)
        {
            mangleParent(p);
            TemplateInstance *ti = p->isTemplateInstance();
            if (ti && !ti->isTemplateMixin())
            {
                mangleTemplateInstance(ti);
            }
            else if (p->getIdent())
            {
                mangleIdentifier(p->ident, s);
                if (FuncDeclaration *f = p->isFuncDeclaration())
                    mangleFunc(f, true);
            }
            else
                buf->writeByte('0');
        }
    }

    void mangleFunc(FuncDeclaration *fd, bool inParent)
    {
        //printf("deco = '%s'\n", fd->type->deco ? fd->type->deco : "null");
        //printf("fd->type = %s\n", fd->type->toChars());
        if (fd->needThis() || fd->isNested())
            buf->writeByte('M');
        if (inParent)
        {
            TypeFunction *tf = (TypeFunction *)fd->type;
            TypeFunction *tfo = (TypeFunction *)fd->originalType;
            mangleFuncType(tf, tfo, 0, NULL);
        }
        else if (fd->type)
        {
            visitWithMask(fd->type, 0);
        }
        else
        {
            printf("[%s] %s no type\n", fd->loc.toChars(), fd->toChars());
            assert(0);  // don't mangle function until semantic3 done.
        }
    }

    /************************************************************
     * Write length prefixed string to buf.
     */
    void toBuffer(const char *id, Dsymbol *s)
    {
        size_t len = strlen(id);
        if (buf->length() + len >= 8 * 1024 * 1024) // 8 megs ought be enough for anyone
            s->error("excessive length %llu for symbol, possible recursive expansion?", buf->length() + len);
        else
        {
            buf->print(len);
            buf->write(id, len);
        }
    }

    static const char *externallyMangledIdentifier(Declaration *d)
    {
        if (!d->parent || d->parent->isModule() || d->linkage == LINKcpp) // if at global scope
        {
            switch (d->linkage)
            {
                case LINKd:
                    break;
                case LINKc:
                case LINKwindows:
                case LINKobjc:
                    return d->ident->toChars();
                case LINKcpp:
                    return target.cpp.toMangle(d);
                case LINKdefault:
                    d->error("forward declaration");
                    return d->ident->toChars();
                default:
                    fprintf(stderr, "'%s', linkage = %d\n", d->toChars(), d->linkage);
                    assert(0);
            }
        }
        return NULL;
    }

    void visit(Declaration *d)
    {
        //printf("Declaration::mangle(this = %p, '%s', parent = '%s', linkage = %d)\n",
        //        d, d->toChars(), d->parent ? d->parent->toChars() : "null", d->linkage);
        if (const char *id = externallyMangledIdentifier(d))
        {
            buf->writestring(id);
            return;
        }
        buf->writestring("_D");
        mangleDecl(d);
    }

    /******************************************************************************
     * Normally FuncDeclaration and FuncAliasDeclaration have overloads.
     * If and only if there is no overloads, mangle() could return
     * exact mangled name.
     *
     *      module test;
     *      void foo(long) {}           // _D4test3fooFlZv
     *      void foo(string) {}         // _D4test3fooFAyaZv
     *
     *      // from FuncDeclaration::mangle().
     *      pragma(msg, foo.mangleof);  // prints unexact mangled name "4test3foo"
     *                                  // by calling Dsymbol::mangle()
     *
     *      // from FuncAliasDeclaration::mangle()
     *      pragma(msg, __traits(getOverloads, test, "foo")[0].mangleof);  // "_D4test3fooFlZv"
     *      pragma(msg, __traits(getOverloads, test, "foo")[1].mangleof);  // "_D4test3fooFAyaZv"
     *
     * If a function has no overloads, .mangleof property still returns exact mangled name.
     *
     *      void bar() {}
     *      pragma(msg, bar.mangleof);  // still prints "_D4test3barFZv"
     *                                  // by calling FuncDeclaration::mangleExact().
     */
    void visit(FuncDeclaration *fd)
    {
        if (fd->isUnique())
            mangleExact(fd);
        else
            visit((Dsymbol *)fd);
    }

    // ditto
    void visit(FuncAliasDeclaration *fd)
    {
        FuncDeclaration *f = fd->toAliasFunc();
        FuncAliasDeclaration *fa = f->isFuncAliasDeclaration();
        if (!fd->hasOverloads && !fa)
        {
            mangleExact(f);
            return;
        }
        if (fa)
        {
            mangleSymbol(fa);
            return;
        }
        visit((Dsymbol *)fd);
    }

    void visit(OverDeclaration *od)
    {
        if (od->overnext)
        {
            visit((Dsymbol *)od);
            return;
        }

        if (FuncDeclaration *fd = od->aliassym->isFuncDeclaration())
        {
            if (!od->hasOverloads || fd->isUnique())
            {
                mangleExact(fd);
                return;
            }
        }
        if (TemplateDeclaration *td = od->aliassym->isTemplateDeclaration())
        {
            if (!od->hasOverloads || td->overnext == NULL)
            {
                mangleSymbol(td);
                return;
            }
        }
        visit((Dsymbol *)od);
    }

    void mangleExact(FuncDeclaration *fd)
    {
        assert(!fd->isFuncAliasDeclaration());

        if (fd->mangleOverride.length)
        {
            buf->writestring(fd->mangleOverride.ptr);
            return;
        }

        if (fd->isMain())
        {
            buf->writestring("_Dmain");
            return;
        }

        if (fd->isWinMain() || fd->isDllMain() || fd->ident == Id::tls_get_addr)
        {
            buf->writestring(fd->ident->toChars());
            return;
        }

        visit((Declaration *)fd);
    }

    void visit(VarDeclaration *vd)
    {
        if (vd->mangleOverride.length)
        {
            buf->writestring(vd->mangleOverride.ptr);
            return;
        }

        visit((Declaration *)vd);
    }

    void visit(AggregateDeclaration *ad)
    {
        ClassDeclaration *cd = ad->isClassDeclaration();
        Dsymbol *parentsave = ad->parent;
        if (cd)
        {
            /* These are reserved to the compiler, so keep simple
             * names for them.
             */
            if ((cd->ident == Id::Exception && cd->parent->ident == Id::object) ||
                cd->ident == Id::TypeInfo ||
                cd->ident == Id::TypeInfo_Struct ||
                cd->ident == Id::TypeInfo_Class ||
                cd->ident == Id::TypeInfo_Tuple ||
                cd == ClassDeclaration::object ||
                cd == Type::typeinfoclass ||
                cd == Module::moduleinfo ||
                strncmp(cd->ident->toChars(), "TypeInfo_", 9) == 0)
            {
                // Don't mangle parent
                ad->parent = NULL;
            }
        }

        visit((Dsymbol *)ad);

        ad->parent = parentsave;
    }

    void visit(TemplateInstance *ti)
    {
        if (!ti->tempdecl)
            ti->error("is not defined");
        else
            mangleParent(ti);

        if (ti->isTemplateMixin() && ti->ident)
            mangleIdentifier(ti->ident, ti);
        else
            mangleTemplateInstance(ti);
    }

    void mangleTemplateInstance(TemplateInstance *ti)
    {
        TemplateDeclaration *tempdecl = ti->tempdecl->isTemplateDeclaration();
        assert(tempdecl);

        // Use "__U" for the symbols declared inside template constraint.
        const char T = ti->members ? 'T' : 'U';
        buf->printf("__%c", T);
        mangleIdentifier(tempdecl->ident, tempdecl);

        Objects *args = ti->tiargs;
        size_t nparams = tempdecl->parameters->length - (tempdecl->isVariadic() ? 1 : 0);
        for (size_t i = 0; i < args->length; i++)
        {
            RootObject *o = (*args)[i];
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);
            Tuple *va = isTuple(o);
            //printf("\to [%d] %p ta %p ea %p sa %p va %p\n", i, o, ta, ea, sa, va);
            if (i < nparams && (*tempdecl->parameters)[i]->specialization())
                buf->writeByte('H'); // https://issues.dlang.org/show_bug.cgi?id=6574
            if (ta)
            {
                buf->writeByte('T');
                visitWithMask(ta, 0);
            }
            else if (ea)
            {
                // Don't interpret it yet, it might actually be an alias template parameter.
                // Only constfold manifest constants, not const/immutable lvalues, see https://issues.dlang.org/show_bug.cgi?id=17339.
                const bool keepLvalue = true;
                ea = ea->optimize(WANTvalue, keepLvalue);
                if (ea->op == TOKvar)
                {
                    sa = ((VarExp *)ea)->var;
                    ea = NULL;
                    goto Lsa;
                }
                if (ea->op == TOKthis)
                {
                    sa = ((ThisExp *)ea)->var;
                    ea = NULL;
                    goto Lsa;
                }
                if (ea->op == TOKfunction)
                {
                    if (((FuncExp *)ea)->td)
                        sa = ((FuncExp *)ea)->td;
                    else
                        sa = ((FuncExp *)ea)->fd;
                    ea = NULL;
                    goto Lsa;
                }
                buf->writeByte('V');
                if (ea->op == TOKtuple)
                {
                    ea->error("tuple is not a valid template value argument");
                    continue;
                }
                // Now that we know it is not an alias, we MUST obtain a value
                unsigned olderr = global.errors;
                ea = ea->ctfeInterpret();
                if (ea->op == TOKerror || olderr != global.errors)
                    continue;

                /* Use type mangling that matches what it would be for a function parameter
                */
                visitWithMask(ea->type, 0);
                ea->accept(this);
            }
            else if (sa)
            {
            Lsa:
                sa = sa->toAlias();
                if (Declaration *d = sa->isDeclaration())
                {
                    if (FuncAliasDeclaration *fad = d->isFuncAliasDeclaration())
                        d = fad->toAliasFunc();
                    if (d->mangleOverride.length)
                    {
                        buf->writeByte('X');
                        toBuffer(d->mangleOverride.ptr, d);
                        continue;
                    }
                    if (const char *id = externallyMangledIdentifier(d))
                    {
                        buf->writeByte('X');
                        toBuffer(id, d);
                        continue;
                    }
                    if (!d->type || !d->type->deco)
                    {
                        ti->error("forward reference of %s %s", d->kind(), d->toChars());
                        continue;
                    }
                }
                buf->writeByte('S');
                mangleSymbol(sa);
            }
            else if (va)
            {
                assert(i + 1 == args->length); // must be last one
                args = &va->objects;
                i = -(size_t)1;
            }
            else
                assert(0);
        }
        buf->writeByte('Z');
    }

    void visit(Dsymbol *s)
    {
        mangleParent(s);
        if (s->ident)
            mangleIdentifier(s->ident, s);
        else
            toBuffer(s->toChars(), s);
        //printf("Dsymbol::mangle() %s = %s\n", s->toChars(), id);
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(Expression *e)
    {
        e->error("expression %s is not a valid template value argument", e->toChars());
    }

    void visit(IntegerExp *e)
    {
        if ((sinteger_t)e->value < 0)
        {
            buf->writeByte('N');
            buf->print(-e->value);
        }
        else
        {
            buf->writeByte('i');
            buf->print(e->value);
        }
    }

    void visit(RealExp *e)
    {
        buf->writeByte('e');
        realToMangleBuffer(e->value);
    }

    void realToMangleBuffer(real_t value)
    {
        /* Rely on %A to get portable mangling.
         * Must munge result to get only identifier characters.
         *
         * Possible values from %A  => mangled result
         * NAN                      => NAN
         * -INF                     => NINF
         * INF                      => INF
         * -0X1.1BC18BA997B95P+79   => N11BC18BA997B95P79
         * 0X1.9P+2                 => 19P2
         */

        if (CTFloat::isNaN(value))
            buf->writestring("NAN");        // no -NAN bugs
        else if (CTFloat::isInfinity(value))
            buf->writestring(value < CTFloat::zero ? "NINF" : "INF");
        else
        {
            const size_t BUFFER_LEN = 36;
            char buffer[BUFFER_LEN];
            size_t n = CTFloat::sprint(buffer, 'A', value);
            assert(n < BUFFER_LEN);
            for (size_t i = 0; i < n; i++)
            {
                char c = buffer[i];
                switch (c)
                {
                    case '-':
                        buf->writeByte('N');
                        break;

                    case '+':
                    case 'X':
                    case '.':
                        break;

                    case '0':
                        if (i < 2)
                            break;          // skip leading 0X
                        /* fall through */
                    default:
                        buf->writeByte(c);
                        break;
                }
            }
        }
    }

    void visit(ComplexExp *e)
    {
        buf->writeByte('c');
        realToMangleBuffer(e->toReal());
        buf->writeByte('c');        // separate the two
        realToMangleBuffer(e->toImaginary());
    }

    void visit(NullExp *)
    {
        buf->writeByte('n');
    }

    void visit(StringExp *e)
    {
        char m;
        OutBuffer tmp;
        utf8_t *q;
        size_t qlen;

        /* Write string in UTF-8 format
         */
        switch (e->sz)
        {
            case 1:
                m = 'a';
                q = (utf8_t *)e->string;
                qlen = e->len;
                break;

            case 2:
                m = 'w';
                for (size_t u = 0; u < e->len; )
                {
                    unsigned c;
                    const char *p = utf_decodeWchar((unsigned short *)e->string, e->len, &u, &c);
                    if (p)
                        e->error("%s", p);
                    else
                        tmp.writeUTF8(c);
                }
                q = (utf8_t *)tmp.slice().ptr;
                qlen = tmp.length();
                break;

            case 4:
                m = 'd';
                for (size_t u = 0; u < e->len; u++)
                {
                    unsigned c = ((unsigned *)e->string)[u];
                    if (!utf_isValidDchar(c))
                        e->error("invalid UCS-32 char \\U%08x", c);
                    else
                        tmp.writeUTF8(c);
                }
                q = (utf8_t *)tmp.slice().ptr;
                qlen = tmp.length();
                break;

            default:
                assert(0);
        }
        buf->reserve(1 + 11 + 2 * qlen);
        buf->writeByte(m);
        buf->print(qlen);
        buf->writeByte('_');    // nbytes <= 11

        for (utf8_t *p = (utf8_t *)buf->slice().ptr + buf->length(), *pend = p + 2 * qlen;
             p < pend; p += 2, ++q)
        {
            utf8_t hi = *q >> 4 & 0xF;
            p[0] = (utf8_t)(hi < 10 ? hi + '0' : hi - 10 + 'a');
            utf8_t lo = *q & 0xF;
            p[1] = (utf8_t)(lo < 10 ? lo + '0' : lo - 10 + 'a');
        }
        buf->setsize(buf->length() + 2 * qlen);
    }

    void visit(ArrayLiteralExp *e)
    {
        size_t dim = e->elements ? e->elements->length : 0;
        buf->writeByte('A');
        buf->print(dim);
        for (size_t i = 0; i < dim; i++)
        {
            e->getElement(i)->accept(this);
        }
    }

    void visit(AssocArrayLiteralExp *e)
    {
        size_t dim = e->keys->length;
        buf->writeByte('A');
        buf->print(dim);
        for (size_t i = 0; i < dim; i++)
        {
            (*e->keys)[i]->accept(this);
            (*e->values)[i]->accept(this);
        }
    }

    void visit(StructLiteralExp *e)
    {
        size_t dim = e->elements ? e->elements->length : 0;
        buf->writeByte('S');
        buf->print(dim);
        for (size_t i = 0; i < dim; i++)
        {
            Expression *ex = (*e->elements)[i];
            if (ex)
                ex->accept(this);
            else
                buf->writeByte('v');        // 'v' for void
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    void paramsToDecoBuffer(Parameters *parameters)
    {
        //printf("Parameter::paramsToDecoBuffer()\n");
        Parameter_foreach(parameters, &paramsToDecoBufferDg, (void *)this);
    }

    static int paramsToDecoBufferDg(void *ctx, size_t, Parameter *p)
    {
        p->accept((Visitor *)ctx);
        return 0;
    }

    void visit(Parameter *p)
    {
        if (p->storageClass & STCscope && !(p->storageClass & STCscopeinferred))
            buf->writeByte('M');
        // 'return inout ref' is the same as 'inout ref'
        if ((p->storageClass & (STCreturn | STCwild)) == STCreturn)
            buf->writestring("Nk");
        switch (p->storageClass & (STCin | STCout | STCref | STClazy))
        {
            case 0:
            case STCin:
                break;
            case STCout:
                buf->writeByte('J');
                break;
            case STCref:
                buf->writeByte('K');
                break;
            case STClazy:
                buf->writeByte('L');
                break;
            default:
                assert(0);
        }
        visitWithMask(p->type, 0);
    }
};

/******************************************************************************
 * Returns exact mangled name of function.
 */
const char *mangleExact(FuncDeclaration *fd)
{
    if (!fd->mangleString)
    {
        OutBuffer buf;
        Mangler v(&buf);
        v.mangleExact(fd);
        fd->mangleString = buf.extractChars();
    }
    return fd->mangleString;
}

void mangleToBuffer(Type *t, OutBuffer *buf)
{
    Mangler v(buf);
    v.visitWithMask(t, 0);
}

void mangleToBuffer(Expression *e, OutBuffer *buf)
{
    Mangler v(buf);
    e->accept(&v);
}

void mangleToBuffer(Dsymbol *s, OutBuffer *buf)
{
    Mangler v(buf);
    s->accept(&v);
}

void mangleToBuffer(TemplateInstance *ti, OutBuffer *buf)
{
    Mangler v(buf);
    v.mangleTemplateInstance(ti);
}

/**********************************************
 * Convert a string representing a type (the deco) and
 * return its equivalent Type.
 * Params:
 *      deco = string containing the deco
 * Returns:
 *      null for failed to convert
 *      Type for succeeded
 */

Type *decoToType(const char *deco)
{
    if (!deco)
        return NULL;

    //printf("decoToType(): %s\n", deco)
    if (StringValue *sv = Type::stringtable.lookup(deco, strlen(deco)))
    {
        if (sv->ptrvalue)
        {
            Type *t = (Type *)sv->ptrvalue;
            assert(t->deco);
            return t;
        }
    }
    return NULL;
}
