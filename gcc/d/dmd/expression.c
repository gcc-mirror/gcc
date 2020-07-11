
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/expression.c
 */

#include "root/dsystem.h"
#include "root/rmem.h"
#include "root/root.h"

#include "errors.h"
#include "mtype.h"
#include "init.h"
#include "expression.h"
#include "template.h"
#include "utf.h"
#include "enum.h"
#include "scope.h"
#include "statement.h"
#include "declaration.h"
#include "aggregate.h"
#include "import.h"
#include "id.h"
#include "dsymbol.h"
#include "module.h"
#include "attrib.h"
#include "hdrgen.h"
#include "parse.h"
#include "doc.h"
#include "root/aav.h"
#include "nspace.h"
#include "ctfe.h"
#include "target.h"

bool walkPostorder(Expression *e, StoppableVisitor *v);
bool checkParamArgumentEscape(Scope *sc, FuncDeclaration *fdc, Identifier *par, Expression *arg, bool gag);
bool checkAccess(AggregateDeclaration *ad, Loc loc, Scope *sc, Dsymbol *smember);
VarDeclaration *copyToTemp(StorageClass stc, const char *name, Expression *e);
Expression *extractSideEffect(Scope *sc, const char *name, Expression **e0, Expression *e, bool alwaysCopy = false);
char *MODtoChars(MOD mod);
bool MODimplicitConv(MOD modfrom, MOD modto);
MOD MODmerge(MOD mod1, MOD mod2);
void MODMatchToBuffer(OutBuffer *buf, unsigned char lhsMod, unsigned char rhsMod);
Expression *trySemantic(Expression *e, Scope *sc);
Expression *semantic(Expression *e, Scope *sc);
Expression *semanticX(DotIdExp *exp, Scope *sc);
Expression *semanticY(DotIdExp *exp, Scope *sc, int flag);
Expression *semanticY(DotTemplateInstanceExp *exp, Scope *sc, int flag);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);
bool checkUnsafeAccess(Scope *sc, Expression *e, bool readonly, bool printmsg);

/*************************************************************
 * Given var, we need to get the
 * right 'this' pointer if var is in an outer class, but our
 * existing 'this' pointer is in an inner class.
 * Input:
 *      e1      existing 'this'
 *      ad      struct or class we need the correct 'this' for
 *      var     the specific member of ad we're accessing
 */

Expression *getRightThis(Loc loc, Scope *sc, AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int flag = 0)
{
    //printf("\ngetRightThis(e1 = %s, ad = %s, var = %s)\n", e1->toChars(), ad->toChars(), var->toChars());
 L1:
    Type *t = e1->type->toBasetype();
    //printf("e1->type = %s, var->type = %s\n", e1->type->toChars(), var->type->toChars());

    /* If e1 is not the 'this' pointer for ad
     */
    if (ad &&
        !(t->ty == Tpointer && t->nextOf()->ty == Tstruct &&
          ((TypeStruct *)t->nextOf())->sym == ad)
        &&
        !(t->ty == Tstruct &&
          ((TypeStruct *)t)->sym == ad)
       )
    {
        ClassDeclaration *cd = ad->isClassDeclaration();
        ClassDeclaration *tcd = t->isClassHandle();

        /* e1 is the right this if ad is a base class of e1
         */
        if (!cd || !tcd ||
            !(tcd == cd || cd->isBaseOf(tcd, NULL))
           )
        {
            /* Only classes can be inner classes with an 'outer'
             * member pointing to the enclosing class instance
             */
            if (tcd && tcd->isNested())
            {
                /* e1 is the 'this' pointer for an inner class: tcd.
                 * Rewrite it as the 'this' pointer for the outer class.
                 */

                e1 = new DotVarExp(loc, e1, tcd->vthis);
                e1->type = tcd->vthis->type;
                e1->type = e1->type->addMod(t->mod);
                // Do not call checkNestedRef()
                //e1 = semantic(e1, sc);

                // Skip up over nested functions, and get the enclosing
                // class type.
                int n = 0;
                Dsymbol *s;
                for (s = tcd->toParent();
                     s && s->isFuncDeclaration();
                     s = s->toParent())
                {
                    FuncDeclaration *f = s->isFuncDeclaration();
                    if (f->vthis)
                    {
                        //printf("rewriting e1 to %s's this\n", f->toChars());
                        n++;
                        e1 = new VarExp(loc, f->vthis);
                    }
                    else
                    {
                        e1->error("need 'this' of type %s to access member %s"
                                  " from static function %s",
                            ad->toChars(), var->toChars(), f->toChars());
                        e1 = new ErrorExp();
                        return e1;
                    }
                }
                if (s && s->isClassDeclaration())
                {
                    e1->type = s->isClassDeclaration()->type;
                    e1->type = e1->type->addMod(t->mod);
                    if (n > 1)
                        e1 = semantic(e1, sc);
                }
                else
                    e1 = semantic(e1, sc);
                goto L1;
            }

            /* Can't find a path from e1 to ad
             */
            if (flag)
                return NULL;
            e1->error("this for %s needs to be type %s not type %s",
                var->toChars(), ad->toChars(), t->toChars());
            return new ErrorExp();
        }
    }
    return e1;
}

/*****************************************
 * Determine if 'this' is available.
 * If it is, return the FuncDeclaration that has it.
 */

FuncDeclaration *hasThis(Scope *sc)
{
    //printf("hasThis()\n");
    Dsymbol *p = sc->parent;
    while (p && p->isTemplateMixin())
        p = p->parent;
    FuncDeclaration *fdthis = p ? p->isFuncDeclaration() : NULL;
    //printf("fdthis = %p, '%s'\n", fdthis, fdthis ? fdthis->toChars() : "");

    // Go upwards until we find the enclosing member function
    FuncDeclaration *fd = fdthis;
    while (1)
    {
        if (!fd)
        {
            goto Lno;
        }
        if (!fd->isNested())
            break;

        Dsymbol *parent = fd->parent;
        while (1)
        {
            if (!parent)
                goto Lno;
            TemplateInstance *ti = parent->isTemplateInstance();
            if (ti)
                parent = ti->parent;
            else
                break;
        }
        fd = parent->isFuncDeclaration();
    }

    if (!fd->isThis())
    {   //printf("test '%s'\n", fd->toChars());
        goto Lno;
    }

    assert(fd->vthis);
    return fd;

Lno:
    return NULL;                // don't have 'this' available
}

bool isNeedThisScope(Scope *sc, Declaration *d)
{
    if (sc->intypeof == 1)
        return false;

    AggregateDeclaration *ad = d->isThis();
    if (!ad)
        return false;
    //printf("d = %s, ad = %s\n", d->toChars(), ad->toChars());

    for (Dsymbol *s = sc->parent; s; s = s->toParent2())
    {
        //printf("\ts = %s %s, toParent2() = %p\n", s->kind(), s->toChars(), s->toParent2());
        if (AggregateDeclaration *ad2 = s->isAggregateDeclaration())
        {
            if (ad2 == ad)
                return false;
            else if (ad2->isNested())
                continue;
            else
                return true;
        }
        if (FuncDeclaration *f = s->isFuncDeclaration())
        {
            if (f->isMember2())
                break;
        }
    }
    return true;
}

/***************************************
 * Pull out any properties.
 */

Expression *resolvePropertiesX(Scope *sc, Expression *e1, Expression *e2 = NULL)
{
    //printf("resolvePropertiesX, e1 = %s %s, e2 = %s\n", Token::toChars(e1->op), e1->toChars(), e2 ? e2->toChars() : NULL);
    Loc loc = e1->loc;

    OverloadSet *os;
    Dsymbol *s;
    Objects *tiargs;
    Type *tthis;
    if (e1->op == TOKdot)
    {
        DotExp *de = (DotExp *)e1;
        if (de->e2->op == TOKoverloadset)
        {
            tiargs = NULL;
            tthis  = de->e1->type;
            os = ((OverExp *)de->e2)->vars;
            goto Los;
        }
    }
    else if (e1->op == TOKoverloadset)
    {
        tiargs = NULL;
        tthis  = NULL;
        os = ((OverExp *)e1)->vars;
    Los:
        assert(os);
        FuncDeclaration *fd = NULL;
        if (e2)
        {
            e2 = semantic(e2, sc);
            if (e2->op == TOKerror)
                return new ErrorExp();
            e2 = resolveProperties(sc, e2);

            Expressions a;
            a.push(e2);

            for (size_t i = 0; i < os->a.length; i++)
            {
                FuncDeclaration *f = resolveFuncCall(loc, sc, os->a[i], tiargs, tthis, &a, 1);
                if (f)
                {
                    if (f->errors)
                        return new ErrorExp();
                    fd = f;
                    assert(fd->type->ty == Tfunction);
                }
            }
            if (fd)
            {
                Expression *e = new CallExp(loc, e1, e2);
                return semantic(e, sc);
            }
        }
        {
            for (size_t i = 0; i < os->a.length; i++)
            {
                FuncDeclaration *f = resolveFuncCall(loc, sc, os->a[i], tiargs, tthis, NULL, 1);
                if (f)
                {
                    if (f->errors)
                        return new ErrorExp();
                    fd = f;
                    assert(fd->type->ty == Tfunction);
                    TypeFunction *tf = (TypeFunction *)fd->type;
                    if (!tf->isref && e2)
                        goto Leproplvalue;
                }
            }
            if (fd)
            {
                Expression *e = new CallExp(loc, e1);
                if (e2)
                    e = new AssignExp(loc, e, e2);
                return semantic(e, sc);
            }
        }
        if (e2)
            goto Leprop;
    }
    else if (e1->op == TOKdotti)
    {
        DotTemplateInstanceExp* dti = (DotTemplateInstanceExp *)e1;
        if (!dti->findTempDecl(sc))
            goto Leprop;
        if (!dti->ti->semanticTiargs(sc))
            goto Leprop;
        tiargs = dti->ti->tiargs;
        tthis  = dti->e1->type;
        if ((os = dti->ti->tempdecl->isOverloadSet()) != NULL)
            goto Los;
        if ((s = dti->ti->tempdecl) != NULL)
            goto Lfd;
    }
    else if (e1->op == TOKdottd)
    {
        DotTemplateExp *dte = (DotTemplateExp *)e1;
        s      = dte->td;
        tiargs = NULL;
        tthis  = dte->e1->type;
        goto Lfd;
    }
    else if (e1->op == TOKscope)
    {
        s = ((ScopeExp *)e1)->sds;
        TemplateInstance *ti = s->isTemplateInstance();
        if (ti && !ti->semanticRun && ti->tempdecl)
        {
            //assert(ti->needsTypeInference(sc));
            if (!ti->semanticTiargs(sc))
                goto Leprop;
            tiargs = ti->tiargs;
            tthis  = NULL;
            if ((os = ti->tempdecl->isOverloadSet()) != NULL)
                goto Los;
            if ((s = ti->tempdecl) != NULL)
                goto Lfd;
        }
    }
    else if (e1->op == TOKtemplate)
    {
        s      = ((TemplateExp *)e1)->td;
        tiargs = NULL;
        tthis  = NULL;
        goto Lfd;
    }
    else if (e1->op == TOKdotvar && e1->type && e1->type->toBasetype()->ty == Tfunction)
    {
        DotVarExp *dve = (DotVarExp *)e1;
        s      = dve->var->isFuncDeclaration();
        tiargs = NULL;
        tthis  = dve->e1->type;
        goto Lfd;
    }
    else if (e1->op == TOKvar && e1->type && e1->type->toBasetype()->ty == Tfunction)
    {
        s      = ((VarExp *)e1)->var->isFuncDeclaration();
        tiargs = NULL;
        tthis  = NULL;
    Lfd:
        assert(s);
        if (e2)
        {
            e2 = semantic(e2, sc);
            if (e2->op == TOKerror)
                return new ErrorExp();
            e2 = resolveProperties(sc, e2);

            Expressions a;
            a.push(e2);

            FuncDeclaration *fd = resolveFuncCall(loc, sc, s, tiargs, tthis, &a, 1);
            if (fd && fd->type)
            {
                if (fd->errors)
                    return new ErrorExp();
                assert(fd->type->ty == Tfunction);
                Expression *e = new CallExp(loc, e1, e2);
                return semantic(e, sc);
            }
        }
        {
            FuncDeclaration *fd = resolveFuncCall(loc, sc, s, tiargs, tthis, NULL, 1);
            if (fd && fd->type)
            {
                if (fd->errors)
                    return new ErrorExp();
                assert(fd->type->ty == Tfunction);
                TypeFunction *tf = (TypeFunction *)fd->type;
                if (!e2 || tf->isref)
                {
                    Expression *e = new CallExp(loc, e1);
                    if (e2)
                        e = new AssignExp(loc, e, e2);
                    return semantic(e, sc);
                }
            }
        }
        if (FuncDeclaration *fd = s->isFuncDeclaration())
        {
            // Keep better diagnostic message for invalid property usage of functions
            assert(fd->type->ty == Tfunction);
            Expression *e = new CallExp(loc, e1, e2);
            return semantic(e, sc);
        }
        if (e2)
            goto Leprop;
    }
    if (e1->op == TOKvar)
    {
        VarExp *ve = (VarExp *)e1;
        VarDeclaration *v = ve->var->isVarDeclaration();
        if (v && ve->checkPurity(sc, v))
            return new ErrorExp();
    }
    if (e2)
        return NULL;

    if (e1->type &&
        e1->op != TOKtype)      // function type is not a property
    {
        /* Look for e1 being a lazy parameter; rewrite as delegate call
         */
        if (e1->op == TOKvar)
        {
            VarExp *ve = (VarExp *)e1;

            if (ve->var->storage_class & STClazy)
            {
                Expression *e = new CallExp(loc, e1);
                return semantic(e, sc);
            }
        }
        else if (e1->op == TOKdotvar)
        {
            // Check for reading overlapped pointer field in @safe code.
            if (checkUnsafeAccess(sc, e1, true, true))
                return new ErrorExp();
        }
        else if (e1->op == TOKdot)
        {
            e1->error("expression has no value");
            return new ErrorExp();
        }
        else if (e1->op == TOKcall)
        {
            CallExp *ce = (CallExp *)e1;
            // Check for reading overlapped pointer field in @safe code.
            if (checkUnsafeAccess(sc, ce->e1, true, true))
                return new ErrorExp();
        }
    }

    if (!e1->type)
    {
        error(loc, "cannot resolve type for %s", e1->toChars());
        e1 = new ErrorExp();
    }
    return e1;

Leprop:
    error(loc, "not a property %s", e1->toChars());
    return new ErrorExp();

Leproplvalue:
    error(loc, "%s is not an lvalue", e1->toChars());
    return new ErrorExp();
}

Expression *resolveProperties(Scope *sc, Expression *e)
{
    //printf("resolveProperties(%s)\n", e->toChars());

    e = resolvePropertiesX(sc, e);
    if (e->checkRightThis(sc))
        return new ErrorExp();
    return e;
}

/******************************
 * Check the tail CallExp is really property function call.
 */
static bool checkPropertyCall(Expression *e)
{
    while (e->op == TOKcomma)
        e = ((CommaExp *)e)->e2;

    if (e->op == TOKcall)
    {
        CallExp *ce = (CallExp *)e;
        TypeFunction *tf;
        if (ce->f)
        {
            tf = (TypeFunction *)ce->f->type;
            /* If a forward reference to ce->f, try to resolve it
             */
            if (!tf->deco && ce->f->semanticRun < PASSsemanticdone)
            {
                ce->f->semantic(NULL);
                tf = (TypeFunction *)ce->f->type;
            }
        }
        else if (ce->e1->type->ty == Tfunction)
            tf = (TypeFunction *)ce->e1->type;
        else if (ce->e1->type->ty == Tdelegate)
            tf = (TypeFunction *)ce->e1->type->nextOf();
        else if (ce->e1->type->ty == Tpointer && ce->e1->type->nextOf()->ty == Tfunction)
            tf = (TypeFunction *)ce->e1->type->nextOf();
        else
            assert(0);
    }
    return false;
}

/******************************
 * If e1 is a property function (template), resolve it.
 */

Expression *resolvePropertiesOnly(Scope *sc, Expression *e1)
{
    //printf("e1 = %s %s\n", Token::toChars(e1->op), e1->toChars());
    OverloadSet *os;
    FuncDeclaration *fd;
    TemplateDeclaration *td;

    if (e1->op == TOKdot)
    {
        DotExp *de = (DotExp *)e1;
        if (de->e2->op == TOKoverloadset)
        {
            os = ((OverExp *)de->e2)->vars;
            goto Los;
        }
    }
    else if (e1->op == TOKoverloadset)
    {
        os = ((OverExp *)e1)->vars;
    Los:
        assert(os);
        for (size_t i = 0; i < os->a.length; i++)
        {
            Dsymbol *s = os->a[i];
            fd = s->isFuncDeclaration();
            td = s->isTemplateDeclaration();
            if (fd)
            {
                if (((TypeFunction *)fd->type)->isproperty)
                    return resolveProperties(sc, e1);
            }
            else if (td && td->onemember &&
                     (fd = td->onemember->isFuncDeclaration()) != NULL)
            {
                if (((TypeFunction *)fd->type)->isproperty ||
                    (fd->storage_class2 & STCproperty) ||
                    (td->_scope->stc & STCproperty))
                {
                    return resolveProperties(sc, e1);
                }
            }
        }
    }
    else if (e1->op == TOKdotti)
    {
        DotTemplateInstanceExp* dti = (DotTemplateInstanceExp *)e1;
        if (dti->ti->tempdecl && (td = dti->ti->tempdecl->isTemplateDeclaration()) != NULL)
            goto Ltd;
    }
    else if (e1->op == TOKdottd)
    {
        td = ((DotTemplateExp *)e1)->td;
        goto Ltd;
    }
    else if (e1->op == TOKscope)
    {
        Dsymbol *s = ((ScopeExp *)e1)->sds;
        TemplateInstance *ti = s->isTemplateInstance();
        if (ti && !ti->semanticRun && ti->tempdecl)
        {
            if ((td = ti->tempdecl->isTemplateDeclaration()) != NULL)
                goto Ltd;
        }
    }
    else if (e1->op == TOKtemplate)
    {
        td = ((TemplateExp *)e1)->td;
    Ltd:
        assert(td);
        if (td->onemember &&
            (fd = td->onemember->isFuncDeclaration()) != NULL)
        {
            if (((TypeFunction *)fd->type)->isproperty ||
                (fd->storage_class2 & STCproperty) ||
                (td->_scope->stc & STCproperty))
            {
                return resolveProperties(sc, e1);
            }
        }
    }
    else if (e1->op == TOKdotvar && e1->type->ty == Tfunction)
    {
        DotVarExp *dve = (DotVarExp *)e1;
        fd = dve->var->isFuncDeclaration();
        goto Lfd;
    }
    else if (e1->op == TOKvar && e1->type->ty == Tfunction &&
        (sc->intypeof || !((VarExp *)e1)->var->needThis()))
    {
        fd = ((VarExp *)e1)->var->isFuncDeclaration();
    Lfd:
        assert(fd);
        if (((TypeFunction *)fd->type)->isproperty)
            return resolveProperties(sc, e1);
    }
    return e1;
}


// TODO: merge with Scope::search::searchScopes()
static Dsymbol *searchScopes(Scope *sc, Loc loc, Identifier *ident, int flags)
{
    Dsymbol *s = NULL;
    for (Scope *scx = sc; scx; scx = scx->enclosing)
    {
        if (!scx->scopesym)
            continue;
        if (scx->scopesym->isModule())
            flags |= SearchUnqualifiedModule;    // tell Module.search() that SearchLocalsOnly is to be obeyed
        s = scx->scopesym->search(loc, ident, flags);
        if (s)
        {
            // overload set contains only module scope symbols.
            if (s->isOverloadSet())
                break;
            // selective/renamed imports also be picked up
            if (AliasDeclaration *ad = s->isAliasDeclaration())
            {
                if (ad->_import)
                    break;
            }
            // See only module scope symbols for UFCS target.
            Dsymbol *p = s->toParent2();
            if (p && p->isModule())
                break;
        }
        s = NULL;

        // Stop when we hit a module, but keep going if that is not just under the global scope
        if (scx->scopesym->isModule() && !(scx->enclosing && !scx->enclosing->enclosing))
            break;
    }
    return s;
}

/******************************
 * Find symbol in accordance with the UFCS name look up rule
 */

Expression *searchUFCS(Scope *sc, UnaExp *ue, Identifier *ident)
{
    //printf("searchUFCS(ident = %s)\n", ident->toChars());
    Loc loc = ue->loc;
    int flags = 0;
    Dsymbol *s = NULL;

    if (sc->flags & SCOPEignoresymbolvisibility)
        flags |= IgnoreSymbolVisibility;

    Dsymbol *sold = NULL;
    if (global.params.bug10378 || global.params.check10378)
    {
        sold = searchScopes(sc, loc, ident, flags | IgnoreSymbolVisibility);
        if (!global.params.check10378)
        {
            s = sold;
            goto Lsearchdone;
        }
    }

    // First look in local scopes
    s = searchScopes(sc, loc, ident, flags | SearchLocalsOnly);
    if (!s)
    {
        // Second look in imported modules
        s = searchScopes(sc, loc, ident, flags | SearchImportsOnly);

        /** Still find private symbols, so that symbols that weren't access
         * checked by the compiler remain usable.  Once the deprecation is over,
         * this should be moved to search_correct instead.
         */
        if (!s && !(flags & IgnoreSymbolVisibility))
        {
            s = searchScopes(sc, loc, ident, flags | SearchLocalsOnly | IgnoreSymbolVisibility);
            if (!s)
                s = searchScopes(sc, loc, ident, flags | SearchImportsOnly | IgnoreSymbolVisibility);
            if (s)
                ::deprecation(loc, "%s is not visible from module %s", s->toPrettyChars(), sc->_module->toChars());
        }
    }
    if (global.params.check10378)
    {
        Dsymbol *snew = s;
        if (sold != snew)
            Scope::deprecation10378(loc, sold, snew);
        if (global.params.bug10378)
            s = sold;
    }
Lsearchdone:

    if (!s)
        return ue->e1->type->Type::getProperty(loc, ident, 0);

    FuncDeclaration *f = s->isFuncDeclaration();
    if (f)
    {
        TemplateDeclaration *td = getFuncTemplateDecl(f);
        if (td)
        {
            if (td->overroot)
                td = td->overroot;
            s = td;
        }
    }

    if (ue->op == TOKdotti)
    {
        DotTemplateInstanceExp *dti = (DotTemplateInstanceExp *)ue;
        TemplateInstance *ti = new TemplateInstance(loc, s->ident);
        ti->tiargs = dti->ti->tiargs;   // for better diagnostic message
        if (!ti->updateTempDecl(sc, s))
            return new ErrorExp();
        return new ScopeExp(loc, ti);
    }
    else
    {
        //printf("-searchUFCS() %s\n", s->toChars());
        return new DsymbolExp(loc, s);
    }
}

/******************************
 * check e is exp.opDispatch!(tiargs) or not
 * It's used to switch to UFCS the semantic analysis path
 */

bool isDotOpDispatch(Expression *e)
{
    return e->op == TOKdotti &&
           ((DotTemplateInstanceExp *)e)->ti->name == Id::opDispatch;
}

/******************************
 * Pull out callable entity with UFCS.
 */

Expression *resolveUFCS(Scope *sc, CallExp *ce)
{
    Loc loc = ce->loc;
    Expression *eleft;
    Expression *e;

    if (ce->e1->op == TOKdotid)
    {
        DotIdExp *die = (DotIdExp *)ce->e1;
        Identifier *ident = die->ident;

        Expression *ex = semanticX(die, sc);
        if (ex != die)
        {
            ce->e1 = ex;
            return NULL;
        }
        eleft = die->e1;

        Type *t = eleft->type->toBasetype();
        if (t->ty == Tarray || t->ty == Tsarray ||
            t->ty == Tnull  || (t->isTypeBasic() && t->ty != Tvoid))
        {
            /* Built-in types and arrays have no callable properties, so do shortcut.
             * It is necessary in: e.init()
             */
        }
        else if (t->ty == Taarray)
        {
            if (ident == Id::remove)
            {
                /* Transform:
                 *  aa.remove(arg) into delete aa[arg]
                 */
                if (!ce->arguments || ce->arguments->length != 1)
                {
                    ce->error("expected key as argument to aa.remove()");
                    return new ErrorExp();
                }
                if (!eleft->type->isMutable())
                {
                    ce->error("cannot remove key from %s associative array %s",
                            MODtoChars(t->mod), eleft->toChars());
                    return new ErrorExp();
                }
                Expression *key = (*ce->arguments)[0];
                key = semantic(key, sc);
                key = resolveProperties(sc, key);

                TypeAArray *taa = (TypeAArray *)t;
                key = key->implicitCastTo(sc, taa->index);

                if (key->checkValue())
                    return new ErrorExp();

                semanticTypeInfo(sc, taa->index);

                return new RemoveExp(loc, eleft, key);
            }
        }
        else
        {
            if (Expression *ey = semanticY(die, sc, 1))
            {
                if (ey->op == TOKerror)
                    return ey;
                ce->e1 = ey;
                if (isDotOpDispatch(ey))
                {
                    unsigned errors = global.startGagging();
                    e = semantic(ce->syntaxCopy(), sc);
                    if (!global.endGagging(errors))
                        return e;
                    /* fall down to UFCS */
                }
                else
                    return NULL;
            }
        }
        e = searchUFCS(sc, die, ident);
    }
    else if (ce->e1->op == TOKdotti)
    {
        DotTemplateInstanceExp *dti = (DotTemplateInstanceExp *)ce->e1;
        if (Expression *ey = semanticY(dti, sc, 1))
        {
            ce->e1 = ey;
            return NULL;
        }
        eleft = dti->e1;
        e = searchUFCS(sc, dti, dti->ti->name);
    }
    else
        return NULL;

    // Rewrite
    ce->e1 = e;
    if (!ce->arguments)
        ce->arguments = new Expressions();
    ce->arguments->shift(eleft);

    return NULL;
}

/******************************
 * Pull out property with UFCS.
 */

Expression *resolveUFCSProperties(Scope *sc, Expression *e1, Expression *e2 = NULL)
{
    Loc loc = e1->loc;
    Expression *eleft;
    Expression *e;

    if (e1->op == TOKdotid)
    {
        DotIdExp *die = (DotIdExp *)e1;
        eleft = die->e1;
        e = searchUFCS(sc, die, die->ident);
    }
    else if (e1->op == TOKdotti)
    {
        DotTemplateInstanceExp *dti;
        dti = (DotTemplateInstanceExp *)e1;
        eleft = dti->e1;
        e = searchUFCS(sc, dti, dti->ti->name);
    }
    else
        return NULL;

    if (e == NULL)
        return NULL;

    // Rewrite
    if (e2)
    {
        // run semantic without gagging
        e2 = semantic(e2, sc);

        /* f(e1) = e2
         */
        Expression *ex = e->copy();
        Expressions *a1 = new Expressions();
        a1->setDim(1);
        (*a1)[0] = eleft;
        ex = new CallExp(loc, ex, a1);
        ex = trySemantic(ex, sc);

        /* f(e1, e2)
         */
        Expressions *a2 = new Expressions();
        a2->setDim(2);
        (*a2)[0] = eleft;
        (*a2)[1] = e2;
        e = new CallExp(loc, e, a2);
        if (ex)
        {   // if fallback setter exists, gag errors
            e = trySemantic(e, sc);
            if (!e)
            {   checkPropertyCall(ex);
                ex = new AssignExp(loc, ex, e2);
                return semantic(ex, sc);
            }
        }
        else
        {   // strict setter prints errors if fails
            e = semantic(e, sc);
        }
        checkPropertyCall(e);
        return e;
    }
    else
    {
        /* f(e1)
         */
        Expressions *arguments = new Expressions();
        arguments->setDim(1);
        (*arguments)[0] = eleft;
        e = new CallExp(loc, e, arguments);
        e = semantic(e, sc);
        checkPropertyCall(e);
        return semantic(e, sc);
    }
}

/******************************
 * Perform semantic() on an array of Expressions.
 */

bool arrayExpressionSemantic(Expressions *exps, Scope *sc, bool preserveErrors)
{
    bool err = false;
    if (exps)
    {
        for (size_t i = 0; i < exps->length; i++)
        {
            Expression *e = (*exps)[i];
            if (e)
            {
                e = semantic(e, sc);
                if (e->op == TOKerror)
                    err = true;
                if (preserveErrors || e->op != TOKerror)
                    (*exps)[i] = e;
            }
        }
    }
    return err;
}

/****************************************
 * Expand tuples.
 * Input:
 *      exps    aray of Expressions
 * Output:
 *      exps    rewritten in place
 */

void expandTuples(Expressions *exps)
{
    //printf("expandTuples()\n");
    if (exps)
    {
        for (size_t i = 0; i < exps->length; i++)
        {
            Expression *arg = (*exps)[i];
            if (!arg)
                continue;

            // Look for tuple with 0 members
            if (arg->op == TOKtype)
            {
                TypeExp *e = (TypeExp *)arg;
                if (e->type->toBasetype()->ty == Ttuple)
                {
                    TypeTuple *tt = (TypeTuple *)e->type->toBasetype();

                    if (!tt->arguments || tt->arguments->length == 0)
                    {
                        exps->remove(i);
                        if (i == exps->length)
                            return;
                        i--;
                        continue;
                    }
                }
            }

            // Inline expand all the tuples
            while (arg->op == TOKtuple)
            {
                TupleExp *te = (TupleExp *)arg;
                exps->remove(i);                // remove arg
                exps->insert(i, te->exps);      // replace with tuple contents
                if (i == exps->length)
                    return;             // empty tuple, no more arguments
                (*exps)[i] = Expression::combine(te->e0, (*exps)[i]);
                arg = (*exps)[i];
            }
        }
    }
}

/****************************************
 * Expand alias this tuples.
 */

TupleDeclaration *isAliasThisTuple(Expression *e)
{
    if (!e->type)
        return NULL;

    Type *t = e->type->toBasetype();
Lagain:
    if (Dsymbol *s = t->toDsymbol(NULL))
    {
        AggregateDeclaration *ad = s->isAggregateDeclaration();
        if (ad)
        {
            s = ad->aliasthis;
            if (s && s->isVarDeclaration())
            {
                TupleDeclaration *td = s->isVarDeclaration()->toAlias()->isTupleDeclaration();
                if (td && td->isexp)
                    return td;
            }
            if (Type *att = t->aliasthisOf())
            {
                t = att;
                goto Lagain;
            }
        }
    }
    return NULL;
}

int expandAliasThisTuples(Expressions *exps, size_t starti)
{
    if (!exps || exps->length == 0)
        return -1;

    for (size_t u = starti; u < exps->length; u++)
    {
        Expression *exp = (*exps)[u];
        TupleDeclaration *td = isAliasThisTuple(exp);
        if (td)
        {
            exps->remove(u);
            for (size_t i = 0; i<td->objects->length; ++i)
            {
                Expression *e = isExpression((*td->objects)[i]);
                assert(e);
                assert(e->op == TOKdsymbol);
                DsymbolExp *se = (DsymbolExp *)e;
                Declaration *d = se->s->isDeclaration();
                assert(d);
                e = new DotVarExp(exp->loc, exp, d);
                assert(d->type);
                e->type = d->type;
                exps->insert(u + i, e);
            }
            return (int)u;
        }
    }

    return -1;
}

/****************************************
 * The common type is determined by applying ?: to each pair.
 * Output:
 *      exps[]  properties resolved, implicitly cast to common type, rewritten in place
 *      *pt     if pt is not NULL, set to the common type
 * Returns:
 *      true    a semantic error was detected
 */

bool arrayExpressionToCommonType(Scope *sc, Expressions *exps, Type **pt)
{
    /* Still have a problem with:
     *  ubyte[][] = [ cast(ubyte[])"hello", [1]];
     * which works if the array literal is initialized top down with the ubyte[][]
     * type, but fails with this function doing bottom up typing.
     */
    //printf("arrayExpressionToCommonType()\n");
    IntegerExp integerexp(0);
    CondExp condexp(Loc(), &integerexp, NULL, NULL);

    Type *t0 = NULL;
    Expression *e0 = NULL;      // dead-store to prevent spurious warning
    size_t j0 = ~0;             // dead-store to prevent spurious warning
    bool foundType = false;

    for (size_t i = 0; i < exps->length; i++)
    {
        Expression *e = (*exps)[i];
        if (!e)
            continue;

        e = resolveProperties(sc, e);
        if (!e->type)
        {
            e->error("%s has no value", e->toChars());
            t0 = Type::terror;
            continue;
        }
        if (e->op == TOKtype)
        {
            foundType = true;   // do not break immediately, there might be more errors
            e->checkValue();    // report an error "type T has no value"
            t0 = Type::terror;
            continue;
        }
        if (e->type->ty == Tvoid)
        {
            // void expressions do not concur to the determination of the common
            // type.
            continue;
        }
        if (checkNonAssignmentArrayOp(e))
        {
            t0 = Type::terror;
            continue;
        }

        e = doCopyOrMove(sc, e);

        if (!foundType && t0 && !t0->equals(e->type))
        {
            /* This applies ?: to merge the types. It's backwards;
             * ?: should call this function to merge types.
             */
            condexp.type = NULL;
            condexp.e1 = e0;
            condexp.e2 = e;
            condexp.loc = e->loc;
            Expression *ex = semantic(&condexp, sc);
            if (ex->op == TOKerror)
                e = ex;
            else
            {
                (*exps)[j0] = condexp.e1;
                e = condexp.e2;
            }
        }
        j0 = i;
        e0 = e;
        t0 = e->type;
        if (e->op != TOKerror)
            (*exps)[i] = e;
    }

    if (!t0)
        t0 = Type::tvoid;               // [] is typed as void[]
    else if (t0->ty != Terror)
    {
        for (size_t i = 0; i < exps->length; i++)
        {
            Expression *e = (*exps)[i];
            if (!e)
                continue;

            e = e->implicitCastTo(sc, t0);
            //assert(e->op != TOKerror);
            if (e->op == TOKerror)
            {
                /* Bugzilla 13024: a workaround for the bug in typeMerge -
                 * it should paint e1 and e2 by deduced common type,
                 * but doesn't in this particular case.
                 */
                t0 = Type::terror;
                break;
            }
            (*exps)[i] = e;
        }
    }
    if (pt)
        *pt = t0;

    return (t0 == Type::terror);
}

/****************************************
 * Get TemplateDeclaration enclosing FuncDeclaration.
 */

TemplateDeclaration *getFuncTemplateDecl(Dsymbol *s)
{
    FuncDeclaration *f = s->isFuncDeclaration();
    if (f && f->parent)
    {
        TemplateInstance *ti = f->parent->isTemplateInstance();
        if (ti && !ti->isTemplateMixin() &&
            ti->tempdecl && ((TemplateDeclaration *)ti->tempdecl)->onemember &&
            ti->tempdecl->ident == f->ident)
        {
            return (TemplateDeclaration *)ti->tempdecl;
        }
    }
    return NULL;
}

/************************************************
 * If we want the value of this expression, but do not want to call
 * the destructor on it.
 */

Expression *valueNoDtor(Expression *e)
{
    if (e->op == TOKcall)
    {
        /* The struct value returned from the function is transferred
         * so do not call the destructor on it.
         * Recognize:
         *       ((S _ctmp = S.init), _ctmp).this(...)
         * and make sure the destructor is not called on _ctmp
         * BUG: if e is a CommaExp, we should go down the right side.
         */
        CallExp *ce = (CallExp *)e;
        if (ce->e1->op == TOKdotvar)
        {
            DotVarExp *dve = (DotVarExp *)ce->e1;
            if (dve->var->isCtorDeclaration())
            {
                // It's a constructor call
                if (dve->e1->op == TOKcomma)
                {
                    CommaExp *comma = (CommaExp *)dve->e1;
                    if (comma->e2->op == TOKvar)
                    {
                        VarExp *ve = (VarExp *)comma->e2;
                        VarDeclaration *ctmp = ve->var->isVarDeclaration();
                        if (ctmp)
                        {
                            ctmp->storage_class |= STCnodtor;
                            assert(!ce->isLvalue());
                        }
                    }
                }
            }
        }
    }
    else if (e->op == TOKvar)
    {
        VarDeclaration *vtmp = ((VarExp *)e)->var->isVarDeclaration();
        if (vtmp && vtmp->storage_class & STCrvalue)
        {
            vtmp->storage_class |= STCnodtor;
        }
    }
    return e;
}

/********************************************
 * Issue an error if default construction is disabled for type t.
 * Default construction is required for arrays and 'out' parameters.
 * Returns:
 *      true    an error was issued
 */
bool checkDefCtor(Loc loc, Type *t)
{
    t = t->baseElemOf();
    if (t->ty == Tstruct)
    {
        StructDeclaration *sd = ((TypeStruct *)t)->sym;
        if (sd->noDefaultCtor)
        {
            sd->error(loc, "default construction is disabled");
            return true;
        }
    }
    return false;
}

/*********************************************
 * If e is an instance of a struct, and that struct has a copy constructor,
 * rewrite e as:
 *    (tmp = e),tmp
 * Input:
 *      sc      just used to specify the scope of created temporary variable
 */
Expression *callCpCtor(Scope *sc, Expression *e)
{
    Type *tv = e->type->baseElemOf();
    if (tv->ty == Tstruct)
    {
        StructDeclaration *sd = ((TypeStruct *)tv)->sym;
        if (sd->postblit)
        {
            /* Create a variable tmp, and replace the argument e with:
             *      (tmp = e),tmp
             * and let AssignExp() handle the construction.
             * This is not the most efficent, ideally tmp would be constructed
             * directly onto the stack.
             */
            VarDeclaration *tmp = copyToTemp(STCrvalue, "__copytmp", e);
            tmp->storage_class |= STCnodtor;
            tmp->semantic(sc);
            Expression *de = new DeclarationExp(e->loc, tmp);
            Expression *ve = new VarExp(e->loc, tmp);
            de->type = Type::tvoid;
            ve->type = e->type;
            e = Expression::combine(de, ve);
        }
    }
    return e;
}

/************************************************
 * Handle the postblit call on lvalue, or the move of rvalue.
 */
Expression *doCopyOrMove(Scope *sc, Expression *e)
{
    if (e->op == TOKquestion)
    {
        CondExp *ce = (CondExp *)e;
        ce->e1 = doCopyOrMove(sc, ce->e1);
        ce->e2 = doCopyOrMove(sc, ce->e2);
    }
    else
    {
        e = e->isLvalue() ? callCpCtor(sc, e) : valueNoDtor(e);
    }
    return e;
}

/****************************************
 * Now that we know the exact type of the function we're calling,
 * the arguments[] need to be adjusted:
 *      1. implicitly convert argument to the corresponding parameter type
 *      2. add default arguments for any missing arguments
 *      3. do default promotions on arguments corresponding to ...
 *      4. add hidden _arguments[] argument
 *      5. call copy constructor for struct value arguments
 * Input:
 *      tf      type of the function
 *      fd      the function being called, NULL if called indirectly
 * Output:
 *      *prettype return type of function
 *      *peprefix expression to execute before arguments[] are evaluated, NULL if none
 * Returns:
 *      true    errors happened
 */

bool functionParameters(Loc loc, Scope *sc, TypeFunction *tf,
        Type *tthis, Expressions *arguments, FuncDeclaration *fd, Type **prettype, Expression **peprefix)
{
    //printf("functionParameters()\n");
    assert(arguments);
    assert(fd || tf->next);
    size_t nargs = arguments ? arguments->length : 0;
    size_t nparams = tf->parameterList.length();
    unsigned olderrors = global.errors;
    bool err = false;
    *prettype = Type::terror;
    Expression *eprefix = NULL;
    *peprefix = NULL;

    if (nargs > nparams && tf->parameterList.varargs == VARARGnone)
    {
        error(loc, "expected %llu arguments, not %llu for non-variadic function type %s", (ulonglong)nparams, (ulonglong)nargs, tf->toChars());
        return true;
    }

    // If inferring return type, and semantic3() needs to be run if not already run
    if (!tf->next && fd->inferRetType)
    {
        fd->functionSemantic();
    }
    else if (fd && fd->parent)
    {
        TemplateInstance *ti = fd->parent->isTemplateInstance();
        if (ti && ti->tempdecl)
        {
            fd->functionSemantic3();
        }
    }
    bool isCtorCall = fd && fd->needThis() && fd->isCtorDeclaration();

    size_t n = (nargs > nparams) ? nargs : nparams;   // n = max(nargs, nparams)

    /* If the function return type has wildcards in it, we'll need to figure out the actual type
     * based on the actual argument types.
     */
    MOD wildmatch = 0;
    if (tthis && tf->isWild() && !isCtorCall)
    {
        Type *t = tthis;
        if (t->isImmutable())
            wildmatch = MODimmutable;
        else if (t->isWildConst())
            wildmatch = MODwildconst;
        else if (t->isWild())
            wildmatch = MODwild;
        else if (t->isConst())
            wildmatch = MODconst;
        else
            wildmatch = MODmutable;
    }

    int done = 0;
    for (size_t i = 0; i < n; i++)
    {
        Expression *arg;

        if (i < nargs)
            arg = (*arguments)[i];
        else
            arg = NULL;

        if (i < nparams)
        {
            Parameter *p = tf->parameterList[i];

            if (!arg)
            {
                if (!p->defaultArg)
                {
                    if (tf->parameterList.varargs == VARARGtypesafe && i + 1 == nparams)
                        goto L2;
                    error(loc, "expected %llu function arguments, not %llu", (ulonglong)nparams, (ulonglong)nargs);
                    return true;
                }
                arg = p->defaultArg;
                arg = inlineCopy(arg, sc);
                // __FILE__, __LINE__, __MODULE__, __FUNCTION__, and __PRETTY_FUNCTION__
                arg = arg->resolveLoc(loc, sc);
                arguments->push(arg);
                nargs++;
            }

            if (tf->parameterList.varargs == VARARGtypesafe && i + 1 == nparams)
            {
                //printf("\t\tvarargs == 2, p->type = '%s'\n", p->type->toChars());
                {
                    MATCH m;
                    if ((m = arg->implicitConvTo(p->type)) > MATCHnomatch)
                    {
                        if (p->type->nextOf() && arg->implicitConvTo(p->type->nextOf()) >= m)
                            goto L2;
                        else if (nargs != nparams)
                        {   error(loc, "expected %llu function arguments, not %llu", (ulonglong)nparams, (ulonglong)nargs);
                            return true;
                        }
                        goto L1;
                    }
                }
             L2:
                Type *tb = p->type->toBasetype();
                Type *tret = p->isLazyArray();
                switch (tb->ty)
                {
                    case Tsarray:
                    case Tarray:
                    {
                        /* Create a static array variable v of type arg->type:
                         *  T[dim] __arrayArg = [ arguments[i], ..., arguments[nargs-1] ];
                         *
                         * The array literal in the initializer of the hidden variable
                         * is now optimized. See Bugzilla 2356.
                         */
                        Type *tbn = ((TypeArray *)tb)->next;

                        Expressions *elements = new Expressions();
                        elements->setDim(nargs - i);
                        for (size_t u = 0; u < elements->length; u++)
                        {
                            Expression *a = (*arguments)[i + u];
                            if (tret && a->implicitConvTo(tret))
                            {
                                a = a->implicitCastTo(sc, tret);
                                a = a->optimize(WANTvalue);
                                a = toDelegate(a, a->type, sc);
                            }
                            else
                                a = a->implicitCastTo(sc, tbn);
                            (*elements)[u] = a;
                        }
                        // Bugzilla 14395: Convert to a static array literal, or its slice.
                        arg = new ArrayLiteralExp(loc, tbn->sarrayOf(nargs - i), elements);
                        if (tb->ty == Tarray)
                        {
                            arg = new SliceExp(loc, arg, NULL, NULL);
                            arg->type = p->type;
                        }
                        break;
                    }
                    case Tclass:
                    {
                        /* Set arg to be:
                         *      new Tclass(arg0, arg1, ..., argn)
                         */
                        Expressions *args = new Expressions();
                        args->setDim(nargs - i);
                        for (size_t u = i; u < nargs; u++)
                            (*args)[u - i] = (*arguments)[u];
                        arg = new NewExp(loc, NULL, NULL, p->type, args);
                        break;
                    }
                    default:
                        if (!arg)
                        {
                            error(loc, "not enough arguments");
                            return true;
                        }
                        break;
                }
                arg = semantic(arg, sc);
                //printf("\targ = '%s'\n", arg->toChars());
                arguments->setDim(i + 1);
                (*arguments)[i] =  arg;
                nargs = i + 1;
                done = 1;
            }

        L1:
            if (!(p->storageClass & STClazy && p->type->ty == Tvoid))
            {
                bool isRef = (p->storageClass & (STCref | STCout)) != 0;
                if (unsigned char wm = arg->type->deduceWild(p->type, isRef))
                {
                    if (wildmatch)
                        wildmatch = MODmerge(wildmatch, wm);
                    else
                        wildmatch = wm;
                    //printf("[%d] p = %s, a = %s, wm = %d, wildmatch = %d\n", i, p->type->toChars(), arg->type->toChars(), wm, wildmatch);
                }
            }
        }
        if (done)
            break;
    }
    if ((wildmatch == MODmutable || wildmatch == MODimmutable) &&
        tf->next->hasWild() &&
        (tf->isref || !tf->next->implicitConvTo(tf->next->immutableOf())))
    {
        if (fd)
        {
            /* If the called function may return the reference to
             * outer inout data, it should be rejected.
             *
             * void foo(ref inout(int) x) {
             *   ref inout(int) bar(inout(int)) { return x; }
             *   struct S { ref inout(int) bar() inout { return x; } }
             *   bar(int.init) = 1;  // bad!
             *   S().bar() = 1;      // bad!
             * }
             */
            Dsymbol *s = NULL;
            if (fd->isThis() || fd->isNested())
                s = fd->toParent2();
            for (; s; s = s->toParent2())
            {
                if (AggregateDeclaration *ad = s->isAggregateDeclaration())
                {
                    if (ad->isNested())
                        continue;
                    break;
                }
                if (FuncDeclaration *ff = s->isFuncDeclaration())
                {
                    if (((TypeFunction *)ff->type)->iswild)
                        goto Linouterr;

                    if (ff->isNested() || ff->isThis())
                        continue;
                }
                break;
            }
        }
        else if (tf->isWild())
        {
        Linouterr:
            const char *s = wildmatch == MODmutable ? "mutable" : MODtoChars(wildmatch);
            error(loc, "modify inout to %s is not allowed inside inout function", s);
            return true;
        }
    }

    assert(nargs >= nparams);
    for (size_t i = 0; i < nargs; i++)
    {
        Expression *arg = (*arguments)[i];
        assert(arg);
        if (i < nparams)
        {
            Parameter *p = tf->parameterList[i];

            if (!(p->storageClass & STClazy && p->type->ty == Tvoid))
            {
                Type *tprm = p->type;
                if (p->type->hasWild())
                    tprm = p->type->substWildTo(wildmatch);
                if (!tprm->equals(arg->type))
                {
                    //printf("arg->type = %s, p->type = %s\n", arg->type->toChars(), p->type->toChars());
                    arg = arg->implicitCastTo(sc, tprm);
                    arg = arg->optimize(WANTvalue, (p->storageClass & (STCref | STCout)) != 0);
                }
            }
            if (p->storageClass & STCref)
            {
                arg = arg->toLvalue(sc, arg);

                // Look for mutable misaligned pointer, etc., in @safe mode
                err |= checkUnsafeAccess(sc, arg, false, true);
            }
            else if (p->storageClass & STCout)
            {
                Type *t = arg->type;
                if (!t->isMutable() || !t->isAssignable())  // check blit assignable
                {
                    arg->error("cannot modify struct %s with immutable members", arg->toChars());
                    err = true;
                }
                else
                {
                    // Look for misaligned pointer, etc., in @safe mode
                    err |= checkUnsafeAccess(sc, arg, false, true);
                    err |= checkDefCtor(arg->loc, t);   // t must be default constructible
                }
                arg = arg->toLvalue(sc, arg);
            }
            else if (p->storageClass & STClazy)
            {
                // Convert lazy argument to a delegate
                if (p->type->ty == Tvoid)
                    arg = toDelegate(arg, p->type, sc);
                else
                    arg = toDelegate(arg, arg->type, sc);
            }

            //printf("arg: %s\n", arg->toChars());
            //printf("type: %s\n", arg->type->toChars());
            if (tf->parameterEscapes(p))
            {
                /* Argument value can escape from the called function.
                 * Check arg to see if it matters.
                 */
                if (global.params.vsafe)
                    err |= checkParamArgumentEscape(sc, fd, p->ident, arg, false);
            }
            else
            {
                /* Argument value cannot escape from the called function.
                 */
                Expression *a = arg;
                if (a->op == TOKcast)
                    a = ((CastExp *)a)->e1;

                if (a->op == TOKfunction)
                {
                    /* Function literals can only appear once, so if this
                     * appearance was scoped, there cannot be any others.
                     */
                    FuncExp *fe = (FuncExp *)a;
                    fe->fd->tookAddressOf = 0;
                }
                else if (a->op == TOKdelegate)
                {
                    /* For passing a delegate to a scoped parameter,
                     * this doesn't count as taking the address of it.
                     * We only worry about 'escaping' references to the function.
                     */
                    DelegateExp *de = (DelegateExp *)a;
                    if (de->e1->op == TOKvar)
                    {   VarExp *ve = (VarExp *)de->e1;
                        FuncDeclaration *f = ve->var->isFuncDeclaration();
                        if (f)
                        {   f->tookAddressOf--;
                            //printf("tookAddressOf = %d\n", f->tookAddressOf);
                        }
                    }
                }
            }
            arg = arg->optimize(WANTvalue, (p->storageClass & (STCref | STCout)) != 0);
        }
        else
        {
            // These will be the trailing ... arguments

            // If not D linkage, do promotions
            if (tf->linkage != LINKd)
            {
                // Promote bytes, words, etc., to ints
                arg = integralPromotions(arg, sc);

                // Promote floats to doubles
                switch (arg->type->ty)
                {
                    case Tfloat32:
                        arg = arg->castTo(sc, Type::tfloat64);
                        break;

                    case Timaginary32:
                        arg = arg->castTo(sc, Type::timaginary64);
                        break;
                }

                if (tf->parameterList.varargs == VARARGvariadic)
                {
                    const char *p = tf->linkage == LINKc ? "extern(C)" : "extern(C++)";
                    if (arg->type->ty == Tarray)
                    {
                        arg->error("cannot pass dynamic arrays to %s vararg functions", p);
                        err = true;
                    }
                    if (arg->type->ty == Tsarray)
                    {
                        arg->error("cannot pass static arrays to %s vararg functions", p);
                        err = true;
                    }
                }
            }

            // Do not allow types that need destructors
            if (arg->type->needsDestruction())
            {
                arg->error("cannot pass types that need destruction as variadic arguments");
                err = true;
            }

            // Convert static arrays to dynamic arrays
            // BUG: I don't think this is right for D2
            Type *tb = arg->type->toBasetype();
            if (tb->ty == Tsarray)
            {
                TypeSArray *ts = (TypeSArray *)tb;
                Type *ta = ts->next->arrayOf();
                if (ts->size(arg->loc) == 0)
                    arg = new NullExp(arg->loc, ta);
                else
                    arg = arg->castTo(sc, ta);
            }
            if (tb->ty == Tstruct)
            {
                //arg = callCpCtor(sc, arg);
            }

            // Give error for overloaded function addresses
            if (arg->op == TOKsymoff)
            {   SymOffExp *se = (SymOffExp *)arg;
                if (se->hasOverloads &&
                    !se->var->isFuncDeclaration()->isUnique())
                {   arg->error("function %s is overloaded", arg->toChars());
                    err = true;
                }
            }
            if (arg->checkValue())
                err = true;
            arg = arg->optimize(WANTvalue);
        }
        (*arguments)[i] = arg;
    }

    /* Remaining problems:
     * 1. order of evaluation - some function push L-to-R, others R-to-L. Until we resolve what array assignment does (which is
     *    implemented by calling a function) we'll defer this for now.
     * 2. value structs (or static arrays of them) that need to be copy constructed
     * 3. value structs (or static arrays of them) that have destructors, and subsequent arguments that may throw before the
     *    function gets called (functions normally destroy their parameters)
     * 2 and 3 are handled by doing the argument construction in 'eprefix' so that if a later argument throws, they are cleaned
     * up properly. Pushing arguments on the stack then cannot fail.
     */
    if (1)
    {
        /* TODO: tackle problem 1)
         */
        const bool leftToRight = true; // TODO: something like !fd.isArrayOp
        if (!leftToRight)
            assert(nargs == nparams); // no variadics for RTL order, as they would probably be evaluated LTR and so add complexity

        const ptrdiff_t start = (leftToRight ? 0 : (ptrdiff_t)nargs - 1);
        const ptrdiff_t end = (leftToRight ? (ptrdiff_t)nargs : -1);
        const ptrdiff_t step = (leftToRight ? 1 : -1);

        /* Compute indices of last throwing argument and first arg needing destruction.
         * Used to not set up destructors unless an arg needs destruction on a throw
         * in a later argument.
         */
        ptrdiff_t lastthrow = -1;
        ptrdiff_t firstdtor = -1;
        for (ptrdiff_t i = start; i != end; i += step)
        {
            Expression *arg = (*arguments)[i];
            if (canThrow(arg, sc->func, false))
                lastthrow = i;
            if (firstdtor == -1 && arg->type->needsDestruction())
            {
                Parameter *p = (i >= (ptrdiff_t)nparams ? NULL : tf->parameterList[i]);
                if (!(p && (p->storageClass & (STClazy | STCref | STCout))))
                    firstdtor = i;
            }
        }

        /* Does problem 3) apply to this call?
         */
        const bool needsPrefix = (firstdtor >= 0 && lastthrow >= 0
            && (lastthrow - firstdtor) * step > 0);

        /* If so, initialize 'eprefix' by declaring the gate
         */
        VarDeclaration *gate = NULL;
        if (needsPrefix)
        {
            // eprefix => bool __gate [= false]
            Identifier *idtmp = Identifier::generateId("__gate");
            gate = new VarDeclaration(loc, Type::tbool, idtmp, NULL);
            gate->storage_class |= STCtemp | STCctfe | STCvolatile;
            gate->semantic(sc);

            Expression *ae = new DeclarationExp(loc, gate);
            eprefix = semantic(ae, sc);
        }

        for (ptrdiff_t i = start; i != end; i += step)
        {
            Expression *arg = (*arguments)[i];

            Parameter *parameter = (i >= (ptrdiff_t)nparams ? NULL : tf->parameterList[i]);
            const bool isRef = (parameter && (parameter->storageClass & (STCref | STCout)));
            const bool isLazy = (parameter && (parameter->storageClass & STClazy));

            /* Skip lazy parameters
             */
            if (isLazy)
                continue;

            /* Do we have a gate? Then we have a prefix and we're not yet past the last throwing arg.
             * Declare a temporary variable for this arg and append that declaration to 'eprefix',
             * which will implicitly take care of potential problem 2) for this arg.
             * 'eprefix' will therefore finally contain all args up to and including the last
             * potentially throwing arg, excluding all lazy parameters.
             */
            if (gate)
            {
                const bool needsDtor = (!isRef && arg->type->needsDestruction() && i != lastthrow);

                /* Declare temporary 'auto __pfx = arg' (needsDtor) or 'auto __pfy = arg' (!needsDtor)
                 */
                VarDeclaration *tmp = copyToTemp(0,
                    needsDtor ? "__pfx" : "__pfy",
                    !isRef ? arg : arg->addressOf());
                tmp->semantic(sc);

                /* Modify the destructor so it only runs if gate==false, i.e.,
                 * only if there was a throw while constructing the args
                 */
                if (!needsDtor)
                {
                    if (tmp->edtor)
                    {
                        assert(i == lastthrow);
                        tmp->edtor = NULL;
                    }
                }
                else
                {
                    // edtor => (__gate || edtor)
                    assert(tmp->edtor);
                    Expression *e = tmp->edtor;
                    e = new LogicalExp(e->loc, TOKoror, new VarExp(e->loc, gate), e);
                    tmp->edtor = semantic(e, sc);
                    //printf("edtor: %s\n", tmp->edtor->toChars());
                }

                // eprefix => (eprefix, auto __pfx/y = arg)
                DeclarationExp *ae = new DeclarationExp(loc, tmp);
                eprefix = Expression::combine(eprefix, semantic(ae, sc));

                // arg => __pfx/y
                arg = new VarExp(loc, tmp);
                arg = semantic(arg, sc);
                if (isRef)
                {
                    arg = new PtrExp(loc, arg);
                    arg = semantic(arg, sc);
                }

                /* Last throwing arg? Then finalize eprefix => (eprefix, gate = true),
                 * i.e., disable the dtors right after constructing the last throwing arg.
                 * From now on, the callee will take care of destructing the args because
                 * the args are implicitly moved into function parameters.
                 *
                 * Set gate to null to let the next iterations know they don't need to
                 * append to eprefix anymore.
                 */
                if (i == lastthrow)
                {
                    Expression *e = new AssignExp(gate->loc, new VarExp(gate->loc, gate), new IntegerExp(gate->loc, 1, Type::tbool));
                    eprefix = Expression::combine(eprefix, semantic(e, sc));
                    gate = NULL;
                }
            }
            else
            {
                /* No gate, no prefix to append to.
                 * Handle problem 2) by calling the copy constructor for value structs
                 * (or static arrays of them) if appropriate.
                 */
                Type *tv = arg->type->baseElemOf();
                if (!isRef && tv->ty == Tstruct)
                    arg = doCopyOrMove(sc, arg);
            }

            (*arguments)[i] = arg;
        }
    }
    //if (eprefix) printf("eprefix: %s\n", eprefix->toChars());

    // If D linkage and variadic, add _arguments[] as first argument
    if (tf->isDstyleVariadic())
    {
        assert(arguments->length >= nparams);

        Parameters *args = new Parameters;
        args->setDim(arguments->length - nparams);
        for (size_t i = 0; i < arguments->length - nparams; i++)
        {
            Parameter *arg = new Parameter(STCin, (*arguments)[nparams + i]->type, NULL, NULL);
            (*args)[i] = arg;
        }

        TypeTuple *tup = new TypeTuple(args);
        Expression *e = new TypeidExp(loc, tup);
        e = semantic(e, sc);
        arguments->insert(0, e);
    }

    Type *tret = tf->next;
    if (isCtorCall)
    {
        //printf("[%s] fd = %s %s, %d %d %d\n", loc.toChars(), fd->toChars(), fd->type->toChars(),
        //    wildmatch, tf->isWild(), fd->isolateReturn());
        if (!tthis)
        {
            assert(sc->intypeof || global.errors);
            tthis = fd->isThis()->type->addMod(fd->type->mod);
        }
        if (tf->isWild() && !fd->isolateReturn())
        {
            if (wildmatch)
                tret = tret->substWildTo(wildmatch);
            int offset;
            if (!tret->implicitConvTo(tthis) &&
                !(MODimplicitConv(tret->mod, tthis->mod) && tret->isBaseOf(tthis, &offset) && offset == 0))
            {
                const char* s1 = tret ->isNaked() ? " mutable" : tret ->modToChars();
                const char* s2 = tthis->isNaked() ? " mutable" : tthis->modToChars();
                ::error(loc, "inout constructor %s creates%s object, not%s",
                        fd->toPrettyChars(), s1, s2);
                err = true;
            }
        }
        tret = tthis;
    }
    else if (wildmatch && tret)
    {
        /* Adjust function return type based on wildmatch
         */
        //printf("wildmatch = x%x, tret = %s\n", wildmatch, tret->toChars());
        tret = tret->substWildTo(wildmatch);
    }
    *prettype = tret;
    *peprefix = eprefix;
    return (err || olderrors != global.errors);
}

/******************************** Expression **************************/

Expression::Expression(Loc loc, TOK op, int size)
{
    //printf("Expression::Expression(op = %d) this = %p\n", op, this);
    this->loc = loc;
    this->op = op;
    this->size = (unsigned char)size;
    this->parens = 0;
    type = NULL;
}

void Expression::_init()
{
    CTFEExp::cantexp = new CTFEExp(TOKcantexp);
    CTFEExp::voidexp = new CTFEExp(TOKvoidexp);
    CTFEExp::breakexp = new CTFEExp(TOKbreak);
    CTFEExp::continueexp = new CTFEExp(TOKcontinue);
    CTFEExp::gotoexp = new CTFEExp(TOKgoto);
}

Expression *Expression::syntaxCopy()
{
    //printf("Expression::syntaxCopy()\n");
    //print();
    return copy();
}

/*********************************
 * Does *not* do a deep copy.
 */

Expression *Expression::copy()
{
    Expression *e;
    if (!size)
    {
        assert(0);
    }
    void *pe = mem.xmalloc(size);
    //printf("Expression::copy(op = %d) e = %p\n", op, pe);
    e = (Expression *)memcpy(pe, (void *)this, size);
    return e;
}

void Expression::print()
{
    fprintf(stderr, "%s\n", toChars());
    fflush(stderr);
}

const char *Expression::toChars()
{
    OutBuffer buf;
    HdrGenState hgs;
    toCBuffer(this, &buf, &hgs);
    return buf.extractChars();
}

void Expression::error(const char *format, ...) const
{
    if (type != Type::terror)
    {
        va_list ap;
        va_start(ap, format);
        ::verror(loc, format, ap);
        va_end( ap );
    }
}

void Expression::warning(const char *format, ...) const
{
    if (type != Type::terror)
    {
        va_list ap;
        va_start(ap, format);
        ::vwarning(loc, format, ap);
        va_end( ap );
    }
}

void Expression::deprecation(const char *format, ...) const
{
    if (type != Type::terror)
    {
        va_list ap;
        va_start(ap, format);
        ::vdeprecation(loc, format, ap);
        va_end( ap );
    }
}

/**********************************
 * Combine e1 and e2 by CommaExp if both are not NULL.
 */
Expression *Expression::combine(Expression *e1, Expression *e2)
{
    if (e1)
    {
        if (e2)
        {
            e1 = new CommaExp(e1->loc, e1, e2);
            e1->type = e2->type;
        }
    }
    else
        e1 = e2;
    return e1;
}

/**********************************
 * If 'e' is a tree of commas, returns the leftmost expression
 * by stripping off it from the tree. The remained part of the tree
 * is returned via *pe0.
 * Otherwise 'e' is directly returned and *pe0 is set to NULL.
 */
Expression *Expression::extractLast(Expression *e, Expression **pe0)
{
    if (e->op != TOKcomma)
    {
        *pe0 = NULL;
        return e;
    }

    CommaExp *ce = (CommaExp *)e;
    if (ce->e2->op != TOKcomma)
    {
        *pe0 = ce->e1;
        return ce->e2;
    }
    else
    {
        *pe0 = e;

        Expression **pce = &ce->e2;
        while (((CommaExp *)(*pce))->e2->op == TOKcomma)
        {
            pce = &((CommaExp *)(*pce))->e2;
        }
        assert((*pce)->op == TOKcomma);
        ce = (CommaExp *)(*pce);
        *pce = ce->e1;

        return ce->e2;
    }
}

dinteger_t Expression::toInteger()
{
    //printf("Expression %s\n", Token::toChars(op));
    error("integer constant expression expected instead of %s", toChars());
    return 0;
}

uinteger_t Expression::toUInteger()
{
    //printf("Expression %s\n", Token::toChars(op));
    return (uinteger_t)toInteger();
}

real_t Expression::toReal()
{
    error("floating point constant expression expected instead of %s", toChars());
    return CTFloat::zero;
}

real_t Expression::toImaginary()
{
    error("floating point constant expression expected instead of %s", toChars());
    return CTFloat::zero;
}

complex_t Expression::toComplex()
{
    error("floating point constant expression expected instead of %s", toChars());
    return complex_t(CTFloat::zero);
}

StringExp *Expression::toStringExp()
{
    return NULL;
}

TupleExp *Expression::toTupleExp()
{
    return NULL;
}

/***************************************
 * Return !=0 if expression is an lvalue.
 */

bool Expression::isLvalue()
{
    return false;
}

/*******************************
 * Give error if we're not an lvalue.
 * If we can, convert expression to be an lvalue.
 */

Expression *Expression::toLvalue(Scope *, Expression *e)
{
    if (!e)
        e = this;
    else if (!loc.filename)
        loc = e->loc;

    if (e->op == TOKtype)
        error("%s '%s' is a type, not an lvalue", e->type->kind(), e->type->toChars());
    else
        error("%s is not an lvalue", e->toChars());

    return new ErrorExp();
}

/***************************************
 * Parameters:
 *      sc:     scope
 *      flag:   1: do not issue error message for invalid modification
 * Returns:
 *      0:      is not modifiable
 *      1:      is modifiable in default == being related to type->isMutable()
 *      2:      is modifiable, because this is a part of initializing.
 */

int Expression::checkModifiable(Scope *, int)
{
    return type ? 1 : 0;    // default modifiable
}

Expression *Expression::modifiableLvalue(Scope *sc, Expression *e)
{
    //printf("Expression::modifiableLvalue() %s, type = %s\n", toChars(), type->toChars());

    // See if this expression is a modifiable lvalue (i.e. not const)
    if (checkModifiable(sc) == 1)
    {
        assert(type);
        if (!type->isMutable())
        {
            error("cannot modify %s expression %s", MODtoChars(type->mod), toChars());
            return new ErrorExp();
        }
        else if (!type->isAssignable())
        {
            error("cannot modify struct %s %s with immutable members", toChars(), type->toChars());
            return new ErrorExp();
        }
    }
    return toLvalue(sc, e);
}

/****************************************
 * Check that the expression has a valid type.
 * If not, generates an error "... has no type".
 * Returns:
 *      true if the expression is not valid.
 * Note:
 *      When this function returns true, `checkValue()` should also return true.
 */
bool Expression::checkType()
{
    return false;
}

/****************************************
 * Check that the expression has a valid value.
 * If not, generates an error "... has no value".
 * Returns:
 *      true if the expression is not valid or has void type.
 */
bool Expression::checkValue()
{
    if (type && type->toBasetype()->ty == Tvoid)
    {
        error("expression %s is void and has no value", toChars());
        //print(); halt();
        if (!global.gag)
            type = Type::terror;
        return true;
    }
    return false;
}

bool Expression::checkScalar()
{
    if (op == TOKerror)
        return true;
    if (type->toBasetype()->ty == Terror)
        return true;
    if (!type->isscalar())
    {
        error("'%s' is not a scalar, it is a %s", toChars(), type->toChars());
        return true;
    }
    return checkValue();
}

bool Expression::checkNoBool()
{
    if (op == TOKerror)
        return true;
    if (type->toBasetype()->ty == Terror)
        return true;
    if (type->toBasetype()->ty == Tbool)
    {
        error("operation not allowed on bool '%s'", toChars());
        return true;
    }
    return false;
}

bool Expression::checkIntegral()
{
    if (op == TOKerror)
        return true;
    if (type->toBasetype()->ty == Terror)
        return true;
    if (!type->isintegral())
    {
        error("'%s' is not of integral type, it is a %s", toChars(), type->toChars());
        return true;
    }
    return checkValue();
}

bool Expression::checkArithmetic()
{
    if (op == TOKerror)
        return true;
    if (type->toBasetype()->ty == Terror)
        return true;
    if (!type->isintegral() && !type->isfloating())
    {
        error("'%s' is not of arithmetic type, it is a %s", toChars(), type->toChars());
        return true;
    }
    return checkValue();
}

void Expression::checkDeprecated(Scope *sc, Dsymbol *s)
{
    s->checkDeprecated(loc, sc);
}

/*********************************************
 * Calling function f.
 * Check the purity, i.e. if we're in a pure function
 * we can only call other pure functions.
 * Returns true if error occurs.
 */
bool Expression::checkPurity(Scope *sc, FuncDeclaration *f)
{
    if (!sc->func)
        return false;
    if (sc->func == f)
        return false;
    if (sc->intypeof == 1)
        return false;
    if (sc->flags & (SCOPEctfe | SCOPEdebug))
        return false;

    /* Given:
     * void f() {
     *   pure void g() {
     *     /+pure+/ void h() {
     *       /+pure+/ void i() { }
     *     }
     *   }
     * }
     * g() can call h() but not f()
     * i() can call h() and g() but not f()
     */

    // Find the closest pure parent of the calling function
    FuncDeclaration *outerfunc = sc->func;
    FuncDeclaration *calledparent = f;

    if (outerfunc->isInstantiated())
    {
        // The attributes of outerfunc should be inferred from the call of f.
    }
    else if (f->isInstantiated())
    {
        // The attributes of f are inferred from its body.
    }
    else if (f->isFuncLiteralDeclaration())
    {
        // The attributes of f are always inferred in its declared place.
    }
    else
    {
        /* Today, static local functions are impure by default, but they cannot
         * violate purity of enclosing functions.
         *
         *  auto foo() pure {      // non instantiated funciton
         *    static auto bar() {  // static, without pure attribute
         *      impureFunc();      // impure call
         *      // Although impureFunc is called inside bar, f(= impureFunc)
         *      // is not callable inside pure outerfunc(= foo <- bar).
         *    }
         *
         *    bar();
         *    // Although bar is called inside foo, f(= bar) is callable
         *    // bacause calledparent(= foo) is same with outerfunc(= foo).
         *  }
         */

        while (outerfunc->toParent2() &&
               outerfunc->isPureBypassingInference() == PUREimpure &&
               outerfunc->toParent2()->isFuncDeclaration())
        {
            outerfunc = outerfunc->toParent2()->isFuncDeclaration();
            if (outerfunc->type->ty == Terror)
                return true;
        }
        while (calledparent->toParent2() &&
               calledparent->isPureBypassingInference() == PUREimpure &&
               calledparent->toParent2()->isFuncDeclaration())
        {
            calledparent = calledparent->toParent2()->isFuncDeclaration();
            if (calledparent->type->ty == Terror)
                return true;
        }
    }

    // If the caller has a pure parent, then either the called func must be pure,
    // OR, they must have the same pure parent.
    if (!f->isPure() && calledparent != outerfunc)
    {
        FuncDeclaration *ff = outerfunc;
        if (sc->flags & SCOPEcompile ? ff->isPureBypassingInference() >= PUREweak : ff->setImpure())
        {
            error("pure %s '%s' cannot call impure %s '%s'",
                ff->kind(), ff->toPrettyChars(), f->kind(), f->toPrettyChars());
            return true;
        }
    }
    return false;
}

/*******************************************
 * Accessing variable v.
 * Check for purity and safety violations.
 * Returns true if error occurs.
 */
bool Expression::checkPurity(Scope *sc, VarDeclaration *v)
{
    //printf("v = %s %s\n", v->type->toChars(), v->toChars());

    /* Look for purity and safety violations when accessing variable v
     * from current function.
     */
    if (!sc->func)
        return false;
    if (sc->intypeof == 1)
        return false;   // allow violations inside typeof(expression)
    if (sc->flags & (SCOPEctfe | SCOPEdebug))
        return false;   // allow violations inside compile-time evaluated expressions and debug conditionals
    if (v->ident == Id::ctfe)
        return false;   // magic variable never violates pure and safe
    if (v->isImmutable())
        return false;   // always safe and pure to access immutables...
    if (v->isConst() && !v->isRef() && (v->isDataseg() || v->isParameter()) &&
        v->type->implicitConvTo(v->type->immutableOf()))
        return false;   // or const global/parameter values which have no mutable indirections
    if (v->storage_class & STCmanifest)
        return false;   // ...or manifest constants

    bool err = false;
    if (v->isDataseg())
    {
        // Bugzilla 7533: Accessing implicit generated __gate is pure.
        if (v->ident == Id::gate)
            return false;

        /* Accessing global mutable state.
         * Therefore, this function and all its immediately enclosing
         * functions must be pure.
         */
        /* Today, static local functions are impure by default, but they cannot
         * violate purity of enclosing functions.
         *
         *  auto foo() pure {      // non instantiated funciton
         *    static auto bar() {  // static, without pure attribute
         *      globalData++;      // impure access
         *      // Although globalData is accessed inside bar,
         *      // it is not accessible inside pure foo.
         *    }
         *  }
         */
        for (Dsymbol *s = sc->func; s; s = s->toParent2())
        {
            FuncDeclaration *ff = s->isFuncDeclaration();
            if (!ff)
                break;
            if (sc->flags & SCOPEcompile ? ff->isPureBypassingInference() >= PUREweak : ff->setImpure())
            {
                error("pure %s '%s' cannot access mutable static data '%s'",
                    ff->kind(), ff->toPrettyChars(), v->toChars());
                err = true;
                break;
            }
            /* If the enclosing is an instantiated function or a lambda, its
             * attribute inference result is preferred.
             */
            if (ff->isInstantiated())
                break;
            if (ff->isFuncLiteralDeclaration())
                break;
        }
    }
    else
    {
        /* Given:
         * void f() {
         *   int fx;
         *   pure void g() {
         *     int gx;
         *     /+pure+/ void h() {
         *       int hx;
         *       /+pure+/ void i() { }
         *     }
         *   }
         * }
         * i() can modify hx and gx but not fx
         */

        Dsymbol *vparent = v->toParent2();
        for (Dsymbol *s = sc->func; !err && s; s = s->toParent2())
        {
            if (s == vparent)
                break;

            if (AggregateDeclaration *ad = s->isAggregateDeclaration())
            {
                if (ad->isNested())
                    continue;
                break;
            }
            FuncDeclaration *ff = s->isFuncDeclaration();
            if (!ff)
                break;
            if (ff->isNested() || ff->isThis())
            {
                if (ff->type->isImmutable() ||
                    (ff->type->isShared() && !MODimplicitConv(ff->type->mod, v->type->mod)))
                {
                    OutBuffer ffbuf;
                    OutBuffer vbuf;
                    MODMatchToBuffer(&ffbuf, ff->type->mod, v->type->mod);
                    MODMatchToBuffer(&vbuf, v->type->mod, ff->type->mod);
                    error("%s%s '%s' cannot access %sdata '%s'",
                        ffbuf.peekChars(), ff->kind(), ff->toPrettyChars(), vbuf.peekChars(), v->toChars());
                    err = true;
                    break;
                }
                continue;
            }
            break;
        }
    }

    /* Do not allow safe functions to access __gshared data
     */
    if (v->storage_class & STCgshared)
    {
        if (sc->func->setUnsafe())
        {
            error("safe %s '%s' cannot access __gshared data '%s'",
                sc->func->kind(), sc->func->toChars(), v->toChars());
            err = true;
        }
    }

    return err;
}

/*********************************************
 * Calling function f.
 * Check the safety, i.e. if we're in a @safe function
 * we can only call @safe or @trusted functions.
 * Returns true if error occurs.
 */
bool Expression::checkSafety(Scope *sc, FuncDeclaration *f)
{
    if (!sc->func)
        return false;
    if (sc->func == f)
        return false;
    if (sc->intypeof == 1)
        return false;
    if (sc->flags & SCOPEctfe)
        return false;

    if (!f->isSafe() && !f->isTrusted())
    {
        if (sc->flags & SCOPEcompile ? sc->func->isSafeBypassingInference() : sc->func->setUnsafe())
        {
            if (loc.linnum == 0)  // e.g. implicitly generated dtor
                loc = sc->func->loc;

            error("@safe %s '%s' cannot call @system %s '%s'",
                sc->func->kind(), sc->func->toPrettyChars(), f->kind(), f->toPrettyChars());
            return true;
        }
    }
    return false;
}

/*********************************************
 * Calling function f.
 * Check the @nogc-ness, i.e. if we're in a @nogc function
 * we can only call other @nogc functions.
 * Returns true if error occurs.
 */
bool Expression::checkNogc(Scope *sc, FuncDeclaration *f)
{
    if (!sc->func)
        return false;
    if (sc->func == f)
        return false;
    if (sc->intypeof == 1)
        return false;
    if (sc->flags & SCOPEctfe)
        return false;

    if (!f->isNogc())
    {
        if (sc->flags & SCOPEcompile ? sc->func->isNogcBypassingInference() : sc->func->setGC())
        {
            if (loc.linnum == 0)  // e.g. implicitly generated dtor
                loc = sc->func->loc;

            error("@nogc %s '%s' cannot call non-@nogc %s '%s'",
                sc->func->kind(), sc->func->toPrettyChars(), f->kind(), f->toPrettyChars());
            return true;
        }
    }
    return false;
}

/********************************************
 * Check that the postblit is callable if t is an array of structs.
 * Returns true if error happens.
 */
bool Expression::checkPostblit(Scope *sc, Type *t)
{
    t = t->baseElemOf();
    if (t->ty == Tstruct)
    {
        // Bugzilla 11395: Require TypeInfo generation for array concatenation
        semanticTypeInfo(sc, t);

        StructDeclaration *sd = ((TypeStruct *)t)->sym;
        if (sd->postblit)
        {
            if (sd->postblit->storage_class & STCdisable)
            {
                sd->error(loc, "is not copyable because it is annotated with @disable");
                return true;
            }
            //checkDeprecated(sc, sd->postblit);        // necessary?
            checkPurity(sc, sd->postblit);
            checkSafety(sc, sd->postblit);
            checkNogc(sc, sd->postblit);
            //checkAccess(sd, loc, sc, sd->postblit);   // necessary?
            return false;
        }
    }
    return false;
}

bool Expression::checkRightThis(Scope *sc)
{
    if (op == TOKerror)
        return true;
    if (op == TOKvar && type->ty != Terror)
    {
        VarExp *ve = (VarExp *)this;
        if (isNeedThisScope(sc, ve->var))
        {
            //printf("checkRightThis sc->intypeof = %d, ad = %p, func = %p, fdthis = %p\n",
            //        sc->intypeof, sc->getStructClassScope(), func, fdthis);
            error("need 'this' for '%s' of type '%s'", ve->var->toChars(), ve->var->type->toChars());
            return true;
        }
    }
    return false;
}

/*******************************
 * Check whether the expression allows RMW operations, error with rmw operator diagnostic if not.
 * ex is the RHS expression, or NULL if ++/-- is used (for diagnostics)
 * Returns true if error occurs.
 */
bool Expression::checkReadModifyWrite(TOK rmwOp, Expression *ex)
{
    //printf("Expression::checkReadModifyWrite() %s %s", toChars(), ex ? ex->toChars() : "");
    if (!type || !type->isShared())
        return false;

    // atomicOp uses opAssign (+=/-=) rather than opOp (++/--) for the CT string literal.
    switch (rmwOp)
    {
        case TOKplusplus:
        case TOKpreplusplus:
            rmwOp = TOKaddass;
            break;

        case TOKminusminus:
        case TOKpreminusminus:
            rmwOp = TOKminass;
            break;

        default:
            break;
    }

    deprecation("read-modify-write operations are not allowed for shared variables. "
                "Use core.atomic.atomicOp!\"%s\"(%s, %s) instead.",
                Token::tochars[rmwOp], toChars(), ex ? ex->toChars() : "1");
    return false;

    // note: enable when deprecation becomes an error.
    // return true;
}

/*****************************
 * If expression can be tested for true or false,
 * returns the modified expression.
 * Otherwise returns ErrorExp.
 */
Expression *Expression::toBoolean(Scope *sc)
{
    // Default is 'yes' - do nothing
    Expression *e = this;
    Type *t = type;
    Type *tb = type->toBasetype();
    Type *att = NULL;
Lagain:
    // Structs can be converted to bool using opCast(bool)()
    if (tb->ty == Tstruct)
    {
        AggregateDeclaration *ad = ((TypeStruct *)tb)->sym;
        /* Don't really need to check for opCast first, but by doing so we
         * get better error messages if it isn't there.
         */
        Dsymbol *fd = search_function(ad, Id::_cast);
        if (fd)
        {
            e = new CastExp(loc, e, Type::tbool);
            e = semantic(e, sc);
            return e;
        }

        // Forward to aliasthis.
        if (ad->aliasthis && tb != att)
        {
            if (!att && tb->checkAliasThisRec())
                att = tb;
            e = resolveAliasThis(sc, e);
            t = e->type;
            tb = e->type->toBasetype();
            goto Lagain;
        }
    }

    if (!t->isBoolean())
    {
        if (tb != Type::terror)
            error("expression %s of type %s does not have a boolean value", toChars(), t->toChars());
        return new ErrorExp();
    }
    return e;
}

/******************************
 * Take address of expression.
 */

Expression *Expression::addressOf()
{
    //printf("Expression::addressOf()\n");
    Expression *e = new AddrExp(loc, this);
    e->type = type->pointerTo();
    return e;
}

/******************************
 * If this is a reference, dereference it.
 */

Expression *Expression::deref()
{
    //printf("Expression::deref()\n");
    // type could be null if forward referencing an 'auto' variable
    if (type && type->ty == Treference)
    {
        Expression *e = new PtrExp(loc, this);
        e->type = ((TypeReference *)type)->next;
        return e;
    }
    return this;
}

/********************************
 * Does this expression statically evaluate to a boolean 'result' (true or false)?
 */
bool Expression::isBool(bool)
{
    return false;
}

IntegerExp *Expression::isIntegerExp()
{
    return op == TOKint64 ? (IntegerExp *)this : NULL;
}

ErrorExp *Expression::isErrorExp()
{
    return op == TOKerror ? (ErrorExp *)this : NULL;
}

VoidInitExp *Expression::isVoidInitExp()
{
    return op == TOKvoid ? (VoidInitExp *)this : NULL;
}

RealExp *Expression::isRealExp()
{
    return op == TOKfloat64 ? (RealExp *)this : NULL;
}

ComplexExp *Expression::isComplexExp()
{
    return op == TOKcomplex80 ? (ComplexExp *)this : NULL;
}

IdentifierExp *Expression::isIdentifierExp()
{
    return op == TOKidentifier ? (IdentifierExp *)this : NULL;
}

DollarExp *Expression::isDollarExp()
{
    return op == TOKdollar ? (DollarExp *)this : NULL;
}

DsymbolExp *Expression::isDsymbolExp()
{
    return op == TOKdsymbol ? (DsymbolExp *)this : NULL;
}

ThisExp *Expression::isThisExp()
{
    return op == TOKthis ? (ThisExp *)this : NULL;
}

SuperExp *Expression::isSuperExp()
{
    return op == TOKsuper ? (SuperExp *)this : NULL;
}

NullExp *Expression::isNullExp()
{
    return op == TOKnull ? (NullExp *)this : NULL;
}

StringExp *Expression::isStringExp()
{
    return op == TOKstring ? (StringExp *)this : NULL;
}

TupleExp *Expression::isTupleExp()
{
    return op == TOKtuple ? (TupleExp *)this : NULL;
}

ArrayLiteralExp *Expression::isArrayLiteralExp()
{
    return op == TOKarrayliteral ? (ArrayLiteralExp *)this : NULL;
}

AssocArrayLiteralExp *Expression::isAssocArrayLiteralExp()
{
    return op == TOKassocarrayliteral ? (AssocArrayLiteralExp *)this : NULL;
}

StructLiteralExp *Expression::isStructLiteralExp()
{
    return op == TOKstructliteral ? (StructLiteralExp *)this : NULL;
}

TypeExp *Expression::isTypeExp()
{
    return op == TOKtype ? (TypeExp *)this : NULL;
}

ScopeExp *Expression::isScopeExp()
{
    return op == TOKscope ? (ScopeExp *)this : NULL;
}

TemplateExp *Expression::isTemplateExp()
{
    return op == TOKtemplate ? (TemplateExp *)this : NULL;
}

NewExp *Expression::isNewExp()
{
    return op == TOKnew ? (NewExp *)this : NULL;
}

NewAnonClassExp *Expression::isNewAnonClassExp()
{
    return op == TOKnewanonclass ? (NewAnonClassExp *)this : NULL;
}

SymOffExp *Expression::isSymOffExp()
{
    return op == TOKsymoff ? (SymOffExp *)this : NULL;
}

VarExp *Expression::isVarExp()
{
    return op == TOKvar ? (VarExp *)this : NULL;
}

OverExp *Expression::isOverExp()
{
    return op == TOKoverloadset ? (OverExp *)this : NULL;
}

FuncExp *Expression::isFuncExp()
{
    return op == TOKfunction ? (FuncExp *)this : NULL;
}

DeclarationExp *Expression::isDeclarationExp()
{
    return op == TOKdeclaration ? (DeclarationExp *)this : NULL;
}

TypeidExp *Expression::isTypeidExp()
{
    return op == TOKtypeid ? (TypeidExp *)this : NULL;
}

TraitsExp *Expression::isTraitsExp()
{
    return op == TOKtraits ? (TraitsExp *)this : NULL;
}

HaltExp *Expression::isHaltExp()
{
    return op == TOKhalt ? (HaltExp *)this : NULL;
}

IsExp *Expression::isExp()
{
    return op == TOKis ? (IsExp *)this : NULL;
}

CompileExp *Expression::isCompileExp()
{
    return op == TOKmixin ? (CompileExp *)this : NULL;
}

ImportExp *Expression::isImportExp()
{
    return op == TOKimport ? (ImportExp *)this : NULL;
}

AssertExp *Expression::isAssertExp()
{
    return op == TOKassert ? (AssertExp *)this : NULL;
}

DotIdExp *Expression::isDotIdExp()
{
    return op == TOKdotid ? (DotIdExp *)this : NULL;
}

DotTemplateExp *Expression::isDotTemplateExp()
{
    return op == TOKdotti ? (DotTemplateExp *)this : NULL;
}

DotVarExp *Expression::isDotVarExp()
{
    return op == TOKdotvar ? (DotVarExp *)this : NULL;
}

DotTemplateInstanceExp *Expression::isDotTemplateInstanceExp()
{
    return op == TOKdotti ? (DotTemplateInstanceExp *)this : NULL;
}

DelegateExp *Expression::isDelegateExp()
{
    return op == TOKdelegate ? (DelegateExp *)this : NULL;
}

DotTypeExp *Expression::isDotTypeExp()
{
    return op == TOKdottype ? (DotTypeExp *)this : NULL;
}

CallExp *Expression::isCallExp()
{
    return op == TOKcall ? (CallExp *)this : NULL;
}

AddrExp *Expression::isAddrExp()
{
    return op == TOKaddress ? (AddrExp *)this : NULL;
}

PtrExp *Expression::isPtrExp()
{
    return op == TOKstar ? (PtrExp *)this : NULL;
}

NegExp *Expression::isNegExp()
{
    return op == TOKneg ? (NegExp *)this : NULL;
}

UAddExp *Expression::isUAddExp()
{
    return op == TOKuadd ? (UAddExp *)this : NULL;
}

ComExp *Expression::isComExp()
{
    return op == TOKtilde ? (ComExp *)this : NULL;
}

NotExp *Expression::isNotExp()
{
    return op == TOKnot ? (NotExp *)this : NULL;
}

DeleteExp *Expression::isDeleteExp()
{
    return op == TOKdelete ? (DeleteExp *)this : NULL;
}

CastExp *Expression::isCastExp()
{
    return op == TOKcast ? (CastExp *)this : NULL;
}

VectorExp *Expression::isVectorExp()
{
    return op == TOKvector ? (VectorExp *)this : NULL;
}

VectorArrayExp *Expression::isVectorArrayExp()
{
    return op == TOKvectorarray ? (VectorArrayExp *)this : NULL;
}

SliceExp *Expression::isSliceExp()
{
    return op == TOKslice ? (SliceExp *)this : NULL;
}

ArrayLengthExp *Expression::isArrayLengthExp()
{
    return op == TOKarraylength ? (ArrayLengthExp *)this : NULL;
}

ArrayExp *Expression::isArrayExp()
{
    return op == TOKarray ? (ArrayExp *)this : NULL;
}

DotExp *Expression::isDotExp()
{
    return op == TOKdot ? (DotExp *)this : NULL;
}

CommaExp *Expression::isCommaExp()
{
    return op == TOKcomma ? (CommaExp *)this : NULL;
}

IntervalExp *Expression::isIntervalExp()
{
    return op == TOKinterval ? (IntervalExp *)this : NULL;
}

DelegatePtrExp *Expression::isDelegatePtrExp()
{
    return op == TOKdelegateptr ? (DelegatePtrExp *)this : NULL;
}

DelegateFuncptrExp *Expression::isDelegateFuncptrExp()
{
    return op == TOKdelegatefuncptr ? (DelegateFuncptrExp *)this : NULL;
}

IndexExp *Expression::isIndexExp()
{
    return op == TOKindex ? (IndexExp *)this : NULL;
}

PostExp *Expression::isPostExp()
{
    return (op == TOKplusplus || op == TOKminusminus) ? (PostExp *)this : NULL;
}

PreExp *Expression::isPreExp()
{
    return (op == TOKpreplusplus || op == TOKpreminusminus) ? (PreExp *)this : NULL;
}

AssignExp *Expression::isAssignExp()
{
    return op == TOKassign ? (AssignExp *)this : NULL;
}

ConstructExp *Expression::isConstructExp()
{
    return op == TOKconstruct ? (ConstructExp *)this : NULL;
}

BlitExp *Expression::isBlitExp()
{
    return op == TOKblit ? (BlitExp *)this : NULL;
}

AddAssignExp *Expression::isAddAssignExp()
{
    return op == TOKaddass ? (AddAssignExp *)this : NULL;
}

MinAssignExp *Expression::isMinAssignExp()
{
    return op == TOKminass ? (MinAssignExp *)this : NULL;
}

MulAssignExp *Expression::isMulAssignExp()
{
    return op == TOKmulass ? (MulAssignExp *)this : NULL;
}


DivAssignExp *Expression::isDivAssignExp()
{
    return op == TOKdivass ? (DivAssignExp *)this : NULL;
}

ModAssignExp *Expression::isModAssignExp()
{
    return op == TOKmodass ? (ModAssignExp *)this : NULL;
}

AndAssignExp *Expression::isAndAssignExp()
{
    return op == TOKandass ? (AndAssignExp *)this : NULL;
}

OrAssignExp *Expression::isOrAssignExp()
{
    return op == TOKorass ? (OrAssignExp *)this : NULL;
}

XorAssignExp *Expression::isXorAssignExp()
{
    return op == TOKxorass ? (XorAssignExp *)this : NULL;
}

PowAssignExp *Expression::isPowAssignExp()
{
    return op == TOKpowass ? (PowAssignExp *)this : NULL;
}


ShlAssignExp *Expression::isShlAssignExp()
{
    return op == TOKshlass ? (ShlAssignExp *)this : NULL;
}

ShrAssignExp *Expression::isShrAssignExp()
{
    return op == TOKshrass ? (ShrAssignExp *)this : NULL;
}

UshrAssignExp *Expression::isUshrAssignExp()
{
    return op == TOKushrass ? (UshrAssignExp *)this : NULL;
}

CatAssignExp *Expression::isCatAssignExp()
{
    return op == TOKcatass ? (CatAssignExp *)this : NULL;
}

AddExp *Expression::isAddExp()
{
    return op == TOKadd ? (AddExp *)this : NULL;
}

MinExp *Expression::isMinExp()
{
    return op == TOKmin ? (MinExp *)this : NULL;
}

CatExp *Expression::isCatExp()
{
    return op == TOKcat ? (CatExp *)this : NULL;
}

MulExp *Expression::isMulExp()
{
    return op == TOKmul ? (MulExp *)this : NULL;
}

DivExp *Expression::isDivExp()
{
    return op == TOKdiv ? (DivExp *)this : NULL;
}

ModExp *Expression::isModExp()
{
    return op == TOKmod ? (ModExp *)this : NULL;
}

PowExp *Expression::isPowExp()
{
    return op == TOKpow ? (PowExp *)this : NULL;
}

ShlExp *Expression::isShlExp()
{
    return op == TOKshl ? (ShlExp *)this : NULL;
}

ShrExp *Expression::isShrExp()
{
    return op == TOKshr ? (ShrExp *)this : NULL;
}

UshrExp *Expression::isUshrExp()
{
    return op == TOKushr ? (UshrExp *)this : NULL;
}

AndExp *Expression::isAndExp()
{
    return op == TOKand ? (AndExp *)this : NULL;
}

OrExp *Expression::isOrExp()
{
    return op == TOKor ? (OrExp *)this : NULL;
}

XorExp *Expression::isXorExp()
{
    return op == TOKxor ? (XorExp *)this : NULL;
}

LogicalExp *Expression::isLogicalExp()
{
    return (op == TOKandand || op == TOKoror) ? (LogicalExp *)this : NULL;
}

InExp *Expression::isInExp()
{
    return op == TOKin ? (InExp *)this : NULL;
}

RemoveExp *Expression::isRemoveExp()
{
    return op == TOKremove ? (RemoveExp *)this : NULL;
}

EqualExp *Expression::isEqualExp()
{
    return (op == TOKequal || op == TOKnotequal) ? (EqualExp *)this : NULL;
}

IdentityExp *Expression::isIdentityExp()
{
    return (op == TOKidentity || op == TOKnotidentity) ? (IdentityExp *)this : NULL;
}

CondExp *Expression::isCondExp()
{
    return op == TOKquestion ? (CondExp *)this : NULL;
}

DefaultInitExp *Expression::isDefaultInitExp()
{
    return op == TOKdefault ? (DefaultInitExp *)this : NULL;
}

FileInitExp *Expression::isFileInitExp()
{
    return (op == TOKfile || op == TOKfilefullpath) ? (FileInitExp *)this : NULL;
}

LineInitExp *Expression::isLineInitExp()
{
    return op == TOKline ? (LineInitExp *)this : NULL;
}

ModuleInitExp *Expression::isModuleInitExp()
{
    return op == TOKmodulestring ? (ModuleInitExp *)this : NULL;
}

FuncInitExp *Expression::isFuncInitExp()
{
    return op == TOKfuncstring ? (FuncInitExp *)this : NULL;
}

PrettyFuncInitExp *Expression::isPrettyFuncInitExp()
{
    return op == TOKprettyfunc ? (PrettyFuncInitExp *)this : NULL;
}

ClassReferenceExp *Expression::isClassReferenceExp()
{
    return op == TOKclassreference ? (ClassReferenceExp *)this : NULL;
}


/****************************************
 * Resolve __FILE__, __LINE__, __MODULE__, __FUNCTION__, __PRETTY_FUNCTION__ to loc.
 */

Expression *Expression::resolveLoc(Loc, Scope *)
{
    return this;
}

Expressions *Expression::arraySyntaxCopy(Expressions *exps)
{
    Expressions *a = NULL;
    if (exps)
    {
        a = new Expressions();
        a->setDim(exps->length);
        for (size_t i = 0; i < a->length; i++)
        {
            Expression *e = (*exps)[i];
            (*a)[i] = e ? e->syntaxCopy() : NULL;
        }
    }
    return a;
}

/************************************************
 * Destructors are attached to VarDeclarations.
 * Hence, if expression returns a temp that needs a destructor,
 * make sure and create a VarDeclaration for that temp.
 */

Expression *Expression::addDtorHook(Scope *)
{
    return this;
}

/******************************** IntegerExp **************************/

IntegerExp::IntegerExp(Loc loc, dinteger_t value, Type *type)
        : Expression(loc, TOKint64, sizeof(IntegerExp))
{
    //printf("IntegerExp(value = %lld, type = '%s')\n", value, type ? type->toChars() : "");
    assert(type);
    if (!type->isscalar())
    {
        //printf("%s, loc = %d\n", toChars(), loc.linnum);
        if (type->ty != Terror)
            error("integral constant must be scalar type, not %s", type->toChars());
        type = Type::terror;
    }
    this->type = type;
    setInteger(value);
}

IntegerExp::IntegerExp(dinteger_t value)
        : Expression(Loc(), TOKint64, sizeof(IntegerExp))
{
    this->type = Type::tint32;
    this->value = (d_int32) value;
}

IntegerExp *IntegerExp::create(Loc loc, dinteger_t value, Type *type)
{
    return new IntegerExp(loc, value, type);
}

bool IntegerExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (((Expression *)o)->op == TOKint64)
    {
        IntegerExp *ne = (IntegerExp *)o;
        if (type->toHeadMutable()->equals(ne->type->toHeadMutable()) &&
            value == ne->value)
        {
            return true;
        }
    }
    return false;
}

void IntegerExp::setInteger(dinteger_t value)
{
    this->value = value;
    normalize();
}

void IntegerExp::normalize()
{
    /* 'Normalize' the value of the integer to be in range of the type
     */
    switch (type->toBasetype()->ty)
    {
        case Tbool:         value = (value != 0);           break;
        case Tint8:         value = (d_int8)  value;        break;
        case Tchar:
        case Tuns8:         value = (d_uns8)  value;        break;
        case Tint16:        value = (d_int16) value;        break;
        case Twchar:
        case Tuns16:        value = (d_uns16) value;        break;
        case Tint32:        value = (d_int32) value;        break;
        case Tdchar:
        case Tuns32:        value = (d_uns32) value;        break;
        case Tint64:        value = (d_int64) value;        break;
        case Tuns64:        value = (d_uns64) value;        break;
        case Tpointer:
            if (target.ptrsize == 8)
                value = (d_uns64) value;
            else if (target.ptrsize == 4)
                value = (d_uns32) value;
            else if (target.ptrsize == 2)
                value = (d_uns16) value;
            else
                assert(0);
            break;
        default:
            break;
    }
}

dinteger_t IntegerExp::toInteger()
{
    normalize();   // necessary until we fix all the paints of 'type'
    return value;
}

real_t IntegerExp::toReal()
{
    normalize();   // necessary until we fix all the paints of 'type'
    Type *t = type->toBasetype();
    if (t->ty == Tuns64)
        return ldouble((d_uns64)value);
    else
        return ldouble((d_int64)value);
}

real_t IntegerExp::toImaginary()
{
    return CTFloat::zero;
}

complex_t IntegerExp::toComplex()
{
    return (complex_t)toReal();
}

bool IntegerExp::isBool(bool result)
{
    bool r = toInteger() != 0;
    return result ? r : !r;
}

Expression *IntegerExp::toLvalue(Scope *, Expression *e)
{
    if (!e)
        e = this;
    else if (!loc.filename)
        loc = e->loc;
    e->error("constant %s is not an lvalue", e->toChars());
    return new ErrorExp();
}

/******************************** ErrorExp **************************/

/* Use this expression for error recovery.
 * It should behave as a 'sink' to prevent further cascaded error messages.
 */

ErrorExp::ErrorExp()
    : Expression(Loc(), TOKerror, sizeof(ErrorExp))
{
    type = Type::terror;
}

Expression *ErrorExp::toLvalue(Scope *, Expression *)
{
    return this;
}

/******************************** RealExp **************************/

RealExp::RealExp(Loc loc, real_t value, Type *type)
        : Expression(loc, TOKfloat64, sizeof(RealExp))
{
    //printf("RealExp::RealExp(%Lg)\n", value);
    this->value = value;
    this->type = type;
}

RealExp *RealExp::create(Loc loc, real_t value, Type *type)
{
    return new RealExp(loc, value,type);
}

dinteger_t RealExp::toInteger()
{
    return (sinteger_t) toReal();
}

uinteger_t RealExp::toUInteger()
{
    return (uinteger_t) toReal();
}

real_t RealExp::toReal()
{
    return type->isreal() ? value : CTFloat::zero;
}

real_t RealExp::toImaginary()
{
    return type->isreal() ? CTFloat::zero : value;
}

complex_t RealExp::toComplex()
{
    return complex_t(toReal(), toImaginary());
}

/********************************
 * Test to see if two reals are the same.
 * Regard NaN's as equivalent.
 * Regard +0 and -0 as different.
 */

int RealEquals(real_t x1, real_t x2)
{
    return (CTFloat::isNaN(x1) && CTFloat::isNaN(x2)) ||
        CTFloat::isIdentical(x1, x2);
}

bool RealExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (((Expression *)o)->op == TOKfloat64)
    {
        RealExp *ne = (RealExp *)o;
        if (type->toHeadMutable()->equals(ne->type->toHeadMutable()) &&
            RealEquals(value, ne->value))
        {
            return true;
        }
    }
    return false;
}

bool RealExp::isBool(bool result)
{
    return result ? (bool)value : !(bool)value;
}

/******************************** ComplexExp **************************/

ComplexExp::ComplexExp(Loc loc, complex_t value, Type *type)
        : Expression(loc, TOKcomplex80, sizeof(ComplexExp)), value(value)
{
    this->type = type;
    //printf("ComplexExp::ComplexExp(%s)\n", toChars());
}

ComplexExp *ComplexExp::create(Loc loc, complex_t value, Type *type)
{
    return new ComplexExp(loc, value, type);
}

dinteger_t ComplexExp::toInteger()
{
    return (sinteger_t) toReal();
}

uinteger_t ComplexExp::toUInteger()
{
    return (uinteger_t) toReal();
}

real_t ComplexExp::toReal()
{
    return creall(value);
}

real_t ComplexExp::toImaginary()
{
    return cimagl(value);
}

complex_t ComplexExp::toComplex()
{
    return value;
}

bool ComplexExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (((Expression *)o)->op == TOKcomplex80)
    {
        ComplexExp *ne = (ComplexExp *)o;
        if (type->toHeadMutable()->equals(ne->type->toHeadMutable()) &&
            RealEquals(creall(value), creall(ne->value)) &&
            RealEquals(cimagl(value), cimagl(ne->value)))
        {
            return true;
        }
    }
    return false;
}

bool ComplexExp::isBool(bool result)
{
    if (result)
        return (bool)(value);
    else
        return !value;
}

/******************************** IdentifierExp **************************/

IdentifierExp::IdentifierExp(Loc loc, Identifier *ident)
        : Expression(loc, TOKidentifier, sizeof(IdentifierExp))
{
    this->ident = ident;
}

IdentifierExp *IdentifierExp::create(Loc loc, Identifier *ident)
{
    return new IdentifierExp(loc, ident);
}

bool IdentifierExp::isLvalue()
{
    return true;
}

Expression *IdentifierExp::toLvalue(Scope *, Expression *)
{
    return this;
}

/******************************** DollarExp **************************/

DollarExp::DollarExp(Loc loc)
        : IdentifierExp(loc, Id::dollar)
{
}

/******************************** DsymbolExp **************************/

DsymbolExp::DsymbolExp(Loc loc, Dsymbol *s, bool hasOverloads)
        : Expression(loc, TOKdsymbol, sizeof(DsymbolExp))
{
    this->s = s;
    this->hasOverloads = hasOverloads;
}

/****************************************
 * Resolve a symbol `s` and wraps it in an expression object.
 * Params:
 *      hasOverloads = works if the aliased symbol is a function.
 *          true:  it's overloaded and will be resolved later.
 *          false: it's exact function symbol.
 */
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads)
{
Lagain:
    Expression *e;

    //printf("DsymbolExp:: %p '%s' is a symbol\n", this, toChars());
    //printf("s = '%s', s->kind = '%s'\n", s->toChars(), s->kind());
    Dsymbol *olds = s;
    Declaration *d = s->isDeclaration();
    if (d && (d->storage_class & STCtemplateparameter))
    {
        s = s->toAlias();
    }
    else
    {
        if (!s->isFuncDeclaration())        // functions are checked after overloading
            s->checkDeprecated(loc, sc);

        // Bugzilla 12023: if 's' is a tuple variable, the tuple is returned.
        s = s->toAlias();

        //printf("s = '%s', s->kind = '%s', s->needThis() = %p\n", s->toChars(), s->kind(), s->needThis());
        if (s != olds && !s->isFuncDeclaration())
            s->checkDeprecated(loc, sc);
    }

    if (EnumMember *em = s->isEnumMember())
    {
        return em->getVarExp(loc, sc);
    }
    if (VarDeclaration *v = s->isVarDeclaration())
    {
        //printf("Identifier '%s' is a variable, type '%s'\n", toChars(), v->type->toChars());
        if (!v->type ||                    // during variable type inference
            (!v->type->deco && v->inuse))  // during variable type semantic
        {
            if (v->inuse)    // variable type depends on the variable itself
                ::error(loc, "circular reference to %s '%s'", v->kind(), v->toPrettyChars());
            else             // variable type cannot be determined
                ::error(loc, "forward reference to %s '%s'", v->kind(), v->toPrettyChars());
            return new ErrorExp();
        }
        if (v->type->ty == Terror)
            return new ErrorExp();

        if ((v->storage_class & STCmanifest) && v->_init)
        {
            if (v->inuse)
            {
                ::error(loc, "circular initialization of %s '%s'", v->kind(), v->toPrettyChars());
                return new ErrorExp();
            }

            e = v->expandInitializer(loc);
            v->inuse++;
            e = semantic(e, sc);
            v->inuse--;
            return e;
        }

        // Change the ancestor lambdas to delegate before hasThis(sc) call.
        if (v->checkNestedReference(sc, loc))
            return new ErrorExp();

        if (v->needThis() && hasThis(sc))
            e = new DotVarExp(loc, new ThisExp(loc), v);
        else
            e = new VarExp(loc, v);
        e = semantic(e, sc);
        return e;
    }
    if (FuncLiteralDeclaration *fld = s->isFuncLiteralDeclaration())
    {
        //printf("'%s' is a function literal\n", fld->toChars());
        e = new FuncExp(loc, fld);
        return semantic(e, sc);
    }
    if (FuncDeclaration *f = s->isFuncDeclaration())
    {
        f = f->toAliasFunc();
        if (!f->functionSemantic())
            return new ErrorExp();

        if (!hasOverloads && f->checkForwardRef(loc))
            return new ErrorExp();

        FuncDeclaration *fd = s->isFuncDeclaration();
        fd->type = f->type;
        return new VarExp(loc, fd, hasOverloads);
    }
    if (OverDeclaration *od = s->isOverDeclaration())
    {
        e = new VarExp(loc, od, true);
        e->type = Type::tvoid;
        return e;
    }
    if (OverloadSet *o = s->isOverloadSet())
    {
        //printf("'%s' is an overload set\n", o->toChars());
        return new OverExp(loc, o);
    }

    if (Import *imp = s->isImport())
    {
        if (!imp->pkg)
        {
            ::error(loc, "forward reference of import %s", imp->toChars());
            return new ErrorExp();
        }
        ScopeExp *ie = new ScopeExp(loc, imp->pkg);
        return semantic(ie, sc);
    }
    if (Package *pkg = s->isPackage())
    {
        ScopeExp *ie = new ScopeExp(loc, pkg);
        return semantic(ie, sc);
    }
    if (Module *mod = s->isModule())
    {
        ScopeExp *ie = new ScopeExp(loc, mod);
        return semantic(ie, sc);
    }

    if (Nspace *ns = s->isNspace())
    {
        ScopeExp *ie = new ScopeExp(loc, ns);
        return semantic(ie, sc);
    }

    if (Type *t = s->getType())
    {
        return semantic(new TypeExp(loc, t), sc);
    }

    if (TupleDeclaration *tup = s->isTupleDeclaration())
    {
        if (tup->needThis() && hasThis(sc))
            e = new DotVarExp(loc, new ThisExp(loc), tup);
        else
            e = new TupleExp(loc, tup);
        e = semantic(e, sc);
        return e;
    }

    if (TemplateInstance *ti = s->isTemplateInstance())
    {
        ti->semantic(sc);
        if (!ti->inst || ti->errors)
            return new ErrorExp();
        s = ti->toAlias();
        if (!s->isTemplateInstance())
            goto Lagain;
        e = new ScopeExp(loc, ti);
        e = semantic(e, sc);
        return e;
    }
    if (TemplateDeclaration *td = s->isTemplateDeclaration())
    {
        Dsymbol *p = td->toParent2();
        FuncDeclaration *fdthis = hasThis(sc);
        AggregateDeclaration *ad = p ? p->isAggregateDeclaration() : NULL;
        if (fdthis && ad && isAggregate(fdthis->vthis->type) == ad &&
            (td->_scope->stc & STCstatic) == 0)
        {
            e = new DotTemplateExp(loc, new ThisExp(loc), td);
        }
        else
            e = new TemplateExp(loc, td);
        e = semantic(e, sc);
        return e;
    }

    ::error(loc, "%s '%s' is not a variable", s->kind(), s->toChars());
    return new ErrorExp();
}

bool DsymbolExp::isLvalue()
{
    return true;
}

Expression *DsymbolExp::toLvalue(Scope *, Expression *)
{
    return this;
}

/******************************** ThisExp **************************/

ThisExp::ThisExp(Loc loc)
        : Expression(loc, TOKthis, sizeof(ThisExp))
{
    //printf("ThisExp::ThisExp() loc = %d\n", loc.linnum);
    var = NULL;
}

bool ThisExp::isBool(bool result)
{
    return result ? true : false;
}

bool ThisExp::isLvalue()
{
    // Class `this` should be an rvalue; struct `this` should be an lvalue.
    return type->toBasetype()->ty != Tclass;
}

Expression *ThisExp::toLvalue(Scope *sc, Expression *e)
{
    if (type->toBasetype()->ty == Tclass)
    {
        // Class `this` is an rvalue; struct `this` is an lvalue.
        return Expression::toLvalue(sc, e);
    }
    return this;
}

/******************************** SuperExp **************************/

SuperExp::SuperExp(Loc loc)
        : ThisExp(loc)
{
    op = TOKsuper;
}

/******************************** NullExp **************************/

NullExp::NullExp(Loc loc, Type *type)
        : Expression(loc, TOKnull, sizeof(NullExp))
{
    committed = 0;
    this->type = type;
}

bool NullExp::equals(RootObject *o)
{
    if (o && o->dyncast() == DYNCAST_EXPRESSION)
    {
        Expression *e = (Expression *)o;
        if (e->op == TOKnull &&
            type->equals(e->type))
        {
            return true;
        }
    }
    return false;
}

bool NullExp::isBool(bool result)
{
    return result ? false : true;
}

StringExp *NullExp::toStringExp()
{
    if (implicitConvTo(Type::tstring))
    {
        StringExp *se = new StringExp(loc, (char*)mem.xcalloc(1, 1), 0);
        se->type = Type::tstring;
        return se;
    }
    return NULL;
}

/******************************** StringExp **************************/

StringExp::StringExp(Loc loc, char *string)
        : Expression(loc, TOKstring, sizeof(StringExp))
{
    this->string = string;
    this->len = strlen(string);
    this->sz = 1;
    this->committed = 0;
    this->postfix = 0;
    this->ownedByCtfe = OWNEDcode;
}

StringExp::StringExp(Loc loc, void *string, size_t len)
        : Expression(loc, TOKstring, sizeof(StringExp))
{
    this->string = string;
    this->len = len;
    this->sz = 1;
    this->committed = 0;
    this->postfix = 0;
    this->ownedByCtfe = OWNEDcode;
}

StringExp::StringExp(Loc loc, void *string, size_t len, utf8_t postfix)
        : Expression(loc, TOKstring, sizeof(StringExp))
{
    this->string = string;
    this->len = len;
    this->sz = 1;
    this->committed = 0;
    this->postfix = postfix;
    this->ownedByCtfe = OWNEDcode;
}

StringExp *StringExp::create(Loc loc, char *s)
{
    return new StringExp(loc, s);
}

StringExp *StringExp::create(Loc loc, void *string, size_t len)
{
    return new StringExp(loc, string, len);
}

bool StringExp::equals(RootObject *o)
{
    //printf("StringExp::equals('%s') %s\n", o->toChars(), toChars());
    if (o && o->dyncast() == DYNCAST_EXPRESSION)
    {
        Expression *e = (Expression *)o;
        if (e->op == TOKstring)
        {
            return compare(o) == 0;
        }
    }
    return false;
}

/**********************************
 * Return the number of code units the string would be if it were re-encoded
 * as tynto.
 * Params:
 *      tynto = code unit type of the target encoding
 * Returns:
 *      number of code units
 */

size_t StringExp::numberOfCodeUnits(int tynto) const
{
    int encSize;
    switch (tynto)
    {
        case 0:      return len;
        case Tchar:  encSize = 1; break;
        case Twchar: encSize = 2; break;
        case Tdchar: encSize = 4; break;
        default:
            assert(0);
    }
    if (sz == encSize)
        return len;

    size_t result = 0;
    dchar_t c;

    switch (sz)
    {
        case 1:
            for (size_t u = 0; u < len;)
            {
                if (const char *p = utf_decodeChar((utf8_t *)string, len, &u, &c))
                {
                    error("%s", p);
                    return 0;
                }
                result += utf_codeLength(encSize, c);
            }
            break;

        case 2:
            for (size_t u = 0; u < len;)
            {
                if (const char *p = utf_decodeWchar((utf16_t *)string, len, &u, &c))
                {
                    error("%s", p);
                    return 0;
                }
                result += utf_codeLength(encSize, c);
            }
            break;

        case 4:
            for (size_t u = 0; u < len;)
            {
                c = *((utf32_t *)((char *)string + u));
                u += 4;
                result += utf_codeLength(encSize, c);
            }
            break;

        default:
            assert(0);
    }
    return result;
}

/**********************************************
 * Write the contents of the string to dest.
 * Use numberOfCodeUnits() to determine size of result.
 * Params:
 *  dest = destination
 *  tyto = encoding type of the result
 *  zero = add terminating 0
 */
void StringExp::writeTo(void *dest, bool zero, int tyto) const
{
    int encSize;
    switch (tyto)
    {
        case 0:      encSize = sz; break;
        case Tchar:  encSize = 1; break;
        case Twchar: encSize = 2; break;
        case Tdchar: encSize = 4; break;
        default:
            assert(0);
    }
    if (sz == encSize)
    {
        memcpy(dest, string, len * sz);
        if (zero)
            memset((char *)dest + len * sz, 0, sz);
    }
    else
        assert(0);
}

/**************************************************
 * If the string data is UTF-8 and can be accessed directly,
 * return a pointer to it.
 * Do not assume a terminating 0.
 * Returns:
 *  pointer to string data if possible, null if not
 */
char *StringExp::toPtr()
{
    return (sz == 1) ? (char*)string : NULL;
}

StringExp *StringExp::toStringExp()
{
    return this;
}

/****************************************
 * Convert string to char[].
 */

StringExp *StringExp::toUTF8(Scope *sc)
{
    if (sz != 1)
    {   // Convert to UTF-8 string
        committed = 0;
        Expression *e = castTo(sc, Type::tchar->arrayOf());
        e = e->optimize(WANTvalue);
        assert(e->op == TOKstring);
        StringExp *se = (StringExp *)e;
        assert(se->sz == 1);
        return se;
    }
    return this;
}

int StringExp::compare(RootObject *obj)
{
    //printf("StringExp::compare()\n");
    // Used to sort case statement expressions so we can do an efficient lookup
    StringExp *se2 = (StringExp *)(obj);

    // This is a kludge so isExpression() in template.c will return 5
    // for StringExp's.
    if (!se2)
        return 5;

    assert(se2->op == TOKstring);

    size_t len1 = len;
    size_t len2 = se2->len;

    //printf("sz = %d, len1 = %d, len2 = %d\n", sz, (int)len1, (int)len2);
    if (len1 == len2)
    {
        switch (sz)
        {
            case 1:
                return memcmp((char *)string, (char *)se2->string, len1);

            case 2:
            {
                d_uns16 *s1 = (d_uns16 *)string;
                d_uns16 *s2 = (d_uns16 *)se2->string;

                for (size_t u = 0; u < len; u++)
                {
                    if (s1[u] != s2[u])
                        return s1[u] - s2[u];
                }
            }
            break;

            case 4:
            {
                d_uns32 *s1 = (d_uns32 *)string;
                d_uns32 *s2 = (d_uns32 *)se2->string;

                for (size_t u = 0; u < len; u++)
                {
                    if (s1[u] != s2[u])
                        return s1[u] - s2[u];
                }
            }
            break;

            default:
                assert(0);
        }
    }
    return (int)(len1 - len2);
}

bool StringExp::isBool(bool result)
{
    return result ? true : false;
}


bool StringExp::isLvalue()
{
    /* string literal is rvalue in default, but
     * conversion to reference of static array is only allowed.
     */
    return (type && type->toBasetype()->ty == Tsarray);
}

Expression *StringExp::toLvalue(Scope *sc, Expression *e)
{
    //printf("StringExp::toLvalue(%s) type = %s\n", toChars(), type ? type->toChars() : NULL);
    return (type && type->toBasetype()->ty == Tsarray)
            ? this : Expression::toLvalue(sc, e);
}

Expression *StringExp::modifiableLvalue(Scope *, Expression *)
{
    error("cannot modify string literal %s", toChars());
    return new ErrorExp();
}

unsigned StringExp::charAt(uinteger_t i) const
{   unsigned value;

    switch (sz)
    {
        case 1:
            value = ((utf8_t *)string)[(size_t)i];
            break;

        case 2:
            value = ((unsigned short *)string)[(size_t)i];
            break;

        case 4:
            value = ((unsigned int *)string)[(size_t)i];
            break;

        default:
            assert(0);
            break;
    }
    return value;
}

/************************ ArrayLiteralExp ************************************/

// [ e1, e2, e3, ... ]

ArrayLiteralExp::ArrayLiteralExp(Loc loc, Type *type, Expressions *elements)
    : Expression(loc, TOKarrayliteral, sizeof(ArrayLiteralExp))
{
    this->basis = NULL;
    this->type = type;
    this->elements = elements;
    this->ownedByCtfe = OWNEDcode;
}

ArrayLiteralExp::ArrayLiteralExp(Loc loc, Type *type, Expression *e)
    : Expression(loc, TOKarrayliteral, sizeof(ArrayLiteralExp))
{
    this->basis = NULL;
    this->type = type;
    elements = new Expressions;
    elements->push(e);
    this->ownedByCtfe = OWNEDcode;
}

ArrayLiteralExp::ArrayLiteralExp(Loc loc, Type *type, Expression *basis, Expressions *elements)
    : Expression(loc, TOKarrayliteral, sizeof(ArrayLiteralExp))
{
    this->basis = basis;
    this->type = type;
    this->elements = elements;
    this->ownedByCtfe = OWNEDcode;
}

ArrayLiteralExp *ArrayLiteralExp::create(Loc loc, Expressions *elements)
{
    return new ArrayLiteralExp(loc, NULL, elements);
}

bool ArrayLiteralExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (o && o->dyncast() == DYNCAST_EXPRESSION &&
        ((Expression *)o)->op == TOKarrayliteral)
    {
        ArrayLiteralExp *ae = (ArrayLiteralExp *)o;
        if (elements->length != ae->elements->length)
            return false;
        if (elements->length == 0 &&
            !type->equals(ae->type))
        {
            return false;
        }
        for (size_t i = 0; i < elements->length; i++)
        {
            Expression *e1 = (*elements)[i];
            Expression *e2 = (*ae->elements)[i];
            if (!e1)
                e1 = basis;
            if (!e2)
                e2 = basis;
            if (e1 != e2 &&
                (!e1 || !e2 || !e1->equals(e2)))
                return false;
        }
        return true;
    }
    return false;
}

Expression *ArrayLiteralExp::syntaxCopy()
{
    return new ArrayLiteralExp(loc,
        NULL,
        basis ? basis->syntaxCopy() : NULL,
        arraySyntaxCopy(elements));
}

Expression *ArrayLiteralExp::getElement(d_size_t i)
{
    Expression *el = (*elements)[i];
    if (!el)
        el = basis;
    return el;
}

static void appendArrayLiteral(Expressions *elems, ArrayLiteralExp *ale)
{
    if (!ale->elements)
        return;
    size_t d = elems->length;
    elems->append(ale->elements);
    for (size_t i = d; i < elems->length; i++)
    {
        Expression *el = (*elems)[i];
        if (!el)
            (*elems)[i] = ale->basis;
    }
}

/* Copy element `Expressions` in the parameters when they're `ArrayLiteralExp`s.
 * Params:
 *      e1  = If it's ArrayLiteralExp, its `elements` will be copied.
 *            Otherwise, `e1` itself will be pushed into the new `Expressions`.
 *      e2  = If it's not `null`, it will be pushed/appended to the new
 *            `Expressions` by the same way with `e1`.
 * Returns:
 *      Newly allocated `Expressions`. Note that it points to the original
 *      `Expression` values in e1 and e2.
 */
Expressions* ArrayLiteralExp::copyElements(Expression *e1, Expression *e2)
{
    Expressions *elems = new Expressions();

    if (e1->op == TOKarrayliteral)
        appendArrayLiteral(elems, (ArrayLiteralExp *)e1);
    else
        elems->push(e1);

    if (e2)
    {
        if (e2->op == TOKarrayliteral)
            appendArrayLiteral(elems, (ArrayLiteralExp *)e2);
        else
            elems->push(e2);
    }

    return elems;
}

bool ArrayLiteralExp::isBool(bool result)
{
    size_t dim = elements ? elements->length : 0;
    return result ? (dim != 0) : (dim == 0);
}

StringExp *ArrayLiteralExp::toStringExp()
{
    TY telem = type->nextOf()->toBasetype()->ty;

    if (telem == Tchar || telem == Twchar || telem == Tdchar ||
        (telem == Tvoid && (!elements || elements->length == 0)))
    {
        unsigned char sz = 1;
        if (telem == Twchar) sz = 2;
        else if (telem == Tdchar) sz = 4;

        OutBuffer buf;
        if (elements)
        {
            for (size_t i = 0; i < elements->length; ++i)
            {
                Expression *ch = getElement(i);
                if (ch->op != TOKint64)
                    return NULL;
                if (sz == 1)
                    buf.writeByte((unsigned)ch->toInteger());
                else if (sz == 2)
                    buf.writeword((unsigned)ch->toInteger());
                else
                    buf.write4((unsigned)ch->toInteger());
            }
        }
        char prefix;
             if (sz == 1) { prefix = 'c'; buf.writeByte(0); }
        else if (sz == 2) { prefix = 'w'; buf.writeword(0); }
        else              { prefix = 'd'; buf.write4(0); }

        const size_t len = buf.length() / sz - 1;
        StringExp *se = new StringExp(loc, buf.extractData(), len, prefix);
        se->sz = sz;
        se->type = type;
        return se;
    }
    return NULL;
}

/************************ AssocArrayLiteralExp ************************************/

// [ key0 : value0, key1 : value1, ... ]

AssocArrayLiteralExp::AssocArrayLiteralExp(Loc loc,
                Expressions *keys, Expressions *values)
    : Expression(loc, TOKassocarrayliteral, sizeof(AssocArrayLiteralExp))
{
    assert(keys->length == values->length);
    this->keys = keys;
    this->values = values;
    this->ownedByCtfe = OWNEDcode;
}

bool AssocArrayLiteralExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (o && o->dyncast() == DYNCAST_EXPRESSION &&
        ((Expression *)o)->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)o;
        if (keys->length != ae->keys->length)
            return false;
        size_t count = 0;
        for (size_t i = 0; i < keys->length; i++)
        {
            for (size_t j = 0; j < ae->keys->length; j++)
            {
                if ((*keys)[i]->equals((*ae->keys)[j]))
                {
                    if (!(*values)[i]->equals((*ae->values)[j]))
                        return false;
                    ++count;
                }
            }
        }
        return count == keys->length;
    }
    return false;
}

Expression *AssocArrayLiteralExp::syntaxCopy()
{
    return new AssocArrayLiteralExp(loc,
        arraySyntaxCopy(keys), arraySyntaxCopy(values));
}

bool AssocArrayLiteralExp::isBool(bool result)
{
    size_t dim = keys->length;
    return result ? (dim != 0) : (dim == 0);
}

/************************ StructLiteralExp ************************************/

// sd( e1, e2, e3, ... )

StructLiteralExp::StructLiteralExp(Loc loc, StructDeclaration *sd, Expressions *elements, Type *stype)
    : Expression(loc, TOKstructliteral, sizeof(StructLiteralExp))
{
    this->sd = sd;
    if (!elements)
        elements = new Expressions();
    this->elements = elements;
    this->stype = stype;
    this->useStaticInit = false;
    this->sym = NULL;
    this->ownedByCtfe = OWNEDcode;
    this->origin = this;
    this->stageflags = 0;
    this->inlinecopy = NULL;
    //printf("StructLiteralExp::StructLiteralExp(%s)\n", toChars());
}

StructLiteralExp *StructLiteralExp::create(Loc loc, StructDeclaration *sd, void *elements, Type *stype)
{
    return new StructLiteralExp(loc, sd, (Expressions *)elements, stype);
}

bool StructLiteralExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (o && o->dyncast() == DYNCAST_EXPRESSION &&
        ((Expression *)o)->op == TOKstructliteral)
    {
        StructLiteralExp *se = (StructLiteralExp *)o;
        if (!type->equals(se->type))
            return false;
        if (elements->length != se->elements->length)
            return false;
        for (size_t i = 0; i < elements->length; i++)
        {
            Expression *e1 = (*elements)[i];
            Expression *e2 = (*se->elements)[i];
            if (e1 != e2 &&
                (!e1 || !e2 || !e1->equals(e2)))
                return false;
        }
        return true;
    }
    return false;
}

Expression *StructLiteralExp::syntaxCopy()
{
    StructLiteralExp *exp = new StructLiteralExp(loc, sd, arraySyntaxCopy(elements), type ? type : stype);
    exp->origin = this;
    return exp;
}

Expression *StructLiteralExp::addDtorHook(Scope *sc)
{
    /* If struct requires a destructor, rewrite as:
     *    (S tmp = S()),tmp
     * so that the destructor can be hung on tmp.
     */
    if (sd->dtor && sc->func)
    {
        /* Make an identifier for the temporary of the form:
         *   __sl%s%d, where %s is the struct name
         */
        const size_t len = 10;
        char buf[len + 1];
        buf[len] = 0;
        strcpy(buf, "__sl");
        strncat(buf, sd->ident->toChars(), len - 4 - 1);
        assert(buf[len] == 0);

        VarDeclaration *tmp = copyToTemp(0, buf, this);
        Expression *ae = new DeclarationExp(loc, tmp);
        Expression *e = new CommaExp(loc, ae, new VarExp(loc, tmp));
        e = semantic(e, sc);
        return e;
    }
    return this;
}

/**************************************
 * Gets expression at offset of type.
 * Returns NULL if not found.
 */

Expression *StructLiteralExp::getField(Type *type, unsigned offset)
{
    //printf("StructLiteralExp::getField(this = %s, type = %s, offset = %u)\n",
    //  /*toChars()*/"", type->toChars(), offset);
    Expression *e = NULL;
    int i = getFieldIndex(type, offset);

    if (i != -1)
    {
        //printf("\ti = %d\n", i);
        if (i == (int)sd->fields.length - 1 && sd->isNested())
            return NULL;

        assert(i < (int)elements->length);
        e = (*elements)[i];
        if (e)
        {
            //printf("e = %s, e->type = %s\n", e->toChars(), e->type->toChars());

            /* If type is a static array, and e is an initializer for that array,
             * then the field initializer should be an array literal of e.
             */
            if (e->type->castMod(0) != type->castMod(0) && type->ty == Tsarray)
            {   TypeSArray *tsa = (TypeSArray *)type;
                size_t length = (size_t)tsa->dim->toInteger();
                Expressions *z = new Expressions;
                z->setDim(length);
                for (size_t q = 0; q < length; ++q)
                    (*z)[q] = e->copy();
                e = new ArrayLiteralExp(loc, type, z);
            }
            else
            {
                e = e->copy();
                e->type = type;
            }
            if (useStaticInit && e->op == TOKstructliteral &&
                e->type->needsNested())
            {
                StructLiteralExp *se = (StructLiteralExp *)e;
                se->useStaticInit = true;
            }
        }
    }
    return e;
}

/************************************
 * Get index of field.
 * Returns -1 if not found.
 */

int StructLiteralExp::getFieldIndex(Type *type, unsigned offset)
{
    /* Find which field offset is by looking at the field offsets
     */
    if (elements->length)
    {
        for (size_t i = 0; i < sd->fields.length; i++)
        {
            VarDeclaration *v = sd->fields[i];

            if (offset == v->offset &&
                type->size() == v->type->size())
            {
                /* context field might not be filled. */
                if (i == sd->fields.length - 1 && sd->isNested())
                    return (int)i;
                Expression *e = (*elements)[i];
                if (e)
                {
                    return (int)i;
                }
                break;
            }
        }
    }
    return -1;
}

/************************ TypeDotIdExp ************************************/

/* Things like:
 *      int.size
 *      foo.size
 *      (foo).size
 *      cast(foo).size
 */

DotIdExp *typeDotIdExp(Loc loc, Type *type, Identifier *ident)
{
    return new DotIdExp(loc, new TypeExp(loc, type), ident);
}


/************************************************************/

// Mainly just a placeholder

TypeExp::TypeExp(Loc loc, Type *type)
    : Expression(loc, TOKtype, sizeof(TypeExp))
{
    //printf("TypeExp::TypeExp(%s)\n", type->toChars());
    this->type = type;
}

Expression *TypeExp::syntaxCopy()
{
    return new TypeExp(loc, type->syntaxCopy());
}

bool TypeExp::checkType()
{
    error("type %s is not an expression", toChars());
    return true;
}

bool TypeExp::checkValue()
{
    error("type %s has no value", toChars());
    return true;
}

/************************************************************/

/***********************************************************
 * Mainly just a placeholder of
 *  Package, Module, Nspace, and TemplateInstance (including TemplateMixin)
 *
 * A template instance that requires IFTI:
 *      foo!tiargs(fargs)       // foo!tiargs
 * is left until CallExp::semantic() or resolveProperties()
 */
ScopeExp::ScopeExp(Loc loc, ScopeDsymbol *sds)
    : Expression(loc, TOKscope, sizeof(ScopeExp))
{
    //printf("ScopeExp::ScopeExp(sds = '%s')\n", sds->toChars());
    //static int count; if (++count == 38) *(char*)0=0;
    this->sds = sds;
    assert(!sds->isTemplateDeclaration());   // instead, you should use TemplateExp
}

Expression *ScopeExp::syntaxCopy()
{
    return new ScopeExp(loc, (ScopeDsymbol *)sds->syntaxCopy(NULL));
}

bool ScopeExp::checkType()
{
    if (sds->isPackage())
    {
        error("%s %s has no type", sds->kind(), sds->toChars());
        return true;
    }
    if (TemplateInstance *ti = sds->isTemplateInstance())
    {
        //assert(ti->needsTypeInference(sc));
        if (ti->tempdecl &&
            ti->semantictiargsdone &&
            ti->semanticRun == PASSinit)
        {
            error("partial %s %s has no type", sds->kind(), toChars());
            return true;
        }
    }
    return false;
}

bool ScopeExp::checkValue()
{
    error("%s %s has no value", sds->kind(), sds->toChars());
    return true;
}

/********************** TemplateExp **************************************/

// Mainly just a placeholder

TemplateExp::TemplateExp(Loc loc, TemplateDeclaration *td, FuncDeclaration *fd)
    : Expression(loc, TOKtemplate, sizeof(TemplateExp))
{
    //printf("TemplateExp(): %s\n", td->toChars());
    this->td = td;
    this->fd = fd;
}

bool TemplateExp::checkType()
{
    error("%s %s has no type", td->kind(), toChars());
    return true;
}

bool TemplateExp::checkValue()
{
    error("%s %s has no value", td->kind(), toChars());
    return true;
}

bool TemplateExp::isLvalue()
{
    return fd != NULL;
}

Expression *TemplateExp::toLvalue(Scope *sc, Expression *e)
{
    if (!fd)
        return Expression::toLvalue(sc, e);

    assert(sc);
    return resolve(loc, sc, fd, true);
}

/********************** NewExp **************************************/

/* thisexp.new(newargs) newtype(arguments) */

NewExp::NewExp(Loc loc, Expression *thisexp, Expressions *newargs,
        Type *newtype, Expressions *arguments)
    : Expression(loc, TOKnew, sizeof(NewExp))
{
    this->thisexp = thisexp;
    this->newargs = newargs;
    this->newtype = newtype;
    this->arguments = arguments;
    argprefix = NULL;
    member = NULL;
    allocator = NULL;
    onstack = 0;
}

NewExp *NewExp::create(Loc loc, Expression *thisexp, Expressions *newargs,
        Type *newtype, Expressions *arguments)
{
    return new NewExp(loc, thisexp, newargs, newtype, arguments);
}

Expression *NewExp::syntaxCopy()
{
    return new NewExp(loc,
        thisexp ? thisexp->syntaxCopy() : NULL,
        arraySyntaxCopy(newargs),
        newtype->syntaxCopy(), arraySyntaxCopy(arguments));
}

/********************** NewAnonClassExp **************************************/

NewAnonClassExp::NewAnonClassExp(Loc loc, Expression *thisexp,
        Expressions *newargs, ClassDeclaration *cd, Expressions *arguments)
    : Expression(loc, TOKnewanonclass, sizeof(NewAnonClassExp))
{
    this->thisexp = thisexp;
    this->newargs = newargs;
    this->cd = cd;
    this->arguments = arguments;
}

Expression *NewAnonClassExp::syntaxCopy()
{
    return new NewAnonClassExp(loc,
        thisexp ? thisexp->syntaxCopy() : NULL,
        arraySyntaxCopy(newargs),
        (ClassDeclaration *)cd->syntaxCopy(NULL),
        arraySyntaxCopy(arguments));
}

/********************** SymbolExp **************************************/

SymbolExp::SymbolExp(Loc loc, TOK op, int size, Declaration *var, bool hasOverloads)
    : Expression(loc, op, size)
{
    assert(var);
    this->var = var;
    this->hasOverloads = hasOverloads;
}

/********************** SymOffExp **************************************/

SymOffExp::SymOffExp(Loc loc, Declaration *var, dinteger_t offset, bool hasOverloads)
    : SymbolExp(loc, TOKsymoff, sizeof(SymOffExp), var,
                var->isVarDeclaration() ? false : hasOverloads)
{
    if (VarDeclaration *v = var->isVarDeclaration())
    {
        // FIXME: This error report will never be handled anyone.
        // It should be done before the SymOffExp construction.
        if (v->needThis())
            ::error(loc, "need 'this' for address of %s", v->toChars());
    }
    this->offset = offset;
}

bool SymOffExp::isBool(bool result)
{
    return result ? true : false;
}

/******************************** VarExp **************************/

VarExp::VarExp(Loc loc, Declaration *var, bool hasOverloads)
    : SymbolExp(loc, TOKvar, sizeof(VarExp), var,
                var->isVarDeclaration() ? false : hasOverloads)
{
    //printf("VarExp(this = %p, '%s', loc = %s)\n", this, var->toChars(), loc.toChars());
    //if (strcmp(var->ident->toChars(), "func") == 0) halt();
    this->type = var->type;
}

VarExp *VarExp::create(Loc loc, Declaration *var, bool hasOverloads)
{
    return new VarExp(loc, var, hasOverloads);
}

bool VarExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (((Expression *)o)->op == TOKvar)
    {
        VarExp *ne = (VarExp *)o;
        if (type->toHeadMutable()->equals(ne->type->toHeadMutable()) &&
            var == ne->var)
        {
            return true;
        }
    }
    return false;
}

bool VarExp::isLvalue()
{
    if (var->storage_class & (STClazy | STCrvalue | STCmanifest))
        return false;
    return true;
}

Expression *VarExp::toLvalue(Scope *, Expression *)
{
    if (var->storage_class & STCmanifest)
    {
        error("manifest constant '%s' is not lvalue", var->toChars());
        return new ErrorExp();
    }
    if (var->storage_class & STClazy)
    {
        error("lazy variables cannot be lvalues");
        return new ErrorExp();
    }
    if (var->ident == Id::ctfe)
    {
        error("compiler-generated variable __ctfe is not an lvalue");
        return new ErrorExp();
    }
    if (var->ident == Id::dollar)   // Bugzilla 13574
    {
        error("'$' is not an lvalue");
        return new ErrorExp();
    }
    return this;
}

int VarExp::checkModifiable(Scope *sc, int flag)
{
    //printf("VarExp::checkModifiable %s", toChars());
    assert(type);
    return var->checkModify(loc, sc, type, NULL, flag);
}

Expression *VarExp::modifiableLvalue(Scope *sc, Expression *e)
{
    //printf("VarExp::modifiableLvalue('%s')\n", var->toChars());
    if (var->storage_class & STCmanifest)
    {
        error("cannot modify manifest constant '%s'", toChars());
        return new ErrorExp();
    }
    // See if this expression is a modifiable lvalue (i.e. not const)
    return Expression::modifiableLvalue(sc, e);
}


/******************************** OverExp **************************/

OverExp::OverExp(Loc loc, OverloadSet *s)
        : Expression(loc, TOKoverloadset, sizeof(OverExp))
{
    //printf("OverExp(this = %p, '%s')\n", this, var->toChars());
    vars = s;
    type = Type::tvoid;
}

bool OverExp::isLvalue()
{
    return true;
}

Expression *OverExp::toLvalue(Scope *, Expression *)
{
    return this;
}

/******************************** TupleExp **************************/

TupleExp::TupleExp(Loc loc, Expression *e0, Expressions *exps)
        : Expression(loc, TOKtuple, sizeof(TupleExp))
{
    //printf("TupleExp(this = %p)\n", this);
    this->e0 = e0;
    this->exps = exps;
}

TupleExp::TupleExp(Loc loc, Expressions *exps)
        : Expression(loc, TOKtuple, sizeof(TupleExp))
{
    //printf("TupleExp(this = %p)\n", this);
    this->e0 = NULL;
    this->exps = exps;
}

TupleExp::TupleExp(Loc loc, TupleDeclaration *tup)
        : Expression(loc, TOKtuple, sizeof(TupleExp))
{
    this->e0 = NULL;
    this->exps = new Expressions();

    this->exps->reserve(tup->objects->length);
    for (size_t i = 0; i < tup->objects->length; i++)
    {   RootObject *o = (*tup->objects)[i];
        if (Dsymbol *s = getDsymbol(o))
        {
            /* If tuple element represents a symbol, translate to DsymbolExp
             * to supply implicit 'this' if needed later.
             */
            Expression *e = new DsymbolExp(loc, s);
            this->exps->push(e);
        }
        else if (o->dyncast() == DYNCAST_EXPRESSION)
        {
            Expression *e = ((Expression *)o)->copy();
            e->loc = loc;    // Bugzilla 15669
            this->exps->push(e);
        }
        else if (o->dyncast() == DYNCAST_TYPE)
        {
            Type *t = (Type *)o;
            Expression *e = new TypeExp(loc, t);
            this->exps->push(e);
        }
        else
        {
            error("%s is not an expression", o->toChars());
        }
    }
}

bool TupleExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (((Expression *)o)->op == TOKtuple)
    {
        TupleExp *te = (TupleExp *)o;
        if (exps->length != te->exps->length)
            return false;
        if ((e0 && !e0->equals(te->e0)) || (!e0 && te->e0))
            return false;
        for (size_t i = 0; i < exps->length; i++)
        {
            Expression *e1 = (*exps)[i];
            Expression *e2 = (*te->exps)[i];
            if (!e1->equals(e2))
                return false;
        }
        return true;
    }
    return false;
}

Expression *TupleExp::syntaxCopy()
{
    return new TupleExp(loc, e0 ? e0->syntaxCopy() : NULL, arraySyntaxCopy(exps));
}

TupleExp *TupleExp::toTupleExp()
{
    return this;
}

/******************************** FuncExp *********************************/

FuncExp::FuncExp(Loc loc, Dsymbol *s)
        : Expression(loc, TOKfunction, sizeof(FuncExp))
{
    this->td = s->isTemplateDeclaration();
    this->fd = s->isFuncLiteralDeclaration();
    if (td)
    {
        assert(td->literal);
        assert(td->members && td->members->length == 1);
        fd = (*td->members)[0]->isFuncLiteralDeclaration();
    }
    tok = fd->tok;  // save original kind of function/delegate/(infer)
    assert(fd->fbody);
}

bool FuncExp::equals(RootObject *o)
{
    if (this == o)
        return true;
    if (o->dyncast() != DYNCAST_EXPRESSION)
        return false;
    if (((Expression *)o)->op == TOKfunction)
    {
        FuncExp *fe = (FuncExp *)o;
        return fd == fe->fd;
    }
    return false;
}

void FuncExp::genIdent(Scope *sc)
{
    if (fd->ident == Id::empty)
    {
        const char *s;
        if (fd->fes)                        s = "__foreachbody";
        else if (fd->tok == TOKreserved)    s = "__lambda";
        else if (fd->tok == TOKdelegate)    s = "__dgliteral";
        else                                s = "__funcliteral";

        DsymbolTable *symtab;
        if (FuncDeclaration *func = sc->parent->isFuncDeclaration())
        {
            if (func->localsymtab == NULL)
            {
                // Inside template constraint, symtab is not set yet.
                // Initialize it lazily.
                func->localsymtab = new DsymbolTable();
            }
            symtab = func->localsymtab;
        }
        else
        {
            ScopeDsymbol *sds = sc->parent->isScopeDsymbol();
            if (!sds->symtab)
            {
                // Inside template constraint, symtab may not be set yet.
                // Initialize it lazily.
                assert(sds->isTemplateInstance());
                sds->symtab = new DsymbolTable();
            }
            symtab = sds->symtab;
        }
        assert(symtab);
        int num = (int)dmd_aaLen(symtab->tab) + 1;
        Identifier *id = Identifier::generateId(s, num);
        fd->ident = id;
        if (td) td->ident = id;
        symtab->insert(td ? (Dsymbol *)td : (Dsymbol *)fd);
    }
}

Expression *FuncExp::syntaxCopy()
{
    if (td)
        return new FuncExp(loc, td->syntaxCopy(NULL));
    else if (fd->semanticRun == PASSinit)
        return new FuncExp(loc, fd->syntaxCopy(NULL));
    else    // Bugzilla 13481: Prevent multiple semantic analysis of lambda body.
        return new FuncExp(loc, fd);
}

MATCH FuncExp::matchType(Type *to, Scope *sc, FuncExp **presult, int flag)
{
    //printf("FuncExp::matchType('%s'), to=%s\n", type ? type->toChars() : "null", to->toChars());
    if (presult)
        *presult = NULL;

    TypeFunction *tof = NULL;
    if (to->ty == Tdelegate)
    {
        if (tok == TOKfunction)
        {
            if (!flag)
                error("cannot match function literal to delegate type '%s'", to->toChars());
            return MATCHnomatch;
        }
        tof = (TypeFunction *)to->nextOf();
    }
    else if (to->ty == Tpointer && to->nextOf()->ty == Tfunction)
    {
        if (tok == TOKdelegate)
        {
            if (!flag)
                error("cannot match delegate literal to function pointer type '%s'", to->toChars());
            return MATCHnomatch;
        }
        tof = (TypeFunction *)to->nextOf();
    }

    if (td)
    {
        if (!tof)
        {
        L1:
            if (!flag)
                error("cannot infer parameter types from %s", to->toChars());
            return MATCHnomatch;
        }

        // Parameter types inference from 'tof'
        assert(td->_scope);
        TypeFunction *tf = (TypeFunction *)fd->type;
        //printf("\ttof = %s\n", tof->toChars());
        //printf("\ttf  = %s\n", tf->toChars());
        size_t dim = tf->parameterList.length();

        if (tof->parameterList.length() != dim ||
            tof->parameterList.varargs != tf->parameterList.varargs)
            goto L1;

        Objects *tiargs = new Objects();
        tiargs->reserve(td->parameters->length);

        for (size_t i = 0; i < td->parameters->length; i++)
        {
            TemplateParameter *tp = (*td->parameters)[i];
            size_t u = 0;
            for (; u < dim; u++)
            {
                Parameter *p = tf->parameterList[u];
                if (p->type->ty == Tident &&
                    ((TypeIdentifier *)p->type)->ident == tp->ident)
                {
                    break;
                }
            }
            assert(u < dim);
            Parameter *pto = tof->parameterList[u];
            Type *t = pto->type;
            if (t->ty == Terror)
                goto L1;
            tiargs->push(t);
        }

        // Set target of return type inference
        if (!tf->next && tof->next)
            fd->treq = to;

        TemplateInstance *ti = new TemplateInstance(loc, td, tiargs);
        Expression *ex = new ScopeExp(loc, ti);
        ex = ::semantic(ex, td->_scope);

        // Reset inference target for the later re-semantic
        fd->treq = NULL;

        if (ex->op == TOKerror)
            return MATCHnomatch;
        if (ex->op != TOKfunction)
            goto L1;
        return ((FuncExp *)ex)->matchType(to, sc, presult, flag);
    }

    if (!tof || !tof->next)
        return MATCHnomatch;

    assert(type && type != Type::tvoid);
    TypeFunction *tfx = (TypeFunction *)fd->type;
    bool convertMatch = (type->ty != to->ty);

    if (fd->inferRetType && tfx->next->implicitConvTo(tof->next) == MATCHconvert)
    {
        /* If return type is inferred and covariant return,
         * tweak return statements to required return type.
         *
         * interface I {}
         * class C : Object, I{}
         *
         * I delegate() dg = delegate() { return new class C(); }
         */
        convertMatch = true;

        TypeFunction *tfy = new TypeFunction(tfx->parameterList, tof->next, tfx->linkage, STCundefined);
        tfy->mod = tfx->mod;
        tfy->isnothrow  = tfx->isnothrow;
        tfy->isnogc     = tfx->isnogc;
        tfy->purity     = tfx->purity;
        tfy->isproperty = tfx->isproperty;
        tfy->isref      = tfx->isref;
        tfy->iswild     = tfx->iswild;
        tfy->deco = tfy->merge()->deco;

        tfx = tfy;
    }

    Type *tx;
    if (tok == TOKdelegate ||
        (tok == TOKreserved && (type->ty == Tdelegate ||
                                (type->ty == Tpointer && to->ty == Tdelegate))))
    {
        // Allow conversion from implicit function pointer to delegate
        tx = new TypeDelegate(tfx);
        tx->deco = tx->merge()->deco;
    }
    else
    {
        assert(tok == TOKfunction ||
               (tok == TOKreserved && type->ty == Tpointer));
        tx = tfx->pointerTo();
    }
    //printf("\ttx = %s, to = %s\n", tx->toChars(), to->toChars());

    MATCH m = tx->implicitConvTo(to);
    if (m > MATCHnomatch)
    {
        // MATCHexact:      exact type match
        // MATCHconst:      covairiant type match (eg. attributes difference)
        // MATCHconvert:    context conversion
        m = convertMatch ? MATCHconvert : tx->equals(to) ? MATCHexact : MATCHconst;

        if (presult)
        {
            (*presult) = (FuncExp *)copy();
            (*presult)->type = to;

            // Bugzilla 12508: Tweak function body for covariant returns.
            (*presult)->fd->modifyReturns(sc, tof->next);
        }
    }
    else if (!flag)
    {
        error("cannot implicitly convert expression (%s) of type %s to %s",
                toChars(), tx->toChars(), to->toChars());
    }
    return m;
}

const char *FuncExp::toChars()
{
    return fd->toChars();
}

bool FuncExp::checkType()
{
    if (td)
    {
        error("template lambda has no type");
        return true;
    }
    return false;
}

bool FuncExp::checkValue()
{
    if (td)
    {
        error("template lambda has no value");
        return true;
    }
    return false;
}

/******************************** DeclarationExp **************************/

DeclarationExp::DeclarationExp(Loc loc, Dsymbol *declaration)
        : Expression(loc, TOKdeclaration, sizeof(DeclarationExp))
{
    this->declaration = declaration;
}

Expression *DeclarationExp::syntaxCopy()
{
    return new DeclarationExp(loc, declaration->syntaxCopy(NULL));
}

bool DeclarationExp::hasCode()
{
    if (VarDeclaration *vd = declaration->isVarDeclaration())
    {
        return !(vd->storage_class & (STCmanifest | STCstatic));
    }
    return false;
}

/************************ TypeidExp ************************************/

/*
 *      typeid(int)
 */

TypeidExp::TypeidExp(Loc loc, RootObject *o)
    : Expression(loc, TOKtypeid, sizeof(TypeidExp))
{
    this->obj = o;
}

Expression *TypeidExp::syntaxCopy()
{
    return new TypeidExp(loc, objectSyntaxCopy(obj));
}

/************************ TraitsExp ************************************/
/*
 *      __traits(identifier, args...)
 */

TraitsExp::TraitsExp(Loc loc, Identifier *ident, Objects *args)
    : Expression(loc, TOKtraits, sizeof(TraitsExp))
{
    this->ident = ident;
    this->args = args;
}

Expression *TraitsExp::syntaxCopy()
{
    return new TraitsExp(loc, ident, TemplateInstance::arraySyntaxCopy(args));
}

/************************************************************/

HaltExp::HaltExp(Loc loc)
        : Expression(loc, TOKhalt, sizeof(HaltExp))
{
}

/************************************************************/

IsExp::IsExp(Loc loc, Type *targ, Identifier *id, TOK tok,
        Type *tspec, TOK tok2, TemplateParameters *parameters)
        : Expression(loc, TOKis, sizeof(IsExp))
{
    this->targ = targ;
    this->id = id;
    this->tok = tok;
    this->tspec = tspec;
    this->tok2 = tok2;
    this->parameters = parameters;
}

Expression *IsExp::syntaxCopy()
{
    // This section is identical to that in TemplateDeclaration::syntaxCopy()
    TemplateParameters *p = NULL;
    if (parameters)
    {
        p = new TemplateParameters();
        p->setDim(parameters->length);
        for (size_t i = 0; i < p->length; i++)
            (*p)[i] = (*parameters)[i]->syntaxCopy();
    }
    return new IsExp(loc,
        targ->syntaxCopy(),
        id,
        tok,
        tspec ? tspec->syntaxCopy() : NULL,
        tok2,
        p);
}

void unSpeculative(Scope *sc, RootObject *o);

/************************************************************/

UnaExp::UnaExp(Loc loc, TOK op, int size, Expression *e1)
        : Expression(loc, op, size)
{
    this->e1 = e1;
    this->att1 = NULL;
}

Expression *UnaExp::syntaxCopy()
{
    UnaExp *e = (UnaExp *)copy();
    e->type = NULL;
    e->e1 = e->e1->syntaxCopy();
    return e;
}

/********************************
 * The type for a unary expression is incompatible.
 * Print error message.
 * Returns:
 *  ErrorExp
 */
Expression *UnaExp::incompatibleTypes()
{
    if (e1->type->toBasetype() == Type::terror)
        return e1;

    if (e1->op == TOKtype)
    {
        error("incompatible type for (%s(%s)): cannot use '%s' with types",
              Token::toChars(op), e1->toChars(), Token::toChars(op));
    }
    else
    {
        error("incompatible type for (%s(%s)): '%s'",
              Token::toChars(op), e1->toChars(), e1->type->toChars());
    }
    return new ErrorExp();
}

Expression *UnaExp::resolveLoc(Loc loc, Scope *sc)
{
    e1 = e1->resolveLoc(loc, sc);
    return this;
}

/************************************************************/

BinExp::BinExp(Loc loc, TOK op, int size, Expression *e1, Expression *e2)
        : Expression(loc, op, size)
{
    this->e1 = e1;
    this->e2 = e2;

    this->att1 = NULL;
    this->att2 = NULL;
}

Expression *BinExp::syntaxCopy()
{
    BinExp *e = (BinExp *)copy();
    e->type = NULL;
    e->e1 = e->e1->syntaxCopy();
    e->e2 = e->e2->syntaxCopy();
    return e;
}

Expression *BinExp::checkOpAssignTypes(Scope *sc)
{
    // At that point t1 and t2 are the merged types. type is the original type of the lhs.
    Type *t1 = e1->type;
    Type *t2 = e2->type;

    // T opAssign floating yields a floating. Prevent truncating conversions (float to int).
    // See issue 3841.
    // Should we also prevent double to float (type->isfloating() && type->size() < t2 ->size()) ?
    if (op == TOKaddass || op == TOKminass ||
        op == TOKmulass || op == TOKdivass || op == TOKmodass ||
        op == TOKpowass)
    {
        if ((type->isintegral() && t2->isfloating()))
        {
            warning("%s %s %s is performing truncating conversion",
                    type->toChars(), Token::toChars(op), t2->toChars());
        }
    }

    // generate an error if this is a nonsensical *=,/=, or %=, eg real *= imaginary
    if (op == TOKmulass || op == TOKdivass || op == TOKmodass)
    {
        // Any multiplication by an imaginary or complex number yields a complex result.
        // r *= c, i*=c, r*=i, i*=i are all forbidden operations.
        const char *opstr = Token::toChars(op);
        if (t1->isreal() && t2->iscomplex())
        {
            error("%s %s %s is undefined. Did you mean %s %s %s.re ?",
                t1->toChars(), opstr, t2->toChars(),
                t1->toChars(), opstr, t2->toChars());
            return new ErrorExp();
        }
        else if (t1->isimaginary() && t2->iscomplex())
        {
            error("%s %s %s is undefined. Did you mean %s %s %s.im ?",
                t1->toChars(), opstr, t2->toChars(),
                t1->toChars(), opstr, t2->toChars());
            return new ErrorExp();
        }
        else if ((t1->isreal() || t1->isimaginary()) &&
            t2->isimaginary())
        {
            error("%s %s %s is an undefined operation", t1->toChars(), opstr, t2->toChars());
            return new ErrorExp();
        }
    }

    // generate an error if this is a nonsensical += or -=, eg real += imaginary
    if (op == TOKaddass || op == TOKminass)
    {
        // Addition or subtraction of a real and an imaginary is a complex result.
        // Thus, r+=i, r+=c, i+=r, i+=c are all forbidden operations.
        if ((t1->isreal() && (t2->isimaginary() || t2->iscomplex())) ||
            (t1->isimaginary() && (t2->isreal() || t2->iscomplex())))
        {
            error("%s %s %s is undefined (result is complex)",
                t1->toChars(), Token::toChars(op), t2->toChars());
            return new ErrorExp();
        }
        if (type->isreal() || type->isimaginary())
        {
            assert(global.errors || t2->isfloating());
            e2 = e2->castTo(sc, t1);
        }
    }

    if (op == TOKmulass)
    {
        if (t2->isfloating())
        {
            if (t1->isreal())
            {
                if (t2->isimaginary() || t2->iscomplex())
                {
                    e2 = e2->castTo(sc, t1);
                }
            }
            else if (t1->isimaginary())
            {
                if (t2->isimaginary() || t2->iscomplex())
                {
                    switch (t1->ty)
                    {
                        case Timaginary32: t2 = Type::tfloat32; break;
                        case Timaginary64: t2 = Type::tfloat64; break;
                        case Timaginary80: t2 = Type::tfloat80; break;
                        default:
                            assert(0);
                    }
                    e2 = e2->castTo(sc, t2);
                }
            }
        }
    }
    else if (op == TOKdivass)
    {
        if (t2->isimaginary())
        {
            if (t1->isreal())
            {
                // x/iv = i(-x/v)
                // Therefore, the result is 0
                e2 = new CommaExp(loc, e2, new RealExp(loc, CTFloat::zero, t1));
                e2->type = t1;
                Expression *e = new AssignExp(loc, e1, e2);
                e->type = t1;
                return e;
            }
            else if (t1->isimaginary())
            {
                Type *t3;
                switch (t1->ty)
                {
                    case Timaginary32: t3 = Type::tfloat32; break;
                    case Timaginary64: t3 = Type::tfloat64; break;
                    case Timaginary80: t3 = Type::tfloat80; break;
                    default:
                        assert(0);
                }
                e2 = e2->castTo(sc, t3);
                Expression *e = new AssignExp(loc, e1, e2);
                e->type = t1;
                return e;
            }
        }
    }
    else if (op == TOKmodass)
    {
        if (t2->iscomplex())
        {
            error("cannot perform modulo complex arithmetic");
            return new ErrorExp();
        }
    }
    return this;
}

/********************************
 * The types for a binary expression are incompatible.
 * Print error message.
 * Returns:
 *  ErrorExp
 */
Expression *BinExp::incompatibleTypes()
{
    if (e1->type->toBasetype() == Type::terror)
        return e1;
    if (e2->type->toBasetype() == Type::terror)
        return e2;

    // CondExp uses 'a ? b : c' but we're comparing 'b : c'
    TOK thisOp = (op == TOKquestion) ? TOKcolon : op;
    if (e1->op == TOKtype || e2->op == TOKtype)
    {
        error("incompatible types for ((%s) %s (%s)): cannot use '%s' with types",
            e1->toChars(), Token::toChars(thisOp), e2->toChars(), Token::toChars(op));
    }
    else
    {
        error("incompatible types for ((%s) %s (%s)): '%s' and '%s'",
            e1->toChars(), Token::toChars(thisOp), e2->toChars(),
            e1->type->toChars(), e2->type->toChars());
    }
    return new ErrorExp();
}

bool BinExp::checkIntegralBin()
{
    bool r1 = e1->checkIntegral();
    bool r2 = e2->checkIntegral();
    return (r1 || r2);
}

bool BinExp::checkArithmeticBin()
{
    bool r1 = e1->checkArithmetic();
    bool r2 = e2->checkArithmetic();
    return (r1 || r2);
}

/********************** BinAssignExp **************************************/

BinAssignExp::BinAssignExp(Loc loc, TOK op, int size, Expression *e1, Expression *e2)
        : BinExp(loc, op, size, e1, e2)
{
}

bool BinAssignExp::isLvalue()
{
    return true;
}

Expression *BinAssignExp::toLvalue(Scope *, Expression *)
{
    // Lvalue-ness will be handled in glue layer.
    return this;
}

Expression *BinAssignExp::modifiableLvalue(Scope *sc, Expression *)
{
    // should check e1->checkModifiable() ?
    return toLvalue(sc, this);
}

/************************************************************/

CompileExp::CompileExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKmixin, sizeof(CompileExp), e)
{
}

/************************************************************/

ImportExp::ImportExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKimport, sizeof(ImportExp), e)
{
}

/************************************************************/

AssertExp::AssertExp(Loc loc, Expression *e, Expression *msg)
        : UnaExp(loc, TOKassert, sizeof(AssertExp), e)
{
    this->msg = msg;
}

Expression *AssertExp::syntaxCopy()
{
    return new AssertExp(loc, e1->syntaxCopy(), msg ? msg->syntaxCopy() : NULL);
}

/************************************************************/

DotIdExp::DotIdExp(Loc loc, Expression *e, Identifier *ident)
        : UnaExp(loc, TOKdotid, sizeof(DotIdExp), e)
{
    this->ident = ident;
    this->wantsym = false;
    this->noderef = false;
}

DotIdExp *DotIdExp::create(Loc loc, Expression *e, Identifier *ident)
{
    return new DotIdExp(loc, e, ident);
}

/********************** DotTemplateExp ***********************************/

// Mainly just a placeholder

DotTemplateExp::DotTemplateExp(Loc loc, Expression *e, TemplateDeclaration *td)
        : UnaExp(loc, TOKdottd, sizeof(DotTemplateExp), e)

{
    this->td = td;
}

/************************************************************/

DotVarExp::DotVarExp(Loc loc, Expression *e, Declaration *var, bool hasOverloads)
        : UnaExp(loc, TOKdotvar, sizeof(DotVarExp), e)
{
    //printf("DotVarExp()\n");
    this->var = var;
    this->hasOverloads = var->isVarDeclaration() ? false : hasOverloads;
}

bool DotVarExp::isLvalue()
{
    return true;
}

Expression *DotVarExp::toLvalue(Scope *, Expression *)
{
    //printf("DotVarExp::toLvalue(%s)\n", toChars());
    return this;
}

/***********************************************
 * Mark variable v as modified if it is inside a constructor that var
 * is a field in.
 */
int modifyFieldVar(Loc loc, Scope *sc, VarDeclaration *var, Expression *e1)
{
    //printf("modifyFieldVar(var = %s)\n", var->toChars());
    Dsymbol *s = sc->func;
    while (1)
    {
        FuncDeclaration *fd = NULL;
        if (s)
            fd = s->isFuncDeclaration();
        if (fd &&
            ((fd->isCtorDeclaration() && var->isField()) ||
             (fd->isStaticCtorDeclaration() && !var->isField())) &&
            fd->toParent2() == var->toParent2() &&
            (!e1 || e1->op == TOKthis)
           )
        {
            bool result = true;

            var->ctorinit = true;
            //printf("setting ctorinit\n");

            if (var->isField() && sc->fieldinit && !sc->intypeof)
            {
                assert(e1);
                bool mustInit = ((var->storage_class & STCnodefaultctor) != 0 ||
                                 var->type->needsNested());

                size_t dim = sc->fieldinit_dim;
                AggregateDeclaration *ad = fd->isMember2();
                assert(ad);
                size_t i;
                for (i = 0; i < dim; i++)   // same as findFieldIndexByName in ctfeexp.c ?
                {
                    if (ad->fields[i] == var)
                        break;
                }
                assert(i < dim);
                unsigned fi = sc->fieldinit[i];

                if (fi & CSXthis_ctor)
                {
                    if (var->type->isMutable() && e1->type->isMutable())
                        result = false;
                    else
                    {
                        const char *modStr = !var->type->isMutable() ? MODtoChars(var->type->mod) : MODtoChars(e1->type->mod);
                        ::error(loc, "%s field '%s' initialized multiple times", modStr, var->toChars());
                    }
                }
                else if (sc->noctor || (fi & CSXlabel))
                {
                    if (!mustInit && var->type->isMutable() && e1->type->isMutable())
                        result = false;
                    else
                    {
                        const char *modStr = !var->type->isMutable() ? MODtoChars(var->type->mod) : MODtoChars(e1->type->mod);
                        ::error(loc, "%s field '%s' initialization is not allowed in loops or after labels", modStr, var->toChars());
                    }
                }
                sc->fieldinit[i] |= CSXthis_ctor;
                if (var->overlapped) // Bugzilla 15258
                {
                    for (size_t j = 0; j < ad->fields.length; j++)
                    {
                        VarDeclaration *v = ad->fields[j];
                        if (v == var || !var->isOverlappedWith(v))
                            continue;
                        v->ctorinit = true;
                        sc->fieldinit[j] = CSXthis_ctor;
                    }
                }
            }
            else if (fd != sc->func)
            {
                if (var->type->isMutable())
                    result = false;
                else if (sc->func->fes)
                {
                    const char *p = var->isField() ? "field" : var->kind();
                    ::error(loc, "%s %s '%s' initialization is not allowed in foreach loop",
                        MODtoChars(var->type->mod), p, var->toChars());
                }
                else
                {
                    const char *p = var->isField() ? "field" : var->kind();
                    ::error(loc, "%s %s '%s' initialization is not allowed in nested function '%s'",
                        MODtoChars(var->type->mod), p, var->toChars(), sc->func->toChars());
                }
            }
            return result;
        }
        else
        {
            if (s)
            {
                s = s->toParent2();
                continue;
            }
        }
        break;
    }
    return false;
}

int DotVarExp::checkModifiable(Scope *sc, int flag)
{
    //printf("DotVarExp::checkModifiable %s %s\n", toChars(), type->toChars());
    if (checkUnsafeAccess(sc, this, false, !flag))
        return 2;

    if (e1->op == TOKthis)
        return var->checkModify(loc, sc, type, e1, flag);

    //printf("\te1 = %s\n", e1->toChars());
    return e1->checkModifiable(sc, flag);
}

Expression *DotVarExp::modifiableLvalue(Scope *sc, Expression *e)
{
    return Expression::modifiableLvalue(sc, e);
}

/************************************************************/

/* Things like:
 *      foo.bar!(args)
 */

DotTemplateInstanceExp::DotTemplateInstanceExp(Loc loc, Expression *e, Identifier *name, Objects *tiargs)
        : UnaExp(loc, TOKdotti, sizeof(DotTemplateInstanceExp), e)
{
    //printf("DotTemplateInstanceExp()\n");
    this->ti = new TemplateInstance(loc, name);
    this->ti->tiargs = tiargs;
}

DotTemplateInstanceExp::DotTemplateInstanceExp(Loc loc, Expression *e, TemplateInstance *ti)
        : UnaExp(loc, TOKdotti, sizeof(DotTemplateInstanceExp), e)
{
    this->ti = ti;
}

Expression *DotTemplateInstanceExp::syntaxCopy()
{
    return new DotTemplateInstanceExp(loc,
        e1->syntaxCopy(),
        ti->name,
        TemplateInstance::arraySyntaxCopy(ti->tiargs));
}

bool DotTemplateInstanceExp::findTempDecl(Scope *sc)
{
    if (ti->tempdecl)
        return true;

    Expression *e = new DotIdExp(loc, e1, ti->name);
    e = semantic(e, sc);
    if (e->op == TOKdot)
        e = ((DotExp *)e)->e2;

    Dsymbol *s = NULL;
    switch (e->op)
    {
        case TOKoverloadset:    s = ((OverExp *)e)->vars;       break;
        case TOKdottd:          s = ((DotTemplateExp *)e)->td;  break;
        case TOKscope:          s = ((ScopeExp *)e)->sds;       break;
        case TOKdotvar:         s = ((DotVarExp *)e)->var;      break;
        case TOKvar:            s = ((VarExp *)e)->var;         break;
        default:                return false;
    }
    return ti->updateTempDecl(sc, s);
}

/************************************************************/

DelegateExp::DelegateExp(Loc loc, Expression *e, FuncDeclaration *f, bool hasOverloads)
        : UnaExp(loc, TOKdelegate, sizeof(DelegateExp), e)
{
    this->func = f;
    this->hasOverloads = hasOverloads;
}

/************************************************************/

DotTypeExp::DotTypeExp(Loc loc, Expression *e, Dsymbol *s)
        : UnaExp(loc, TOKdottype, sizeof(DotTypeExp), e)
{
    this->sym = s;
    this->type = NULL;
}

/************************************************************/

CallExp::CallExp(Loc loc, Expression *e, Expressions *exps)
        : UnaExp(loc, TOKcall, sizeof(CallExp), e)
{
    this->arguments = exps;
    this->f = NULL;
    this->directcall = false;
}

CallExp::CallExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKcall, sizeof(CallExp), e)
{
    this->arguments = NULL;
    this->f = NULL;
    this->directcall = false;
}

CallExp::CallExp(Loc loc, Expression *e, Expression *earg1)
        : UnaExp(loc, TOKcall, sizeof(CallExp), e)
{
    Expressions *arguments = new Expressions();
    if (earg1)
    {
        arguments->setDim(1);
        (*arguments)[0] = earg1;
    }
    this->arguments = arguments;
    this->f = NULL;
    this->directcall = false;
}

CallExp::CallExp(Loc loc, Expression *e, Expression *earg1, Expression *earg2)
        : UnaExp(loc, TOKcall, sizeof(CallExp), e)
{
    Expressions *arguments = new Expressions();
    arguments->setDim(2);
    (*arguments)[0] = earg1;
    (*arguments)[1] = earg2;

    this->arguments = arguments;
    this->f = NULL;
    this->directcall = false;
}

CallExp *CallExp::create(Loc loc, Expression *e, Expressions *exps)
{
    return new CallExp(loc, e, exps);
}

CallExp *CallExp::create(Loc loc, Expression *e)
{
    return new CallExp(loc, e);
}

CallExp *CallExp::create(Loc loc, Expression *e, Expression *earg1)
{
    return new CallExp(loc, e, earg1);
}

Expression *CallExp::syntaxCopy()
{
    return new CallExp(loc, e1->syntaxCopy(), arraySyntaxCopy(arguments));
}

bool CallExp::isLvalue()
{
    Type *tb = e1->type->toBasetype();
    if (tb->ty == Tdelegate || tb->ty == Tpointer)
        tb = tb->nextOf();
    if (tb->ty == Tfunction && ((TypeFunction *)tb)->isref)
    {
        if (e1->op == TOKdotvar)
            if (((DotVarExp *)e1)->var->isCtorDeclaration())
                return false;
        return true;               // function returns a reference
    }
    return false;
}

Expression *CallExp::toLvalue(Scope *sc, Expression *e)
{
    if (isLvalue())
        return this;
    return Expression::toLvalue(sc, e);
}

Expression *CallExp::addDtorHook(Scope *sc)
{
    /* Only need to add dtor hook if it's a type that needs destruction.
     * Use same logic as VarDeclaration::callScopeDtor()
     */

    if (e1->type && e1->type->ty == Tfunction)
    {
        TypeFunction *tf = (TypeFunction *)e1->type;
        if (tf->isref)
            return this;
    }

    Type *tv = type->baseElemOf();
    if (tv->ty == Tstruct)
    {
        TypeStruct *ts = (TypeStruct *)tv;
        StructDeclaration *sd = ts->sym;
        if (sd->dtor)
        {
            /* Type needs destruction, so declare a tmp
             * which the back end will recognize and call dtor on
             */
            VarDeclaration *tmp = copyToTemp(0, "__tmpfordtor", this);
            DeclarationExp *de = new DeclarationExp(loc, tmp);
            VarExp *ve = new VarExp(loc, tmp);
            Expression *e = new CommaExp(loc, de, ve);
            e = semantic(e, sc);
            return e;
        }
    }
    return this;
}

FuncDeclaration *isFuncAddress(Expression *e, bool *hasOverloads = NULL)
{
    if (e->op == TOKaddress)
    {
        Expression *ae1 = ((AddrExp *)e)->e1;
        if (ae1->op == TOKvar)
        {
            VarExp *ve = (VarExp *)ae1;
            if (hasOverloads)
                *hasOverloads = ve->hasOverloads;
            return ve->var->isFuncDeclaration();
        }
        if (ae1->op == TOKdotvar)
        {
            DotVarExp *dve = (DotVarExp *)ae1;
            if (hasOverloads)
                *hasOverloads = dve->hasOverloads;
            return dve->var->isFuncDeclaration();
        }
    }
    else
    {
        if (e->op == TOKsymoff)
        {
            SymOffExp *soe = (SymOffExp *)e;
            if (hasOverloads)
                *hasOverloads = soe->hasOverloads;
            return soe->var->isFuncDeclaration();
        }
        if (e->op == TOKdelegate)
        {
            DelegateExp *dge = (DelegateExp *)e;
            if (hasOverloads)
                *hasOverloads = dge->hasOverloads;
            return dge->func->isFuncDeclaration();
        }
    }
    return NULL;
}

/************************************************************/

AddrExp::AddrExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKaddress, sizeof(AddrExp), e)
{
}

AddrExp::AddrExp(Loc loc, Expression *e, Type *t)
        : UnaExp(loc, TOKaddress, sizeof(AddrExp), e)
{
    type = t;
}

/************************************************************/

PtrExp::PtrExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKstar, sizeof(PtrExp), e)
{
//    if (e->type)
//      type = ((TypePointer *)e->type)->next;
}

PtrExp::PtrExp(Loc loc, Expression *e, Type *t)
        : UnaExp(loc, TOKstar, sizeof(PtrExp), e)
{
    type = t;
}

bool PtrExp::isLvalue()
{
    return true;
}

Expression *PtrExp::toLvalue(Scope *, Expression *)
{
    return this;
}

int PtrExp::checkModifiable(Scope *sc, int flag)
{
    if (e1->op == TOKsymoff)
    {   SymOffExp *se = (SymOffExp *)e1;
        return se->var->checkModify(loc, sc, type, NULL, flag);
    }
    else if (e1->op == TOKaddress)
    {
        AddrExp *ae = (AddrExp *)e1;
        return ae->e1->checkModifiable(sc, flag);
    }
    return 1;
}

Expression *PtrExp::modifiableLvalue(Scope *sc, Expression *e)
{
    //printf("PtrExp::modifiableLvalue() %s, type %s\n", toChars(), type->toChars());
    return Expression::modifiableLvalue(sc, e);
}

/************************************************************/

NegExp::NegExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKneg, sizeof(NegExp), e)
{
}

/************************************************************/

UAddExp::UAddExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKuadd, sizeof(UAddExp), e)
{
}

/************************************************************/

ComExp::ComExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKtilde, sizeof(ComExp), e)
{
}

/************************************************************/

NotExp::NotExp(Loc loc, Expression *e)
        : UnaExp(loc, TOKnot, sizeof(NotExp), e)
{
}

/************************************************************/

DeleteExp::DeleteExp(Loc loc, Expression *e, bool isRAII)
        : UnaExp(loc, TOKdelete, sizeof(DeleteExp), e)
{
    this->isRAII = isRAII;
}

Expression *DeleteExp::toBoolean(Scope *)
{
    error("delete does not give a boolean result");
    return new ErrorExp();
}

/************************************************************/

CastExp::CastExp(Loc loc, Expression *e, Type *t)
        : UnaExp(loc, TOKcast, sizeof(CastExp), e)
{
    this->to = t;
    this->mod = (unsigned char)~0;
}

/* For cast(const) and cast(immutable)
 */
CastExp::CastExp(Loc loc, Expression *e, unsigned char mod)
        : UnaExp(loc, TOKcast, sizeof(CastExp), e)
{
    this->to = NULL;
    this->mod = mod;
}

Expression *CastExp::syntaxCopy()
{
    return to ? new CastExp(loc, e1->syntaxCopy(), to->syntaxCopy())
              : new CastExp(loc, e1->syntaxCopy(), mod);
}

/************************************************************/

VectorExp::VectorExp(Loc loc, Expression *e, Type *t)
        : UnaExp(loc, TOKvector, sizeof(VectorExp), e)
{
    assert(t->ty == Tvector);
    to = (TypeVector *)t;
    dim = ~0;
    ownedByCtfe = OWNEDcode;
}

VectorExp *VectorExp::create(Loc loc, Expression *e, Type *t)
{
    return new VectorExp(loc, e, t);
}

Expression *VectorExp::syntaxCopy()
{
    return new VectorExp(loc, e1->syntaxCopy(), to->syntaxCopy());
}

/************************************************************/

VectorArrayExp::VectorArrayExp(Loc loc, Expression *e1)
        : UnaExp(loc, TOKvectorarray, sizeof(VectorArrayExp), e1)
{
}

bool VectorArrayExp::isLvalue()
{
    return e1->isLvalue();
}

Expression *VectorArrayExp::toLvalue(Scope *sc, Expression *e)
{
    e1 = e1->toLvalue(sc, e);
    return this;
}

/************************************************************/

SliceExp::SliceExp(Loc loc, Expression *e1, IntervalExp *ie)
        : UnaExp(loc, TOKslice, sizeof(SliceExp), e1)
{
    this->upr = ie ? ie->upr : NULL;
    this->lwr = ie ? ie->lwr : NULL;
    lengthVar = NULL;
    upperIsInBounds = false;
    lowerIsLessThanUpper = false;
    arrayop = false;
}

SliceExp::SliceExp(Loc loc, Expression *e1, Expression *lwr, Expression *upr)
        : UnaExp(loc, TOKslice, sizeof(SliceExp), e1)
{
    this->upr = upr;
    this->lwr = lwr;
    lengthVar = NULL;
    upperIsInBounds = false;
    lowerIsLessThanUpper = false;
    arrayop = false;
}

Expression *SliceExp::syntaxCopy()
{
    SliceExp *se = new SliceExp(loc, e1->syntaxCopy(),
        lwr ? lwr->syntaxCopy() : NULL,
        upr ? upr->syntaxCopy() : NULL);
    se->lengthVar = this->lengthVar;    // bug7871
    return se;
}

int SliceExp::checkModifiable(Scope *sc, int flag)
{
    //printf("SliceExp::checkModifiable %s\n", toChars());
    if (e1->type->ty == Tsarray ||
        (e1->op == TOKindex && e1->type->ty != Tarray) ||
        e1->op == TOKslice)
    {
        return e1->checkModifiable(sc, flag);
    }
    return 1;
}

bool SliceExp::isLvalue()
{
    /* slice expression is rvalue in default, but
     * conversion to reference of static array is only allowed.
     */
    return (type && type->toBasetype()->ty == Tsarray);
}

Expression *SliceExp::toLvalue(Scope *sc, Expression *e)
{
    //printf("SliceExp::toLvalue(%s) type = %s\n", toChars(), type ? type->toChars() : NULL);
    return (type && type->toBasetype()->ty == Tsarray)
            ? this : Expression::toLvalue(sc, e);
}

Expression *SliceExp::modifiableLvalue(Scope *, Expression *)
{
    error("slice expression %s is not a modifiable lvalue", toChars());
    return this;
}

bool SliceExp::isBool(bool result)
{
    return e1->isBool(result);
}

/********************** ArrayLength **************************************/

ArrayLengthExp::ArrayLengthExp(Loc loc, Expression *e1)
        : UnaExp(loc, TOKarraylength, sizeof(ArrayLengthExp), e1)
{
}

Expression *opAssignToOp(Loc loc, TOK op, Expression *e1, Expression *e2)
{   Expression *e;

    switch (op)
    {
        case TOKaddass:   e = new AddExp(loc, e1, e2);  break;
        case TOKminass:   e = new MinExp(loc, e1, e2);  break;
        case TOKmulass:   e = new MulExp(loc, e1, e2);  break;
        case TOKdivass:   e = new DivExp(loc, e1, e2);  break;
        case TOKmodass:   e = new ModExp(loc, e1, e2);  break;
        case TOKandass:   e = new AndExp(loc, e1, e2);  break;
        case TOKorass:    e = new OrExp (loc, e1, e2);  break;
        case TOKxorass:   e = new XorExp(loc, e1, e2);  break;
        case TOKshlass:   e = new ShlExp(loc, e1, e2);  break;
        case TOKshrass:   e = new ShrExp(loc, e1, e2);  break;
        case TOKushrass:  e = new UshrExp(loc, e1, e2); break;
        default:        assert(0);
    }
    return e;
}

/*********************
 * Rewrite:
 *    array.length op= e2
 * as:
 *    array.length = array.length op e2
 * or:
 *    auto tmp = &array;
 *    (*tmp).length = (*tmp).length op e2
 */

Expression *ArrayLengthExp::rewriteOpAssign(BinExp *exp)
{
    Expression *e;

    assert(exp->e1->op == TOKarraylength);
    ArrayLengthExp *ale = (ArrayLengthExp *)exp->e1;
    if (ale->e1->op == TOKvar)
    {
        e = opAssignToOp(exp->loc, exp->op, ale, exp->e2);
        e = new AssignExp(exp->loc, ale->syntaxCopy(), e);
    }
    else
    {
        /*    auto tmp = &array;
         *    (*tmp).length = (*tmp).length op e2
         */
        VarDeclaration *tmp = copyToTemp(0, "__arraylength", new AddrExp(ale->loc, ale->e1));

        Expression *e1 = new ArrayLengthExp(ale->loc, new PtrExp(ale->loc, new VarExp(ale->loc, tmp)));
        Expression *elvalue = e1->syntaxCopy();
        e = opAssignToOp(exp->loc, exp->op, e1, exp->e2);
        e = new AssignExp(exp->loc, elvalue, e);
        e = new CommaExp(exp->loc, new DeclarationExp(ale->loc, tmp), e);
    }
    return e;
}

/*********************** IntervalExp ********************************/

// Mainly just a placeholder

IntervalExp::IntervalExp(Loc loc, Expression *lwr, Expression *upr)
        : Expression(loc, TOKinterval, sizeof(IntervalExp))
{
    this->lwr = lwr;
    this->upr = upr;
}

Expression *IntervalExp::syntaxCopy()
{
    return new IntervalExp(loc, lwr->syntaxCopy(), upr->syntaxCopy());
}

/********************** DelegatePtrExp **************************************/

DelegatePtrExp::DelegatePtrExp(Loc loc, Expression *e1)
        : UnaExp(loc, TOKdelegateptr, sizeof(DelegatePtrExp), e1)
{
}

bool DelegatePtrExp::isLvalue()
{
    return e1->isLvalue();
}

Expression *DelegatePtrExp::toLvalue(Scope *sc, Expression *e)
{
    e1 = e1->toLvalue(sc, e);
    return this;
}

Expression *DelegatePtrExp::modifiableLvalue(Scope *sc, Expression *e)
{
    if (sc->func->setUnsafe())
    {
        error("cannot modify delegate pointer in @safe code %s", toChars());
        return new ErrorExp();
    }
    return Expression::modifiableLvalue(sc, e);
}

/********************** DelegateFuncptrExp **************************************/

DelegateFuncptrExp::DelegateFuncptrExp(Loc loc, Expression *e1)
        : UnaExp(loc, TOKdelegatefuncptr, sizeof(DelegateFuncptrExp), e1)
{
}

bool DelegateFuncptrExp::isLvalue()
{
    return e1->isLvalue();
}

Expression *DelegateFuncptrExp::toLvalue(Scope *sc, Expression *e)
{
    e1 = e1->toLvalue(sc, e);
    return this;
}

Expression *DelegateFuncptrExp::modifiableLvalue(Scope *sc, Expression *e)
{
    if (sc->func->setUnsafe())
    {
        error("cannot modify delegate function pointer in @safe code %s", toChars());
        return new ErrorExp();
    }
    return Expression::modifiableLvalue(sc, e);
}

/*********************** ArrayExp *************************************/

// e1 [ i1, i2, i3, ... ]

ArrayExp::ArrayExp(Loc loc, Expression *e1, Expression *index)
        : UnaExp(loc, TOKarray, sizeof(ArrayExp), e1)
{
    arguments = new Expressions();
    if (index)
        arguments->push(index);
    lengthVar = NULL;
    currentDimension = 0;
}

ArrayExp::ArrayExp(Loc loc, Expression *e1, Expressions *args)
        : UnaExp(loc, TOKarray, sizeof(ArrayExp), e1)
{
    arguments = args;
    lengthVar = NULL;
    currentDimension = 0;
}

Expression *ArrayExp::syntaxCopy()
{
    ArrayExp *ae = new ArrayExp(loc, e1->syntaxCopy(), arraySyntaxCopy(arguments));
    ae->lengthVar = this->lengthVar;    // bug7871
    return ae;
}

bool ArrayExp::isLvalue()
{
    if (type && type->toBasetype()->ty == Tvoid)
        return false;
    return true;
}

Expression *ArrayExp::toLvalue(Scope *, Expression *)
{
    if (type && type->toBasetype()->ty == Tvoid)
        error("voids have no value");
    return this;
}

/************************* DotExp ***********************************/

DotExp::DotExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKdot, sizeof(DotExp), e1, e2)
{
}

/************************* CommaExp ***********************************/

CommaExp::CommaExp(Loc loc, Expression *e1, Expression *e2, bool generated)
        : BinExp(loc, TOKcomma, sizeof(CommaExp), e1, e2)
{
    isGenerated = generated;
    allowCommaExp = generated;
}

bool CommaExp::isLvalue()
{
    return e2->isLvalue();
}

Expression *CommaExp::toLvalue(Scope *sc, Expression *)
{
    e2 = e2->toLvalue(sc, NULL);
    return this;
}

int CommaExp::checkModifiable(Scope *sc, int flag)
{
    return e2->checkModifiable(sc, flag);
}

Expression *CommaExp::modifiableLvalue(Scope *sc, Expression *e)
{
    e2 = e2->modifiableLvalue(sc, e);
    return this;
}

bool CommaExp::isBool(bool result)
{
    return e2->isBool(result);
}

Expression *CommaExp::toBoolean(Scope *sc)
{
    Expression *ex2 = e2->toBoolean(sc);
    if (ex2->op == TOKerror)
        return ex2;
    e2 = ex2;
    type = e2->type;
    return this;
}

Expression *CommaExp::addDtorHook(Scope *sc)
{
    e2 = e2->addDtorHook(sc);
    return this;
}

/************************** IndexExp **********************************/

// e1 [ e2 ]

IndexExp::IndexExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKindex, sizeof(IndexExp), e1, e2)
{
    //printf("IndexExp::IndexExp('%s')\n", toChars());
    lengthVar = NULL;
    modifiable = false;     // assume it is an rvalue
    indexIsInBounds = false;
}

Expression *IndexExp::syntaxCopy()
{
    IndexExp *ie = new IndexExp(loc, e1->syntaxCopy(), e2->syntaxCopy());
    ie->lengthVar = this->lengthVar;    // bug7871
    return ie;
}

bool IndexExp::isLvalue()
{
    return true;
}

Expression *IndexExp::toLvalue(Scope *, Expression *)
{
    return this;
}

int IndexExp::checkModifiable(Scope *sc, int flag)
{
    if (e1->type->ty == Tsarray ||
        e1->type->ty == Taarray ||
        (e1->op == TOKindex && e1->type->ty != Tarray) ||
        e1->op == TOKslice)
    {
        return e1->checkModifiable(sc, flag);
    }
    return 1;
}

Expression *IndexExp::modifiableLvalue(Scope *sc, Expression *e)
{
    //printf("IndexExp::modifiableLvalue(%s)\n", toChars());
    Expression *ex = markSettingAAElem();
    if (ex->op == TOKerror)
        return ex;

    return Expression::modifiableLvalue(sc, e);
}

Expression *IndexExp::markSettingAAElem()
{
    if (e1->type->toBasetype()->ty == Taarray)
    {
        Type *t2b = e2->type->toBasetype();
        if (t2b->ty == Tarray && t2b->nextOf()->isMutable())
        {
            error("associative arrays can only be assigned values with immutable keys, not %s", e2->type->toChars());
            return new ErrorExp();
        }
        modifiable = true;

        if (e1->op == TOKindex)
        {
            Expression *ex = ((IndexExp *)e1)->markSettingAAElem();
            if (ex->op == TOKerror)
                return ex;
            assert(ex == e1);
        }
    }
    return this;
}

/************************* PostExp ***********************************/

PostExp::PostExp(TOK op, Loc loc, Expression *e)
        : BinExp(loc, op, sizeof(PostExp), e,
          new IntegerExp(loc, 1, Type::tint32))
{
}

/************************* PreExp ***********************************/

PreExp::PreExp(TOK op, Loc loc, Expression *e)
        : UnaExp(loc, op, sizeof(PreExp), e)
{
}

/************************************************************/

/* op can be TOKassign, TOKconstruct, or TOKblit */

AssignExp::AssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKassign, sizeof(AssignExp), e1, e2)
{
    memset = 0;
}

bool AssignExp::isLvalue()
{
    // Array-op 'x[] = y[]' should make an rvalue.
    // Setting array length 'x.length = v' should make an rvalue.
    if (e1->op == TOKslice ||
        e1->op == TOKarraylength)
    {
        return false;
    }
    return true;
}

Expression *AssignExp::toLvalue(Scope *sc, Expression *ex)
{
    if (e1->op == TOKslice ||
        e1->op == TOKarraylength)
    {
        return Expression::toLvalue(sc, ex);
    }

    /* In front-end level, AssignExp should make an lvalue of e1.
     * Taking the address of e1 will be handled in low level layer,
     * so this function does nothing.
     */
    return this;
}

Expression *AssignExp::toBoolean(Scope *)
{
    // Things like:
    //  if (a = b) ...
    // are usually mistakes.

    error("assignment cannot be used as a condition, perhaps == was meant?");
    return new ErrorExp();
}

/************************************************************/

ConstructExp::ConstructExp(Loc loc, Expression *e1, Expression *e2)
    : AssignExp(loc, e1, e2)
{
    op = TOKconstruct;
}

ConstructExp::ConstructExp(Loc loc, VarDeclaration *v, Expression *e2)
    : AssignExp(loc, new VarExp(loc, v), e2)
{
    assert(v->type && e1->type);
    op = TOKconstruct;

    if (v->storage_class & (STCref | STCout))
        memset |= referenceInit;
}

/************************************************************/

BlitExp::BlitExp(Loc loc, Expression *e1, Expression *e2)
    : AssignExp(loc, e1, e2)
{
    op = TOKblit;
}

BlitExp::BlitExp(Loc loc, VarDeclaration *v, Expression *e2)
    : AssignExp(loc, new VarExp(loc, v), e2)
{
    assert(v->type && e1->type);
    op = TOKblit;

    if (v->storage_class & (STCref | STCout))
        memset |= referenceInit;
}

/************************************************************/

AddAssignExp::AddAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKaddass, sizeof(AddAssignExp), e1, e2)
{
}

/************************************************************/

MinAssignExp::MinAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKminass, sizeof(MinAssignExp), e1, e2)
{
}

/************************************************************/

CatAssignExp::CatAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKcatass, sizeof(CatAssignExp), e1, e2)
{
}

/************************************************************/

MulAssignExp::MulAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKmulass, sizeof(MulAssignExp), e1, e2)
{
}

/************************************************************/

DivAssignExp::DivAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKdivass, sizeof(DivAssignExp), e1, e2)
{
}

/************************************************************/

ModAssignExp::ModAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKmodass, sizeof(ModAssignExp), e1, e2)
{
}

/************************************************************/

ShlAssignExp::ShlAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKshlass, sizeof(ShlAssignExp), e1, e2)
{
}

/************************************************************/

ShrAssignExp::ShrAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKshrass, sizeof(ShrAssignExp), e1, e2)
{
}

/************************************************************/

UshrAssignExp::UshrAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKushrass, sizeof(UshrAssignExp), e1, e2)
{
}

/************************************************************/

AndAssignExp::AndAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKandass, sizeof(AndAssignExp), e1, e2)
{
}

/************************************************************/

OrAssignExp::OrAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKorass, sizeof(OrAssignExp), e1, e2)
{
}

/************************************************************/

XorAssignExp::XorAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKxorass, sizeof(XorAssignExp), e1, e2)
{
}

/***************** PowAssignExp *******************************************/

PowAssignExp::PowAssignExp(Loc loc, Expression *e1, Expression *e2)
        : BinAssignExp(loc, TOKpowass, sizeof(PowAssignExp), e1, e2)
{
}

/************************* AddExp *****************************/

AddExp::AddExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKadd, sizeof(AddExp), e1, e2)
{
}

/************************************************************/

MinExp::MinExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKmin, sizeof(MinExp), e1, e2)
{
}

/************************* CatExp *****************************/

CatExp::CatExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKcat, sizeof(CatExp), e1, e2)
{
}

/************************************************************/

MulExp::MulExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKmul, sizeof(MulExp), e1, e2)
{
}

/************************************************************/

DivExp::DivExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKdiv, sizeof(DivExp), e1, e2)
{
}

/************************************************************/

ModExp::ModExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKmod, sizeof(ModExp), e1, e2)
{
}

/************************************************************/

PowExp::PowExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKpow, sizeof(PowExp), e1, e2)
{
}

/************************************************************/

ShlExp::ShlExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKshl, sizeof(ShlExp), e1, e2)
{
}

/************************************************************/

ShrExp::ShrExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKshr, sizeof(ShrExp), e1, e2)
{
}

/************************************************************/

UshrExp::UshrExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKushr, sizeof(UshrExp), e1, e2)
{
}

/************************************************************/

AndExp::AndExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKand, sizeof(AndExp), e1, e2)
{
}

/************************************************************/

OrExp::OrExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKor, sizeof(OrExp), e1, e2)
{
}

/************************************************************/

XorExp::XorExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKxor, sizeof(XorExp), e1, e2)
{
}

/************************************************************/

LogicalExp::LogicalExp(Loc loc, TOK op, Expression *e1, Expression *e2)
        : BinExp(loc, op, sizeof(LogicalExp), e1, e2)
{
}

Expression *LogicalExp::toBoolean(Scope *sc)
{
    Expression *ex2 = e2->toBoolean(sc);
    if (ex2->op == TOKerror)
        return ex2;
    e2 = ex2;
    return this;
}

/************************************************************/

InExp::InExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKin, sizeof(InExp), e1, e2)
{
}

/************************************************************/

/* This deletes the key e1 from the associative array e2
 */

RemoveExp::RemoveExp(Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, TOKremove, sizeof(RemoveExp), e1, e2)
{
    type = Type::tbool;
}

/************************************************************/

CmpExp::CmpExp(TOK op, Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, op, sizeof(CmpExp), e1, e2)
{
}

/************************************************************/

EqualExp::EqualExp(TOK op, Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, op, sizeof(EqualExp), e1, e2)
{
    assert(op == TOKequal || op == TOKnotequal);
}

/************************************************************/

IdentityExp::IdentityExp(TOK op, Loc loc, Expression *e1, Expression *e2)
        : BinExp(loc, op, sizeof(IdentityExp), e1, e2)
{
}

/****************************************************************/

CondExp::CondExp(Loc loc, Expression *econd, Expression *e1, Expression *e2)
        : BinExp(loc, TOKquestion, sizeof(CondExp), e1, e2)
{
    this->econd = econd;
}

Expression *CondExp::syntaxCopy()
{
    return new CondExp(loc, econd->syntaxCopy(), e1->syntaxCopy(), e2->syntaxCopy());
}

void CondExp::hookDtors(Scope *sc)
{
    class DtorVisitor : public StoppableVisitor
    {
    public:
        Scope *sc;
        CondExp *ce;
        VarDeclaration *vcond;
        bool isThen;

        DtorVisitor(Scope *sc, CondExp *ce)
        {
            this->sc = sc;
            this->ce = ce;
            this->vcond = NULL;
        }

        void visit(Expression *)
        {
            //printf("(e = %s)\n", e->toChars());
        }

        void visit(DeclarationExp *e)
        {
            VarDeclaration *v = e->declaration->isVarDeclaration();
            if (v && !v->isDataseg())
            {
                if (v->_init)
                {
                    ExpInitializer *ei = v->_init->isExpInitializer();
                    if (ei)
                        ei->exp->accept(this);
                }

                if (v->needsScopeDtor())
                {
                    if (!vcond)
                    {
                        vcond = copyToTemp(STCvolatile, "__cond", ce->econd);
                        vcond->semantic(sc);

                        Expression *de = new DeclarationExp(ce->econd->loc, vcond);
                        de = semantic(de, sc);

                        Expression *ve = new VarExp(ce->econd->loc, vcond);
                        ce->econd = Expression::combine(de, ve);
                    }

                    //printf("\t++v = %s, v->edtor = %s\n", v->toChars(), v->edtor->toChars());
                    Expression *ve = new VarExp(vcond->loc, vcond);
                    if (isThen)
                        v->edtor = new LogicalExp(v->edtor->loc, TOKandand, ve, v->edtor);
                    else
                        v->edtor = new LogicalExp(v->edtor->loc, TOKoror, ve, v->edtor);
                    v->edtor = semantic(v->edtor, sc);
                    //printf("\t--v = %s, v->edtor = %s\n", v->toChars(), v->edtor->toChars());
                }
            }
        }
    };

    DtorVisitor v(sc, this);
    //printf("+%s\n", toChars());
    v.isThen = true;    walkPostorder(e1, &v);
    v.isThen = false;   walkPostorder(e2, &v);
    //printf("-%s\n", toChars());
}

bool CondExp::isLvalue()
{
    return e1->isLvalue() && e2->isLvalue();
}


Expression *CondExp::toLvalue(Scope *sc, Expression *)
{
    // convert (econd ? e1 : e2) to *(econd ? &e1 : &e2)
    CondExp *e = (CondExp *)copy();
    e->e1 = e1->toLvalue(sc, NULL)->addressOf();
    e->e2 = e2->toLvalue(sc, NULL)->addressOf();
    e->type = type->pointerTo();
    return new PtrExp(loc, e, type);
}

int CondExp::checkModifiable(Scope *sc, int flag)
{
    return e1->checkModifiable(sc, flag) && e2->checkModifiable(sc, flag);
}

Expression *CondExp::modifiableLvalue(Scope *sc, Expression *)
{
    //error("conditional expression %s is not a modifiable lvalue", toChars());
    e1 = e1->modifiableLvalue(sc, e1);
    e2 = e2->modifiableLvalue(sc, e2);
    return toLvalue(sc, this);
}

Expression *CondExp::toBoolean(Scope *sc)
{
    Expression *ex1 = e1->toBoolean(sc);
    Expression *ex2 = e2->toBoolean(sc);
    if (ex1->op == TOKerror)
        return ex1;
    if (ex2->op == TOKerror)
        return ex2;
    e1 = ex1;
    e2 = ex2;
    return this;
}

/****************************************************************/

DefaultInitExp::DefaultInitExp(Loc loc, TOK subop, int size)
    : Expression(loc, TOKdefault, size)
{
    this->subop = subop;
}

/****************************************************************/

FileInitExp::FileInitExp(Loc loc, TOK tok)
    : DefaultInitExp(loc, tok, sizeof(FileInitExp))
{
}

Expression *FileInitExp::resolveLoc(Loc loc, Scope *sc)
{
    //printf("FileInitExp::resolve() %s\n", toChars());
    const char *s = loc.filename ? loc.filename : sc->_module->ident->toChars();
    if (subop == TOKfilefullpath)
        s = FileName::combine(sc->_module->srcfilePath, s);
    Expression *e = new StringExp(loc, const_cast<char *>(s));
    e = semantic(e, sc);
    e = e->castTo(sc, type);
    return e;
}

/****************************************************************/

LineInitExp::LineInitExp(Loc loc)
    : DefaultInitExp(loc, TOKline, sizeof(LineInitExp))
{
}

Expression *LineInitExp::resolveLoc(Loc loc, Scope *sc)
{
    Expression *e = new IntegerExp(loc, loc.linnum, Type::tint32);
    e = e->castTo(sc, type);
    return e;
}

/****************************************************************/

ModuleInitExp::ModuleInitExp(Loc loc)
    : DefaultInitExp(loc, TOKmodulestring, sizeof(ModuleInitExp))
{
}

Expression *ModuleInitExp::resolveLoc(Loc loc, Scope *sc)
{
    const char *s;
    if (sc->callsc)
        s = sc->callsc->_module->toPrettyChars();
    else
        s = sc->_module->toPrettyChars();
    Expression *e = new StringExp(loc, const_cast<char *>(s));
    e = semantic(e, sc);
    e = e->castTo(sc, type);
    return e;
}

/****************************************************************/

FuncInitExp::FuncInitExp(Loc loc)
    : DefaultInitExp(loc, TOKfuncstring, sizeof(FuncInitExp))
{
}

Expression *FuncInitExp::resolveLoc(Loc loc, Scope *sc)
{
    const char *s;
    if (sc->callsc && sc->callsc->func)
        s = sc->callsc->func->Dsymbol::toPrettyChars();
    else if (sc->func)
        s = sc->func->Dsymbol::toPrettyChars();
    else
        s = "";
    Expression *e = new StringExp(loc, const_cast<char *>(s));
    e = semantic(e, sc);
    e = e->castTo(sc, type);
    return e;
}

/****************************************************************/

PrettyFuncInitExp::PrettyFuncInitExp(Loc loc)
    : DefaultInitExp(loc, TOKprettyfunc, sizeof(PrettyFuncInitExp))
{
}

Expression *PrettyFuncInitExp::resolveLoc(Loc loc, Scope *sc)
{
    FuncDeclaration *fd;
    if (sc->callsc && sc->callsc->func)
        fd = sc->callsc->func;
    else
        fd = sc->func;

    const char *s;
    if (fd)
    {
        const char *funcStr = fd->Dsymbol::toPrettyChars();
        OutBuffer buf;
        functionToBufferWithIdent((TypeFunction *)fd->type, &buf, funcStr);
        s = buf.extractChars();
    }
    else
    {
        s = "";
    }

    Expression *e = new StringExp(loc, const_cast<char *>(s));
    e = semantic(e, sc);
    e = e->castTo(sc, type);
    return e;
}

/****************************************************************/

Expression *extractOpDollarSideEffect(Scope *sc, UnaExp *ue)
{
    Expression *e0;
    Expression *e1 = Expression::extractLast(ue->e1, &e0);
    // Bugzilla 12585: Extract the side effect part if ue->e1 is comma.

    if (!isTrivialExp(e1))
    {
        /* Even if opDollar is needed, 'e1' should be evaluate only once. So
         * Rewrite:
         *      e1.opIndex( ... use of $ ... )
         *      e1.opSlice( ... use of $ ... )
         * as:
         *      (ref __dop = e1, __dop).opIndex( ... __dop.opDollar ...)
         *      (ref __dop = e1, __dop).opSlice( ... __dop.opDollar ...)
         */
        e1 = extractSideEffect(sc, "__dop", &e0, e1, false);
        assert(e1->op == TOKvar);
        VarExp *ve = (VarExp *)e1;
        ve->var->storage_class |= STCexptemp;     // lifetime limited to expression
    }
    ue->e1 = e1;
    return e0;
}

/**************************************
 * Runs semantic on ae->arguments. Declares temporary variables
 * if '$' was used.
 */
Expression *resolveOpDollar(Scope *sc, ArrayExp *ae, Expression **pe0)
{
    assert(!ae->lengthVar);

    *pe0 = NULL;

    AggregateDeclaration *ad = isAggregate(ae->e1->type);
    Dsymbol *slice = search_function(ad, Id::slice);
    //printf("slice = %s %s\n", slice->kind(), slice->toChars());

    for (size_t i = 0; i < ae->arguments->length; i++)
    {
        if (i == 0)
            *pe0 = extractOpDollarSideEffect(sc, ae);

        Expression *e = (*ae->arguments)[i];
        if (e->op == TOKinterval && !(slice && slice->isTemplateDeclaration()))
        {
        Lfallback:
            if (ae->arguments->length == 1)
                return NULL;
            ae->error("multi-dimensional slicing requires template opSlice");
            return new ErrorExp();
        }
        //printf("[%d] e = %s\n", i, e->toChars());

        // Create scope for '$' variable for this dimension
        ArrayScopeSymbol *sym = new ArrayScopeSymbol(sc, ae);
        sym->loc = ae->loc;
        sym->parent = sc->scopesym;
        sc = sc->push(sym);
        ae->lengthVar = NULL;       // Create it only if required
        ae->currentDimension = i;   // Dimension for $, if required

        e = semantic(e, sc);
        e = resolveProperties(sc, e);

        if (ae->lengthVar && sc->func)
        {
            // If $ was used, declare it now
            Expression *de = new DeclarationExp(ae->loc, ae->lengthVar);
            de = semantic(de, sc);
            *pe0 = Expression::combine(*pe0, de);
        }
        sc = sc->pop();

        if (e->op == TOKinterval)
        {
            IntervalExp *ie = (IntervalExp *)e;

            Objects *tiargs = new Objects();
            Expression *edim = new IntegerExp(ae->loc, i, Type::tsize_t);
            edim = semantic(edim, sc);
            tiargs->push(edim);

            Expressions *fargs = new Expressions();
            fargs->push(ie->lwr);
            fargs->push(ie->upr);

            unsigned xerrors = global.startGagging();
            sc = sc->push();
            FuncDeclaration *fslice = resolveFuncCall(ae->loc, sc, slice, tiargs, ae->e1->type, fargs, 1);
            sc = sc->pop();
            global.endGagging(xerrors);
            if (!fslice)
                goto Lfallback;

            e = new DotTemplateInstanceExp(ae->loc, ae->e1, slice->ident, tiargs);
            e = new CallExp(ae->loc, e, fargs);
            e = semantic(e, sc);
        }

        if (!e->type)
        {
            ae->error("%s has no value", e->toChars());
            e = new ErrorExp();
        }
        if (e->op == TOKerror)
            return e;

        (*ae->arguments)[i] = e;
    }

    return ae;
}

/***********************************************************
 * Resolve `exp` as a compile-time known string.
 * Params:
 *  sc  = scope
 *  exp = Expression which expected as a string
 *  s   = What the string is expected for, will be used in error diagnostic.
 * Returns:
 *  String literal, or `null` if error happens.
 */
StringExp *semanticString(Scope *sc, Expression *exp, const char *s)
{
    sc = sc->startCTFE();
    exp = semantic(exp, sc);
    exp = resolveProperties(sc, exp);
    sc = sc->endCTFE();

    if (exp->op == TOKerror)
        return NULL;

    Expression *e = exp;
    if (exp->type->isString())
    {
        e = e->ctfeInterpret();
        if (e->op == TOKerror)
            return NULL;
    }

    StringExp *se = e->toStringExp();
    if (!se)
    {
        exp->error("string expected for %s, not (%s) of type %s",
            s, exp->toChars(), exp->type->toChars());
        return NULL;
    }
    return se;
}

/**************************************
 * Runs semantic on se->lwr and se->upr. Declares a temporary variable
 * if '$' was used.
 */
Expression *resolveOpDollar(Scope *sc, ArrayExp *ae, IntervalExp *ie, Expression **pe0)
{
    //assert(!ae->lengthVar);
    if (!ie)
        return ae;

    VarDeclaration *lengthVar = ae->lengthVar;

    // create scope for '$'
    ArrayScopeSymbol *sym = new ArrayScopeSymbol(sc, ae);
    sym->loc = ae->loc;
    sym->parent = sc->scopesym;
    sc = sc->push(sym);

    for (size_t i = 0; i < 2; ++i)
    {
        Expression *e = i == 0 ? ie->lwr : ie->upr;
        e = semantic(e, sc);
        e = resolveProperties(sc, e);
        if (!e->type)
        {
            ae->error("%s has no value", e->toChars());
            return new ErrorExp();
        }
        (i == 0 ? ie->lwr : ie->upr) = e;
    }

    if (lengthVar != ae->lengthVar && sc->func)
    {
        // If $ was used, declare it now
        Expression *de = new DeclarationExp(ae->loc, ae->lengthVar);
        de = semantic(de, sc);
        *pe0 = Expression::combine(*pe0, de);
    }
    sc = sc->pop();

    return ae;
}

Expression *BinExp::reorderSettingAAElem(Scope *sc)
{
    BinExp *be = this;

    if (be->e1->op != TOKindex)
        return be;
    IndexExp *ie = (IndexExp *)be->e1;
    if (ie->e1->type->toBasetype()->ty != Taarray)
        return be;

    /* Fix evaluation order of setting AA element. (Bugzilla 3825)
     * Rewrite:
     *     aa[k1][k2][k3] op= val;
     * as:
     *     auto ref __aatmp = aa;
     *     auto ref __aakey3 = k1, __aakey2 = k2, __aakey1 = k3;
     *     auto ref __aaval = val;
     *     __aatmp[__aakey3][__aakey2][__aakey1] op= __aaval;  // assignment
     */

    Expression *e0 = NULL;
    while (1)
    {
        Expression *de = NULL;
        ie->e2 = extractSideEffect(sc, "__aakey", &de, ie->e2);
        e0 = Expression::combine(de, e0);

        Expression *ie1 = ie->e1;
        if (ie1->op != TOKindex ||
            ((IndexExp *)ie1)->e1->type->toBasetype()->ty != Taarray)
        {
            break;
        }
        ie = (IndexExp *)ie1;
    }
    assert(ie->e1->type->toBasetype()->ty == Taarray);

    Expression *de = NULL;
    ie->e1 = extractSideEffect(sc, "__aatmp", &de, ie->e1);
    e0 = Expression::combine(de, e0);

    be->e2 = extractSideEffect(sc, "__aaval", &e0, be->e2, true);

    //printf("-e0 = %s, be = %s\n", e0->toChars(), be->toChars());
    return Expression::combine(e0, be);
}
