
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "root/dsystem.h"
#include "root/aav.h"

#include "dsymbol.h"
#include "aggregate.h"
#include "aliasthis.h"
#include "attrib.h"
#include "cond.h"
#include "declaration.h"
#include "enum.h"
#include "errors.h"
#include "hdrgen.h"
#include "id.h"
#include "import.h"
#include "init.h"
#include "mars.h"
#include "module.h"
#include "nspace.h"
#include "objc.h"
#include "parse.h"
#include "scope.h"
#include "statement.h"
#include "staticassert.h"
#include "target.h"
#include "template.h"
#include "utf.h"
#include "version.h"
#include "visitor.h"

bool allowsContractWithoutBody(FuncDeclaration *funcdecl);
bool checkFrameAccess(Loc loc, Scope *sc, AggregateDeclaration *ad, size_t istart = 0);
VarDeclaration *copyToTemp(StorageClass stc, const char *name, Expression *e);
Initializer *inferType(Initializer *init, Scope *sc);
void MODtoBuffer(OutBuffer *buf, MOD mod);
bool reliesOnTident(Type *t, TemplateParameters *tparams = NULL, size_t iStart = 0);
bool expressionsToString(OutBuffer &buf, Scope *sc, Expressions *exps);
bool symbolIsVisible(Scope *sc, Dsymbol *s);
Objc *objc();

static unsigned setMangleOverride(Dsymbol *s, char *sym)
{
    AttribDeclaration *ad = s->isAttribDeclaration();

    if (ad)
    {
        Dsymbols *decls = ad->include(NULL);
        unsigned nestedCount = 0;

        if (decls && decls->length)
            for (size_t i = 0; i < decls->length; ++i)
                nestedCount += setMangleOverride((*decls)[i], sym);

        return nestedCount;
    }
    else if (s->isFuncDeclaration() || s->isVarDeclaration())
    {
        s->isDeclaration()->mangleOverride = sym;
        return 1;
    }
    else
        return 0;
}

/**********************************
 * Decide if attributes for this function can be inferred from examining
 * the function body.
 * Returns:
 *  true if can
 */
static bool canInferAttributes(FuncDeclaration *fd, Scope *sc)
{
    if (!fd->fbody)
        return false;

    if (fd->isVirtualMethod())
        return false;               // since they may be overridden

    if (sc->func &&
        /********** this is for backwards compatibility for the moment ********/
        (!fd->isMember() || (sc->func->isSafeBypassingInference() && !fd->isInstantiated())))
        return true;

    if (fd->isFuncLiteralDeclaration() ||               // externs are not possible with literals
        (fd->storage_class & STCinference) ||           // do attribute inference
        (fd->inferRetType && !fd->isCtorDeclaration()))
        return true;

    if (fd->isInstantiated())
    {
        TemplateInstance *ti = fd->parent->isTemplateInstance();
        if (ti == NULL || ti->isTemplateMixin() || ti->tempdecl->ident == fd->ident)
            return true;
    }

    return false;
}

/*****************************************
 * Initialize for inferring the attributes of this function.
 */
static void initInferAttributes(FuncDeclaration *fd)
{
    //printf("initInferAttributes() for %s\n", toPrettyChars());
    TypeFunction *tf = fd->type->toTypeFunction();
    if (tf->purity == PUREimpure) // purity not specified
        fd->flags |= FUNCFLAGpurityInprocess;

    if (tf->trust == TRUSTdefault)
        fd->flags |= FUNCFLAGsafetyInprocess;

    if (!tf->isnothrow)
        fd->flags |= FUNCFLAGnothrowInprocess;

    if (!tf->isnogc)
        fd->flags |= FUNCFLAGnogcInprocess;

    if (!fd->isVirtual() || fd->introducing)
        fd->flags |= FUNCFLAGreturnInprocess;

    // Initialize for inferring STCscope
    if (global.params.vsafe)
        fd->flags |= FUNCFLAGinferScope;
}

static void badObjectDotD(ClassDeclaration *cd)
{
    cd->error("missing or corrupt object.d");
    fatal();
}

/* Bugzilla 12078, 12143 and 15733:
 * While resolving base classes and interfaces, a base may refer
 * the member of this derived class. In that time, if all bases of
 * this class can  be determined, we can go forward the semantc process
 * beyond the Lancestorsdone. To do the recursive semantic analysis,
 * temporarily set and unset `_scope` around exp().
 */
static Type *resolveBase(ClassDeclaration *cd, Scope *sc, Scope *&scx, Type *type)
{
    if (!scx)
    {
        scx = sc->copy();
        scx->setNoFree();
    }
    cd->_scope = scx;
    Type *t = typeSemantic(type, cd->loc, sc);
    cd->_scope = NULL;
    return t;
}

static void resolveBase(ClassDeclaration *cd, Scope *sc, Scope *&scx, ClassDeclaration *sym)
{
    if (!scx)
    {
        scx = sc->copy();
        scx->setNoFree();
    }
    cd->_scope = scx;
    dsymbolSemantic(sym, NULL);
    cd->_scope = NULL;
}

class DsymbolSemanticVisitor : public Visitor
{
public:
    Scope *sc;

    DsymbolSemanticVisitor(Scope *sc)
    {
        this->sc = sc;
    }

    void visit(Dsymbol *dsym)
    {
        dsym->error("%p has no semantic routine", dsym);
    }

    void visit(ScopeDsymbol *) { }
    void visit(Declaration *) { }

    void visit(AliasThis *dsym)
    {
        if (dsym->semanticRun != PASSinit)
            return;

        if (dsym->_scope)
        {
            sc = dsym->_scope;
            dsym->_scope = NULL;
        }

        if (!sc)
            return;

        dsym->semanticRun = PASSsemantic;

        Dsymbol *p = sc->parent->pastMixin();
        AggregateDeclaration *ad = p->isAggregateDeclaration();
        if (!ad)
        {
            error(dsym->loc, "alias this can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            return;
        }

        assert(ad->members);
        Dsymbol *s = ad->search(dsym->loc, dsym->ident);
        if (!s)
        {
            s = sc->search(dsym->loc, dsym->ident, NULL);
            if (s)
                error(dsym->loc, "%s is not a member of %s", s->toChars(), ad->toChars());
            else
                error(dsym->loc, "undefined identifier %s", dsym->ident->toChars());
            return;
        }
        else if (ad->aliasthis && s != ad->aliasthis)
        {
            error(dsym->loc, "there can be only one alias this");
            return;
        }

        if (ad->type->ty == Tstruct && ((TypeStruct *)ad->type)->sym != ad)
        {
            AggregateDeclaration *ad2 = ((TypeStruct *)ad->type)->sym;
            assert(ad2->type == Type::terror);
            ad->aliasthis = ad2->aliasthis;
            return;
        }

        /* disable the alias this conversion so the implicit conversion check
         * doesn't use it.
         */
        ad->aliasthis = NULL;

        Dsymbol *sx = s;
        if (sx->isAliasDeclaration())
            sx = sx->toAlias();
        Declaration *d = sx->isDeclaration();
        if (d && !d->isTupleDeclaration())
        {
            Type *t = d->type;
            assert(t);
            if (ad->type->implicitConvTo(t) > MATCHnomatch)
            {
                error(dsym->loc, "alias this is not reachable as %s already converts to %s", ad->toChars(), t->toChars());
            }
        }

        ad->aliasthis = s;
        dsym->semanticRun = PASSsemanticdone;
    }

    void visit(AliasDeclaration *dsym)
    {
        if (dsym->semanticRun >= PASSsemanticdone)
            return;
        assert(dsym->semanticRun <= PASSsemantic);

        dsym->storage_class |= sc->stc & STCdeprecated;
        dsym->protection = sc->protection;
        dsym->userAttribDecl = sc->userAttribDecl;

        if (!sc->func && dsym->inNonRoot())
            return;

        aliasSemantic(dsym, sc);
    }

    void visit(VarDeclaration *dsym)
    {
        //if (dsym->semanticRun > PASSinit)
        //    return;
        //dsym->semanticRun = PASSsemantic;

        if (dsym->semanticRun >= PASSsemanticdone)
            return;

        Scope *scx = NULL;
        if (dsym->_scope)
        {
            sc = dsym->_scope;
            scx = sc;
            dsym->_scope = NULL;
        }

        if (!sc)
            return;

        dsym->semanticRun = PASSsemantic;

        /* Pick up storage classes from context, but except synchronized,
         * override, abstract, and final.
         */
        dsym->storage_class |= (sc->stc & ~(STCsynchronized | STCoverride | STCabstract | STCfinal));
        if (dsym->storage_class & STCextern && dsym->_init)
            dsym->error("extern symbols cannot have initializers");

        dsym->userAttribDecl = sc->userAttribDecl;

        AggregateDeclaration *ad = dsym->isThis();
        if (ad)
            dsym->storage_class |= ad->storage_class & STC_TYPECTOR;

        /* If auto type inference, do the inference
         */
        int inferred = 0;
        if (!dsym->type)
        {
            dsym->inuse++;

            // Infering the type requires running semantic,
            // so mark the scope as ctfe if required
            bool needctfe = (dsym->storage_class & (STCmanifest | STCstatic)) != 0;
            if (needctfe) sc = sc->startCTFE();

            //printf("inferring type for %s with init %s\n", dsym->toChars(), dsym->_init->toChars());
            dsym->_init = inferType(dsym->_init, sc);
            dsym->type = initializerToExpression(dsym->_init)->type;

            if (needctfe) sc = sc->endCTFE();

            dsym->inuse--;
            inferred = 1;

            /* This is a kludge to support the existing syntax for RAII
             * declarations.
             */
            dsym->storage_class &= ~STCauto;
            dsym->originalType = dsym->type->syntaxCopy();
        }
        else
        {
            if (!dsym->originalType)
                dsym->originalType = dsym->type->syntaxCopy();

            /* Prefix function attributes of variable declaration can affect
             * its type:
             *      pure nothrow void function() fp;
             *      static assert(is(typeof(fp) == void function() pure nothrow));
             */
            Scope *sc2 = sc->push();
            sc2->stc |= (dsym->storage_class & STC_FUNCATTR);
            dsym->inuse++;
            dsym->type = typeSemantic(dsym->type, dsym->loc, sc2);
            dsym->inuse--;
            sc2->pop();
        }
        //printf(" semantic type = %s\n", dsym->type ? dsym->type->toChars() : "null");
        if (dsym->type->ty == Terror)
            dsym->errors = true;

        dsym->type->checkDeprecated(dsym->loc, sc);
        dsym->linkage = sc->linkage;
        dsym->parent = sc->parent;
        //printf("this = %p, parent = %p, '%s'\n", dsym, dsym->parent, dsym->parent->toChars());
        dsym->protection = sc->protection;

        /* If scope's alignment is the default, use the type's alignment,
         * otherwise the scope overrrides.
         */
        dsym->alignment = sc->alignment();
        if (dsym->alignment == STRUCTALIGN_DEFAULT)
            dsym->alignment = dsym->type->alignment();          // use type's alignment

        //printf("sc->stc = %x\n", sc->stc);
        //printf("storage_class = x%x\n", dsym->storage_class);

        if (global.params.vcomplex)
            dsym->type->checkComplexTransition(dsym->loc);

        // Calculate type size + safety checks
        if (sc->func && !sc->intypeof)
        {
            if ((dsym->storage_class & STCgshared) && !dsym->isMember())
            {
                if (sc->func->setUnsafe())
                    dsym->error("__gshared not allowed in safe functions; use shared");
            }
        }

        Dsymbol *parent = dsym->toParent();

        Type *tb = dsym->type->toBasetype();
        Type *tbn = tb->baseElemOf();
        if (tb->ty == Tvoid && !(dsym->storage_class & STClazy))
        {
            if (inferred)
            {
                dsym->error("type %s is inferred from initializer %s, and variables cannot be of type void",
                    dsym->type->toChars(), dsym->_init->toChars());
            }
            else
                dsym->error("variables cannot be of type void");
            dsym->type = Type::terror;
            tb = dsym->type;
        }
        if (tb->ty == Tfunction)
        {
            dsym->error("cannot be declared to be a function");
            dsym->type = Type::terror;
            tb = dsym->type;
        }
        if (tb->ty == Tstruct)
        {
            TypeStruct *ts = (TypeStruct *)tb;
            if (!ts->sym->members)
            {
                dsym->error("no definition of struct `%s`", ts->toChars());

                // Explain why the definition is required when it's part of another type
                if (!dsym->type->isTypeStruct())
                {
                    // Prefer Loc of the dependant type
                    Dsymbol *s = dsym->type->toDsymbol(sc);
                    Loc loc = s ? s->loc : dsym->loc;
                    errorSupplemental(loc, "required by type `%s`", dsym->type->toChars());
                }

                // Flag variable as error to avoid invalid error messages due to unknown size
                dsym->type = Type::terror;
            }
        }
        if ((dsym->storage_class & STCauto) && !inferred)
            dsym->error("storage class `auto` has no effect if type is not inferred, did you mean `scope`?");

        if (tb->ty == Ttuple)
        {
            /* Instead, declare variables for each of the tuple elements
             * and add those.
             */
            TypeTuple *tt = (TypeTuple *)tb;
            size_t nelems = Parameter::dim(tt->arguments);
            Expression *ie = (dsym->_init && !dsym->_init->isVoidInitializer()) ? initializerToExpression(dsym->_init) : NULL;
            if (ie)
                ie = expressionSemantic(ie, sc);

            if (nelems > 0 && ie)
            {
                Expressions *iexps = new Expressions();
                iexps->push(ie);

                Expressions *exps = new Expressions();

                for (size_t pos = 0; pos < iexps->length; pos++)
                {
                Lexpand1:
                    Expression *e = (*iexps)[pos];
                    Parameter *arg = Parameter::getNth(tt->arguments, pos);
                    arg->type = typeSemantic(arg->type, dsym->loc, sc);
                    //printf("[%d] iexps->length = %d, ", pos, iexps->length);
                    //printf("e = (%s %s, %s), ", Token::tochars[e->op], e->toChars(), e->type->toChars());
                    //printf("arg = (%s, %s)\n", arg->toChars(), arg->type->toChars());

                    if (e != ie)
                    {
                    if (iexps->length > nelems)
                        goto Lnomatch;
                    if (e->type->implicitConvTo(arg->type))
                        continue;
                    }

                    if (e->op == TOKtuple)
                    {
                        TupleExp *te = (TupleExp *)e;
                        if (iexps->length - 1 + te->exps->length > nelems)
                            goto Lnomatch;

                        iexps->remove(pos);
                        iexps->insert(pos, te->exps);
                        (*iexps)[pos] = Expression::combine(te->e0, (*iexps)[pos]);
                        goto Lexpand1;
                    }
                    else if (isAliasThisTuple(e))
                    {
                        VarDeclaration *v = copyToTemp(0, "__tup", e);
                        dsymbolSemantic(v, sc);
                        VarExp *ve = new VarExp(dsym->loc, v);
                        ve->type = e->type;

                        exps->setDim(1);
                        (*exps)[0] = ve;
                        expandAliasThisTuples(exps, 0);

                        for (size_t u = 0; u < exps->length ; u++)
                        {
                        Lexpand2:
                            Expression *ee = (*exps)[u];
                            arg = Parameter::getNth(tt->arguments, pos + u);
                            arg->type = typeSemantic(arg->type, dsym->loc, sc);
                            //printf("[%d+%d] exps->length = %d, ", pos, u, exps->length);
                            //printf("ee = (%s %s, %s), ", Token::tochars[ee->op], ee->toChars(), ee->type->toChars());
                            //printf("arg = (%s, %s)\n", arg->toChars(), arg->type->toChars());

                            size_t iexps_dim = iexps->length - 1 + exps->length;
                            if (iexps_dim > nelems)
                                goto Lnomatch;
                            if (ee->type->implicitConvTo(arg->type))
                                continue;

                            if (expandAliasThisTuples(exps, u) != -1)
                                goto Lexpand2;
                        }

                        if ((*exps)[0] != ve)
                        {
                            Expression *e0 = (*exps)[0];
                            (*exps)[0] = new CommaExp(dsym->loc, new DeclarationExp(dsym->loc, v), e0);
                            (*exps)[0]->type = e0->type;

                            iexps->remove(pos);
                            iexps->insert(pos, exps);
                            goto Lexpand1;
                        }
                    }
                }
                if (iexps->length < nelems)
                    goto Lnomatch;

                ie = new TupleExp(dsym->_init->loc, iexps);
            }
    Lnomatch:

            if (ie && ie->op == TOKtuple)
            {
                TupleExp *te = (TupleExp *)ie;
                size_t tedim = te->exps->length;
                if (tedim != nelems)
                {
                    error(dsym->loc, "tuple of %d elements cannot be assigned to tuple of %d elements", (int)tedim, (int)nelems);
                    for (size_t u = tedim; u < nelems; u++) // fill dummy expression
                        te->exps->push(new ErrorExp());
                }
            }

            Objects *exps = new Objects();
            exps->setDim(nelems);
            for (size_t i = 0; i < nelems; i++)
            {
                Parameter *arg = Parameter::getNth(tt->arguments, i);

                OutBuffer buf;
                buf.printf("__%s_field_%llu", dsym->ident->toChars(), (ulonglong)i);
                const char *name = buf.extractChars();
                Identifier *id = Identifier::idPool(name);

                Initializer *ti;
                if (ie)
                {
                    Expression *einit = ie;
                    if (ie->op == TOKtuple)
                    {
                        TupleExp *te = (TupleExp *)ie;
                        einit = (*te->exps)[i];
                        if (i == 0)
                            einit = Expression::combine(te->e0, einit);
                    }
                    ti = new ExpInitializer(einit->loc, einit);
                }
                else
                    ti = dsym->_init ? dsym->_init->syntaxCopy() : NULL;

                VarDeclaration *v = new VarDeclaration(dsym->loc, arg->type, id, ti);
                v->storage_class |= STCtemp | STClocal | dsym->storage_class;
                if (arg->storageClass & STCparameter)
                    v->storage_class |= arg->storageClass;
                //printf("declaring field %s of type %s\n", v->toChars(), v->type->toChars());
                dsymbolSemantic(v, sc);

                if (sc->scopesym)
                {
                    //printf("adding %s to %s\n", v->toChars(), sc->scopesym->toChars());
                    if (sc->scopesym->members)
                        sc->scopesym->members->push(v);
                }

                Expression *e = new DsymbolExp(dsym->loc, v);
                (*exps)[i] = e;
            }
            TupleDeclaration *v2 = new TupleDeclaration(dsym->loc, dsym->ident, exps);
            v2->parent = dsym->parent;
            v2->isexp = true;
            dsym->aliassym = v2;
            dsym->semanticRun = PASSsemanticdone;
            return;
        }

        /* Storage class can modify the type
         */
        dsym->type = dsym->type->addStorageClass(dsym->storage_class);

        /* Adjust storage class to reflect type
         */
        if (dsym->type->isConst())
        {
            dsym->storage_class |= STCconst;
            if (dsym->type->isShared())
                dsym->storage_class |= STCshared;
        }
        else if (dsym->type->isImmutable())
            dsym->storage_class |= STCimmutable;
        else if (dsym->type->isShared())
            dsym->storage_class |= STCshared;
        else if (dsym->type->isWild())
            dsym->storage_class |= STCwild;

        if (StorageClass stc = dsym->storage_class & (STCsynchronized | STCoverride | STCabstract | STCfinal))
        {
            if (stc == STCfinal)
                dsym->error("cannot be final, perhaps you meant const?");
            else
            {
                OutBuffer buf;
                stcToBuffer(&buf, stc);
                dsym->error("cannot be %s", buf.peekChars());
            }
            dsym->storage_class &= ~stc;  // strip off
        }

        if (dsym->storage_class & STCscope)
        {
            StorageClass stc = dsym->storage_class & (STCstatic | STCextern | STCmanifest | STCtls | STCgshared);
            if (stc)
            {
                OutBuffer buf;
                stcToBuffer(&buf, stc);
                dsym->error("cannot be `scope` and `%s`", buf.peekChars());
            }
            else if (dsym->isMember())
            {
                dsym->error("field cannot be `scope`");
            }
            else if (!dsym->type->hasPointers())
            {
                dsym->storage_class &= ~STCscope;     // silently ignore; may occur in generic code
            }
        }

        if (dsym->storage_class & (STCstatic | STCextern | STCmanifest | STCtemplateparameter | STCtls | STCgshared | STCctfe))
        {
        }
        else
        {
            AggregateDeclaration *aad = parent->isAggregateDeclaration();
            if (aad)
            {
                if (global.params.vfield &&
                    dsym->storage_class & (STCconst | STCimmutable) && dsym->_init && !dsym->_init->isVoidInitializer())
                {
                    const char *s = (dsym->storage_class & STCimmutable) ? "immutable" : "const";
                    message(dsym->loc, "`%s.%s` is `%s` field", ad->toPrettyChars(), dsym->toChars(), s);
                }
                dsym->storage_class |= STCfield;
                if (tbn->ty == Tstruct && ((TypeStruct *)tbn)->sym->noDefaultCtor)
                {
                    if (!dsym->isThisDeclaration() && !dsym->_init)
                        aad->noDefaultCtor = true;
                }
            }

            InterfaceDeclaration *id = parent->isInterfaceDeclaration();
            if (id)
            {
                dsym->error("field not allowed in interface");
            }
            else if (aad && aad->sizeok == SIZEOKdone)
            {
                dsym->error("cannot be further field because it will change the determined %s size", aad->toChars());
            }

            /* Templates cannot add fields to aggregates
             */
            TemplateInstance *ti = parent->isTemplateInstance();
            if (ti)
            {
                // Take care of nested templates
                while (1)
                {
                    TemplateInstance *ti2 = ti->tempdecl->parent->isTemplateInstance();
                    if (!ti2)
                        break;
                    ti = ti2;
                }

                // If it's a member template
                AggregateDeclaration *ad2 = ti->tempdecl->isMember();
                if (ad2 && dsym->storage_class != STCundefined)
                {
                    dsym->error("cannot use template to add field to aggregate `%s`", ad2->toChars());
                }
            }
        }

        if ((dsym->storage_class & (STCref | STCparameter | STCforeach | STCtemp | STCresult)) == STCref && dsym->ident != Id::This)
        {
            dsym->error("only parameters or foreach declarations can be ref");
        }

        if (dsym->type->hasWild())
        {
            if (dsym->storage_class & (STCstatic | STCextern | STCtls | STCgshared | STCmanifest | STCfield) ||
                dsym->isDataseg()
                )
            {
                dsym->error("only parameters or stack based variables can be inout");
            }
            FuncDeclaration *func = sc->func;
            if (func)
            {
                if (func->fes)
                    func = func->fes->func;
                bool isWild = false;
                for (FuncDeclaration *fd = func; fd; fd = fd->toParent2()->isFuncDeclaration())
                {
                    if (((TypeFunction *)fd->type)->iswild)
                    {
                        isWild = true;
                        break;
                    }
                }
                if (!isWild)
                {
                    dsym->error("inout variables can only be declared inside inout functions");
                }
            }
        }

        if (!(dsym->storage_class & (STCctfe | STCref | STCresult)) && tbn->ty == Tstruct &&
            ((TypeStruct *)tbn)->sym->noDefaultCtor)
        {
            if (!dsym->_init)
            {
                if (dsym->isField())
                {
                    /* For fields, we'll check the constructor later to make sure it is initialized
                     */
                    dsym->storage_class |= STCnodefaultctor;
                }
                else if (dsym->storage_class & STCparameter)
                    ;
                else
                    dsym->error("default construction is disabled for type %s", dsym->type->toChars());
            }
        }

        FuncDeclaration *fd = parent->isFuncDeclaration();
        if (dsym->type->isscope() && !(dsym->storage_class & STCnodtor))
        {
            if (dsym->storage_class & (STCfield | STCout | STCref | STCstatic | STCmanifest | STCtls | STCgshared) || !fd)
            {
                dsym->error("globals, statics, fields, manifest constants, ref and out parameters cannot be scope");
            }

            if (!(dsym->storage_class & STCscope))
            {
                if (!(dsym->storage_class & STCparameter) && dsym->ident != Id::withSym)
                    dsym->error("reference to scope class must be scope");
            }
        }

        // Calculate type size + safety checks
        if (sc->func && !sc->intypeof)
        {
            if (dsym->_init && dsym->_init->isVoidInitializer() && dsym->type->hasPointers()) // get type size
            {
                if (sc->func->setUnsafe())
                    dsym->error("void initializers for pointers not allowed in safe functions");
            }
            else if (!dsym->_init &&
                     !(dsym->storage_class & (STCstatic | STCextern | STCtls | STCgshared | STCmanifest | STCfield | STCparameter)) &&
                     dsym->type->hasVoidInitPointers())
            {
                if (sc->func->setUnsafe())
                    dsym->error("void initializers for pointers not allowed in safe functions");
            }
        }

        if (!dsym->_init && !fd)
        {
            // If not mutable, initializable by constructor only
            dsym->storage_class |= STCctorinit;
        }

        if (dsym->_init)
            dsym->storage_class |= STCinit;     // remember we had an explicit initializer
        else if (dsym->storage_class & STCmanifest)
            dsym->error("manifest constants must have initializers");

        bool isBlit = false;
        d_uns64 sz = 0;
        if (!dsym->_init && !sc->inunion && !(dsym->storage_class & (STCstatic | STCgshared | STCextern)) && fd &&
            (!(dsym->storage_class & (STCfield | STCin | STCforeach | STCparameter | STCresult))
             || (dsym->storage_class & STCout)) &&
            (sz = dsym->type->size()) != 0)
        {
            // Provide a default initializer
            //printf("Providing default initializer for '%s'\n", dsym->toChars());
            if (sz == SIZE_INVALID && dsym->type->ty != Terror)
                dsym->error("size of type %s is invalid", dsym->type->toChars());

            Type *tv = dsym->type;
            while (tv->ty == Tsarray)    // Don't skip Tenum
                tv = tv->nextOf();
            if (tv->needsNested())
            {
                /* Nested struct requires valid enclosing frame pointer.
                 * In StructLiteralExp::toElem(), it's calculated.
                 */
                assert(tv->toBasetype()->ty == Tstruct);
                checkFrameAccess(dsym->loc, sc, ((TypeStruct *)tbn)->sym);

                Expression *e = tv->defaultInitLiteral(dsym->loc);
                e = new BlitExp(dsym->loc, new VarExp(dsym->loc, dsym), e);
                e = expressionSemantic(e, sc);
                dsym->_init = new ExpInitializer(dsym->loc, e);
                goto Ldtor;
            }
            if (tv->ty == Tstruct && ((TypeStruct *)tv)->sym->zeroInit == 1)
            {
                /* If a struct is all zeros, as a special case
                 * set it's initializer to the integer 0.
                 * In AssignExp::toElem(), we check for this and issue
                 * a memset() to initialize the struct.
                 * Must do same check in interpreter.
                 */
                Expression *e = new IntegerExp(dsym->loc, 0, Type::tint32);
                e = new BlitExp(dsym->loc, new VarExp(dsym->loc, dsym), e);
                e->type = dsym->type;         // don't type check this, it would fail
                dsym->_init = new ExpInitializer(dsym->loc, e);
                goto Ldtor;
            }
            if (dsym->type->baseElemOf()->ty == Tvoid)
            {
                dsym->error("%s does not have a default initializer", dsym->type->toChars());
            }
            else if (Expression *e = dsym->type->defaultInit(dsym->loc))
            {
                dsym->_init = new ExpInitializer(dsym->loc, e);
            }
            // Default initializer is always a blit
            isBlit = true;
        }

        if (dsym->_init)
        {
            sc = sc->push();
            sc->stc &= ~(STC_TYPECTOR | STCpure | STCnothrow | STCnogc | STCref | STCdisable);

            ExpInitializer *ei = dsym->_init->isExpInitializer();
            if (ei)     // Bugzilla 13424: Preset the required type to fail in FuncLiteralDeclaration::semantic3
                ei->exp = inferType(ei->exp, dsym->type);

            // If inside function, there is no semantic3() call
            if (sc->func || sc->intypeof == 1)
            {
                // If local variable, use AssignExp to handle all the various
                // possibilities.
                if (fd &&
                    !(dsym->storage_class & (STCmanifest | STCstatic | STCtls | STCgshared | STCextern)) &&
                    !dsym->_init->isVoidInitializer())
                {
                    //printf("fd = '%s', var = '%s'\n", fd->toChars(), dsym->toChars());
                    if (!ei)
                    {
                        ArrayInitializer *ai = dsym->_init->isArrayInitializer();
                        Expression *e;
                        if (ai && tb->ty == Taarray)
                            e = ai->toAssocArrayLiteral();
                        else
                            e = initializerToExpression(dsym->_init);
                        if (!e)
                        {
                            // Run semantic, but don't need to interpret
                            dsym->_init = initializerSemantic(dsym->_init, sc, dsym->type, INITnointerpret);
                            e = initializerToExpression(dsym->_init);
                            if (!e)
                            {
                                dsym->error("is not a static and cannot have static initializer");
                                e = new ErrorExp();
                            }
                        }
                        ei = new ExpInitializer(dsym->_init->loc, e);
                        dsym->_init = ei;
                    }

                    Expression *exp = ei->exp;
                    Expression *e1 = new VarExp(dsym->loc, dsym);
                    if (isBlit)
                        exp = new BlitExp(dsym->loc, e1, exp);
                    else
                        exp = new ConstructExp(dsym->loc, e1, exp);
                    dsym->canassign++;
                    exp = expressionSemantic(exp, sc);
                    dsym->canassign--;
                    exp = exp->optimize(WANTvalue);

                    if (exp->op == TOKerror)
                    {
                        dsym->_init = new ErrorInitializer();
                        ei = NULL;
                    }
                    else
                        ei->exp = exp;

                    if (ei && dsym->isScope())
                    {
                        Expression *ex = ei->exp;
                        while (ex->op == TOKcomma)
                            ex = ((CommaExp *)ex)->e2;
                        if (ex->op == TOKblit || ex->op == TOKconstruct)
                            ex = ((AssignExp *)ex)->e2;
                        if (ex->op == TOKnew)
                        {
                            // See if initializer is a NewExp that can be allocated on the stack
                            NewExp *ne = (NewExp *)ex;
                            if (dsym->type->toBasetype()->ty == Tclass)
                            {
                                if (ne->newargs && ne->newargs->length > 1)
                                {
                                    dsym->mynew = true;
                                }
                                else
                                {
                                    ne->onstack = true;
                                    dsym->onstack = true;
                                }
                            }
                        }
                        else if (ex->op == TOKfunction)
                        {
                            // or a delegate that doesn't escape a reference to the function
                            FuncDeclaration *f = ((FuncExp *)ex)->fd;
                            f->tookAddressOf--;
                        }
                    }
                }
                else
                {
                    // Bugzilla 14166: Don't run CTFE for the temporary variables inside typeof
                    dsym->_init = initializerSemantic(dsym->_init, sc, dsym->type, sc->intypeof == 1 ? INITnointerpret : INITinterpret);
                }
            }
            else if (parent->isAggregateDeclaration())
            {
                dsym->_scope = scx ? scx : sc->copy();
                dsym->_scope->setNoFree();
            }
            else if (dsym->storage_class & (STCconst | STCimmutable | STCmanifest) ||
                     dsym->type->isConst() || dsym->type->isImmutable())
            {
                /* Because we may need the results of a const declaration in a
                 * subsequent type, such as an array dimension, before semantic2()
                 * gets ordinarily run, try to run semantic2() now.
                 * Ignore failure.
                 */

                if (!inferred)
                {
                    unsigned errors = global.errors;
                    dsym->inuse++;
                    if (ei)
                    {
                        Expression *exp = ei->exp->syntaxCopy();

                        bool needctfe = dsym->isDataseg() || (dsym->storage_class & STCmanifest);
                        if (needctfe) sc = sc->startCTFE();
                        exp = expressionSemantic(exp, sc);
                        exp = resolveProperties(sc, exp);
                        if (needctfe) sc = sc->endCTFE();

                        Type *tb2 = dsym->type->toBasetype();
                        Type *ti = exp->type->toBasetype();

                        /* The problem is the following code:
                         *  struct CopyTest {
                         *     double x;
                         *     this(double a) { x = a * 10.0;}
                         *     this(this) { x += 2.0; }
                         *  }
                         *  const CopyTest z = CopyTest(5.3);  // ok
                         *  const CopyTest w = z;              // not ok, postblit not run
                         *  static assert(w.x == 55.0);
                         * because the postblit doesn't get run on the initialization of w.
                         */
                        if (ti->ty == Tstruct)
                        {
                            StructDeclaration *sd = ((TypeStruct *)ti)->sym;
                            /* Look to see if initializer involves a copy constructor
                             * (which implies a postblit)
                             */
                             // there is a copy constructor
                             // and exp is the same struct
                            if (sd->postblit &&
                                tb2->toDsymbol(NULL) == sd)
                            {
                                // The only allowable initializer is a (non-copy) constructor
                                if (exp->isLvalue())
                                    dsym->error("of type struct %s uses this(this), which is not allowed in static initialization", tb2->toChars());
                            }
                        }
                        ei->exp = exp;
                    }
                    dsym->_init = initializerSemantic(dsym->_init, sc, dsym->type, INITinterpret);
                    dsym->inuse--;
                    if (global.errors > errors)
                    {
                        dsym->_init = new ErrorInitializer();
                        dsym->type = Type::terror;
                    }
                }
                else
                {
                    dsym->_scope = scx ? scx : sc->copy();
                    dsym->_scope->setNoFree();
                }
            }
            sc = sc->pop();
        }

    Ldtor:
        /* Build code to execute destruction, if necessary
         */
        dsym->edtor = dsym->callScopeDtor(sc);
        if (dsym->edtor)
        {
            if (sc->func && dsym->storage_class & (STCstatic | STCgshared))
                dsym->edtor = expressionSemantic(dsym->edtor, sc->_module->_scope);
            else
                dsym->edtor = expressionSemantic(dsym->edtor, sc);

    #if 0 // currently disabled because of std.stdio.stdin, stdout and stderr
            if (dsym->isDataseg() && !(dsym->storage_class & STCextern))
                dsym->error("static storage variables cannot have destructors");
    #endif
        }

        dsym->semanticRun = PASSsemanticdone;

        if (dsym->type->toBasetype()->ty == Terror)
            dsym->errors = true;

        if (sc->scopesym && !sc->scopesym->isAggregateDeclaration())
        {
            for (ScopeDsymbol *sym = sc->scopesym; sym && dsym->endlinnum == 0;
                 sym = sym->parent ? sym->parent->isScopeDsymbol() : NULL)
                dsym->endlinnum = sym->endlinnum;
        }
    }

    void visit(TypeInfoDeclaration *dsym)
    {
        assert(dsym->linkage == LINKc);
    }

    void visit(Import *imp)
    {
        //printf("Import::semantic('%s') %s\n", toPrettyChars(), imp->id->toChars());
        if (imp->semanticRun > PASSinit)
            return;

        if (imp->_scope)
        {
            sc = imp->_scope;
            imp->_scope = NULL;
        }
        if (!sc)
            return;

        imp->semanticRun = PASSsemantic;

        // Load if not already done so
        if (!imp->mod)
        {
            imp->load(sc);
            if (imp->mod)
                imp->mod->importAll(NULL);
        }

        if (imp->mod)
        {
            // Modules need a list of each imported module
            //printf("%s imports %s\n", sc->_module->toChars(), imp->mod->toChars());
            sc->_module->aimports.push(imp->mod);

            if (sc->explicitProtection)
                imp->protection = sc->protection;

            if (!imp->aliasId && !imp->names.length) // neither a selective nor a renamed import
            {
                ScopeDsymbol *scopesym = NULL;
                if (sc->explicitProtection)
                    imp->protection = sc->protection.kind;
                for (Scope *scd = sc; scd; scd = scd->enclosing)
                {
                    if (!scd->scopesym)
                        continue;
                    scopesym = scd->scopesym;
                    break;
                }

                if (!imp->isstatic)
                {
                    scopesym->importScope(imp->mod, imp->protection);
                }

                imp->addPackageAccess(scopesym);
            }

            dsymbolSemantic(imp->mod, NULL);

            if (imp->mod->needmoduleinfo)
            {
                //printf("module4 %s because of %s\n", sc->_module->toChars(), imp->mod->toChars());
                sc->_module->needmoduleinfo = 1;
            }

            sc = sc->push(imp->mod);
            sc->protection = imp->protection;
            for (size_t i = 0; i < imp->aliasdecls.length; i++)
            {
                AliasDeclaration *ad = imp->aliasdecls[i];
                //printf("\tImport %s alias %s = %s, scope = %p\n", toPrettyChars(), imp->aliases[i]->toChars(), imp->names[i]->toChars(), ad->_scope);
                Dsymbol *sym = imp->mod->search(imp->loc, imp->names[i], IgnorePrivateImports);
                if (sym)
                {
                    if (!symbolIsVisible(sc, sym))
                        imp->mod->error(imp->loc, "member `%s` is not visible from module `%s`",
                            imp->names[i]->toChars(), sc->_module->toChars());
                    dsymbolSemantic(ad, sc);
                    // If the import declaration is in non-root module,
                    // analysis of the aliased symbol is deferred.
                    // Therefore, don't see the ad->aliassym or ad->type here.
                }
                else
                {
                    Dsymbol *s = imp->mod->search_correct(imp->names[i]);
                    if (s)
                        imp->mod->error(imp->loc, "import `%s` not found, did you mean %s `%s`?", imp->names[i]->toChars(), s->kind(), s->toPrettyChars());
                    else
                        imp->mod->error(imp->loc, "import `%s` not found", imp->names[i]->toChars());
                    ad->type = Type::terror;
                }
            }
            sc = sc->pop();
        }

        imp->semanticRun = PASSsemanticdone;

        // object self-imports itself, so skip that (Bugzilla 7547)
        // don't list pseudo modules __entrypoint.d, __main.d (Bugzilla 11117, 11164)
        if (global.params.moduleDeps != NULL &&
            !(imp->id == Id::object && sc->_module->ident == Id::object) &&
            sc->_module->ident != Id::entrypoint &&
            strcmp(sc->_module->ident->toChars(), "__main") != 0)
        {
            /* The grammar of the file is:
             *      ImportDeclaration
             *          ::= BasicImportDeclaration [ " : " ImportBindList ] [ " -> "
             *      ModuleAliasIdentifier ] "\n"
             *
             *      BasicImportDeclaration
             *          ::= ModuleFullyQualifiedName " (" FilePath ") : " Protection|"string"
             *              " [ " static" ] : " ModuleFullyQualifiedName " (" FilePath ")"
             *
             *      FilePath
             *          - any string with '(', ')' and '\' escaped with the '\' character
             */

            OutBuffer *ob = global.params.moduleDeps;
            Module* imod = sc->instantiatingModule();
            if (!global.params.moduleDepsFile.length)
                ob->writestring("depsImport ");
            ob->writestring(imod->toPrettyChars());
            ob->writestring(" (");
            escapePath(ob,  imod->srcfile->toChars());
            ob->writestring(") : ");

            // use protection instead of sc->protection because it couldn't be
            // resolved yet, see the comment above
            protectionToBuffer(ob, imp->protection);
            ob->writeByte(' ');
            if (imp->isstatic)
            {
                stcToBuffer(ob, STCstatic);
                ob->writeByte(' ');
            }
            ob->writestring(": ");

            if (imp->packages)
            {
                for (size_t i = 0; i < imp->packages->length; i++)
                {
                    Identifier *pid = (*imp->packages)[i];
                    ob->printf("%s.", pid->toChars());
                }
            }

            ob->writestring(imp->id->toChars());
            ob->writestring(" (");
            if (imp->mod)
                escapePath(ob, imp->mod->srcfile->toChars());
            else
                ob->writestring("???");
            ob->writeByte(')');

            for (size_t i = 0; i < imp->names.length; i++)
            {
                if (i == 0)
                    ob->writeByte(':');
                else
                    ob->writeByte(',');

                Identifier *name = imp->names[i];
                Identifier *alias = imp->aliases[i];

                if (!alias)
                {
                    ob->printf("%s", name->toChars());
                    alias = name;
                }
                else
                    ob->printf("%s=%s", alias->toChars(), name->toChars());
            }

            if (imp->aliasId)
                    ob->printf(" -> %s", imp->aliasId->toChars());

            ob->writenl();
        }

        //printf("-Import::semantic('%s'), pkg = %p\n", imp->toChars(), imp->pkg);
    }

    void attribSemantic(AttribDeclaration *ad)
    {
        if (ad->semanticRun != PASSinit)
            return;
        ad->semanticRun = PASSsemantic;
        Dsymbols *d = ad->include(sc);
        //printf("\tAttribDeclaration::semantic '%s', d = %p\n",toChars(), d);
        if (d)
        {
            Scope *sc2 = ad->newScope(sc);
            bool errors = false;
            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                dsymbolSemantic(s, sc2);
                errors |= s->errors;
            }
            ad->errors |= errors;
            if (sc2 != sc)
                sc2->pop();
        }
        ad->semanticRun = PASSsemanticdone;
    }

    void visit(AttribDeclaration *atd)
    {
        attribSemantic(atd);
    }

    void visit(AnonDeclaration *scd)
    {
        //printf("\tAnonDeclaration::semantic %s %p\n", isunion ? "union" : "struct", scd);
        assert(sc->parent);
        Dsymbol *p = sc->parent->pastMixin();
        AggregateDeclaration *ad = p->isAggregateDeclaration();
        if (!ad)
        {
            error(scd->loc, "%s can only be a part of an aggregate, not %s %s",
                scd->kind(), p->kind(), p->toChars());
            scd->errors = true;
            return;
        }

        if (scd->decl)
        {
            sc = sc->push();
            sc->stc &= ~(STCauto | STCscope | STCstatic | STCtls | STCgshared);
            sc->inunion = scd->isunion;
            sc->flags = 0;

            for (size_t i = 0; i < scd->decl->length; i++)
            {
                Dsymbol *s = (*scd->decl)[i];
                dsymbolSemantic(s, sc);
            }
            sc = sc->pop();
        }
    }

    void visit(PragmaDeclaration *pd)
    {
        // Should be merged with PragmaStatement
        //printf("\tPragmaDeclaration::semantic '%s'\n",toChars());
        if (pd->ident == Id::msg)
        {
            if (pd->args)
            {
                for (size_t i = 0; i < pd->args->length; i++)
                {
                    Expression *e = (*pd->args)[i];

                    sc = sc->startCTFE();
                    e = expressionSemantic(e, sc);
                    e = resolveProperties(sc, e);
                    sc = sc->endCTFE();
                    e = ctfeInterpretForPragmaMsg(e);
                    if (e->op == TOKerror)
                    {
                        errorSupplemental(pd->loc, "while evaluating pragma(msg, %s)", (*pd->args)[i]->toChars());
                        return;
                    }
                    StringExp *se = e->toStringExp();
                    if (se)
                    {
                        se = se->toUTF8(sc);
                        fprintf(stderr, "%.*s", (int)se->len, (char *)se->string);
                    }
                    else
                        fprintf(stderr, "%s", e->toChars());
                }
                fprintf(stderr, "\n");
            }
            goto Lnodecl;
        }
        else if (pd->ident == Id::lib)
        {
            if (!pd->args || pd->args->length != 1)
                pd->error("string expected for library name");
            else
            {
                StringExp *se = semanticString(sc, (*pd->args)[0], "library name");
                if (!se)
                    goto Lnodecl;
                (*pd->args)[0] = se;

                char *name = (char *)mem.xmalloc(se->len + 1);
                memcpy(name, se->string, se->len);
                name[se->len] = 0;
                if (global.params.verbose)
                    message("library   %s", name);
                if (global.params.moduleDeps && !global.params.moduleDepsFile.length)
                {
                    OutBuffer *ob = global.params.moduleDeps;
                    Module *imod = sc->instantiatingModule();
                    ob->writestring("depsLib ");
                    ob->writestring(imod->toPrettyChars());
                    ob->writestring(" (");
                    escapePath(ob, imod->srcfile->toChars());
                    ob->writestring(") : ");
                    ob->writestring((char *) name);
                    ob->writenl();
                }
                mem.xfree(name);
            }
            goto Lnodecl;
        }
        else if (pd->ident == Id::startaddress)
        {
            if (!pd->args || pd->args->length != 1)
                pd->error("function name expected for start address");
            else
            {
                /* Bugzilla 11980:
                 * resolveProperties and ctfeInterpret call are not necessary.
                 */
                Expression *e = (*pd->args)[0];

                sc = sc->startCTFE();
                e = expressionSemantic(e, sc);
                sc = sc->endCTFE();

                (*pd->args)[0] = e;
                Dsymbol *sa = getDsymbol(e);
                if (!sa || !sa->isFuncDeclaration())
                    pd->error("function name expected for start address, not `%s`", e->toChars());
            }
            goto Lnodecl;
        }
        else if (pd->ident == Id::Pinline)
        {
            goto Ldecl;
        }
        else if (pd->ident == Id::mangle)
        {
            if (!pd->args)
                pd->args = new Expressions();
            if (pd->args->length != 1)
            {
                pd->error("string expected for mangled name");
                pd->args->setDim(1);
                (*pd->args)[0] = new ErrorExp();    // error recovery
                goto Ldecl;
            }

            StringExp *se = semanticString(sc, (*pd->args)[0], "mangled name");
            if (!se)
                goto Ldecl;
            (*pd->args)[0] = se; // Will be used for later

            if (!se->len)
            {
                pd->error("zero-length string not allowed for mangled name");
                goto Ldecl;
            }
            if (se->sz != 1)
            {
                pd->error("mangled name characters can only be of type char");
                goto Ldecl;
            }

            /* Note: D language specification should not have any assumption about backend
             * implementation. Ideally pragma(mangle) can accept a string of any content.
             *
             * Therefore, this validation is compiler implementation specific.
             */
            for (size_t i = 0; i < se->len; )
            {
                utf8_t *p = (utf8_t *)se->string;
                dchar_t c = p[i];
                if (c < 0x80)
                {
                    if ((c >= 'A' && c <= 'Z') ||
                        (c >= 'a' && c <= 'z') ||
                        (c >= '0' && c <= '9') ||
                        (c != 0 && strchr("$%().:?@[]_", c)))
                    {
                        ++i;
                        continue;
                    }
                    else
                    {
                        pd->error("char 0x%02x not allowed in mangled name", c);
                        break;
                    }
                }

                if (const char* msg = utf_decodeChar((utf8_t *)se->string, se->len, &i, &c))
                {
                    pd->error("%s", msg);
                    break;
                }

                if (!isUniAlpha(c))
                {
                    pd->error("char 0x%04x not allowed in mangled name", c);
                    break;
                }
            }
        }
        else if (pd->ident == Id::printf || pd->ident == Id::scanf)
        {
            if (pd->args && pd->args->length != 0)
                pd->error("takes no argument");
            goto Ldecl;
        }
        else if (global.params.ignoreUnsupportedPragmas)
        {
            if (global.params.verbose)
            {
                /* Print unrecognized pragmas
                 */
                OutBuffer buf;
                buf.writestring(pd->ident->toChars());
                if (pd->args)
                {
                    for (size_t i = 0; i < pd->args->length; i++)
                    {
                        Expression *e = (*pd->args)[i];

                        sc = sc->startCTFE();
                        e = expressionSemantic(e, sc);
                        e = resolveProperties(sc, e);
                        sc = sc->endCTFE();

                        e = e->ctfeInterpret();
                        if (i == 0)
                            buf.writestring(" (");
                        else
                            buf.writeByte(',');
                        buf.writestring(e->toChars());
                    }
                    if (pd->args->length)
                        buf.writeByte(')');
                }
                message("pragma    %s", buf.peekChars());
            }
            goto Lnodecl;
        }
        else
            error(pd->loc, "unrecognized pragma(%s)", pd->ident->toChars());

    Ldecl:
        if (pd->decl)
        {
            Scope *sc2 = pd->newScope(sc);

            for (size_t i = 0; i < pd->decl->length; i++)
            {
                Dsymbol *s = (*pd->decl)[i];

                dsymbolSemantic(s, sc2);

                if (pd->ident == Id::mangle)
                {
                    assert(pd->args && pd->args->length == 1);
                    if (StringExp *se = (*pd->args)[0]->toStringExp())
                    {
                        char *name = (char *)mem.xmalloc(se->len + 1);
                        memcpy(name, se->string, se->len);
                        name[se->len] = 0;

                        unsigned cnt = setMangleOverride(s, name);
                        if (cnt > 1)
                            pd->error("can only apply to a single declaration");
                    }
                }
            }

            if (sc2 != sc)
                sc2->pop();
        }
        return;

    Lnodecl:
        if (pd->decl)
        {
            pd->error("pragma is missing closing `;`");
            goto Ldecl; // do them anyway, to avoid segfaults.
        }
    }

    void visit(StaticIfDeclaration *sid)
    {
        attribSemantic(sid);
    }

    void visit(StaticForeachDeclaration *sfd)
    {
        attribSemantic(sfd);
    }

    Dsymbols *compileIt(CompileDeclaration *cd)
    {
        //printf("CompileDeclaration::compileIt(loc = %d) %s\n", cd->loc.linnum, cd->exp->toChars());
        OutBuffer buf;
        if (expressionsToString(buf, sc, cd->exps))
            return NULL;

        unsigned errors = global.errors;
        const size_t len = buf.length();
        const char *str = buf.extractChars();
        Parser p(cd->loc, sc->_module, (const utf8_t *)str, len, false);
        p.nextToken();

        Dsymbols *d = p.parseDeclDefs(0);
        if (global.errors != errors)
            return NULL;

        if (p.token.value != TOKeof)
        {
            cd->error("incomplete mixin declaration (%s)", str);
            return NULL;
        }
        return d;
    }

    void visit(CompileDeclaration *cd)
    {
        //printf("CompileDeclaration::semantic()\n");
        if (!cd->compiled)
        {
            cd->decl = compileIt(cd);
            cd->AttribDeclaration::addMember(sc, cd->scopesym);
            cd->compiled = true;

            if (cd->_scope && cd->decl)
            {
                for (size_t i = 0; i < cd->decl->length; i++)
                {
                    Dsymbol *s = (*cd->decl)[i];
                    s->setScope(cd->_scope);
                }
            }
        }
        attribSemantic(cd);
    }

    void visit(UserAttributeDeclaration *uad)
    {
        //printf("UserAttributeDeclaration::semantic() %p\n", this);
        if (uad->decl && !uad->_scope)
            uad->Dsymbol::setScope(sc);  // for function local symbols

        attribSemantic(uad);
    }

    void visit(StaticAssert *sa)
    {
            if (sa->semanticRun < PASSsemanticdone)
                sa->semanticRun = PASSsemanticdone;
    }

    void visit(DebugSymbol *ds)
    {
        //printf("DebugSymbol::semantic() %s\n", ds->toChars());
        if (ds->semanticRun < PASSsemanticdone)
            ds->semanticRun = PASSsemanticdone;
    }

    void visit(VersionSymbol *vs)
    {
        if (vs->semanticRun < PASSsemanticdone)
            vs->semanticRun = PASSsemanticdone;
    }

    void visit(Package *pkg)
    {
        if (pkg->semanticRun < PASSsemanticdone)
            pkg->semanticRun = PASSsemanticdone;
    }

    void visit(Module *m)
    {
        if (m->semanticRun != PASSinit)
            return;

        //printf("+Module::semantic(this = %p, '%s'): parent = %p\n", this, m->toChars(), parent);
        m->semanticRun = PASSsemantic;

        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope *sc = m->_scope;                  // see if already got one from importAll()
        if (!sc)
        {
            sc = Scope::createGlobal(m);      // create root scope
        }

        //printf("Module = %p, linkage = %d\n", sc->scopesym, sc->linkage);

        // Pass 1 semantic routines: do public side of the definition
        for (size_t i = 0; i < m->members->length; i++)
        {
            Dsymbol *s = (*m->members)[i];

            //printf("\tModule('%s'): '%s'.semantic()\n", m->toChars(), s->toChars());
            dsymbolSemantic(s, sc);
            m->runDeferredSemantic();
        }

        if (m->userAttribDecl)
        {
            dsymbolSemantic(m->userAttribDecl, sc);
        }

        if (!m->_scope)
        {
            sc = sc->pop();
            sc->pop();              // 2 pops because Scope::createGlobal() created 2
        }
        m->semanticRun = PASSsemanticdone;
        //printf("-Module::semantic(this = %p, '%s'): parent = %p\n", m, m->toChars(), parent);
    }

    void visit(EnumDeclaration *ed)
    {
        //printf("EnumDeclaration::semantic(sd = %p, '%s') %s\n", sc->scopesym, sc->scopesym->toChars(), ed->toChars());
        //printf("EnumDeclaration::semantic() %p %s\n", ed, ed->toChars());
        if (ed->semanticRun >= PASSsemanticdone)
            return;             // semantic() already completed
        if (ed->semanticRun == PASSsemantic)
        {
            assert(ed->memtype);
            error(ed->loc, "circular reference to enum base type %s", ed->memtype->toChars());
            ed->errors = true;
            ed->semanticRun = PASSsemanticdone;
            return;
        }
        unsigned dprogress_save = Module::dprogress;

        Scope *scx = NULL;
        if (ed->_scope)
        {
            sc = ed->_scope;
            scx = ed->_scope;            // save so we don't make redundant copies
            ed->_scope = NULL;
        }

        if (!sc)
            return;

        ed->parent = sc->parent;
        ed->type = typeSemantic(ed->type, ed->loc, sc);

        ed->protection = sc->protection;
        if (sc->stc & STCdeprecated)
            ed->isdeprecated = true;
        ed->userAttribDecl = sc->userAttribDecl;

        ed->semanticRun = PASSsemantic;

        if (!ed->members && !ed->memtype)               // enum ident;
        {
            ed->semanticRun = PASSsemanticdone;
            return;
        }

        if (!ed->symtab)
            ed->symtab = new DsymbolTable();

        /* The separate, and distinct, cases are:
         *  1. enum { ... }
         *  2. enum : memtype { ... }
         *  3. enum ident { ... }
         *  4. enum ident : memtype { ... }
         *  5. enum ident : memtype;
         *  6. enum ident;
         */

        if (ed->memtype)
        {
            ed->memtype = typeSemantic(ed->memtype, ed->loc, sc);

            /* Check to see if memtype is forward referenced
             */
            if (ed->memtype->ty == Tenum)
            {
                EnumDeclaration *sym = (EnumDeclaration *)ed->memtype->toDsymbol(sc);
                if (!sym->memtype || !sym->members || !sym->symtab || sym->_scope)
                {
                    // memtype is forward referenced, so try again later
                    ed->_scope = scx ? scx : sc->copy();
                    ed->_scope->setNoFree();
                    Module::addDeferredSemantic(ed);
                    Module::dprogress = dprogress_save;
                    //printf("\tdeferring %s\n", ed->toChars());
                    ed->semanticRun = PASSinit;
                    return;
                }
                else
                    // Ensure that semantic is run to detect. e.g. invalid forward references
                    dsymbolSemantic(sym, sc);
            }
            if (ed->memtype->ty == Tvoid)
            {
                ed->error("base type must not be void");
                ed->memtype = Type::terror;
            }
            if (ed->memtype->ty == Terror)
            {
                ed->errors = true;
                if (ed->members)
                {
                    for (size_t i = 0; i < ed->members->length; i++)
                    {
                        Dsymbol *s = (*ed->members)[i];
                        s->errors = true;               // poison all the members
                    }
                }
                ed->semanticRun = PASSsemanticdone;
                return;
            }
        }

        ed->semanticRun = PASSsemanticdone;

        if (!ed->members)               // enum ident : memtype;
            return;

        if (ed->members->length == 0)
        {
            ed->error("enum %s must have at least one member", ed->toChars());
            ed->errors = true;
            return;
        }

        Module::dprogress++;

        Scope *sce;
        if (ed->isAnonymous())
            sce = sc;
        else
        {
            sce = sc->push(ed);
            sce->parent = ed;
        }
        sce = sce->startCTFE();
        sce->setNoFree();                   // needed for getMaxMinValue()

        /* Each enum member gets the sce scope
         */
        for (size_t i = 0; i < ed->members->length; i++)
        {
            EnumMember *em = (*ed->members)[i]->isEnumMember();
            if (em)
                em->_scope = sce;
        }

        if (!ed->added)
        {
            /* addMember() is not called when the EnumDeclaration appears as a function statement,
             * so we have to do what addMember() does and install the enum members in the right symbol
             * table
             */
            ScopeDsymbol *scopesym = NULL;
            if (ed->isAnonymous())
            {
                /* Anonymous enum members get added to enclosing scope.
                 */
                for (Scope *sct = sce; 1; sct = sct->enclosing)
                {
                    assert(sct);
                    if (sct->scopesym)
                    {
                        scopesym = sct->scopesym;
                        if (!sct->scopesym->symtab)
                            sct->scopesym->symtab = new DsymbolTable();
                        break;
                    }
                }
            }
            else
            {
                // Otherwise enum members are in the EnumDeclaration's symbol table
                scopesym = ed;
            }

            for (size_t i = 0; i < ed->members->length; i++)
            {
                EnumMember *em = (*ed->members)[i]->isEnumMember();
                if (em)
                {
                    em->ed = ed;
                    em->addMember(sc, scopesym);
                }
            }
        }

        for (size_t i = 0; i < ed->members->length; i++)
        {
            EnumMember *em = (*ed->members)[i]->isEnumMember();
            if (em)
                dsymbolSemantic(em, em->_scope);
        }
        //printf("defaultval = %lld\n", defaultval);

        //if (defaultval) printf("defaultval: %s %s\n", defaultval->toChars(), defaultval->type->toChars());
        //printf("members = %s\n", ed->members->toChars());
    }

    void visit(EnumMember *em)
    {
        //printf("EnumMember::semantic() %s\n", em->toChars());
        if (em->errors || em->semanticRun >= PASSsemanticdone)
            return;
        if (em->semanticRun == PASSsemantic)
        {
            em->error("circular reference to enum member");
        Lerrors:
            em->errors = true;
            em->semanticRun = PASSsemanticdone;
            return;
        }
        assert(em->ed);

        dsymbolSemantic(em->ed, sc);
        if (em->ed->errors)
            goto Lerrors;

        if (em->errors || em->semanticRun >= PASSsemanticdone)
            return;

        if (em->_scope)
            sc = em->_scope;
        if (!sc)
            return;

        em->semanticRun = PASSsemantic;

        em->protection = em->ed->isAnonymous() ? em->ed->protection : Prot(Prot::public_);
        em->linkage = LINKd;
        em->storage_class |= STCmanifest;

        // https://issues.dlang.org/show_bug.cgi?id=9701
        if (em->ed->isAnonymous())
        {
            if (em->userAttribDecl)
                em->userAttribDecl->userAttribDecl = em->ed->userAttribDecl;
            else
                em->userAttribDecl = em->ed->userAttribDecl;
        }

        // The first enum member is special
        bool first = (em == (*em->ed->members)[0]);

        if (em->origType)
        {
            em->origType = typeSemantic(em->origType, em->loc, sc);
            em->type = em->origType;
            assert(em->value());          // "type id;" is not a valid enum member declaration
        }

        if (em->value())
        {
            Expression *e = em->value();
            assert(e->dyncast() == DYNCAST_EXPRESSION);
            e = expressionSemantic(e, sc);
            e = resolveProperties(sc, e);
            e = e->ctfeInterpret();
            if (e->op == TOKerror)
                goto Lerrors;
            if (first && !em->ed->memtype && !em->ed->isAnonymous())
            {
                em->ed->memtype = e->type;
                if (em->ed->memtype->ty == Terror)
                {
                    em->ed->errors = true;
                    goto Lerrors;
                }
                if (em->ed->memtype->ty != Terror)
                {
                    /* Bugzilla 11746: All of named enum members should have same type
                     * with the first member. If the following members were referenced
                     * during the first member semantic, their types should be unified.
                     */
                    for (size_t i = 0; i < em->ed->members->length; i++)
                    {
                        EnumMember *enm = (*em->ed->members)[i]->isEnumMember();
                        if (!enm || enm == em || enm->semanticRun < PASSsemanticdone || enm->origType)
                            continue;

                        //printf("[%d] enm = %s, enm->semanticRun = %d\n", i, enm->toChars(), enm->semanticRun);
                        Expression *ev = enm->value();
                        ev = ev->implicitCastTo(sc, em->ed->memtype);
                        ev = ev->ctfeInterpret();
                        ev = ev->castTo(sc, em->ed->type);
                        if (ev->op == TOKerror)
                            em->ed->errors = true;
                        enm->value() = ev;
                    }
                    if (em->ed->errors)
                    {
                        em->ed->memtype = Type::terror;
                        goto Lerrors;
                    }
                }
            }

            if (em->ed->memtype && !em->origType)
            {
                e = e->implicitCastTo(sc, em->ed->memtype);
                e = e->ctfeInterpret();

                // save origValue for better json output
                em->origValue = e;

                if (!em->ed->isAnonymous())
                {
                    e = e->castTo(sc, em->ed->type);
                    e = e->ctfeInterpret();
                }
            }
            else if (em->origType)
            {
                e = e->implicitCastTo(sc, em->origType);
                e = e->ctfeInterpret();
                assert(em->ed->isAnonymous());

                // save origValue for better json output
                em->origValue = e;
            }
            em->value() = e;
        }
        else if (first)
        {
            Type *t;
            if (em->ed->memtype)
                t = em->ed->memtype;
            else
            {
                t = Type::tint32;
                if (!em->ed->isAnonymous())
                    em->ed->memtype = t;
            }
            Expression *e = new IntegerExp(em->loc, 0, Type::tint32);
            e = e->implicitCastTo(sc, t);
            e = e->ctfeInterpret();

            // save origValue for better json output
            em->origValue = e;

            if (!em->ed->isAnonymous())
            {
                e = e->castTo(sc, em->ed->type);
                e = e->ctfeInterpret();
            }
            em->value() = e;
        }
        else
        {
            /* Find the previous enum member,
             * and set this to be the previous value + 1
             */
            EnumMember *emprev = NULL;
            for (size_t i = 0; i < em->ed->members->length; i++)
            {
                EnumMember *enm = (*em->ed->members)[i]->isEnumMember();
                if (enm)
                {
                    if (enm == em)
                        break;
                    emprev = enm;
                }
            }
            assert(emprev);
            if (emprev->semanticRun < PASSsemanticdone)    // if forward reference
                dsymbolSemantic(emprev, emprev->_scope);    // resolve it
            if (emprev->errors)
                goto Lerrors;

            Expression *eprev = emprev->value();
            Type *tprev = eprev->type->equals(em->ed->type) ? em->ed->memtype : eprev->type;

            Expression *emax = tprev->getProperty(em->ed->loc, Id::max, 0);
            emax = expressionSemantic(emax, sc);
            emax = emax->ctfeInterpret();

            // Set value to (eprev + 1).
            // But first check that (eprev != emax)
            assert(eprev);
            Expression *e = new EqualExp(TOKequal, em->loc, eprev, emax);
            e = expressionSemantic(e, sc);
            e = e->ctfeInterpret();
            if (e->toInteger())
            {
                em->error("initialization with (%s.%s + 1) causes overflow for type `%s`", emprev->ed->toChars(), emprev->toChars(), em->ed->type->toBasetype()->toChars());
                goto Lerrors;
            }

            // Now set e to (eprev + 1)
            e = new AddExp(em->loc, eprev, new IntegerExp(em->loc, 1, Type::tint32));
            e = expressionSemantic(e, sc);
            e = e->castTo(sc, eprev->type);
            e = e->ctfeInterpret();

            // save origValue (without cast) for better json output
            if (e->op != TOKerror)  // avoid duplicate diagnostics
            {
                assert(emprev->origValue);
                em->origValue = new AddExp(em->loc, emprev->origValue, new IntegerExp(em->loc, 1, Type::tint32));
                em->origValue = expressionSemantic(em->origValue, sc);
                em->origValue = em->origValue->ctfeInterpret();
            }

            if (e->op == TOKerror)
                goto Lerrors;
            if (e->type->isfloating())
            {
                // Check that e != eprev (not always true for floats)
                Expression *etest = new EqualExp(TOKequal, em->loc, e, eprev);
                etest = expressionSemantic(etest, sc);
                etest = etest->ctfeInterpret();
                if (etest->toInteger())
                {
                    em->error("has inexact value, due to loss of precision");
                    goto Lerrors;
                }
            }
            em->value() = e;
        }
        if (!em->origType)
            em->type = em->value()->type;

        assert(em->origValue);
        em->semanticRun = PASSsemanticdone;
    }

    void visit(TemplateDeclaration *tempdecl)
    {
        if (tempdecl->semanticRun != PASSinit)
            return;         // semantic() already run

        // Remember templates defined in module object that we need to know about
        if (sc->_module && sc->_module->ident == Id::object)
        {
            if (tempdecl->ident == Id::RTInfo)
                Type::rtinfo = tempdecl;
        }

        /* Remember Scope for later instantiations, but make
         * a copy since attributes can change.
         */
        if (!tempdecl->_scope)
        {
            tempdecl->_scope = sc->copy();
            tempdecl->_scope->setNoFree();
        }

        tempdecl->semanticRun = PASSsemantic;

        tempdecl->parent = sc->parent;
        tempdecl->protection = sc->protection;
        tempdecl->isstatic = tempdecl->toParent()->isModule() || (tempdecl->_scope->stc & STCstatic);

        if (!tempdecl->isstatic)
        {
            if (AggregateDeclaration *ad = tempdecl->parent->pastMixin()->isAggregateDeclaration())
                ad->makeNested();
        }

        // Set up scope for parameters
        ScopeDsymbol *paramsym = new ScopeDsymbol();
        paramsym->parent = tempdecl->parent;
        Scope *paramscope = sc->push(paramsym);
        paramscope->stc = 0;

        if (global.params.doDocComments)
        {
            tempdecl->origParameters = new TemplateParameters();
            tempdecl->origParameters->setDim(tempdecl->parameters->length);
            for (size_t i = 0; i < tempdecl->parameters->length; i++)
            {
                TemplateParameter *tp = (*tempdecl->parameters)[i];
                (*tempdecl->origParameters)[i] = tp->syntaxCopy();
            }
        }

        for (size_t i = 0; i < tempdecl->parameters->length; i++)
        {
            TemplateParameter *tp = (*tempdecl->parameters)[i];

            if (!tp->declareParameter(paramscope))
            {
                error(tp->loc, "parameter `%s` multiply defined", tp->ident->toChars());
                tempdecl->errors = true;
            }
            if (!tpsemantic(tp, paramscope, tempdecl->parameters))
            {
                tempdecl->errors = true;
            }
            if (i + 1 != tempdecl->parameters->length && tp->isTemplateTupleParameter())
            {
                tempdecl->error("template tuple parameter must be last one");
                tempdecl->errors = true;
            }
        }

        /* Calculate TemplateParameter::dependent
         */
        TemplateParameters tparams;
        tparams.setDim(1);
        for (size_t i = 0; i < tempdecl->parameters->length; i++)
        {
            TemplateParameter *tp = (*tempdecl->parameters)[i];
            tparams[0] = tp;

            for (size_t j = 0; j < tempdecl->parameters->length; j++)
            {
                // Skip cases like: X(T : T)
                if (i == j)
                    continue;

                if (TemplateTypeParameter *ttp = (*tempdecl->parameters)[j]->isTemplateTypeParameter())
                {
                    if (reliesOnTident(ttp->specType, &tparams))
                        tp->dependent = true;
                }
                else if (TemplateAliasParameter *tap = (*tempdecl->parameters)[j]->isTemplateAliasParameter())
                {
                    if (reliesOnTident(tap->specType, &tparams) ||
                        reliesOnTident(isType(tap->specAlias), &tparams))
                    {
                        tp->dependent = true;
                    }
                }
            }
        }

        paramscope->pop();

        // Compute again
        tempdecl->onemember = NULL;
        if (tempdecl->members)
        {
            Dsymbol *s;
            if (Dsymbol::oneMembers(tempdecl->members, &s, tempdecl->ident) && s)
            {
                tempdecl->onemember = s;
                s->parent = tempdecl;
            }
        }

        /* BUG: should check:
         *  o no virtual functions or non-static data members of classes
         */
        tempdecl->semanticRun = PASSsemanticdone;
    }

    void visit(TemplateInstance *ti)
    {
        templateInstanceSemantic(ti, sc, NULL);
    }

    void visit(TemplateMixin *tm)
    {
        if (tm->semanticRun != PASSinit)
        {
            // When a class/struct contains mixin members, and is done over
            // because of forward references, never reach here so semanticRun
            // has been reset to PASSinit.
            return;
        }
        tm->semanticRun = PASSsemantic;

        Scope *scx = NULL;
        if (tm->_scope)
        {
            sc = tm->_scope;
            scx = tm->_scope;            // save so we don't make redundant copies
            tm->_scope = NULL;
        }

        /* Run semantic on each argument, place results in tiargs[],
         * then find best match template with tiargs
         */
        if (!tm->findTempDecl(sc) ||
            !tm->semanticTiargs(sc) ||
            !tm->findBestMatch(sc, NULL))
        {
            if (tm->semanticRun == PASSinit)    // forward reference had occured
            {
                //printf("forward reference - deferring\n");
                tm->_scope = scx ? scx : sc->copy();
                tm->_scope->setNoFree();
                Module::addDeferredSemantic(tm);
                return;
            }

            tm->inst = tm;
            tm->errors = true;
            return;         // error recovery
        }
        TemplateDeclaration *tempdecl = tm->tempdecl->isTemplateDeclaration();
        assert(tempdecl);

        if (!tm->ident)
        {
            /* Assign scope local unique identifier, as same as lambdas.
             */
            const char *s = "__mixin";

            if (FuncDeclaration *func = sc->parent->isFuncDeclaration())
            {
                tm->symtab = func->localsymtab;
                if (tm->symtab)
                {
                    // Inside template constraint, symtab is not set yet.
                    goto L1;
                }
            }
            else
            {
                tm->symtab = sc->parent->isScopeDsymbol()->symtab;
            L1:
                assert(tm->symtab);
                int num = (int)dmd_aaLen(tm->symtab->tab) + 1;
                tm->ident = Identifier::generateId(s, num);
                tm->symtab->insert(tm);
            }
        }

        tm->inst = tm;
        tm->parent = sc->parent;

        /* Detect recursive mixin instantiations.
         */
        for (Dsymbol *s = tm->parent; s; s = s->parent)
        {
            //printf("\ts = '%s'\n", s->toChars());
            TemplateMixin *tmix = s->isTemplateMixin();
            if (!tmix || tempdecl != tmix->tempdecl)
                continue;

            /* Different argument list lengths happen with variadic args
             */
            if (tm->tiargs->length != tmix->tiargs->length)
                continue;

            for (size_t i = 0; i < tm->tiargs->length; i++)
            {
                RootObject *o = (*tm->tiargs)[i];
                Type *ta = isType(o);
                Expression *ea = isExpression(o);
                Dsymbol *sa = isDsymbol(o);
                RootObject *tmo = (*tmix->tiargs)[i];
                if (ta)
                {
                    Type *tmta = isType(tmo);
                    if (!tmta)
                        goto Lcontinue;
                    if (!ta->equals(tmta))
                        goto Lcontinue;
                }
                else if (ea)
                {
                    Expression *tme = isExpression(tmo);
                    if (!tme || !ea->equals(tme))
                        goto Lcontinue;
                }
                else if (sa)
                {
                    Dsymbol *tmsa = isDsymbol(tmo);
                    if (sa != tmsa)
                        goto Lcontinue;
                }
                else
                    assert(0);
            }
            tm->error("recursive mixin instantiation");
            return;

        Lcontinue:
            continue;
        }

        // Copy the syntax trees from the TemplateDeclaration
        tm->members = Dsymbol::arraySyntaxCopy(tempdecl->members);
        if (!tm->members)
            return;

        tm->symtab = new DsymbolTable();

        for (Scope *sce = sc; 1; sce = sce->enclosing)
        {
            ScopeDsymbol *sds = (ScopeDsymbol *)sce->scopesym;
            if (sds)
            {
                sds->importScope(tm, Prot(Prot::public_));
                break;
            }
        }

        Scope *scy = sc->push(tm);
        scy->parent = tm;

        tm->argsym = new ScopeDsymbol();
        tm->argsym->parent = scy->parent;
        Scope *argscope = scy->push(tm->argsym);

        unsigned errorsave = global.errors;

        // Declare each template parameter as an alias for the argument type
        tm->declareParameters(argscope);

        // Add members to enclosing scope, as well as this scope
        for (size_t i = 0; i < tm->members->length; i++)
        {
            Dsymbol *s = (*tm->members)[i];
            s->addMember(argscope, tm);
            //printf("sc->parent = %p, sc->scopesym = %p\n", sc->parent, sc->scopesym);
            //printf("s->parent = %s\n", s->parent->toChars());
        }

        // Do semantic() analysis on template instance members
        Scope *sc2 = argscope->push(tm);
        //size_t deferred_dim = Module::deferred.length;

        static int nest;
        //printf("%d\n", nest);
        if (++nest > global.recursionLimit)
        {
            global.gag = 0;                 // ensure error message gets printed
            tm->error("recursive expansion");
            fatal();
        }

        for (size_t i = 0; i < tm->members->length; i++)
        {
            Dsymbol *s = (*tm->members)[i];
            s->setScope(sc2);
        }

        for (size_t i = 0; i < tm->members->length; i++)
        {
            Dsymbol *s = (*tm->members)[i];
            s->importAll(sc2);
        }

        for (size_t i = 0; i < tm->members->length; i++)
        {
            Dsymbol *s = (*tm->members)[i];
            dsymbolSemantic(s, sc2);
        }

        nest--;

        /* In DeclDefs scope, TemplateMixin does not have to handle deferred symbols.
         * Because the members would already call Module::addDeferredSemantic() for themselves.
         * See Struct, Class, Interface, and EnumDeclaration::semantic().
         */
        //if (!sc->func && Module::deferred.length > deferred_dim) {}

        AggregateDeclaration *ad = tm->toParent()->isAggregateDeclaration();
        if (sc->func && !ad)
        {
            semantic2(tm, sc2);
            semantic3(tm, sc2);
        }

        // Give additional context info if error occurred during instantiation
        if (global.errors != errorsave)
        {
            tm->error("error instantiating");
            tm->errors = true;
        }

        sc2->pop();
        argscope->pop();
        scy->pop();
    }

    void visit(Nspace *ns)
    {
        if (ns->semanticRun != PASSinit)
            return;
        if (ns->_scope)
        {
            sc = ns->_scope;
            ns->_scope = NULL;
        }
        if (!sc)
            return;

        ns->semanticRun = PASSsemantic;
        ns->parent = sc->parent;
        if (ns->members)
        {
            assert(sc);
            sc = sc->push(ns);
            sc->linkage = LINKcpp;          // note that namespaces imply C++ linkage
            sc->parent = ns;

            for (size_t i = 0; i < ns->members->length; i++)
            {
                Dsymbol *s = (*ns->members)[i];
                s->importAll(sc);
            }

            for (size_t i = 0; i < ns->members->length; i++)
            {
                Dsymbol *s = (*ns->members)[i];
                dsymbolSemantic(s, sc);
            }
            sc->pop();
        }
        ns->semanticRun = PASSsemanticdone;
    }


private:
    static bool isPointerToChar(Parameter *p)
    {
        if (TypePointer *tptr = p->type->isTypePointer())
        {
            return tptr->next->ty == Tchar;
        }
        return false;
    }

    static bool isVa_list(Parameter *p, FuncDeclaration *funcdecl, Scope *sc)
    {
        return p->type->equals(target.va_listType(funcdecl->loc, sc));
    }

public:
    void funcDeclarationSemantic(FuncDeclaration *funcdecl)
    {
        TypeFunction *f;
        AggregateDeclaration *ad;
        InterfaceDeclaration *id;

        if (funcdecl->semanticRun != PASSinit && funcdecl->isFuncLiteralDeclaration())
        {
            /* Member functions that have return types that are
             * forward references can have semantic() run more than
             * once on them.
             * See test\interface2.d, test20
             */
            return;
        }

        if (funcdecl->semanticRun >= PASSsemanticdone)
            return;
        assert(funcdecl->semanticRun <= PASSsemantic);
        funcdecl->semanticRun = PASSsemantic;

        if (funcdecl->_scope)
        {
            sc = funcdecl->_scope;
            funcdecl->_scope = NULL;
        }

        if (!sc || funcdecl->errors)
            return;

        funcdecl->parent = sc->parent;
        Dsymbol *parent = funcdecl->toParent();

        funcdecl->foverrides.setDim(0);       // reset in case semantic() is being retried for this function

        funcdecl->storage_class |= sc->stc & ~STCref;
        ad = funcdecl->isThis();
        // Don't nest structs b/c of generated methods which should not access the outer scopes.
        // https://issues.dlang.org/show_bug.cgi?id=16627
        if (ad && !funcdecl->generated)
        {
            funcdecl->storage_class |= ad->storage_class & (STC_TYPECTOR | STCsynchronized);
            ad->makeNested();
        }
        if (sc->func)
            funcdecl->storage_class |= sc->func->storage_class & STCdisable;
        // Remove prefix storage classes silently.
        if ((funcdecl->storage_class & STC_TYPECTOR) && !(ad || funcdecl->isNested()))
            funcdecl->storage_class &= ~STC_TYPECTOR;

        //printf("function storage_class = x%llx, sc->stc = x%llx, %x\n", funcdecl->storage_class, sc->stc, Declaration::isFinal());

        FuncLiteralDeclaration *fld = funcdecl->isFuncLiteralDeclaration();
        if (fld && fld->treq)
        {
            Type *treq = fld->treq;
            assert(treq->nextOf()->ty == Tfunction);
            if (treq->ty == Tdelegate)
                fld->tok = TOKdelegate;
            else if (treq->ty == Tpointer && treq->nextOf()->ty == Tfunction)
                fld->tok = TOKfunction;
            else
                assert(0);
            funcdecl->linkage = treq->nextOf()->toTypeFunction()->linkage;
        }
        else
            funcdecl->linkage = sc->linkage;
        funcdecl->inlining = sc->inlining;
        funcdecl->protection = sc->protection;
        funcdecl->userAttribDecl = sc->userAttribDecl;

        if (!funcdecl->originalType)
            funcdecl->originalType = funcdecl->type->syntaxCopy();
        if (funcdecl->type->ty != Tfunction)
        {
            if (funcdecl->type->ty != Terror)
            {
                funcdecl->error("%s must be a function instead of %s", funcdecl->toChars(), funcdecl->type->toChars());
                funcdecl->type = Type::terror;
            }
            funcdecl->errors = true;
            return;
        }
        if (!funcdecl->type->deco)
        {
            sc = sc->push();
            sc->stc |= funcdecl->storage_class & (STCdisable | STCdeprecated);  // forward to function type
            TypeFunction *tf = funcdecl->type->toTypeFunction();

            if (sc->func)
            {
                /* If the nesting parent is pure without inference,
                 * then this function defaults to pure too.
                 *
                 *  auto foo() pure {
                 *    auto bar() {}     // become a weak purity funciton
                 *    class C {         // nested class
                 *      auto baz() {}   // become a weak purity funciton
                 *    }
                 *
                 *    static auto boo() {}   // typed as impure
                 *    // Even though, boo cannot call any impure functions.
                 *    // See also Expression::checkPurity().
                 *  }
                 */
                if (tf->purity == PUREimpure && (funcdecl->isNested() || funcdecl->isThis()))
                {
                    FuncDeclaration *fd = NULL;
                    for (Dsymbol *p = funcdecl->toParent2(); p; p = p->toParent2())
                    {
                        if (AggregateDeclaration *adx = p->isAggregateDeclaration())
                        {
                            if (adx->isNested())
                                continue;
                            break;
                        }
                        if ((fd = p->isFuncDeclaration()) != NULL)
                            break;
                    }

                    /* If the parent's purity is inferred, then this function's purity needs
                     * to be inferred first.
                     */
                    if (fd && fd->isPureBypassingInference() >= PUREweak &&
                        !funcdecl->isInstantiated())
                    {
                        tf->purity = PUREfwdref;            // default to pure
                    }
                }
            }

            if (tf->isref)      sc->stc |= STCref;
            if (tf->isscope)    sc->stc |= STCscope;
            if (tf->isnothrow)  sc->stc |= STCnothrow;
            if (tf->isnogc)     sc->stc |= STCnogc;
            if (tf->isproperty) sc->stc |= STCproperty;
            if (tf->purity == PUREfwdref)   sc->stc |= STCpure;
            if (tf->trust != TRUSTdefault)
                sc->stc &= ~(STCsafe | STCsystem | STCtrusted);
            if (tf->trust == TRUSTsafe)     sc->stc |= STCsafe;
            if (tf->trust == TRUSTsystem)   sc->stc |= STCsystem;
            if (tf->trust == TRUSTtrusted)  sc->stc |= STCtrusted;

            if (funcdecl->isCtorDeclaration())
            {
                sc->flags |= SCOPEctor;

                Type *tret = ad->handleType();
                assert(tret);
                tret = tret->addStorageClass(funcdecl->storage_class | sc->stc);
                tret = tret->addMod(funcdecl->type->mod);
                tf->next = tret;

                if (ad->isStructDeclaration())
                    sc->stc |= STCref;
            }

            // 'return' on a non-static class member function implies 'scope' as well
            if (ad && ad->isClassDeclaration() && (tf->isreturn || sc->stc & STCreturn) && !(sc->stc & STCstatic))
                sc->stc |= STCscope;

            // If 'this' has no pointers, remove 'scope' as it has no meaning
            if (sc->stc & STCscope && ad && ad->isStructDeclaration() && !ad->type->hasPointers())
            {
                sc->stc &= ~STCscope;
                tf->isscope = false;
            }

            sc->linkage = funcdecl->linkage;

            if (!tf->isNaked() && !(funcdecl->isThis() || funcdecl->isNested()))
            {
                OutBuffer buf;
                MODtoBuffer(&buf, tf->mod);
                funcdecl->error("without `this` cannot be %s", buf.peekChars());
                tf->mod = 0;    // remove qualifiers
            }

            /* Apply const, immutable, wild and shared storage class
             * to the function type. Do this before type semantic.
             */
            StorageClass stc = funcdecl->storage_class;
            if (funcdecl->type->isImmutable())
                stc |= STCimmutable;
            if (funcdecl->type->isConst())
                stc |= STCconst;
            if (funcdecl->type->isShared() || funcdecl->storage_class & STCsynchronized)
                stc |= STCshared;
            if (funcdecl->type->isWild())
                stc |= STCwild;
            switch (stc & STC_TYPECTOR)
            {
                case STCimmutable:
                case STCimmutable | STCconst:
                case STCimmutable | STCwild:
                case STCimmutable | STCwild | STCconst:
                case STCimmutable | STCshared:
                case STCimmutable | STCshared | STCconst:
                case STCimmutable | STCshared | STCwild:
                case STCimmutable | STCshared | STCwild | STCconst:
                    // Don't use immutableOf(), as that will do a merge()
                    funcdecl->type = funcdecl->type->makeImmutable();
                    break;

                case STCconst:
                    funcdecl->type = funcdecl->type->makeConst();
                    break;

                case STCwild:
                    funcdecl->type = funcdecl->type->makeWild();
                    break;

                case STCwild | STCconst:
                    funcdecl->type = funcdecl->type->makeWildConst();
                    break;

                case STCshared:
                    funcdecl->type = funcdecl->type->makeShared();
                    break;

                case STCshared | STCconst:
                    funcdecl->type = funcdecl->type->makeSharedConst();
                    break;

                case STCshared | STCwild:
                    funcdecl->type = funcdecl->type->makeSharedWild();
                    break;

                case STCshared | STCwild | STCconst:
                    funcdecl->type = funcdecl->type->makeSharedWildConst();
                    break;

                case 0:
                    break;

                default:
                    assert(0);
            }

            funcdecl->type = typeSemantic(funcdecl->type, funcdecl->loc, sc);
            sc = sc->pop();
        }
        if (funcdecl->type->ty != Tfunction)
        {
            if (funcdecl->type->ty != Terror)
            {
                funcdecl->error("%s must be a function instead of %s", funcdecl->toChars(), funcdecl->type->toChars());
                funcdecl->type = Type::terror;
            }
            funcdecl->errors = true;
            return;
        }
        else
        {
            // Merge back function attributes into 'originalType'.
            // It's used for mangling, ddoc, and json output.
            TypeFunction *tfo = funcdecl->originalType->toTypeFunction();
            TypeFunction *tfx = funcdecl->type->toTypeFunction();
            tfo->mod        = tfx->mod;
            tfo->isscope    = tfx->isscope;
            tfo->isscopeinferred = tfx->isscopeinferred;
            tfo->isref      = tfx->isref;
            tfo->isnothrow  = tfx->isnothrow;
            tfo->isnogc     = tfx->isnogc;
            tfo->isproperty = tfx->isproperty;
            tfo->purity     = tfx->purity;
            tfo->trust      = tfx->trust;

            funcdecl->storage_class &= ~(STC_TYPECTOR | STC_FUNCATTR);
        }

        f = (TypeFunction *)funcdecl->type;

        if ((funcdecl->storage_class & STCauto) && !f->isref && !funcdecl->inferRetType)
            funcdecl->error("storage class `auto` has no effect if return type is not inferred");
        /* Functions can only be 'scope' if they have a 'this'
         */
        if (f->isscope && !funcdecl->isNested() && !ad)
        {
            funcdecl->error("functions cannot be scope");
        }

        if (f->isreturn && !funcdecl->needThis() && !funcdecl->isNested())
        {
            /* Non-static nested functions have a hidden 'this' pointer to which
             * the 'return' applies
             */
            funcdecl->error("static member has no `this` to which `return` can apply");
        }

        if (funcdecl->isAbstract() && !funcdecl->isVirtual())
        {
            const char *sfunc;
            if (funcdecl->isStatic())
                sfunc = "static";
            else if (funcdecl->protection.kind == Prot::private_ || funcdecl->protection.kind == Prot::package_)
                sfunc = protectionToChars(funcdecl->protection.kind);
            else
                sfunc = "non-virtual";
            funcdecl->error("%s functions cannot be abstract", sfunc);
        }

        if (funcdecl->isOverride() && !funcdecl->isVirtual())
        {
            Prot::Kind kind = funcdecl->prot().kind;
            if ((kind == Prot::private_ || kind == Prot::package_) && funcdecl->isMember())
                funcdecl->error("%s method is not virtual and cannot override", protectionToChars(kind));
            else
                funcdecl->error("cannot override a non-virtual function");
        }

        if (funcdecl->isAbstract() && funcdecl->isFinalFunc())
            funcdecl->error("cannot be both final and abstract");

        if (const unsigned pors = sc->flags & (SCOPEprintf | SCOPEscanf))
        {
            /* printf/scanf-like functions must be of the form:
             *    extern (C/C++) T printf([parameters...], const(char)* format, ...);
             * or:
             *    extern (C/C++) T vprintf([parameters...], const(char)* format, va_list);
             */
            const size_t nparams = f->parameterList.length();
            if ((f->linkage == LINKc || f->linkage == LINKcpp) &&

                ((f->parameterList.varargs == VARARGvariadic &&
                  nparams >= 1 &&
                  isPointerToChar(f->parameterList[nparams - 1])) ||
                 (f->parameterList.varargs == VARARGnone &&
                  nparams >= 2 &&
                  isPointerToChar(f->parameterList[nparams - 2]) &&
                  isVa_list(f->parameterList[nparams - 1], funcdecl, sc))
                )
               )
            {
                funcdecl->flags |= (pors == SCOPEprintf) ? FUNCFLAGprintf : FUNCFLAGscanf;
            }
            else
            {
                const char *p = (pors == SCOPEprintf ? Id::printf : Id::scanf)->toChars();
                if (f->parameterList.varargs == VARARGvariadic)
                {
                    funcdecl->error("`pragma(%s)` functions must be `extern(C) %s %s([parameters...], const(char)*, ...)`"
                                    " not `%s`",
                        p, f->next->toChars(), funcdecl->toChars(), funcdecl->type->toChars());
                }
                else
                {
                    funcdecl->error("`pragma(%s)` functions must be `extern(C) %s %s([parameters...], const(char)*, va_list)`",
                        p, f->next->toChars(), funcdecl->toChars());
                }
            }
        }

        id = parent->isInterfaceDeclaration();
        if (id)
        {
            funcdecl->storage_class |= STCabstract;

            if (funcdecl->isCtorDeclaration() ||
                funcdecl->isPostBlitDeclaration() ||
                funcdecl->isDtorDeclaration() ||
                funcdecl->isInvariantDeclaration() ||
                funcdecl->isNewDeclaration() || funcdecl->isDelete())
                funcdecl->error("constructors, destructors, postblits, invariants, new and delete functions are not allowed in interface %s", id->toChars());
            if (funcdecl->fbody && funcdecl->isVirtual())
                funcdecl->error("function body only allowed in final functions in interface %s", id->toChars());
        }

        if (UnionDeclaration *ud = parent->isUnionDeclaration())
        {
            if (funcdecl->isPostBlitDeclaration() ||
                funcdecl->isDtorDeclaration() ||
                funcdecl->isInvariantDeclaration())
                funcdecl->error("destructors, postblits and invariants are not allowed in union %s", ud->toChars());
        }

        if (parent->isStructDeclaration())
        {
            if (funcdecl->isCtorDeclaration())
            {
                goto Ldone;
            }
        }

        if (ClassDeclaration *cd = parent->isClassDeclaration())
        {
            if (funcdecl->isCtorDeclaration())
            {
                goto Ldone;
            }

            if (funcdecl->storage_class & STCabstract)
                cd->isabstract = ABSyes;

            // if static function, do not put in vtbl[]
            if (!funcdecl->isVirtual())
            {
                //printf("\tnot virtual\n");
                goto Ldone;
            }
            // Suppress further errors if the return type is an error
            if (funcdecl->type->nextOf() == Type::terror)
                goto Ldone;

            bool may_override = false;
            for (size_t i = 0; i < cd->baseclasses->length; i++)
            {
                BaseClass *b = (*cd->baseclasses)[i];
                ClassDeclaration *cbd = b->type->toBasetype()->isClassHandle();
                if (!cbd)
                    continue;
                for (size_t j = 0; j < cbd->vtbl.length; j++)
                {
                    FuncDeclaration *f2 = cbd->vtbl[j]->isFuncDeclaration();
                    if (!f2 || f2->ident != funcdecl->ident)
                        continue;
                    if (cbd->parent && cbd->parent->isTemplateInstance())
                    {
                        if (!f2->functionSemantic())
                            goto Ldone;
                    }
                    may_override = true;
                }
            }
            if (may_override && funcdecl->type->nextOf() == NULL)
            {
                /* If same name function exists in base class but 'this' is auto return,
                 * cannot find index of base class's vtbl[] to override.
                 */
                funcdecl->error("return type inference is not supported if may override base class function");
            }

            /* Find index of existing function in base class's vtbl[] to override
             * (the index will be the same as in cd's current vtbl[])
             */
            int vi = cd->baseClass ? funcdecl->findVtblIndex((Dsymbols*)&cd->baseClass->vtbl, (int)cd->baseClass->vtbl.length)
                                   : -1;

            bool doesoverride = false;
            switch (vi)
            {
                case -1:
            Lintro:
                    /* Didn't find one, so
                     * This is an 'introducing' function which gets a new
                     * slot in the vtbl[].
                     */

                    // Verify this doesn't override previous final function
                    if (cd->baseClass)
                    {
                        Dsymbol *s = cd->baseClass->search(funcdecl->loc, funcdecl->ident);
                        if (s)
                        {
                            FuncDeclaration *f2 = s->isFuncDeclaration();
                            if (f2)
                            {
                                f2 = f2->overloadExactMatch(funcdecl->type);
                                if (f2 && f2->isFinalFunc() && f2->prot().kind != Prot::private_)
                                    funcdecl->error("cannot override final function %s", f2->toPrettyChars());
                            }
                        }
                    }

                    /* These quirky conditions mimic what VC++ appears to do
                     */
                    if (global.params.mscoff && cd->isCPPclass() &&
                        cd->baseClass && cd->baseClass->vtbl.length)
                    {
                        /* if overriding an interface function, then this is not
                         * introducing and don't put it in the class vtbl[]
                         */
                        funcdecl->interfaceVirtual = funcdecl->overrideInterface();
                        if (funcdecl->interfaceVirtual)
                        {
                            //printf("\tinterface function %s\n", funcdecl->toChars());
                            cd->vtblFinal.push(funcdecl);
                            goto Linterfaces;
                        }
                    }

                    if (funcdecl->isFinalFunc())
                    {
                        // Don't check here, as it may override an interface function
                        //if (funcdecl->isOverride())
                            //funcdecl->error("is marked as override, but does not override any function");
                        cd->vtblFinal.push(funcdecl);
                    }
                    else
                    {
                        //printf("\tintroducing function %s\n", funcdecl->toChars());
                        funcdecl->introducing = 1;
                        if (cd->isCPPclass() && target.cpp.reverseOverloads)
                        {
                            // with dmc, overloaded functions are grouped and in reverse order
                            funcdecl->vtblIndex = (int)cd->vtbl.length;
                            for (int i = 0; i < (int)cd->vtbl.length; i++)
                            {
                                if (cd->vtbl[i]->ident == funcdecl->ident && cd->vtbl[i]->parent == parent)
                                {
                                    funcdecl->vtblIndex = (int)i;
                                    break;
                                }
                            }
                            // shift all existing functions back
                            for (int i = (int)cd->vtbl.length; i > funcdecl->vtblIndex; i--)
                            {
                                FuncDeclaration *fd = cd->vtbl[i-1]->isFuncDeclaration();
                                assert(fd);
                                fd->vtblIndex++;
                            }
                            cd->vtbl.insert(funcdecl->vtblIndex, funcdecl);
                        }
                        else
                        {
                            // Append to end of vtbl[]
                            vi = (int)cd->vtbl.length;
                            cd->vtbl.push(funcdecl);
                            funcdecl->vtblIndex = vi;
                        }
                    }
                    break;

                case -2:
                    // can't determine because of forward references
                    funcdecl->errors = true;
                    return;

                default:
                {
                    FuncDeclaration *fdv = cd->baseClass->vtbl[vi]->isFuncDeclaration();
                    FuncDeclaration *fdc = cd->vtbl[vi]->isFuncDeclaration();
                    // This function is covariant with fdv

                    if (fdc == funcdecl)
                    {
                        doesoverride = true;
                        break;
                    }

                    if (fdc->toParent() == parent)
                    {
                        //printf("vi = %d,\tthis = %p %s %s @ [%s]\n\tfdc  = %p %s %s @ [%s]\n\tfdv  = %p %s %s @ [%s]\n",
                        //        vi, funcdecl, funcdecl->toChars(), funcdecl->type->toChars(), funcdecl->loc.toChars(),
                        //            fdc,  fdc ->toChars(), fdc ->type->toChars(), fdc ->loc.toChars(),
                        //            fdv,  fdv ->toChars(), fdv ->type->toChars(), fdv ->loc.toChars());

                        // fdc overrides fdv exactly, then this introduces new function.
                        if (fdc->type->mod == fdv->type->mod && funcdecl->type->mod != fdv->type->mod)
                            goto Lintro;
                    }

                    // This function overrides fdv
                    if (fdv->isFinalFunc())
                        funcdecl->error("cannot override final function %s", fdv->toPrettyChars());

                    if (!funcdecl->isOverride())
                    {
                        if (fdv->isFuture())
                        {
                            ::deprecation(funcdecl->loc, "@__future base class method %s is being overridden by %s; rename the latter",
                                fdv->toPrettyChars(), funcdecl->toPrettyChars());
                            // Treat 'this' as an introducing function, giving it a separate hierarchy in the vtbl[]
                            goto Lintro;
                        }
                        else
                        {
                            int vi2 = funcdecl->findVtblIndex(&cd->baseClass->vtbl, (int)cd->baseClass->vtbl.length, false);
                            if (vi2 < 0)
                                // https://issues.dlang.org/show_bug.cgi?id=17349
                                ::deprecation(funcdecl->loc, "cannot implicitly override base class method `%s` with `%s`; add `override` attribute",
                                    fdv->toPrettyChars(), funcdecl->toPrettyChars());
                            else
                                error(funcdecl->loc, "implicitly overriding base class method %s with %s deprecated; add `override` attribute",
                                    fdv->toPrettyChars(), funcdecl->toPrettyChars());
                        }
                    }

                    doesoverride = true;
                    if (fdc->toParent() == parent)
                    {
                        // If both are mixins, or both are not, then error.
                        // If either is not, the one that is not overrides the other.
                        bool thismixin = funcdecl->parent->isClassDeclaration() != NULL;
                        bool fdcmixin = fdc->parent->isClassDeclaration() != NULL;
                        if (thismixin == fdcmixin)
                        {
                            funcdecl->error("multiple overrides of same function");
                        }
                        else if (!thismixin)    // fdc overrides fdv
                        {
                            // this doesn't override any function
                            break;
                        }
                    }
                    cd->vtbl[vi] = funcdecl;
                    funcdecl->vtblIndex = vi;

                    /* Remember which functions this overrides
                     */
                    funcdecl->foverrides.push(fdv);

                    /* This works by whenever this function is called,
                     * it actually returns tintro, which gets dynamically
                     * cast to type. But we know that tintro is a base
                     * of type, so we could optimize it by not doing a
                     * dynamic cast, but just subtracting the isBaseOf()
                     * offset if the value is != null.
                     */

                    if (fdv->tintro)
                        funcdecl->tintro = fdv->tintro;
                    else if (!funcdecl->type->equals(fdv->type))
                    {
                        /* Only need to have a tintro if the vptr
                         * offsets differ
                         */
                        int offset;
                        if (fdv->type->nextOf()->isBaseOf(funcdecl->type->nextOf(), &offset))
                        {
                            funcdecl->tintro = fdv->type;
                        }
                    }
                    break;
                }
            }

            /* Go through all the interface bases.
             * If this function is covariant with any members of those interface
             * functions, set the tintro.
             */
        Linterfaces:
            for (size_t i = 0; i < cd->interfaces.length; i++)
            {
                BaseClass *b = cd->interfaces.ptr[i];
                vi = funcdecl->findVtblIndex((Dsymbols *)&b->sym->vtbl, (int)b->sym->vtbl.length);
                switch (vi)
                {
                    case -1:
                        break;

                    case -2:
                        // can't determine because of forward references
                        funcdecl->errors = true;
                        return;

                    default:
                    {
                        FuncDeclaration *fdv = (FuncDeclaration *)b->sym->vtbl[vi];
                        Type *ti = NULL;

                        /* Remember which functions this overrides
                         */
                        funcdecl->foverrides.push(fdv);

                        /* Should we really require 'override' when implementing
                         * an interface function?
                         */
                        //if (!funcdecl->isOverride())
                            //warning(funcdecl->loc, "overrides base class function %s, but is not marked with `override`", fdv->toPrettyChars());

                        if (fdv->tintro)
                            ti = fdv->tintro;
                        else if (!funcdecl->type->equals(fdv->type))
                        {
                            /* Only need to have a tintro if the vptr
                             * offsets differ
                             */
                            int offset;
                            if (fdv->type->nextOf()->isBaseOf(funcdecl->type->nextOf(), &offset))
                            {
                                ti = fdv->type;
                            }
                        }
                        if (ti)
                        {
                            if (funcdecl->tintro)
                            {
                                if (!funcdecl->tintro->nextOf()->equals(ti->nextOf()) &&
                                    !funcdecl->tintro->nextOf()->isBaseOf(ti->nextOf(), NULL) &&
                                    !ti->nextOf()->isBaseOf(funcdecl->tintro->nextOf(), NULL))
                                {
                                    funcdecl->error("incompatible covariant types %s and %s", funcdecl->tintro->toChars(), ti->toChars());
                                }
                            }
                            funcdecl->tintro = ti;
                        }
                        goto L2;
                    }
                }
            }

            if (!doesoverride && funcdecl->isOverride() && (funcdecl->type->nextOf() || !may_override))
            {
                BaseClass *bc = NULL;
                Dsymbol *s = NULL;
                for (size_t i = 0; i < cd->baseclasses->length; i++)
                {
                    bc = (*cd->baseclasses)[i];
                    s = bc->sym->search_correct(funcdecl->ident);
                    if (s) break;
                }

                if (s)
                    funcdecl->error("does not override any function, did you mean to override `%s%s`?",
                        bc->sym->isCPPclass() ? "extern (C++) " : "", s->toPrettyChars());
                else
                    funcdecl->error("does not override any function");
            }

        L2: ;

            /* Go through all the interface bases.
             * Disallow overriding any final functions in the interface(s).
             */
            for (size_t i = 0; i < cd->interfaces.length; i++)
            {
                BaseClass *b = cd->interfaces.ptr[i];
                if (b->sym)
                {
                    Dsymbol *s = search_function(b->sym, funcdecl->ident);
                    if (s)
                    {
                        FuncDeclaration *f2 = s->isFuncDeclaration();
                        if (f2)
                        {
                            f2 = f2->overloadExactMatch(funcdecl->type);
                            if (f2 && f2->isFinalFunc() && f2->prot().kind != Prot::private_)
                                funcdecl->error("cannot override final function %s.%s", b->sym->toChars(), f2->toPrettyChars());
                        }
                    }
                }
            }

            if (funcdecl->isOverride())
            {
                if (funcdecl->storage_class & STCdisable)
                    funcdecl->deprecation("overridden functions cannot be annotated @disable");
                if (funcdecl->isDeprecated())
                    funcdecl->deprecation("deprecated functions cannot be annotated @disable");
            }
        }
        else if (funcdecl->isOverride() && !parent->isTemplateInstance())
            funcdecl->error("override only applies to class member functions");

        // Reflect this->type to f because it could be changed by findVtblIndex
        f = funcdecl->type->toTypeFunction();

    Ldone:
        /* Contracts can only appear without a body when they are virtual interface functions
         */
        if (!funcdecl->fbody && !allowsContractWithoutBody(funcdecl))
            funcdecl->error("in and out contracts can only appear without a body when they are virtual interface functions or abstract");

        /* Do not allow template instances to add virtual functions
         * to a class.
         */
        if (funcdecl->isVirtual())
        {
            TemplateInstance *ti = parent->isTemplateInstance();
            if (ti)
            {
                // Take care of nested templates
                while (1)
                {
                    TemplateInstance *ti2 = ti->tempdecl->parent->isTemplateInstance();
                    if (!ti2)
                        break;
                    ti = ti2;
                }

                // If it's a member template
                ClassDeclaration *cd = ti->tempdecl->isClassMember();
                if (cd)
                {
                    funcdecl->error("cannot use template to add virtual function to class `%s`", cd->toChars());
                }
            }
        }

        if (funcdecl->isMain())
            funcdecl->checkDmain();       // Check main() parameters and return type

        /* Purity and safety can be inferred for some functions by examining
         * the function body.
         */
        if (canInferAttributes(funcdecl, sc))
            initInferAttributes(funcdecl);

        Module::dprogress++;
        funcdecl->semanticRun = PASSsemanticdone;

        /* Save scope for possible later use (if we need the
         * function internals)
         */
        funcdecl->_scope = sc->copy();
        funcdecl->_scope->setNoFree();

        static bool printedMain = false;  // semantic might run more than once
        if (global.params.verbose && !printedMain)
        {
            const char *type = funcdecl->isMain() ? "main" : funcdecl->isWinMain() ? "winmain" : funcdecl->isDllMain() ? "dllmain" : (const char *)NULL;
            Module *mod = sc->_module;

            if (type && mod)
            {
                printedMain = true;
                const char *name = mod->srcfile->toChars();
                const char *path = FileName::searchPath(global.path, name, true);
                message("entry     %-10s\t%s", type, path ? path : name);
            }
        }

        if (funcdecl->fbody && funcdecl->isMain() && sc->_module->isRoot())
            Compiler::genCmain(sc);

        assert(funcdecl->type->ty != Terror || funcdecl->errors);

        // semantic for parameters' UDAs
        const size_t nparams = f->parameterList.length();
        for (size_t i = 0; i < nparams; i++)
        {
            Parameter *param = f->parameterList[i];
            if (param && param->userAttribDecl)
                dsymbolSemantic(param->userAttribDecl, sc);
        }
    }

    // Do the semantic analysis on the external interface to the function.
    void visit(FuncDeclaration *funcdecl)
    {
        funcDeclarationSemantic(funcdecl);
    }

    void visit(CtorDeclaration *ctd)
    {
        //printf("CtorDeclaration::semantic() %s\n", ctd->toChars());
        if (ctd->semanticRun >= PASSsemanticdone)
            return;
        if (ctd->_scope)
        {
            sc = ctd->_scope;
            ctd->_scope = NULL;
        }

        ctd->parent = sc->parent;
        Dsymbol *p = ctd->toParent2();
        AggregateDeclaration *ad = p->isAggregateDeclaration();
        if (!ad)
        {
            error(ctd->loc, "constructor can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            ctd->type = Type::terror;
            ctd->errors = true;
            return;
        }

        sc = sc->push();
        sc->stc &= ~STCstatic;              // not a static constructor
        sc->flags |= SCOPEctor;

        funcDeclarationSemantic(ctd);

        sc->pop();

        if (ctd->errors)
            return;

        TypeFunction *tf = ctd->type->toTypeFunction();

        /* See if it's the default constructor
         * But, template constructor should not become a default constructor.
         */
        if (ad && (!ctd->parent->isTemplateInstance() || ctd->parent->isTemplateMixin()))
        {
            const size_t dim = tf->parameterList.length();

            if (StructDeclaration *sd = ad->isStructDeclaration())
            {
                if (dim == 0 && tf->parameterList.varargs == VARARGnone) // empty default ctor w/o any varargs
                {
                    if (ctd->fbody || !(ctd->storage_class & STCdisable) || dim)
                    {
                        ctd->error("default constructor for structs only allowed "
                            "with @disable, no body, and no parameters");
                        ctd->storage_class |= STCdisable;
                        ctd->fbody = NULL;
                    }
                    sd->noDefaultCtor = true;
                }
                else if (dim == 0 && tf->parameterList.varargs) // allow varargs only ctor
                {
                }
                else if (dim && tf->parameterList[0]->defaultArg)
                {
                    // if the first parameter has a default argument, then the rest does as well
                    if (ctd->storage_class & STCdisable)
                    {
                        ctd->deprecation("@disable'd constructor cannot have default "
                                         "arguments for all parameters.");
                        deprecationSupplemental(ctd->loc, "Use @disable this(); if you want to disable default initialization.");
                    }
                    else
                        ctd->deprecation("all parameters have default arguments, "
                                         "but structs cannot have default constructors.");
                }

            }
            else if (dim == 0 && tf->parameterList.varargs == VARARGnone)
            {
                ad->defaultCtor = ctd;
            }
        }
    }

    void visit(PostBlitDeclaration *pbd)
    {
        //printf("PostBlitDeclaration::semantic() %s\n", pbd->toChars());
        //printf("ident: %s, %s, %p, %p\n", pbd->ident->toChars(), Id::dtor->toChars(), pbd->ident, Id::dtor);
        //printf("stc = x%llx\n", sc->stc);
        if (pbd->semanticRun >= PASSsemanticdone)
            return;
        if (pbd->_scope)
        {
            sc = pbd->_scope;
            pbd->_scope = NULL;
        }

        pbd->parent = sc->parent;
        Dsymbol *p = pbd->toParent2();
        StructDeclaration *ad = p->isStructDeclaration();
        if (!ad)
        {
            error(pbd->loc, "postblit can only be a member of struct/union, not %s %s",
                p->kind(), p->toChars());
            pbd->type = Type::terror;
            pbd->errors = true;
            return;
        }
        if (pbd->ident == Id::postblit && pbd->semanticRun < PASSsemantic)
            ad->postblits.push(pbd);
        if (!pbd->type)
            pbd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, pbd->storage_class);

        sc = sc->push();
        sc->stc &= ~STCstatic;              // not static
        sc->linkage = LINKd;

        funcDeclarationSemantic(pbd);

        sc->pop();
    }

    void visit(DtorDeclaration *dd)
    {
        //printf("DtorDeclaration::semantic() %s\n", dd->toChars());
        //printf("ident: %s, %s, %p, %p\n", dd->ident->toChars(), Id::dtor->toChars(), dd->ident, Id::dtor);
        if (dd->semanticRun >= PASSsemanticdone)
            return;
        if (dd->_scope)
        {
            sc = dd->_scope;
            dd->_scope = NULL;
        }

        dd->parent = sc->parent;
        Dsymbol *p = dd->toParent2();
        AggregateDeclaration *ad = p->isAggregateDeclaration();
        if (!ad)
        {
            error(dd->loc, "destructor can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            dd->type = Type::terror;
            dd->errors = true;
            return;
        }
        if (dd->ident == Id::dtor && dd->semanticRun < PASSsemantic)
            ad->dtors.push(dd);
        if (!dd->type)
            dd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, dd->storage_class);

        sc = sc->push();
        sc->stc &= ~STCstatic;              // not a static destructor
        if (sc->linkage != LINKcpp)
            sc->linkage = LINKd;

        funcDeclarationSemantic(dd);

        sc->pop();
    }

    void visit(StaticCtorDeclaration *scd)
    {
        //printf("StaticCtorDeclaration::semantic()\n");
        if (scd->semanticRun >= PASSsemanticdone)
            return;
        if (scd->_scope)
        {
            sc = scd->_scope;
            scd->_scope = NULL;
        }

        scd->parent = sc->parent;
        Dsymbol *p = scd->parent->pastMixin();
        if (!p->isScopeDsymbol())
        {
            const char *s = (scd->isSharedStaticCtorDeclaration() ? "shared " : "");
            error(scd->loc, "%sstatic constructor can only be member of module/aggregate/template, not %s %s",
                s, p->kind(), p->toChars());
            scd->type = Type::terror;
            scd->errors = true;
            return;
        }
        if (!scd->type)
            scd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, scd->storage_class);

        /* If the static ctor appears within a template instantiation,
         * it could get called multiple times by the module constructors
         * for different modules. Thus, protect it with a gate.
         */
        if (scd->isInstantiated() && scd->semanticRun < PASSsemantic)
        {
            /* Add this prefix to the function:
             *      static int gate;
             *      if (++gate != 1) return;
             * Note that this is not thread safe; should not have threads
             * during static construction.
             */
            VarDeclaration *v = new VarDeclaration(Loc(), Type::tint32, Id::gate, NULL);
            v->storage_class = STCtemp | (scd->isSharedStaticCtorDeclaration() ? STCstatic : STCtls);
            Statements *sa = new Statements();
            Statement *s = new ExpStatement(Loc(), v);
            sa->push(s);
            Expression *e = new IdentifierExp(Loc(), v->ident);
            e = new AddAssignExp(Loc(), e, new IntegerExp(1));
            e = new EqualExp(TOKnotequal, Loc(), e, new IntegerExp(1));
            s = new IfStatement(Loc(), NULL, e, new ReturnStatement(Loc(), NULL), NULL, Loc());
            sa->push(s);
            if (scd->fbody)
                sa->push(scd->fbody);
            scd->fbody = new CompoundStatement(Loc(), sa);
        }

        funcDeclarationSemantic(scd);

        // We're going to need ModuleInfo
        Module *m = scd->getModule();
        if (!m)
            m = sc->_module;
        if (m)
        {
            m->needmoduleinfo = 1;
            //printf("module1 %s needs moduleinfo\n", m->toChars());
        }
    }

    void visit(StaticDtorDeclaration *sdd)
    {
        if (sdd->semanticRun >= PASSsemanticdone)
            return;
        if (sdd->_scope)
        {
            sc = sdd->_scope;
            sdd->_scope = NULL;
        }

        sdd->parent = sc->parent;
        Dsymbol *p = sdd->parent->pastMixin();
        if (!p->isScopeDsymbol())
        {
            const char *s = (sdd->isSharedStaticDtorDeclaration() ? "shared " : "");
            error(sdd->loc, "%sstatic destructor can only be member of module/aggregate/template, not %s %s",
                s, p->kind(), p->toChars());
            sdd->type = Type::terror;
            sdd->errors = true;
            return;
        }
        if (!sdd->type)
            sdd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, sdd->storage_class);

        /* If the static ctor appears within a template instantiation,
         * it could get called multiple times by the module constructors
         * for different modules. Thus, protect it with a gate.
         */
        if (sdd->isInstantiated() && sdd->semanticRun < PASSsemantic)
        {
            /* Add this prefix to the function:
             *      static int gate;
             *      if (--gate != 0) return;
             * Increment gate during constructor execution.
             * Note that this is not thread safe; should not have threads
             * during static destruction.
             */
            VarDeclaration *v = new VarDeclaration(Loc(), Type::tint32, Id::gate, NULL);
            v->storage_class = STCtemp | (sdd->isSharedStaticDtorDeclaration() ? STCstatic : STCtls);
            Statements *sa = new Statements();
            Statement *s = new ExpStatement(Loc(), v);
            sa->push(s);
            Expression *e = new IdentifierExp(Loc(), v->ident);
            e = new AddAssignExp(Loc(), e, new IntegerExp(-1));
            e = new EqualExp(TOKnotequal, Loc(), e, new IntegerExp(0));
            s = new IfStatement(Loc(), NULL, e, new ReturnStatement(Loc(), NULL), NULL, Loc());
            sa->push(s);
            if (sdd->fbody)
                sa->push(sdd->fbody);
            sdd->fbody = new CompoundStatement(Loc(), sa);
            sdd->vgate = v;
        }

        funcDeclarationSemantic(sdd);

        // We're going to need ModuleInfo
        Module *m = sdd->getModule();
        if (!m)
            m = sc->_module;
        if (m)
        {
            m->needmoduleinfo = 1;
            //printf("module2 %s needs moduleinfo\n", m->toChars());
        }
    }

    void visit(InvariantDeclaration *invd)
    {
        if (invd->semanticRun >= PASSsemanticdone)
            return;
        if (invd->_scope)
        {
            sc = invd->_scope;
            invd->_scope = NULL;
        }

        invd->parent = sc->parent;
        Dsymbol *p = invd->parent->pastMixin();
        AggregateDeclaration *ad = p->isAggregateDeclaration();
        if (!ad)
        {
            error(invd->loc, "invariant can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            invd->type = Type::terror;
            invd->errors = true;
            return;
        }
        if (invd->ident != Id::classInvariant &&
            invd->semanticRun < PASSsemantic &&
            !ad->isUnionDeclaration()           // users are on their own with union fields
           )
            ad->invs.push(invd);
        if (!invd->type)
            invd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, invd->storage_class);

        sc = sc->push();
        sc->stc &= ~STCstatic;              // not a static invariant
        sc->stc |= STCconst;                // invariant() is always const
        sc->flags = (sc->flags & ~SCOPEcontract) | SCOPEinvariant;
        sc->linkage = LINKd;

        funcDeclarationSemantic(invd);

        sc->pop();
    }

    void visit(UnitTestDeclaration *utd)
    {
        if (utd->semanticRun >= PASSsemanticdone)
            return;
        if (utd->_scope)
        {
            sc = utd->_scope;
            utd->_scope = NULL;
        }

        utd->protection = sc->protection;

        utd->parent = sc->parent;
        Dsymbol *p = utd->parent->pastMixin();
        if (!p->isScopeDsymbol())
        {
            error(utd->loc, "unittest can only be a member of module/aggregate/template, not %s %s",
                p->kind(), p->toChars());
            utd->type = Type::terror;
            utd->errors = true;
            return;
        }

        if (global.params.useUnitTests)
        {
            if (!utd->type)
                utd->type = new TypeFunction(ParameterList(), Type::tvoid, LINKd, utd->storage_class);
            Scope *sc2 = sc->push();
            sc2->linkage = LINKd;
            funcDeclarationSemantic(utd);
            sc2->pop();
        }
    }

    void visit(NewDeclaration *nd)
    {
        //printf("NewDeclaration::semantic()\n");
        if (nd->semanticRun >= PASSsemanticdone)
            return;
        if (nd->_scope)
        {
            sc = nd->_scope;
            nd->_scope = NULL;
        }

        nd->parent = sc->parent;
        Dsymbol *p = nd->parent->pastMixin();
        if (!p->isAggregateDeclaration())
        {
            error(nd->loc, "allocator can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            nd->type = Type::terror;
            nd->errors = true;
            return;
        }
        Type *tret = Type::tvoid->pointerTo();
        if (!nd->type)
            nd->type = new TypeFunction(ParameterList(nd->parameters, nd->varargs), tret, LINKd, nd->storage_class);

        nd->type = typeSemantic(nd->type, nd->loc, sc);

        // Check that there is at least one argument of type size_t
        TypeFunction *tf = nd->type->toTypeFunction();
        if (tf->parameterList.length() < 1)
        {
            nd->error("at least one argument of type size_t expected");
        }
        else
        {
            Parameter *fparam = tf->parameterList[0];
            if (!fparam->type->equals(Type::tsize_t))
                nd->error("first argument must be type size_t, not %s", fparam->type->toChars());
        }

        funcDeclarationSemantic(nd);
    }

    void visit(DeleteDeclaration *deld)
    {
        //printf("DeleteDeclaration::semantic()\n");
        if (deld->semanticRun >= PASSsemanticdone)
            return;
        if (deld->_scope)
        {
            sc = deld->_scope;
            deld->_scope = NULL;
        }

        deld->parent = sc->parent;
        Dsymbol *p = deld->parent->pastMixin();
        if (!p->isAggregateDeclaration())
        {
            error(deld->loc, "deallocator can only be a member of aggregate, not %s %s",
                p->kind(), p->toChars());
            deld->type = Type::terror;
            deld->errors = true;
            return;
        }
        if (!deld->type)
            deld->type = new TypeFunction(ParameterList(deld->parameters), Type::tvoid, LINKd, deld->storage_class);

        deld->type = typeSemantic(deld->type, deld->loc, sc);

        // Check that there is only one argument of type void*
        TypeFunction *tf = deld->type->toTypeFunction();
        if (tf->parameterList.length() != 1)
        {
            deld->error("one argument of type void* expected");
        }
        else
        {
            Parameter *fparam = tf->parameterList[0];
            if (!fparam->type->equals(Type::tvoid->pointerTo()))
                deld->error("one argument of type void* expected, not %s", fparam->type->toChars());
        }

        funcDeclarationSemantic(deld);
    }

    void visit(StructDeclaration *sd)
    {
        //printf("StructDeclaration::semantic(this=%p, %s '%s', sizeok = %d)\n", sd, sd->parent->toChars(), sd->toChars(), sizeok);

        //static int count; if (++count == 20) halt();

        if (sd->semanticRun >= PASSsemanticdone)
            return;
        unsigned errors = global.errors;

        //printf("+StructDeclaration::semantic(this=%p, %s '%s', sizeok = %d)\n", sd, sd->parent->toChars(), sd->toChars(), sizeok);
        Scope *scx = NULL;
        if (sd->_scope)
        {
            sc = sd->_scope;
            scx = sd->_scope;            // save so we don't make redundant copies
            sd->_scope = NULL;
        }

        if (!sd->parent)
        {
            assert(sc->parent && sc->func);
            sd->parent = sc->parent;
        }
        assert(sd->parent && !sd->isAnonymous());

        if (sd->errors)
            sd->type = Type::terror;
        if (sd->semanticRun == PASSinit)
            sd->type = sd->type->addSTC(sc->stc | sd->storage_class);
        sd->type = typeSemantic(sd->type, sd->loc, sc);

        if (sd->type->ty == Tstruct && ((TypeStruct *)sd->type)->sym != sd)
        {
            TemplateInstance *ti = ((TypeStruct *)sd->type)->sym->isInstantiated();
            if (ti && isError(ti))
                ((TypeStruct *)sd->type)->sym = sd;
        }

        // Ungag errors when not speculative
        Ungag ungag = sd->ungagSpeculative();

        if (sd->semanticRun == PASSinit)
        {
            sd->protection = sc->protection;

            sd->alignment = sc->alignment();

            sd->storage_class |= sc->stc;
            if (sd->storage_class & STCdeprecated)
                sd->isdeprecated = true;
            if (sd->storage_class & STCabstract)
                sd->error("structs, unions cannot be abstract");
            sd->userAttribDecl = sc->userAttribDecl;

            if (sc->linkage == LINKcpp)
                sd->classKind = ClassKind::cpp;
        }
        else if (sd->symtab && !scx)
        {
            return;
        }
        sd->semanticRun = PASSsemantic;

        if (!sd->members)               // if opaque declaration
        {
            sd->semanticRun = PASSsemanticdone;
            return;
        }
        if (!sd->symtab)
        {
            sd->symtab = new DsymbolTable();

            for (size_t i = 0; i < sd->members->length; i++)
            {
                Dsymbol *s = (*sd->members)[i];
                //printf("adding member '%s' to '%s'\n", s->toChars(), sd->toChars());
                s->addMember(sc, sd);
            }
        }

        Scope *sc2 = sd->newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        for (size_t i = 0; i < sd->members->length; i++)
        {
            Dsymbol *s = (*sd->members)[i];
            //printf("struct: setScope %s %s\n", s->kind(), s->toChars());
            s->setScope(sc2);
        }

        for (size_t i = 0; i < sd->members->length; i++)
        {
            Dsymbol *s = (*sd->members)[i];
            s->importAll(sc2);
        }

        for (size_t i = 0; i < sd->members->length; i++)
        {
            Dsymbol *s = (*sd->members)[i];
            dsymbolSemantic(s, sc2);
        }

        if (!sd->determineFields())
        {
            assert(sd->type->ty == Terror);
            sc2->pop();
            sd->semanticRun = PASSsemanticdone;
            return;
        }

        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types. For example, buildDtor
         * needs to check existence of elaborate dtor in type of each fields.
         * See the case in compilable/test14838.d
         */
        for (size_t i = 0; i < sd->fields.length; i++)
        {
            VarDeclaration *v = sd->fields[i];
            Type *tb = v->type->baseElemOf();
            if (tb->ty != Tstruct)
                continue;
            StructDeclaration *sdec = ((TypeStruct *)tb)->sym;
            if (sdec->semanticRun >= PASSsemanticdone)
                continue;

            sc2->pop();

            sd->_scope = scx ? scx : sc->copy();
            sd->_scope->setNoFree();
            Module::addDeferredSemantic(sd);

            //printf("\tdeferring %s\n", sd->toChars());
            return;
        }

        /* Look for special member functions.
         */
        sd->aggNew =       (NewDeclaration *)sd->search(Loc(), Id::classNew);
        sd->aggDelete = (DeleteDeclaration *)sd->search(Loc(), Id::classDelete);

        // Look for the constructor
        sd->ctor = sd->searchCtor();

        sd->dtor = buildDtor(sd, sc2);
        sd->postblit = buildPostBlit(sd, sc2);

        buildOpAssign(sd, sc2);
        buildOpEquals(sd, sc2);

        if (global.params.useTypeInfo && Type::dtypeinfo)  // these functions are used for TypeInfo
        {
            sd->xeq = buildXopEquals(sd, sc2);
            sd->xcmp = buildXopCmp(sd, sc2);
            sd->xhash = buildXtoHash(sd, sc2);
        }

        sd->inv = buildInv(sd, sc2);

        Module::dprogress++;
        sd->semanticRun = PASSsemanticdone;
        //printf("-StructDeclaration::semantic(this=%p, '%s')\n", sd, sd->toChars());

        sc2->pop();

        if (sd->ctor)
        {
            Dsymbol *scall = sd->search(Loc(), Id::call);
            if (scall)
            {
                unsigned xerrors = global.startGagging();
                sc = sc->push();
                sc->tinst = NULL;
                sc->minst = NULL;
                FuncDeclaration *fcall = resolveFuncCall(sd->loc, sc, scall, NULL, NULL, NULL, 1);
                sc = sc->pop();
                global.endGagging(xerrors);

                if (fcall && fcall->isStatic())
                {
                    sd->error(fcall->loc, "`static opCall` is hidden by constructors and can never be called");
                    errorSupplemental(fcall->loc, "Please use a factory method instead, or replace all constructors with `static opCall`.");
                }
            }
        }

        if (sd->type->ty == Tstruct && ((TypeStruct *)sd->type)->sym != sd)
        {
            // https://issues.dlang.org/show_bug.cgi?id=19024
            StructDeclaration *sym = ((TypeStruct *)sd->type)->sym;
            sd->error("already exists at %s. Perhaps in another function with the same name?", sym->loc.toChars());
        }

        if (global.errors != errors)
        {
            // The type is no good.
            sd->type = Type::terror;
            sd->errors = true;
            if (sd->deferred)
                sd->deferred->errors = true;
        }

        if (sd->deferred && !global.gag)
        {
            semantic2(sd->deferred, sc);
            semantic3(sd->deferred, sc);
        }
    }

    void interfaceSemantic(ClassDeclaration *cd)
    {
        cd->vtblInterfaces = new BaseClasses();
        cd->vtblInterfaces->reserve(cd->interfaces.length);

        for (size_t i = 0; i < cd->interfaces.length; i++)
        {
            BaseClass *b = cd->interfaces.ptr[i];
            cd->vtblInterfaces->push(b);
            b->copyBaseInterfaces(cd->vtblInterfaces);
        }
    }

    void visit(ClassDeclaration *cldec)
    {
        //printf("ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", cldec->toChars(), cldec->type, sizeok, cldec);
        //printf("\tparent = %p, '%s'\n", sc->parent, sc->parent ? sc->parent->toChars() : "");
        //printf("sc->stc = %x\n", sc->stc);

        //{ static int n;  if (++n == 20) *(char*)0=0; }

        if (cldec->semanticRun >= PASSsemanticdone)
            return;
        unsigned errors = global.errors;

        //printf("+ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", cldec->toChars(), cldec->type, sizeok, cldec);

        Scope *scx = NULL;
        if (cldec->_scope)
        {
            sc = cldec->_scope;
            scx = cldec->_scope;            // save so we don't make redundant copies
            cldec->_scope = NULL;
        }

        if (!cldec->parent)
        {
            assert(sc->parent);
            cldec->parent = sc->parent;
        }

        if (cldec->errors)
            cldec->type = Type::terror;
        cldec->type = typeSemantic(cldec->type, cldec->loc, sc);

        if (cldec->type->ty == Tclass && ((TypeClass *)cldec->type)->sym != cldec)
        {
            TemplateInstance *ti = ((TypeClass *)cldec->type)->sym->isInstantiated();
            if (ti && isError(ti))
                ((TypeClass *)cldec->type)->sym = cldec;
        }

        // Ungag errors when not speculative
        Ungag ungag = cldec->ungagSpeculative();

        if (cldec->semanticRun == PASSinit)
        {
            cldec->protection = sc->protection;

            cldec->storage_class |= sc->stc;
            if (cldec->storage_class & STCdeprecated)
                cldec->isdeprecated = true;
            if (cldec->storage_class & STCauto)
                cldec->error("storage class `auto` is invalid when declaring a class, did you mean to use `scope`?");
            if (cldec->storage_class & STCscope)
                cldec->isscope = true;
            if (cldec->storage_class & STCabstract)
                cldec->isabstract = ABSyes;

            cldec->userAttribDecl = sc->userAttribDecl;

            if (sc->linkage == LINKcpp)
                cldec->classKind = ClassKind::cpp;
            if (sc->linkage == LINKobjc)
                objc()->setObjc(cldec);
        }
        else if (cldec->symtab && !scx)
        {
            return;
        }
        cldec->semanticRun = PASSsemantic;

        if (cldec->baseok < BASEOKdone)
        {
            cldec->baseok = BASEOKin;

            // Expand any tuples in baseclasses[]
            for (size_t i = 0; i < cldec->baseclasses->length; )
            {
                BaseClass *b = (*cldec->baseclasses)[i];
                b->type = resolveBase(cldec, sc, scx, b->type);

                Type *tb = b->type->toBasetype();
                if (tb->ty == Ttuple)
                {
                    TypeTuple *tup = (TypeTuple *)tb;
                    cldec->baseclasses->remove(i);
                    size_t dim = Parameter::dim(tup->arguments);
                    for (size_t j = 0; j < dim; j++)
                    {
                        Parameter *arg = Parameter::getNth(tup->arguments, j);
                        b = new BaseClass(arg->type);
                        cldec->baseclasses->insert(i + j, b);
                    }
                }
                else
                    i++;
            }

            if (cldec->baseok >= BASEOKdone)
            {
                //printf("%s already semantic analyzed, semanticRun = %d\n", cldec->toChars(), cldec->semanticRun);
                if (cldec->semanticRun >= PASSsemanticdone)
                    return;
                goto Lancestorsdone;
            }

            // See if there's a base class as first in baseclasses[]
            if (cldec->baseclasses->length)
            {
                BaseClass *b = (*cldec->baseclasses)[0];
                Type *tb = b->type->toBasetype();
                TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
                if (!tc)
                {
                    if (b->type != Type::terror)
                        cldec->error("base type must be class or interface, not %s", b->type->toChars());
                    cldec->baseclasses->remove(0);
                    goto L7;
                }

                if (tc->sym->isDeprecated())
                {
                    if (!cldec->isDeprecated())
                    {
                        // Deriving from deprecated class makes this one deprecated too
                        cldec->isdeprecated = true;

                        tc->checkDeprecated(cldec->loc, sc);
                    }
                }

                if (tc->sym->isInterfaceDeclaration())
                    goto L7;

                for (ClassDeclaration *cdb = tc->sym; cdb; cdb = cdb->baseClass)
                {
                    if (cdb == cldec)
                    {
                        cldec->error("circular inheritance");
                        cldec->baseclasses->remove(0);
                        goto L7;
                    }
                }

                /* Bugzilla 11034: Essentially, class inheritance hierarchy
                 * and instance size of each classes are orthogonal information.
                 * Therefore, even if tc->sym->sizeof == SIZEOKnone,
                 * we need to set baseClass field for class covariance check.
                 */
                cldec->baseClass = tc->sym;
                b->sym = cldec->baseClass;

                if (tc->sym->baseok < BASEOKdone)
                    resolveBase(cldec, sc, scx, tc->sym); // Try to resolve forward reference
                if (tc->sym->baseok < BASEOKdone)
                {
                    //printf("\ttry later, forward reference of base class %s\n", tc->sym->toChars());
                    if (tc->sym->_scope)
                        Module::addDeferredSemantic(tc->sym);
                    cldec->baseok = BASEOKnone;
                }
             L7: ;
            }

            // Treat the remaining entries in baseclasses as interfaces
            // Check for errors, handle forward references
            for (size_t i = (cldec->baseClass ? 1 : 0); i < cldec->baseclasses->length; )
            {
                BaseClass *b = (*cldec->baseclasses)[i];
                Type *tb = b->type->toBasetype();
                TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
                if (!tc || !tc->sym->isInterfaceDeclaration())
                {
                    if (b->type != Type::terror)
                        cldec->error("base type must be interface, not %s", b->type->toChars());
                    cldec->baseclasses->remove(i);
                    continue;
                }

                // Check for duplicate interfaces
                for (size_t j = (cldec->baseClass ? 1 : 0); j < i; j++)
                {
                    BaseClass *b2 = (*cldec->baseclasses)[j];
                    if (b2->sym == tc->sym)
                    {
                        cldec->error("inherits from duplicate interface %s", b2->sym->toChars());
                        cldec->baseclasses->remove(i);
                        continue;
                    }
                }

                if (tc->sym->isDeprecated())
                {
                    if (!cldec->isDeprecated())
                    {
                        // Deriving from deprecated class makes this one deprecated too
                        cldec->isdeprecated = true;

                        tc->checkDeprecated(cldec->loc, sc);
                    }
                }

                b->sym = tc->sym;

                if (tc->sym->baseok < BASEOKdone)
                    resolveBase(cldec, sc, scx, tc->sym); // Try to resolve forward reference
                if (tc->sym->baseok < BASEOKdone)
                {
                    //printf("\ttry later, forward reference of base %s\n", tc->sym->toChars());
                    if (tc->sym->_scope)
                        Module::addDeferredSemantic(tc->sym);
                    cldec->baseok = BASEOKnone;
                }
                i++;
            }
            if (cldec->baseok == BASEOKnone)
            {
                // Forward referencee of one or more bases, try again later
                cldec->_scope = scx ? scx : sc->copy();
                cldec->_scope->setNoFree();
                Module::addDeferredSemantic(cldec);
                //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, cldec->toChars());
                return;
            }
            cldec->baseok = BASEOKdone;

            // If no base class, and this is not an Object, use Object as base class
            if (!cldec->baseClass && cldec->ident != Id::Object && !cldec->isCPPclass())
            {
                if (!ClassDeclaration::object || ClassDeclaration::object->errors)
                    badObjectDotD(cldec);

                Type *t = ClassDeclaration::object->type;
                t = typeSemantic(t, cldec->loc, sc)->toBasetype();
                if (t->ty == Terror)
                    badObjectDotD(cldec);
                assert(t->ty == Tclass);
                TypeClass *tc = (TypeClass *)t;

                BaseClass *b = new BaseClass(tc);
                cldec->baseclasses->shift(b);

                cldec->baseClass = tc->sym;
                assert(!cldec->baseClass->isInterfaceDeclaration());
                b->sym = cldec->baseClass;
            }
            if (cldec->baseClass)
            {
                if (cldec->baseClass->storage_class & STCfinal)
                    cldec->error("cannot inherit from final class %s", cldec->baseClass->toChars());

                // Inherit properties from base class
                if (cldec->baseClass->isCOMclass())
                    cldec->com = true;
                if (cldec->baseClass->isCPPclass())
                    cldec->classKind = ClassKind::cpp;
                if (cldec->baseClass->isscope)
                    cldec->isscope = true;
                cldec->enclosing = cldec->baseClass->enclosing;
                cldec->storage_class |= cldec->baseClass->storage_class & STC_TYPECTOR;
            }

            cldec->interfaces.length = cldec->baseclasses->length - (cldec->baseClass ? 1 : 0);
            cldec->interfaces.ptr = cldec->baseclasses->tdata() + (cldec->baseClass ? 1 : 0);

            for (size_t i = 0; i < cldec->interfaces.length; i++)
            {
                BaseClass *b = cldec->interfaces.ptr[i];
                // If this is an interface, and it derives from a COM interface,
                // then this is a COM interface too.
                if (b->sym->isCOMinterface())
                    cldec->com = true;
                if (cldec->isCPPclass() && !b->sym->isCPPinterface())
                {
                    error(cldec->loc, "C++ class `%s` cannot implement D interface `%s`",
                        cldec->toPrettyChars(), b->sym->toPrettyChars());
                }
            }

            interfaceSemantic(cldec);
        }
    Lancestorsdone:
        //printf("\tClassDeclaration::semantic(%s) baseok = %d\n", cldec->toChars(), cldec->baseok);

        if (!cldec->members)               // if opaque declaration
        {
            cldec->semanticRun = PASSsemanticdone;
            return;
        }
        if (!cldec->symtab)
        {
            cldec->symtab = new DsymbolTable();

            /* Bugzilla 12152: The semantic analysis of base classes should be finished
             * before the members semantic analysis of this class, in order to determine
             * vtbl in this class. However if a base class refers the member of this class,
             * it can be resolved as a normal forward reference.
             * Call addMember() and setScope() to make this class members visible from the base classes.
             */
            for (size_t i = 0; i < cldec->members->length; i++)
            {
                Dsymbol *s = (*cldec->members)[i];
                s->addMember(sc, cldec);
            }

            Scope *sc2 = cldec->newScope(sc);

            /* Set scope so if there are forward references, we still might be able to
             * resolve individual members like enums.
             */
            for (size_t i = 0; i < cldec->members->length; i++)
            {
                Dsymbol *s = (*cldec->members)[i];
                //printf("[%d] setScope %s %s, sc2 = %p\n", i, s->kind(), s->toChars(), sc2);
                s->setScope(sc2);
            }

            sc2->pop();
        }

        for (size_t i = 0; i < cldec->baseclasses->length; i++)
        {
            BaseClass *b = (*cldec->baseclasses)[i];
            Type *tb = b->type->toBasetype();
            assert(tb->ty == Tclass);
            TypeClass *tc = (TypeClass *)tb;

            if (tc->sym->semanticRun < PASSsemanticdone)
            {
                // Forward referencee of one or more bases, try again later
                cldec->_scope = scx ? scx : sc->copy();
                cldec->_scope->setNoFree();
                if (tc->sym->_scope)
                    Module::addDeferredSemantic(tc->sym);
                Module::addDeferredSemantic(cldec);
                //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, cldec->toChars());
                return;
            }
        }

        if (cldec->baseok == BASEOKdone)
        {
            cldec->baseok = BASEOKsemanticdone;

            // initialize vtbl
            if (cldec->baseClass)
            {
                if (cldec->isCPPclass() && cldec->baseClass->vtbl.length == 0)
                {
                    cldec->error("C++ base class %s needs at least one virtual function", cldec->baseClass->toChars());
                }

                // Copy vtbl[] from base class
                cldec->vtbl.setDim(cldec->baseClass->vtbl.length);
                memcpy(cldec->vtbl.tdata(), cldec->baseClass->vtbl.tdata(), sizeof(void *) * cldec->vtbl.length);

                cldec->vthis = cldec->baseClass->vthis;
            }
            else
            {
                // No base class, so this is the root of the class hierarchy
                cldec->vtbl.setDim(0);
                if (cldec->vtblOffset())
                    cldec->vtbl.push(cldec);            // leave room for classinfo as first member
            }

            /* If this is a nested class, add the hidden 'this'
             * member which is a pointer to the enclosing scope.
             */
            if (cldec->vthis)              // if inheriting from nested class
            {
                // Use the base class's 'this' member
                if (cldec->storage_class & STCstatic)
                    cldec->error("static class cannot inherit from nested class %s", cldec->baseClass->toChars());
                if (cldec->toParent2() != cldec->baseClass->toParent2() &&
                    (!cldec->toParent2() ||
                     !cldec->baseClass->toParent2()->getType() ||
                     !cldec->baseClass->toParent2()->getType()->isBaseOf(cldec->toParent2()->getType(), NULL)))
                {
                    if (cldec->toParent2())
                    {
                        cldec->error("is nested within %s, but super class %s is nested within %s",
                            cldec->toParent2()->toChars(),
                            cldec->baseClass->toChars(),
                            cldec->baseClass->toParent2()->toChars());
                    }
                    else
                    {
                        cldec->error("is not nested, but super class %s is nested within %s",
                            cldec->baseClass->toChars(),
                            cldec->baseClass->toParent2()->toChars());
                    }
                    cldec->enclosing = NULL;
                }
            }
            else
                cldec->makeNested();
        }

        Scope *sc2 = cldec->newScope(sc);

        for (size_t i = 0; i < cldec->members->length; i++)
        {
            Dsymbol *s = (*cldec->members)[i];
            s->importAll(sc2);
        }

        // Note that members.length can grow due to tuple expansion during semantic()
        for (size_t i = 0; i < cldec->members->length; i++)
        {
            Dsymbol *s = (*cldec->members)[i];
            dsymbolSemantic(s, sc2);
        }

        if (!cldec->determineFields())
        {
            assert(cldec->type == Type::terror);
            sc2->pop();
            return;
        }

        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types.
         */
        for (size_t i = 0; i < cldec->fields.length; i++)
        {
            VarDeclaration *v = cldec->fields[i];
            Type *tb = v->type->baseElemOf();
            if (tb->ty != Tstruct)
                continue;
            StructDeclaration *sd = ((TypeStruct *)tb)->sym;
            if (sd->semanticRun >= PASSsemanticdone)
                continue;

            sc2->pop();

            cldec->_scope = scx ? scx : sc->copy();
            cldec->_scope->setNoFree();
            Module::addDeferredSemantic(cldec);
            //printf("\tdeferring %s\n", cldec->toChars());
            return;
        }

        /* Look for special member functions.
         * They must be in this class, not in a base class.
         */

        // Can be in base class
        cldec->aggNew    =    (NewDeclaration *)cldec->search(Loc(), Id::classNew);
        cldec->aggDelete = (DeleteDeclaration *)cldec->search(Loc(), Id::classDelete);

        // Look for the constructor
        cldec->ctor = cldec->searchCtor();

        if (!cldec->ctor && cldec->noDefaultCtor)
        {
            // A class object is always created by constructor, so this check is legitimate.
            for (size_t i = 0; i < cldec->fields.length; i++)
            {
                VarDeclaration *v = cldec->fields[i];
                if (v->storage_class & STCnodefaultctor)
                    error(v->loc, "field %s must be initialized in constructor", v->toChars());
            }
        }

        // If this class has no constructor, but base class has a default
        // ctor, create a constructor:
        //    this() { }
        if (!cldec->ctor && cldec->baseClass && cldec->baseClass->ctor)
        {
            FuncDeclaration *fd = resolveFuncCall(cldec->loc, sc2, cldec->baseClass->ctor, NULL, cldec->type, NULL, 1);
            if (!fd) // try shared base ctor instead
                fd = resolveFuncCall(cldec->loc, sc2, cldec->baseClass->ctor, NULL, cldec->type->sharedOf(), NULL, 1);
            if (fd && !fd->errors)
            {
                //printf("Creating default this(){} for class %s\n", cldec->toChars());
                TypeFunction *btf = fd->type->toTypeFunction();
                TypeFunction *tf = new TypeFunction(ParameterList(), NULL, LINKd, fd->storage_class);
                tf->mod = btf->mod;
                tf->purity = btf->purity;
                tf->isnothrow = btf->isnothrow;
                tf->isnogc = btf->isnogc;
                tf->trust = btf->trust;

                CtorDeclaration *ctor = new CtorDeclaration(cldec->loc, Loc(), 0, tf);
                ctor->fbody = new CompoundStatement(Loc(), new Statements());

                cldec->members->push(ctor);
                ctor->addMember(sc, cldec);
                dsymbolSemantic(ctor, sc2);

                cldec->ctor = ctor;
                cldec->defaultCtor = ctor;
            }
            else
            {
                cldec->error("cannot implicitly generate a default ctor when base class %s is missing a default ctor",
                    cldec->baseClass->toPrettyChars());
            }
        }

        cldec->dtor = buildDtor(cldec, sc2);

        if (FuncDeclaration *f = hasIdentityOpAssign(cldec, sc2))
        {
            if (!(f->storage_class & STCdisable))
                cldec->error(f->loc, "identity assignment operator overload is illegal");
        }

        cldec->inv = buildInv(cldec, sc2);

        Module::dprogress++;
        cldec->semanticRun = PASSsemanticdone;
        //printf("-ClassDeclaration.semantic(%s), type = %p\n", cldec->toChars(), cldec->type);
        //members.print();

        sc2->pop();

        if (cldec->type->ty == Tclass && ((TypeClass *)cldec->type)->sym != cldec)
        {
            // https://issues.dlang.org/show_bug.cgi?id=17492
            ClassDeclaration *cd = ((TypeClass *)cldec->type)->sym;
            cldec->error("already exists at %s. Perhaps in another function with the same name?", cd->loc.toChars());
        }

        if (global.errors != errors)
        {
            // The type is no good.
            cldec->type = Type::terror;
            cldec->errors = true;
            if (cldec->deferred)
                cldec->deferred->errors = true;
        }

        // Verify fields of a synchronized class are not public
        if (cldec->storage_class & STCsynchronized)
        {
            for (size_t i = 0; i < cldec->fields.length; i++)
            {
                VarDeclaration *vd = cldec->fields[i];
                if (!vd->isThisDeclaration() &&
                    !vd->prot().isMoreRestrictiveThan(Prot(Prot::public_)))
                {
                    vd->error("Field members of a synchronized class cannot be %s",
                        protectionToChars(vd->prot().kind));
                }
            }
        }

        if (cldec->deferred && !global.gag)
        {
            semantic2(cldec->deferred, sc);
            semantic3(cldec->deferred, sc);
        }
        //printf("-ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", cldec->toChars(), cldec->type, sizeok, cldec);
    }

    void visit(InterfaceDeclaration *idec)
    {
        //printf("InterfaceDeclaration::semantic(%s), type = %p\n", idec->toChars(), idec->type);
        if (idec->semanticRun >= PASSsemanticdone)
            return;
        unsigned errors = global.errors;

        //printf("+InterfaceDeclaration.semantic(%s), type = %p\n", idec->toChars(), idec->type);

        Scope *scx = NULL;
        if (idec->_scope)
        {
            sc = idec->_scope;
            scx = idec->_scope;            // save so we don't make redundant copies
            idec->_scope = NULL;
        }

        if (!idec->parent)
        {
            assert(sc->parent && sc->func);
            idec->parent = sc->parent;
        }
        assert(idec->parent && !idec->isAnonymous());

        if (idec->errors)
            idec->type = Type::terror;
        idec->type = typeSemantic(idec->type, idec->loc, sc);

        if (idec->type->ty == Tclass && ((TypeClass *)idec->type)->sym != idec)
        {
            TemplateInstance *ti = ((TypeClass *)idec->type)->sym->isInstantiated();
            if (ti && isError(ti))
                ((TypeClass *)idec->type)->sym = idec;
        }

        // Ungag errors when not speculative
        Ungag ungag = idec->ungagSpeculative();

        if (idec->semanticRun == PASSinit)
        {
            idec->protection = sc->protection;

            idec->storage_class |= sc->stc;
            if (idec->storage_class & STCdeprecated)
                idec->isdeprecated = true;

            idec->userAttribDecl = sc->userAttribDecl;
        }
        else if (idec->symtab)
        {
            if (idec->sizeok == SIZEOKdone || !scx)
            {
                idec->semanticRun = PASSsemanticdone;
                return;
            }
        }
        idec->semanticRun = PASSsemantic;

        if (idec->baseok < BASEOKdone)
        {
            idec->baseok = BASEOKin;

            // Expand any tuples in baseclasses[]
            for (size_t i = 0; i < idec->baseclasses->length; )
            {
                BaseClass *b = (*idec->baseclasses)[i];
                b->type = resolveBase(idec, sc, scx, b->type);

                Type *tb = b->type->toBasetype();
                if (tb->ty == Ttuple)
                {
                    TypeTuple *tup = (TypeTuple *)tb;
                    idec->baseclasses->remove(i);
                    size_t dim = Parameter::dim(tup->arguments);
                    for (size_t j = 0; j < dim; j++)
                    {
                        Parameter *arg = Parameter::getNth(tup->arguments, j);
                        b = new BaseClass(arg->type);
                        idec->baseclasses->insert(i + j, b);
                    }
                }
                else
                    i++;
            }

            if (idec->baseok >= BASEOKdone)
            {
                //printf("%s already semantic analyzed, semanticRun = %d\n", idec->toChars(), idec->semanticRun);
                if (idec->semanticRun >= PASSsemanticdone)
                    return;
                goto Lancestorsdone;
            }

            if (!idec->baseclasses->length && sc->linkage == LINKcpp)
                idec->classKind = ClassKind::cpp;
            if (sc->linkage == LINKobjc)
                objc()->setObjc(idec);

            // Check for errors, handle forward references
            for (size_t i = 0; i < idec->baseclasses->length; )
            {
                BaseClass *b = (*idec->baseclasses)[i];
                Type *tb = b->type->toBasetype();
                TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
                if (!tc || !tc->sym->isInterfaceDeclaration())
                {
                    if (b->type != Type::terror)
                        idec->error("base type must be interface, not %s", b->type->toChars());
                    idec->baseclasses->remove(i);
                    continue;
                }

                // Check for duplicate interfaces
                for (size_t j = 0; j < i; j++)
                {
                    BaseClass *b2 = (*idec->baseclasses)[j];
                    if (b2->sym == tc->sym)
                    {
                        idec->error("inherits from duplicate interface %s", b2->sym->toChars());
                        idec->baseclasses->remove(i);
                        continue;
                    }
                }

                if (tc->sym == idec || idec->isBaseOf2(tc->sym))
                {
                    idec->error("circular inheritance of interface");
                    idec->baseclasses->remove(i);
                    continue;
                }

                if (tc->sym->isDeprecated())
                {
                    if (!idec->isDeprecated())
                    {
                        // Deriving from deprecated class makes this one deprecated too
                        idec->isdeprecated = true;

                        tc->checkDeprecated(idec->loc, sc);
                    }
                }

                b->sym = tc->sym;

                if (tc->sym->baseok < BASEOKdone)
                    resolveBase(idec, sc, scx, tc->sym); // Try to resolve forward reference
                if (tc->sym->baseok < BASEOKdone)
                {
                    //printf("\ttry later, forward reference of base %s\n", tc->sym->toChars());
                    if (tc->sym->_scope)
                        Module::addDeferredSemantic(tc->sym);
                    idec->baseok = BASEOKnone;
                }
                i++;
            }
            if (idec->baseok == BASEOKnone)
            {
                // Forward referencee of one or more bases, try again later
                idec->_scope = scx ? scx : sc->copy();
                idec->_scope->setNoFree();
                Module::addDeferredSemantic(idec);
                return;
            }
            idec->baseok = BASEOKdone;

            idec->interfaces.length = idec->baseclasses->length;
            idec->interfaces.ptr = idec->baseclasses->tdata();

            for (size_t i = 0; i < idec->interfaces.length; i++)
            {
                BaseClass *b = idec->interfaces.ptr[i];
                // If this is an interface, and it derives from a COM interface,
                // then this is a COM interface too.
                if (b->sym->isCOMinterface())
                    idec->com = true;
                if (b->sym->isCPPinterface())
                    idec->classKind = ClassKind::cpp;
            }

            interfaceSemantic(idec);
        }
    Lancestorsdone:

        if (!idec->members)               // if opaque declaration
        {
            idec->semanticRun = PASSsemanticdone;
            return;
        }
        if (!idec->symtab)
            idec->symtab = new DsymbolTable();

        for (size_t i = 0; i < idec->baseclasses->length; i++)
        {
            BaseClass *b = (*idec->baseclasses)[i];
            Type *tb = b->type->toBasetype();
            assert(tb->ty == Tclass);
            TypeClass *tc = (TypeClass *)tb;

            if (tc->sym->semanticRun < PASSsemanticdone)
            {
                // Forward referencee of one or more bases, try again later
                idec->_scope = scx ? scx : sc->copy();
                idec->_scope->setNoFree();
                if (tc->sym->_scope)
                    Module::addDeferredSemantic(tc->sym);
                Module::addDeferredSemantic(idec);
                return;
            }
        }

        if (idec->baseok == BASEOKdone)
        {
            idec->baseok = BASEOKsemanticdone;

            // initialize vtbl
            if (idec->vtblOffset())
                idec->vtbl.push(idec);                // leave room at vtbl[0] for classinfo

            // Cat together the vtbl[]'s from base cldec->interfaces
            for (size_t i = 0; i < idec->interfaces.length; i++)
            {
                BaseClass *b = idec->interfaces.ptr[i];

                // Skip if b has already appeared
                for (size_t k = 0; k < i; k++)
                {
                    if (b == idec->interfaces.ptr[k])
                        goto Lcontinue;
                }

                // Copy vtbl[] from base class
                if (b->sym->vtblOffset())
                {
                    size_t d = b->sym->vtbl.length;
                    if (d > 1)
                    {
                        idec->vtbl.reserve(d - 1);
                        for (size_t j = 1; j < d; j++)
                            idec->vtbl.push(b->sym->vtbl[j]);
                    }
                }
                else
                {
                    idec->vtbl.append(&b->sym->vtbl);
                }

              Lcontinue:
                ;
            }
        }

        for (size_t i = 0; i < idec->members->length; i++)
        {
            Dsymbol *s = (*idec->members)[i];
            s->addMember(sc, idec);
        }

        Scope *sc2 = idec->newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        for (size_t i = 0; i < idec->members->length; i++)
        {
            Dsymbol *s = (*idec->members)[i];
            //printf("setScope %s %s\n", s->kind(), s->toChars());
            s->setScope(sc2);
        }

        for (size_t i = 0; i < idec->members->length; i++)
        {
            Dsymbol *s = (*idec->members)[i];
            s->importAll(sc2);
        }

        for (size_t i = 0; i < idec->members->length; i++)
        {
            Dsymbol *s = (*idec->members)[i];
            dsymbolSemantic(s, sc2);
        }

        Module::dprogress++;
        idec->semanticRun = PASSsemanticdone;
        //printf("-InterfaceDeclaration.semantic(%s), type = %p\n", idec->toChars(), idec->type);
        //members->print();

        sc2->pop();

        if (global.errors != errors)
        {
            // The type is no good.
            idec->type = Type::terror;
        }

        assert(idec->type->ty != Tclass || ((TypeClass *)idec->type)->sym == idec);
    }
};

/******************************************************
 * Do template instance semantic for isAliasSeq templates.
 * This is a greatly simplified version of TemplateInstance::semantic().
 */
static void aliasSeqInstanceSemantic(TemplateInstance *tempinst, Scope *sc, TemplateDeclaration *tempdecl)
{
    //printf("[%s] aliasSeqInstanceSemantic('%s')\n", tempinst->loc.toChars(), tempinst->toChars());
    Scope *paramscope = sc->push();
    paramscope->stc = 0;
    paramscope->protection = Prot(Prot::public_);

    TemplateTupleParameter *ttp = (*tempdecl->parameters)[0]->isTemplateTupleParameter();
    Tuple *va = isTuple(tempinst->tdtypes[0]);
    Declaration *d = new TupleDeclaration(tempinst->loc, ttp->ident, &va->objects);
    d->storage_class |= STCtemplateparameter;
    dsymbolSemantic(d, sc);

    paramscope->pop();

    tempinst->aliasdecl = d;

    tempinst->semanticRun = PASSsemanticdone;
}

/******************************************************
 * Do template instance semantic for isAlias templates.
 * This is a greatly simplified version of TemplateInstance::semantic().
 */
static void aliasInstanceSemantic(TemplateInstance *tempinst, Scope *sc, TemplateDeclaration *tempdecl)
{
    //printf("[%s] aliasInstanceSemantic('%s')\n", tempinst->loc.toChars(), tempinst->toChars());
    Scope *paramscope = sc->push();
    paramscope->stc = 0;
    paramscope->protection = Prot(Prot::public_);

    TemplateTypeParameter *ttp = (*tempdecl->parameters)[0]->isTemplateTypeParameter();
    Type *ta = isType(tempinst->tdtypes[0]);
    Declaration *d = new AliasDeclaration(tempinst->loc, ttp->ident, ta->addMod(tempdecl->onemember->isAliasDeclaration()->type->mod));
    d->storage_class |= STCtemplateparameter;
    dsymbolSemantic(d, sc);

    paramscope->pop();

    tempinst->aliasdecl = d;

    tempinst->semanticRun = PASSsemanticdone;
}

void templateInstanceSemantic(TemplateInstance *tempinst, Scope *sc, Expressions *fargs)
{
    //printf("[%s] TemplateInstance::semantic('%s', this=%p, gag = %d, sc = %p)\n", tempinst->loc.toChars(), tempinst->toChars(), tempinst, global.gag, sc);
    if (tempinst->inst)           // if semantic() was already run
    {
        return;
    }
    if (tempinst->semanticRun != PASSinit)
    {
        Ungag ungag(global.gag);
        if (!tempinst->gagged)
            global.gag = 0;
        tempinst->error(tempinst->loc, "recursive template expansion");
        if (tempinst->gagged)
            tempinst->semanticRun = PASSinit;
        else
            tempinst->inst = tempinst;
        tempinst->errors = true;
        return;
    }

    // Get the enclosing template instance from the scope tinst
    tempinst->tinst = sc->tinst;

    // Get the instantiating module from the scope minst
    tempinst->minst = sc->minst;
    // Bugzilla 10920: If the enclosing function is non-root symbol,
    // this instance should be speculative.
    if (!tempinst->tinst && sc->func && sc->func->inNonRoot())
    {
        tempinst->minst = NULL;
    }

    tempinst->gagged = (global.gag > 0);

    tempinst->semanticRun = PASSsemantic;

    /* Find template declaration first,
     * then run semantic on each argument (place results in tiargs[]),
     * last find most specialized template from overload list/set.
     */
    if (!tempinst->findTempDecl(sc, NULL) ||
        !tempinst->semanticTiargs(sc) ||
        !tempinst->findBestMatch(sc, fargs))
    {
Lerror:
        if (tempinst->gagged)
        {
            // Bugzilla 13220: Rollback status for later semantic re-running.
            tempinst->semanticRun = PASSinit;
        }
        else
            tempinst->inst = tempinst;
        tempinst->errors = true;
        return;
    }
    TemplateDeclaration *tempdecl = tempinst->tempdecl->isTemplateDeclaration();
    assert(tempdecl);

    // If tempdecl is a mixin, disallow it
    if (tempdecl->ismixin)
    {
        tempinst->error("mixin templates are not regular templates");
        goto Lerror;
    }

    tempinst->hasNestedArgs(tempinst->tiargs, tempdecl->isstatic);
    if (tempinst->errors)
        goto Lerror;

    /* Greatly simplified semantic processing for AliasSeq templates
     */
    if (tempdecl->isTrivialAliasSeq)
    {
        tempinst->inst = tempinst;
        return aliasSeqInstanceSemantic(tempinst, sc, tempdecl);
    }
    /* Greatly simplified semantic processing for Alias templates
     */
    else if (tempdecl->isTrivialAlias)
    {
        tempinst->inst = tempinst;
        return aliasInstanceSemantic(tempinst, sc, tempdecl);
    }

    /* See if there is an existing TemplateInstantiation that already
     * implements the typeargs. If so, just refer to that one instead.
     */
    tempinst->inst = tempdecl->findExistingInstance(tempinst, fargs);
    TemplateInstance *errinst = NULL;
    if (!tempinst->inst)
    {
        // So, we need to implement 'this' instance.
    }
    else if (tempinst->inst->gagged && !tempinst->gagged && tempinst->inst->errors)
    {
        // If the first instantiation had failed, re-run semantic,
        // so that error messages are shown.
        errinst = tempinst->inst;
    }
    else
    {
        // It's a match
        tempinst->parent = tempinst->inst->parent;
        tempinst->errors = tempinst->inst->errors;

        // If both this and the previous instantiation were gagged,
        // use the number of errors that happened last time.
        global.errors += tempinst->errors;
        global.gaggedErrors += tempinst->errors;

        // If the first instantiation was gagged, but this is not:
        if (tempinst->inst->gagged)
        {
            // It had succeeded, mark it is a non-gagged instantiation,
            // and reuse it.
            tempinst->inst->gagged = tempinst->gagged;
        }

        tempinst->tnext = tempinst->inst->tnext;
        tempinst->inst->tnext = tempinst;

        /* A module can have explicit template instance and its alias
         * in module scope (e,g, `alias Base64 = Base64Impl!('+', '/');`).
         * If the first instantiation 'inst' had happened in non-root module,
         * compiler can assume that its instantiated code would be included
         * in the separately compiled obj/lib file (e.g. phobos.lib).
         *
         * However, if 'this' second instantiation happened in root module,
         * compiler might need to invoke its codegen (Bugzilla 2500 & 2644).
         * But whole import graph is not determined until all semantic pass finished,
         * so 'inst' should conservatively finish the semantic3 pass for the codegen.
         */
        if (tempinst->minst && tempinst->minst->isRoot() && !(tempinst->inst->minst && tempinst->inst->minst->isRoot()))
        {
            /* Swap the position of 'inst' and 'this' in the instantiation graph.
             * Then, the primary instance `inst` will be changed to a root instance,
             * along with all members of `inst` having their scopes updated.
             *
             * Before:
             *  non-root -> A!() -> B!()[inst] -> C!() { members[non-root] }
             *                      |
             *  root     -> D!() -> B!()[this]
             *
             * After:
             *  non-root -> A!() -> B!()[this]
             *                      |
             *  root     -> D!() -> B!()[inst] -> C!() { members[root] }
             */
            Module *mi = tempinst->minst;
            TemplateInstance *ti = tempinst->tinst;
            tempinst->minst = tempinst->inst->minst;
            tempinst->tinst = tempinst->inst->tinst;
            tempinst->inst->minst = mi;
            tempinst->inst->tinst = ti;

            /* https://issues.dlang.org/show_bug.cgi?id=21299
               `minst` has been updated on the primary instance `inst` so it is
               now coming from a root module, however all Dsymbol `inst.members`
               of the instance still have their `_scope.minst` pointing at the
               original non-root module. We must now propagate `minst` to all
               members so that forward referenced dependencies that get
               instantiated will also be appended to the root module, otherwise
               there will be undefined references at link-time.  */
            class InstMemberWalker : public Visitor
            {
            public:
                TemplateInstance *inst;

                InstMemberWalker(TemplateInstance *inst)
                    : inst(inst) { }

                void visit(Dsymbol *d)
                {
                    if (d->_scope)
                        d->_scope->minst = inst->minst;
                }

                void visit(ScopeDsymbol *sds)
                {
                    if (!sds->members)
                        return;
                    for (size_t i = 0; i < sds->members->length; i++)
                    {
                        Dsymbol *s = (*sds->members)[i];
                        s->accept(this);
                    }
                    visit((Dsymbol *)sds);
                }

                void visit(AttribDeclaration *ad)
                {
                    Dsymbols *d = ad->include(NULL);
                    if (!d)
                        return;
                    for (size_t i = 0; i < d->length; i++)
                    {
                        Dsymbol *s = (*d)[i];
                        s->accept(this);
                    }
                    visit((Dsymbol *)ad);
                }

                void visit(ConditionalDeclaration *cd)
                {
                    if (cd->condition->inc)
                        visit((AttribDeclaration *)cd);
                    else
                        visit((Dsymbol *)cd);
                }
            };
            InstMemberWalker v(tempinst->inst);
            tempinst->inst->accept(&v);

            if (tempinst->minst)  // if inst was not speculative
            {
                /* Add 'inst' once again to the root module members[], then the
                 * instance members will get codegen chances.
                 */
                tempinst->inst->appendToModuleMember();
            }
        }

        return;
    }
    unsigned errorsave = global.errors;

    tempinst->inst = tempinst;
    tempinst->parent = tempinst->enclosing ? tempinst->enclosing : tempdecl->parent;
    //printf("parent = '%s'\n", tempinst->parent->kind());

    TemplateInstance *tempdecl_instance_idx = tempdecl->addInstance(tempinst);

    //getIdent();

    // Store the place we added it to in target_symbol_list(_idx) so we can
    // remove it later if we encounter an error.
    Dsymbols *target_symbol_list = tempinst->appendToModuleMember();
    size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list->length - 1 : 0;

    // Copy the syntax trees from the TemplateDeclaration
    tempinst->members = Dsymbol::arraySyntaxCopy(tempdecl->members);

    // resolve TemplateThisParameter
    for (size_t i = 0; i < tempdecl->parameters->length; i++)
    {
        if ((*tempdecl->parameters)[i]->isTemplateThisParameter() == NULL)
            continue;
        Type *t = isType((*tempinst->tiargs)[i]);
        assert(t);
        if (StorageClass stc = ModToStc(t->mod))
        {
            //printf("t = %s, stc = x%llx\n", t->toChars(), stc);
            Dsymbols *s = new Dsymbols();
            s->push(new StorageClassDeclaration(stc, tempinst->members));
            tempinst->members = s;
        }
        break;
    }

    // Create our own scope for the template parameters
    Scope *scope = tempdecl->_scope;
    if (tempdecl->semanticRun == PASSinit)
    {
        tempinst->error("template instantiation %s forward references template declaration %s", tempinst->toChars(), tempdecl->toChars());
        return;
    }

    tempinst->argsym = new ScopeDsymbol();
    tempinst->argsym->parent = scope->parent;
    scope = scope->push(tempinst->argsym);
    scope->tinst = tempinst;
    scope->minst = tempinst->minst;
    //scope->stc = 0;

    // Declare each template parameter as an alias for the argument type
    Scope *paramscope = scope->push();
    paramscope->stc = 0;
    paramscope->protection = Prot(Prot::public_);  // Bugzilla 14169: template parameters should be public
    tempinst->declareParameters(paramscope);
    paramscope->pop();

    // Add members of template instance to template instance symbol table
//    tempinst->parent = scope->scopesym;
    tempinst->symtab = new DsymbolTable();
    for (size_t i = 0; i < tempinst->members->length; i++)
    {
        Dsymbol *s = (*tempinst->members)[i];
        s->addMember(scope, tempinst);
    }

    /* See if there is only one member of template instance, and that
     * member has the same name as the template instance.
     * If so, this template instance becomes an alias for that member.
     */
    //printf("members->length = %d\n", tempinst->members->length);
    if (tempinst->members->length)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(tempinst->members, &s, tempdecl->ident) && s)
        {
            //printf("tempdecl->ident = %s, s = '%s'\n", tempdecl->ident->toChars(), s->kind(), s->toPrettyChars());
            //printf("setting aliasdecl\n");
            tempinst->aliasdecl = s;
        }
    }

    /* If function template declaration
     */
    if (fargs && tempinst->aliasdecl)
    {
        FuncDeclaration *fd = tempinst->aliasdecl->isFuncDeclaration();
        if (fd)
        {
            /* Transmit fargs to type so that TypeFunction::semantic() can
             * resolve any "auto ref" storage classes.
             */
            TypeFunction *tf = (TypeFunction *)fd->type;
            if (tf && tf->ty == Tfunction)
                tf->fargs = fargs;
        }
    }

    // Do semantic() analysis on template instance members
    Scope *sc2;
    sc2 = scope->push(tempinst);
    //printf("enclosing = %d, sc->parent = %s\n", tempinst->enclosing, sc->parent->toChars());
    sc2->parent = tempinst;
    sc2->tinst = tempinst;
    sc2->minst = tempinst->minst;

    tempinst->tryExpandMembers(sc2);

    tempinst->semanticRun = PASSsemanticdone;

    /* ConditionalDeclaration may introduce eponymous declaration,
     * so we should find it once again after semantic.
     */
    if (tempinst->members->length)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(tempinst->members, &s, tempdecl->ident) && s)
        {
            if (!tempinst->aliasdecl || tempinst->aliasdecl != s)
            {
                //printf("tempdecl->ident = %s, s = '%s'\n", tempdecl->ident->toChars(), s->kind(), s->toPrettyChars());
                //printf("setting aliasdecl 2\n");
                tempinst->aliasdecl = s;
            }
        }
    }

    if (global.errors != errorsave)
        goto Laftersemantic;

    /* If any of the instantiation members didn't get semantic() run
     * on them due to forward references, we cannot run semantic2()
     * or semantic3() yet.
     */
    {
    bool found_deferred_ad = false;
    for (size_t i = 0; i < Module::deferred.length; i++)
    {
        Dsymbol *sd = Module::deferred[i];
        AggregateDeclaration *ad = sd->isAggregateDeclaration();
        if (ad && ad->parent && ad->parent->isTemplateInstance())
        {
            //printf("deferred template aggregate: %s %s\n",
            //        sd->parent->toChars(), sd->toChars());
            found_deferred_ad = true;
            if (ad->parent == tempinst)
            {
                ad->deferred = tempinst;
                break;
            }
        }
    }
    if (found_deferred_ad || Module::deferred.length)
        goto Laftersemantic;
    }

    /* The problem is when to parse the initializer for a variable.
     * Perhaps VarDeclaration::semantic() should do it like it does
     * for initializers inside a function.
     */
    //if (sc->parent->isFuncDeclaration())
    {
        /* BUG 782: this has problems if the classes this depends on
         * are forward referenced. Find a way to defer semantic()
         * on this template.
         */
        semantic2(tempinst, sc2);
    }
    if (global.errors != errorsave)
        goto Laftersemantic;

    if ((sc->func || (sc->flags & SCOPEfullinst)) && !tempinst->tinst)
    {
        /* If a template is instantiated inside function, the whole instantiation
         * should be done at that position. But, immediate running semantic3 of
         * dependent templates may cause unresolved forward reference (Bugzilla 9050).
         * To avoid the issue, don't run semantic3 until semantic and semantic2 done.
         */
        TemplateInstances deferred;
        tempinst->deferred = &deferred;

        //printf("Run semantic3 on %s\n", tempinst->toChars());
        tempinst->trySemantic3(sc2);

        for (size_t i = 0; i < deferred.length; i++)
        {
            //printf("+ run deferred semantic3 on %s\n", deferred[i]->toChars());
            semantic3(deferred[i], NULL);
        }

        tempinst->deferred = NULL;
    }
    else if (tempinst->tinst)
    {
        bool doSemantic3 = false;
        if (sc->func && tempinst->aliasdecl && tempinst->aliasdecl->toAlias()->isFuncDeclaration())
        {
            /* Template function instantiation should run semantic3 immediately
             * for attribute inference.
             */
            tempinst->trySemantic3(sc2);
        }
        else if (sc->func)
        {
            /* A lambda function in template arguments might capture the
             * instantiated scope context. For the correct context inference,
             * all instantiated functions should run the semantic3 immediately.
             * See also compilable/test14973.d
             */
            for (size_t i = 0; i < tempinst->tdtypes.length; i++)
            {
                RootObject *oarg = tempinst->tdtypes[i];
                Dsymbol *s = getDsymbol(oarg);
                if (!s)
                    continue;

                if (TemplateDeclaration *td = s->isTemplateDeclaration())
                {
                    if (!td->literal)
                        continue;
                    assert(td->members && td->members->length == 1);
                    s = (*td->members)[0];
                }
                if (FuncLiteralDeclaration *fld = s->isFuncLiteralDeclaration())
                {
                    if (fld->tok == TOKreserved)
                    {
                        doSemantic3 = true;
                        break;
                    }
                }
            }
            //printf("[%s] %s doSemantic3 = %d\n", tempinst->loc.toChars(), tempinst->toChars(), doSemantic3);
        }
        if (doSemantic3)
            tempinst->trySemantic3(sc2);

        TemplateInstance *ti = tempinst->tinst;
        int nest = 0;
        while (ti && !ti->deferred && ti->tinst)
        {
            ti = ti->tinst;
            if (++nest > global.recursionLimit)
            {
                global.gag = 0;            // ensure error message gets printed
                tempinst->error("recursive expansion");
                fatal();
            }
        }
        if (ti && ti->deferred)
        {
            //printf("deferred semantic3 of %p %s, ti = %s, ti->deferred = %p\n", tempinst, tempinst->toChars(), ti->toChars());
            for (size_t i = 0; ; i++)
            {
                if (i == ti->deferred->length)
                {
                    ti->deferred->push(tempinst);
                    break;
                }
                if ((*ti->deferred)[i] == tempinst)
                    break;
            }
        }
    }

    if (tempinst->aliasdecl)
    {
        /* Bugzilla 13816: AliasDeclaration tries to resolve forward reference
         * twice (See inuse check in AliasDeclaration::toAlias()). It's
         * necessary to resolve mutual references of instantiated symbols, but
         * it will left a true recursive alias in tuple declaration - an
         * AliasDeclaration A refers TupleDeclaration B, and B contains A
         * in its elements.  To correctly make it an error, we strictly need to
         * resolve the alias of eponymous member.
         */
        tempinst->aliasdecl = tempinst->aliasdecl->toAlias2();
    }

  Laftersemantic:
    sc2->pop();

    scope->pop();

    // Give additional context info if error occurred during instantiation
    if (global.errors != errorsave)
    {
        if (!tempinst->errors)
        {
            if (!tempdecl->literal)
                tempinst->error(tempinst->loc, "error instantiating");
            if (tempinst->tinst)
                tempinst->tinst->printInstantiationTrace();
        }
        tempinst->errors = true;
        if (tempinst->gagged)
        {
            // Errors are gagged, so remove the template instance from the
            // instance/symbol lists we added it to and reset our state to
            // finish clean and so we can try to instantiate it again later
            // (see bugzilla 4302 and 6602).
            tempdecl->removeInstance(tempdecl_instance_idx);
            if (target_symbol_list)
            {
                // Because we added 'this' in the last position above, we
                // should be able to remove it without messing other indices up.
                assert((*target_symbol_list)[target_symbol_list_idx] == tempinst);
                target_symbol_list->remove(target_symbol_list_idx);
                tempinst->memberOf = NULL;                    // no longer a member
            }
            tempinst->semanticRun = PASSinit;
            tempinst->inst = NULL;
            tempinst->symtab = NULL;
        }
    }
    else if (errinst)
    {
        /* Bugzilla 14541: If the previous gagged instance had failed by
         * circular references, currrent "error reproduction instantiation"
         * might succeed, because of the difference of instantiated context.
         * On such case, the cached error instance needs to be overridden by the
         * succeeded instance.
         */
        //printf("replaceInstance()\n");
        TemplateInstances *tinstances = (TemplateInstances *)dmd_aaGetRvalue((AA *)tempdecl->instances, (void *)tempinst->hash);
        assert(tinstances);
        for (size_t i = 0; i < tinstances->length; i++)
        {
            TemplateInstance *ti = (*tinstances)[i];
            if (ti == errinst)
            {
                (*tinstances)[i] = tempinst;     // override
                break;
            }
        }
    }
}

// function used to perform semantic on AliasDeclaration
void aliasSemantic(AliasDeclaration *ds, Scope *sc)
{
    //printf("AliasDeclaration::semantic() %s\n", ds->toChars());

    // as AliasDeclaration::semantic, in case we're called first.
    // see https://issues.dlang.org/show_bug.cgi?id=21001
    ds->storage_class |= sc->stc & STCdeprecated;
    ds->protection = sc->protection;
    ds->userAttribDecl = sc->userAttribDecl;

    // TypeTraits needs to know if it's located in an AliasDeclaration
    const unsigned oldflags = sc->flags;
    sc->flags |= SCOPEalias;

    if (ds->aliassym)
    {
        FuncDeclaration *fd = ds->aliassym->isFuncLiteralDeclaration();
        TemplateDeclaration *td = ds->aliassym->isTemplateDeclaration();
        if (fd || (td && td->literal))
        {
            if (fd && fd->semanticRun >= PASSsemanticdone)
            {
                sc->flags = oldflags;
                return;
            }

            Expression *e = new FuncExp(ds->loc, ds->aliassym);
            e = expressionSemantic(e, sc);
            if (e->op == TOKfunction)
            {
                FuncExp *fe = (FuncExp *)e;
                ds->aliassym = fe->td ? (Dsymbol *)fe->td : fe->fd;
            }
            else
            {
                ds->aliassym = NULL;
                ds->type = Type::terror;
            }
            sc->flags = oldflags;
            return;
        }

        if (ds->aliassym->isTemplateInstance())
            dsymbolSemantic(ds->aliassym, sc);
        sc->flags = oldflags;
        return;
    }
    ds->inuse = 1;

    // Given:
    //  alias foo.bar.abc def;
    // it is not knowable from the syntax whether this is an alias
    // for a type or an alias for a symbol. It is up to the semantic()
    // pass to distinguish.
    // If it is a type, then type is set and getType() will return that
    // type. If it is a symbol, then aliassym is set and type is NULL -
    // toAlias() will return aliasssym.

    unsigned int errors = global.errors;
    Type *oldtype = ds->type;

    // Ungag errors when not instantiated DeclDefs scope alias
    Ungag ungag(global.gag);
    //printf("%s parent = %s, gag = %d, instantiated = %d\n", ds->toChars(), ds->parent, global.gag, ds->isInstantiated());
    if (ds->parent && global.gag && !ds->isInstantiated() && !ds->toParent2()->isFuncDeclaration())
    {
        //printf("%s type = %s\n", ds->toPrettyChars(), ds->type->toChars());
        global.gag = 0;
    }

    /* This section is needed because Type::resolve() will:
     *   const x = 3;
     *   alias y = x;
     * try to convert identifier x to 3.
     */
    Dsymbol *s = ds->type->toDsymbol(sc);
    if (errors != global.errors)
    {
        s = NULL;
        ds->type = Type::terror;
    }
    if (s && s == ds)
    {
        ds->error("cannot resolve");
        s = NULL;
        ds->type = Type::terror;
    }
    if (!s || !s->isEnumMember())
    {
        Type *t;
        Expression *e;
        Scope *sc2 = sc;
        if (ds->storage_class & (STCref | STCnothrow | STCnogc | STCpure | STCdisable))
        {
            // For 'ref' to be attached to function types, and picked
            // up by Type::resolve(), it has to go into sc.
            sc2 = sc->push();
            sc2->stc |= ds->storage_class & (STCref | STCnothrow | STCnogc | STCpure | STCshared | STCdisable);
        }
        ds->type = ds->type->addSTC(ds->storage_class);
        ds->type->resolve(ds->loc, sc2, &e, &t, &s);
        if (sc2 != sc)
            sc2->pop();

        if (e)  // Try to convert Expression to Dsymbol
        {
            s = getDsymbol(e);
            if (!s)
            {
                if (e->op != TOKerror)
                    ds->error("cannot alias an expression %s", e->toChars());
                t = Type::terror;
            }
        }
        ds->type = t;
    }
    if (s == ds)
    {
        assert(global.errors);
        ds->type = Type::terror;
        s = NULL;
    }
    if (!s) // it's a type alias
    {
        //printf("alias %s resolved to type %s\n", ds->toChars(), ds->type->toChars());
        ds->type = typeSemantic(ds->type, ds->loc, sc);
        ds->aliassym = NULL;
    }
    else    // it's a symbolic alias
    {
        //printf("alias %s resolved to %s %s\n", ds->toChars(), s->kind(), s->toChars());
        ds->type = NULL;
        ds->aliassym = s;
    }
    if (global.gag && errors != global.errors)
    {
        ds->type = oldtype;
        ds->aliassym = NULL;
    }
    ds->inuse = 0;
    ds->semanticRun = PASSsemanticdone;

    if (Dsymbol *sx = ds->overnext)
    {
        ds->overnext = NULL;

        if (!ds->overloadInsert(sx))
            ScopeDsymbol::multiplyDefined(Loc(), sx, ds);
    }
    sc->flags = oldflags;
}


/*************************************
 * Does semantic analysis on the public face of declarations.
 */
void dsymbolSemantic(Dsymbol *dsym, Scope *sc)
{
    DsymbolSemanticVisitor v(sc);
    dsym->accept(&v);
}
