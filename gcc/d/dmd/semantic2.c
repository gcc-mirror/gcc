
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "dsymbol.h"
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "errors.h"
#include "import.h"
#include "init.h"
#include "module.h"
#include "nspace.h"
#include "objc.h"
#include "scope.h"
#include "staticassert.h"
#include "template.h"
#include "visitor.h"

bool evalStaticCondition(Scope *sc, Expression *exp, Expression *e, bool &errors);
void udaExpressionEval(Scope *sc, Expressions *exps);
Objc *objc();

class Semantic2Visitor : public Visitor
{
public:
    Scope *sc;

    Semantic2Visitor(Scope *sc)
    {
        this->sc = sc;
    }

    void visit(Dsymbol *)
    {
        // Most Dsymbols have no further semantic analysis needed
    }

    void visit(StaticAssert *sa)
    {
        //printf("StaticAssert::semantic2() %s\n", toChars());
        ScopeDsymbol *sds = new ScopeDsymbol();
        sc = sc->push(sds);
        sc->tinst = NULL;
        sc->minst = NULL;

        bool errors = false;
        bool result = evalStaticCondition(sc, sa->exp, sa->exp, errors);
        sc = sc->pop();
        if (errors)
        {
            errorSupplemental(sa->loc, "while evaluating: static assert(%s)", sa->exp->toChars());
        }
        else if (!result)
        {
            if (sa->msg)
            {
                sc = sc->startCTFE();
                sa->msg = expressionSemantic(sa->msg, sc);
                sa->msg = resolveProperties(sc, sa->msg);
                sc = sc->endCTFE();
                sa->msg = sa->msg->ctfeInterpret();
                if (StringExp * se = sa->msg->toStringExp())
                {
                    // same with pragma(msg)
                    se = se->toUTF8(sc);
                    sa->error("\"%.*s\"", (int)se->len, (char *)se->string);
                }
                else
                    sa->error("%s", sa->msg->toChars());
            }
            else
                sa->error("(%s) is false", sa->exp->toChars());
            if (sc->tinst)
                sc->tinst->printInstantiationTrace();
            if (!global.gag)
                fatal();
        }
    }

    void visit(TemplateInstance *tempinst)
    {
        if (tempinst->semanticRun >= PASSsemantic2)
            return;
        tempinst->semanticRun = PASSsemantic2;
        if (!tempinst->errors && tempinst->members)
        {
            TemplateDeclaration *tempdecl = tempinst->tempdecl->isTemplateDeclaration();
            assert(tempdecl);

            sc = tempdecl->_scope;
            assert(sc);
            sc = sc->push(tempinst->argsym);
            sc = sc->push(tempinst);
            sc->tinst = tempinst;
            sc->minst = tempinst->minst;

            int needGagging = (tempinst->gagged && !global.gag);
            unsigned int olderrors = global.errors;
            int oldGaggedErrors = -1;       // dead-store to prevent spurious warning
            if (needGagging)
                oldGaggedErrors = global.startGagging();

            for (size_t i = 0; i < tempinst->members->length; i++)
            {
                Dsymbol *s = (*tempinst->members)[i];
                semantic2(s, sc);
                if (tempinst->gagged && global.errors != olderrors)
                    break;
            }

            if (global.errors != olderrors)
            {
                if (!tempinst->errors)
                {
                    if (!tempdecl->literal)
                        tempinst->error(tempinst->loc, "error instantiating");
                    if (tempinst->tinst)
                        tempinst->tinst->printInstantiationTrace();
                }
                tempinst->errors = true;
            }
            if (needGagging)
                global.endGagging(oldGaggedErrors);

            sc = sc->pop();
            sc->pop();
        }
    }

    void visit(TemplateMixin *tmix)
    {
        if (tmix->semanticRun >= PASSsemantic2)
            return;
        tmix->semanticRun = PASSsemantic2;
        if (tmix->members)
        {
            assert(sc);
            sc = sc->push(tmix->argsym);
            sc = sc->push(tmix);
            for (size_t i = 0; i < tmix->members->length; i++)
            {
                Dsymbol *s = (*tmix->members)[i];
                semantic2(s, sc);
            }
            sc = sc->pop();
            sc->pop();
        }
    }

    void visit(VarDeclaration *vd)
    {
        if (vd->semanticRun < PASSsemanticdone && vd->inuse)
            return;

        //printf("VarDeclaration::semantic2('%s')\n", toChars());

        if (vd->_init && !vd->toParent()->isFuncDeclaration())
        {
            vd->inuse++;

            /* https://issues.dlang.org/show_bug.cgi?id=20280
             *
             * Template instances may import modules that have not
             * finished semantic1.
             */
            if (!vd->type)
                dsymbolSemantic(vd, sc);

            // Bugzilla 14166: Don't run CTFE for the temporary variables inside typeof
            vd->_init = initializerSemantic(vd->_init, sc, vd->type, sc->intypeof == 1 ? INITnointerpret : INITinterpret);
            vd->inuse--;
        }
        if (vd->_init && (vd->storage_class & STCmanifest))
        {
            /* Cannot initializer enums with CTFE classreferences and addresses of struct literals.
             * Scan initializer looking for them. Issue error if found.
             */
            if (ExpInitializer *ei = vd->_init->isExpInitializer())
            {
                struct EnumInitializer
                {
                    static bool arrayHasInvalidEnumInitializer(Expressions *elems)
                    {
                        for (size_t i = 0; i < elems->length; i++)
                        {
                            Expression *e = (*elems)[i];
                            if (e && hasInvalidEnumInitializer(e))
                                return true;
                        }
                        return false;
                    }

                    static bool hasInvalidEnumInitializer(Expression *e)
                    {
                        if (e->op == TOKclassreference)
                            return true;
                        if (e->op == TOKaddress && ((AddrExp *)e)->e1->op == TOKstructliteral)
                            return true;
                        if (e->op == TOKarrayliteral)
                            return arrayHasInvalidEnumInitializer(((ArrayLiteralExp *)e)->elements);
                        if (e->op == TOKstructliteral)
                            return arrayHasInvalidEnumInitializer(((StructLiteralExp *)e)->elements);
                        if (e->op == TOKassocarrayliteral)
                        {
                            AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)e;
                            return arrayHasInvalidEnumInitializer(ae->values) ||
                                arrayHasInvalidEnumInitializer(ae->keys);
                        }
                        return false;
                    }
                };
                if (EnumInitializer::hasInvalidEnumInitializer(ei->exp))
                    vd->error(": Unable to initialize enum with class or pointer to struct. Use static const variable instead.");
            }
        }
        else if (vd->_init && vd->isThreadlocal())
        {
            if ((vd->type->ty == Tclass) && vd->type->isMutable() && !vd->type->isShared())
            {
                ExpInitializer *ei = vd->_init->isExpInitializer();
                if (ei && ei->exp->op == TOKclassreference)
                    vd->error("is mutable. Only const or immutable class thread local variable are allowed, not %s", vd->type->toChars());
            }
            else if (vd->type->ty == Tpointer && vd->type->nextOf()->ty == Tstruct && vd->type->nextOf()->isMutable() && !vd->type->nextOf()->isShared())
            {
                ExpInitializer *ei = vd->_init->isExpInitializer();
                if (ei && ei->exp->op == TOKaddress && ((AddrExp *)ei->exp)->e1->op == TOKstructliteral)
                {
                    vd->error("is a pointer to mutable struct. Only pointers to const, immutable or shared struct thread local variable are allowed, not %s", vd->type->toChars());
                }
            }
        }
        vd->semanticRun = PASSsemantic2done;
    }

    void visit(Module *mod)
    {
        //printf("Module::semantic2('%s'): parent = %p\n", toChars(), mod->parent);
        if (mod->semanticRun != PASSsemanticdone)       // semantic() not completed yet - could be recursive call
            return;
        mod->semanticRun = PASSsemantic2;

        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope *sc = Scope::createGlobal(mod);      // create root scope
        //printf("Module = %p\n", sc.scopesym);

        // Pass 2 semantic routines: do initializers and function bodies
        for (size_t i = 0; i < mod->members->length; i++)
        {
            Dsymbol *s = (*mod->members)[i];
            semantic2(s, sc);
        }

        if (mod->userAttribDecl)
        {
            semantic2(mod->userAttribDecl, sc);
        }

        sc = sc->pop();
        sc->pop();
        mod->semanticRun = PASSsemantic2done;
        //printf("-Module::semantic2('%s'): parent = %p\n", toChars(), mod->parent);
    }

    void visit(FuncDeclaration *fd)
    {
        if (fd->semanticRun >= PASSsemantic2done)
            return;

        if (fd->semanticRun < PASSsemanticdone && !fd->errors)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=21614
             *
             * Template instances may import modules that have not
             * finished semantic1.
             */
            dsymbolSemantic(fd, sc);
        }

        assert(fd->semanticRun <= PASSsemantic2);
        fd->semanticRun = PASSsemantic2;

        objc()->setSelector(fd, sc);
        objc()->validateSelector(fd);

        if (fd->parent->isClassDeclaration())
        {
            objc()->checkLinkage(fd);
        }
        if (!fd->type || fd->type->ty != Tfunction)
            return;
        TypeFunction *f = fd->type->toTypeFunction();
        const size_t nparams = f->parameterList.length();
        // semantic for parameters' UDAs
        for (size_t i = 0; i < nparams; i++)
        {
            Parameter *param = f->parameterList[i];
            if (param && param->userAttribDecl)
                semantic2(param->userAttribDecl, sc);
        }
    }

    void visit(Import *i)
    {
        //printf("Import::semantic2('%s')\n", toChars());
        if (i->mod)
        {
            semantic2(i->mod, NULL);
            if (i->mod->needmoduleinfo)
            {
                //printf("module5 %s because of %s\n", sc->_module->toChars(), i->mod->toChars());
                if (sc)
                    sc->_module->needmoduleinfo = 1;
            }
        }
    }

    void visit(Nspace *ns)
    {
        if (ns->semanticRun >= PASSsemantic2)
            return;
        ns->semanticRun = PASSsemantic2;
        if (ns->members)
        {
            assert(sc);
            sc = sc->push(ns);
            sc->linkage = LINKcpp;
            for (size_t i = 0; i < ns->members->length; i++)
            {
                Dsymbol *s = (*ns->members)[i];
                semantic2(s, sc);
            }
            sc->pop();
        }
    }

    void visit(AttribDeclaration *ad)
    {
        Dsymbols *d = ad->include(sc);

        if (d)
        {
            Scope *sc2 = ad->newScope(sc);

            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                semantic2(s, sc2);
            }

            if (sc2 != sc)
                sc2->pop();
        }
    }

    /**
     * Run the DeprecatedDeclaration's semantic2 phase then its members.
     *
     * The message set via a `DeprecatedDeclaration` can be either of:
     * - a string literal
     * - an enum
     * - a static immutable
     * So we need to call ctfe to resolve it.
     * Afterward forwards to the members' semantic2.
     */
    void visit(DeprecatedDeclaration *dd)
    {
        dd->getMessage();
        visit((AttribDeclaration *)dd);
    }

    void visit(AlignDeclaration *ad)
    {
        ad->getAlignment(sc);
        visit((AttribDeclaration *)ad);
    }

    void visit(UserAttributeDeclaration *uad)
    {
        if (uad->decl && uad->atts && uad->atts->length && uad->_scope)
        {
            uad->_scope = NULL;
            udaExpressionEval(sc, uad->atts);
        }
        visit((AttribDeclaration *)uad);
    }

    void visit(AggregateDeclaration *ad)
    {
        //printf("AggregateDeclaration::semantic2(%s) type = %s, errors = %d\n", toChars(), ad->type->toChars(), ad->errors);
        if (!ad->members)
            return;

        if (ad->_scope)
        {
            ad->error("has forward references");
            return;
        }

        Scope *sc2 = ad->newScope(sc);

        ad->determineSize(ad->loc);

        for (size_t i = 0; i < ad->members->length; i++)
        {
            Dsymbol *s = (*ad->members)[i];
            //printf("\t[%d] %s\n", i, s->toChars());
            semantic2(s, sc2);
        }

        sc2->pop();
    }
};

/*************************************
 * Does semantic analysis on initializers and members of aggregates.
 */
void semantic2(Dsymbol *dsym, Scope *sc)
{
    Semantic2Visitor v(sc);
    dsym->accept(&v);
}
