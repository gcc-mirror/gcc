
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Dave Fladebo
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/hdrgen.c
 */

// Routines to emit header files

#include "root/dsystem.h"
#include "root/rmem.h"

#include "mars.h"
#include "id.h"
#include "init.h"

#include "attrib.h"
#include "cond.h"
#include "doc.h"
#include "enum.h"
#include "import.h"
#include "module.h"
#include "mtype.h"
#include "parse.h"
#include "scope.h"
#include "staticassert.h"
#include "target.h"
#include "template.h"
#include "utf.h"
#include "version.h"

#include "declaration.h"
#include "aggregate.h"
#include "expression.h"
#include "ctfe.h"
#include "statement.h"
#include "aliasthis.h"
#include "nspace.h"
#include "hdrgen.h"

void linkageToBuffer(OutBuffer *buf, LINK linkage);
void MODtoBuffer(OutBuffer *buf, MOD mod);

void genhdrfile(Module *m)
{
    OutBuffer buf;
    buf.doindent = 1;

    buf.printf("// D import file generated from '%s'", m->srcfile->toChars());
    buf.writenl();

    HdrGenState hgs;
    hgs.hdrgen = true;

    toCBuffer(m, &buf, &hgs);

    // Transfer image to file
    m->hdrfile->setbuffer(buf.slice().ptr, buf.length());
    buf.extractData();

    ensurePathToNameExists(Loc(), m->hdrfile->toChars());
    writeFile(m->loc, m->hdrfile);
}

/**
 * Dumps the full contents of module `m` to `buf`.
 * Params:
 *   buf = buffer to write to.
 *   m = module to visit all members of.
 */
void moduleToBuffer(OutBuffer *buf, Module *m)
{
    HdrGenState hgs;
    hgs.fullDump = true;
    toCBuffer(m, buf, &hgs);
}

class PrettyPrintVisitor : public Visitor
{
public:
    OutBuffer *buf;
    HdrGenState *hgs;
    bool declstring; // set while declaring alias for string,wstring or dstring
    EnumDeclaration *inEnumDecl;

    PrettyPrintVisitor(OutBuffer *buf, HdrGenState *hgs)
        : buf(buf), hgs(hgs), declstring(false), inEnumDecl(NULL)
    {
    }

    void visit(Statement *)
    {
        buf->printf("Statement::toCBuffer()");
        buf->writenl();
        assert(0);
    }

    void visit(ErrorStatement *)
    {
        buf->printf("__error__");
        buf->writenl();
    }

    void visit(ExpStatement *s)
    {
        if (s->exp && s->exp->op == TOKdeclaration)
        {
            // bypass visit(DeclarationExp)
            ((DeclarationExp *)s->exp)->declaration->accept(this);
            return;
        }
        if (s->exp)
            s->exp->accept(this);
        buf->writeByte(';');
        if (!hgs->forStmtInit)
            buf->writenl();
    }

    void visit(CompileStatement *s)
    {
        buf->writestring("mixin(");
        argsToBuffer(s->exps);
        buf->writestring(");");
        if (!hgs->forStmtInit)
            buf->writenl();
    }

    void visit(CompoundStatement *s)
    {
        for (size_t i = 0; i < s->statements->length; i++)
        {
            Statement *sx = (*s->statements)[i];
            if (sx)
                sx->accept(this);
        }
    }

    void visit(CompoundDeclarationStatement *s)
    {
        bool anywritten = false;
        for (size_t i = 0; i < s->statements->length; i++)
        {
            Statement *sx = (*s->statements)[i];
            ExpStatement *ds = sx ? sx->isExpStatement() : NULL;
            if (ds && ds->exp->op == TOKdeclaration)
            {
                Dsymbol *d = ((DeclarationExp *)ds->exp)->declaration;
                assert(d->isDeclaration());
                if (VarDeclaration *v = d->isVarDeclaration())
                    visitVarDecl(v, anywritten);
                else
                    d->accept(this);
                anywritten = true;
            }
        }
        buf->writeByte(';');
        if (!hgs->forStmtInit)
            buf->writenl();
    }

    void visit(UnrolledLoopStatement *s)
    {
        buf->writestring("unrolled {");
        buf->writenl();
        buf->level++;

        for (size_t i = 0; i < s->statements->length; i++)
        {
            Statement *sx = (*s->statements)[i];
            if (sx)
                sx->accept(this);
        }

        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(ScopeStatement *s)
    {
        buf->writeByte('{');
        buf->writenl();
        buf->level++;

        if (s->statement)
            s->statement->accept(this);

        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(WhileStatement *s)
    {
        buf->writestring("while (");
        s->condition->accept(this);
        buf->writeByte(')');
        buf->writenl();
        if (s->_body)
            s->_body->accept(this);
    }

    void visit(DoStatement *s)
    {
        buf->writestring("do");
        buf->writenl();
        if (s->_body)
            s->_body->accept(this);
        buf->writestring("while (");
        s->condition->accept(this);
        buf->writestring(");");
        buf->writenl();
    }

    void visit(ForStatement *s)
    {
        buf->writestring("for (");
        if (s->_init)
        {
            hgs->forStmtInit++;
            s->_init->accept(this);
            hgs->forStmtInit--;
        }
        else
            buf->writeByte(';');
        if (s->condition)
        {
            buf->writeByte(' ');
            s->condition->accept(this);
        }
        buf->writeByte(';');
        if (s->increment)
        {
            buf->writeByte(' ');
            s->increment->accept(this);
        }
        buf->writeByte(')');
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        if (s->_body)
            s->_body->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void foreachWithoutBody(ForeachStatement *s)
    {
        buf->writestring(Token::toChars(s->op));
        buf->writestring(" (");
        for (size_t i = 0; i < s->parameters->length; i++)
        {
            Parameter *p = (*s->parameters)[i];
            if (i)
                buf->writestring(", ");
            if (stcToBuffer(buf, p->storageClass))
                buf->writeByte(' ');
            if (p->type)
                typeToBuffer(p->type, p->ident);
            else
                buf->writestring(p->ident->toChars());
        }
        buf->writestring("; ");
        s->aggr->accept(this);
        buf->writeByte(')');
        buf->writenl();
    }

    void visit(ForeachStatement *s)
    {
        foreachWithoutBody(s);
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        if (s->_body)
            s->_body->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void foreachRangeWithoutBody(ForeachRangeStatement *s)
    {
        buf->writestring(Token::toChars(s->op));
        buf->writestring(" (");

        if (s->prm->type)
            typeToBuffer(s->prm->type, s->prm->ident);
        else
            buf->writestring(s->prm->ident->toChars());

        buf->writestring("; ");
        s->lwr->accept(this);
        buf->writestring(" .. ");
        s->upr->accept(this);
        buf->writeByte(')');
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
    }

    void visit(ForeachRangeStatement *s)
    {
        foreachRangeWithoutBody(s);
        buf->level++;
        if (s->_body)
            s->_body->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(StaticForeachStatement *s)
    {
        buf->writestring("static ");
        if (s->sfe->aggrfe)
        {
            visit(s->sfe->aggrfe);
        }
        else
        {
            assert(s->sfe->rangefe);
            visit(s->sfe->rangefe);
        }
    }

    void visit(IfStatement *s)
    {
        buf->writestring("if (");
        if (Parameter *p = s->prm)
        {
            StorageClass stc = p->storageClass;
            if (!p->type && !stc)
                stc = STCauto;
            if (stcToBuffer(buf, stc))
                buf->writeByte(' ');
            if (p->type)
                typeToBuffer(p->type, p->ident);
            else
                buf->writestring(p->ident->toChars());
            buf->writestring(" = ");
        }
        s->condition->accept(this);
        buf->writeByte(')');
        buf->writenl();
        if (s->ifbody->isScopeStatement())
        {
            s->ifbody->accept(this);
        }
        else
        {
            buf->level++;
            s->ifbody->accept(this);
            buf->level--;
        }
        if (s->elsebody)
        {
            buf->writestring("else");
            if (!s->elsebody->isIfStatement())
            {
                buf->writenl();
            }
            else
            {
                buf->writeByte(' ');
            }
            if (s->elsebody->isScopeStatement() || s->elsebody->isIfStatement())
            {
                s->elsebody->accept(this);
            }
            else
            {
                buf->level++;
                s->elsebody->accept(this);
                buf->level--;
            }
        }
    }

    void visit(ConditionalStatement *s)
    {
        s->condition->accept(this);
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        if (s->ifbody)
            s->ifbody->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
        if (s->elsebody)
        {
            buf->writestring("else");
            buf->writenl();
            buf->writeByte('{');
            buf->level++;
            buf->writenl();
            s->elsebody->accept(this);
            buf->level--;
            buf->writeByte('}');
        }
        buf->writenl();
    }

    void visit(PragmaStatement *s)
    {
        buf->writestring("pragma (");
        buf->writestring(s->ident->toChars());
        if (s->args && s->args->length)
        {
            buf->writestring(", ");
            argsToBuffer(s->args);
        }
        buf->writeByte(')');
        if (s->_body)
        {
            buf->writenl();
            buf->writeByte('{');
            buf->writenl();
            buf->level++;

            s->_body->accept(this);

            buf->level--;
            buf->writeByte('}');
            buf->writenl();
        }
        else
        {
            buf->writeByte(';');
            buf->writenl();
        }
    }

    void visit(StaticAssertStatement *s)
    {
        s->sa->accept(this);
    }

    void visit(SwitchStatement *s)
    {
        buf->writestring(s->isFinal ? "final switch (" : "switch (");
        s->condition->accept(this);
        buf->writeByte(')');
        buf->writenl();
        if (s->_body)
        {
            if (!s->_body->isScopeStatement())
            {
                buf->writeByte('{');
                buf->writenl();
                buf->level++;
                s->_body->accept(this);
                buf->level--;
                buf->writeByte('}');
                buf->writenl();
            }
            else
            {
                s->_body->accept(this);
            }
        }
    }

    void visit(CaseStatement *s)
    {
        buf->writestring("case ");
        s->exp->accept(this);
        buf->writeByte(':');
        buf->writenl();
        s->statement->accept(this);
    }

    void visit(CaseRangeStatement *s)
    {
        buf->writestring("case ");
        s->first->accept(this);
        buf->writestring(": .. case ");
        s->last->accept(this);
        buf->writeByte(':');
        buf->writenl();
        s->statement->accept(this);
    }

    void visit(DefaultStatement *s)
    {
        buf->writestring("default:");
        buf->writenl();
        s->statement->accept(this);
    }

    void visit(GotoDefaultStatement *)
    {
        buf->writestring("goto default;");
        buf->writenl();
    }

    void visit(GotoCaseStatement *s)
    {
        buf->writestring("goto case");
        if (s->exp)
        {
            buf->writeByte(' ');
            s->exp->accept(this);
        }
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(SwitchErrorStatement *)
    {
        buf->writestring("SwitchErrorStatement::toCBuffer()");
        buf->writenl();
    }

    void visit(ReturnStatement *s)
    {
        buf->printf("return ");
        if (s->exp)
            s->exp->accept(this);
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(BreakStatement *s)
    {
        buf->writestring("break");
        if (s->ident)
        {
            buf->writeByte(' ');
            buf->writestring(s->ident->toChars());
        }
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(ContinueStatement *s)
    {
        buf->writestring("continue");
        if (s->ident)
        {
            buf->writeByte(' ');
            buf->writestring(s->ident->toChars());
        }
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(SynchronizedStatement *s)
    {
        buf->writestring("synchronized");
        if (s->exp)
        {
            buf->writeByte('(');
            s->exp->accept(this);
            buf->writeByte(')');
        }
        if (s->_body)
        {
            buf->writeByte(' ');
            s->_body->accept(this);
        }
    }

    void visit(WithStatement *s)
    {
        buf->writestring("with (");
        s->exp->accept(this);
        buf->writestring(")");
        buf->writenl();
        if (s->_body)
            s->_body->accept(this);
    }

    void visit(TryCatchStatement *s)
    {
        buf->writestring("try");
        buf->writenl();
        if (s->_body)
            s->_body->accept(this);
        for (size_t i = 0; i < s->catches->length; i++)
        {
            Catch *c = (*s->catches)[i];
            visit(c);
        }
    }

    void visit(TryFinallyStatement *s)
    {
        buf->writestring("try");
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        s->_body->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
        buf->writestring("finally");
        buf->writenl();
        if (s->finalbody->isScopeStatement())
        {
            s->finalbody->accept(this);
        }
        else
        {
            buf->level++;
            s->finalbody->accept(this);
            buf->level--;
        }
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(ScopeGuardStatement *s)
    {
        buf->writestring(Token::toChars(s->tok));
        buf->writeByte(' ');
        s->statement->accept(this);
    }

    void visit(ThrowStatement *s)
    {
        buf->printf("throw ");
        s->exp->accept(this);
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(DebugStatement *s)
    {
        if (s->statement)
        {
            s->statement->accept(this);
        }
    }

    void visit(GotoStatement *s)
    {
        buf->writestring("goto ");
        buf->writestring(s->ident->toChars());
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(LabelStatement *s)
    {
        buf->writestring(s->ident->toChars());
        buf->writeByte(':');
        buf->writenl();
        if (s->statement)
            s->statement->accept(this);
    }

    void visit(AsmStatement *s)
    {
        buf->writestring("asm { ");
        Token *t = s->tokens;
        buf->level++;
        while (t)
        {
            buf->writestring(t->toChars());
            if (t->next &&
                t->value != TOKmin      &&
                t->value != TOKcomma    && t->next->value != TOKcomma    &&
                t->value != TOKlbracket && t->next->value != TOKlbracket &&
                                           t->next->value != TOKrbracket &&
                t->value != TOKlparen   && t->next->value != TOKlparen   &&
                                           t->next->value != TOKrparen   &&
                t->value != TOKdot      && t->next->value != TOKdot)
            {
                buf->writeByte(' ');
            }
            t = t->next;
        }
        buf->level--;
        buf->writestring("; }");
        buf->writenl();
    }

    void visit(ImportStatement *s)
    {
        for (size_t i = 0; i < s->imports->length; i++)
        {
            Dsymbol *imp = (*s->imports)[i];
            imp->accept(this);
        }
    }

    void visit(Catch *c)
    {
        buf->writestring("catch");
        if (c->type)
        {
            buf->writeByte('(');
            typeToBuffer(c->type, c->ident);
            buf->writeByte(')');
        }
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        if (c->handler)
            c->handler->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    ////////////////////////////////////////////////////////////////////////////

    /**************************************************
     * An entry point to pretty-print type.
     */
    void typeToBuffer(Type *t, Identifier *ident)
    {
        if (t->ty == Tfunction)
        {
            visitFuncIdentWithPrefix((TypeFunction *)t, ident, NULL);
            return;
        }

        visitWithMask(t, 0);

        if (ident)
        {
            buf->writeByte(' ');
            buf->writestring(ident->toChars());
        }
    }

    void visitWithMask(Type *t, unsigned char modMask)
    {
        // Tuples and functions don't use the type constructor syntax
        if (modMask == t->mod ||
            t->ty == Tfunction ||
            t->ty == Ttuple)
        {
            t->accept(this);
        }
        else
        {
            unsigned char m = t->mod & ~(t->mod & modMask);
            if (m & MODshared)
            {
                MODtoBuffer(buf, MODshared);
                buf->writeByte('(');
            }
            if (m & MODwild)
            {
                MODtoBuffer(buf, MODwild);
                buf->writeByte('(');
            }
            if (m & (MODconst | MODimmutable))
            {
                MODtoBuffer(buf, m & (MODconst | MODimmutable));
                buf->writeByte('(');
            }

            t->accept(this);

            if (m & (MODconst | MODimmutable))
                buf->writeByte(')');
            if (m & MODwild)
                buf->writeByte(')');
            if (m & MODshared)
                buf->writeByte(')');
        }
    }

    void visit(Type *t)
    {
        printf("t = %p, ty = %d\n", t, t->ty);
        assert(0);
    }

    void visit(TypeError *)
    {
        buf->writestring("_error_");
    }

    void visit(TypeBasic *t)
    {
        //printf("TypeBasic::toCBuffer2(t->mod = %d)\n", t->mod);
        buf->writestring(t->dstring);
    }

    void visit(TypeTraits *t)
    {
        //printf("TypeBasic::toCBuffer2(t.mod = %d)\n", t.mod);
        t->exp->accept(this);
    }

    void visit(TypeVector *t)
    {
        //printf("TypeVector::toCBuffer2(t->mod = %d)\n", t->mod);
        buf->writestring("__vector(");
        visitWithMask(t->basetype, t->mod);
        buf->writestring(")");
    }

    void visit(TypeSArray *t)
    {
        visitWithMask(t->next, t->mod);
        buf->writeByte('[');
        sizeToBuffer(t->dim);
        buf->writeByte(']');
    }

    void visit(TypeDArray *t)
    {
        Type *ut = t->castMod(0);
        if (declstring)
            goto L1;
        if (ut->equals(Type::tstring))
            buf->writestring("string");
        else if (ut->equals(Type::twstring))
            buf->writestring("wstring");
        else if (ut->equals(Type::tdstring))
            buf->writestring("dstring");
        else
        {
        L1:
            visitWithMask(t->next, t->mod);
            buf->writestring("[]");
        }
    }

    void visit(TypeAArray *t)
    {
        visitWithMask(t->next, t->mod);
        buf->writeByte('[');
        visitWithMask(t->index, 0);
        buf->writeByte(']');
    }

    void visit(TypePointer *t)
    {
        //printf("TypePointer::toCBuffer2() next = %d\n", t->next->ty);
        if (t->next->ty == Tfunction)
            visitFuncIdentWithPostfix((TypeFunction *)t->next, "function");
        else
        {
            visitWithMask(t->next, t->mod);
            buf->writeByte('*');
        }
    }

    void visit(TypeReference *t)
    {
        visitWithMask(t->next, t->mod);
        buf->writeByte('&');
    }

    void visit(TypeFunction *t)
    {
        //printf("TypeFunction::toCBuffer2() t = %p, ref = %d\n", t, t->isref);
        visitFuncIdentWithPostfix(t, NULL);
    }

    // callback for TypeFunction::attributesApply
    struct PrePostAppendStrings
    {
        OutBuffer *buf;
        bool isPostfixStyle;
        bool isCtor;

        static int fp(void *param, const char *str)
        {
            PrePostAppendStrings *p = (PrePostAppendStrings *)param;

            // don't write 'ref' for ctors
            if (p->isCtor && strcmp(str, "ref") == 0)
                return 0;

            if ( p->isPostfixStyle) p->buf->writeByte(' ');
            p->buf->writestring(str);
            if (!p->isPostfixStyle) p->buf->writeByte(' ');
            return 0;
        }
    };

    void visitFuncIdentWithPostfix(TypeFunction *t, const char *ident)
    {
        if (t->inuse)
        {
            t->inuse = 2;              // flag error to caller
            return;
        }
        t->inuse++;

        PrePostAppendStrings pas;
        pas.buf = buf;
        pas.isCtor = false;
        pas.isPostfixStyle = true;

        if (t->linkage > LINKd && hgs->ddoc != 1 && !hgs->hdrgen)
        {
            linkageToBuffer(buf, t->linkage);
            buf->writeByte(' ');
        }

        if (t->next)
        {
            typeToBuffer(t->next, NULL);
            if (ident)
                buf->writeByte(' ');
        }
        else if (hgs->ddoc)
            buf->writestring("auto ");

        if (ident)
            buf->writestring(ident);

        parametersToBuffer(t->parameterList.parameters, t->parameterList.varargs);

        /* Use postfix style for attributes
         */
        if (t->mod)
        {
            buf->writeByte(' ');
            MODtoBuffer(buf, t->mod);
        }
        t->attributesApply(&pas, &PrePostAppendStrings::fp);

        t->inuse--;
    }
    void visitFuncIdentWithPrefix(TypeFunction *t, Identifier *ident, TemplateDeclaration *td)
    {
        if (t->inuse)
        {
            t->inuse = 2;              // flag error to caller
            return;
        }
        t->inuse++;

        PrePostAppendStrings pas;
        pas.buf = buf;
        pas.isCtor = (ident == Id::ctor);
        pas.isPostfixStyle = false;

        /* Use 'storage class' (prefix) style for attributes
         */
        if (t->mod)
        {
            MODtoBuffer(buf, t->mod);
            buf->writeByte(' ');
        }
        t->attributesApply(&pas, &PrePostAppendStrings::fp);

        if (t->linkage > LINKd && hgs->ddoc != 1 && !hgs->hdrgen)
        {
            linkageToBuffer(buf, t->linkage);
            buf->writeByte(' ');
        }

        if (ident && ident->toHChars2() != ident->toChars())
        {
            // Don't print return type for ctor, dtor, unittest, etc
        }
        else if (t->next)
        {
            typeToBuffer(t->next, NULL);
            if (ident)
                buf->writeByte(' ');
        }
        else if (hgs->ddoc)
            buf->writestring("auto ");

        if (ident)
            buf->writestring(ident->toHChars2());

        if (td)
        {
            buf->writeByte('(');
            for (size_t i = 0; i < td->origParameters->length; i++)
            {
                TemplateParameter *p = (*td->origParameters)[i];
                if (i)
                    buf->writestring(", ");
                p->accept(this);
            }
            buf->writeByte(')');
        }
        parametersToBuffer(t->parameterList.parameters, t->parameterList.varargs);

        t->inuse--;
    }

    void visit(TypeDelegate *t)
    {
        visitFuncIdentWithPostfix((TypeFunction *)t->next, "delegate");
    }

    void visitTypeQualifiedHelper(TypeQualified *t)
    {
        for (size_t i = 0; i < t->idents.length; i++)
        {
            RootObject *id = t->idents[i];

            if (id->dyncast() == DYNCAST_DSYMBOL)
            {
                buf->writeByte('.');
                TemplateInstance *ti = (TemplateInstance *)id;
                ti->accept(this);
            }
            else if (id->dyncast() == DYNCAST_EXPRESSION)
            {
                buf->writeByte('[');
                ((Expression *)id)->accept(this);
                buf->writeByte(']');
            }
            else if (id->dyncast() == DYNCAST_TYPE)
            {
                buf->writeByte('[');
                ((Type *)id)->accept(this);
                buf->writeByte(']');
            }
            else
            {
                buf->writeByte('.');
                buf->writestring(id->toChars());
            }
        }
    }

    void visit(TypeIdentifier *t)
    {
        buf->writestring(t->ident->toChars());
        visitTypeQualifiedHelper(t);
    }

    void visit(TypeInstance *t)
    {
        t->tempinst->accept(this);
        visitTypeQualifiedHelper(t);
    }

    void visit(TypeTypeof *t)
    {
        buf->writestring("typeof(");
        t->exp->accept(this);
        buf->writeByte(')');
        visitTypeQualifiedHelper(t);
    }

    void visit(TypeReturn *t)
    {
        buf->writestring("typeof(return)");
        visitTypeQualifiedHelper(t);
    }

    void visit(TypeEnum *t)
    {
        buf->writestring(t->sym->toChars());
    }

    void visit(TypeStruct *t)
    {
        // Bugzilla 13776: Don't use ti->toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance *ti = t->sym->parent ? t->sym->parent->isTemplateInstance() : NULL;
        if (ti && ti->aliasdecl == t->sym)
            buf->writestring(hgs->fullQual ? ti->toPrettyChars() : ti->toChars());
        else
            buf->writestring(hgs->fullQual ? t->sym->toPrettyChars() : t->sym->toChars());
    }

    void visit(TypeClass *t)
    {
        // Bugzilla 13776: Don't use ti->toAlias() to avoid forward reference error
        // while printing messages.
        TemplateInstance *ti = t->sym->parent->isTemplateInstance();
        if (ti && ti->aliasdecl == t->sym)
            buf->writestring(hgs->fullQual ? ti->toPrettyChars() : ti->toChars());
        else
            buf->writestring(hgs->fullQual ? t->sym->toPrettyChars() : t->sym->toChars());
    }

    void visit(TypeTuple *t)
    {
        parametersToBuffer(t->arguments, 0);
    }

    void visit(TypeSlice *t)
    {
        visitWithMask(t->next, t->mod);

        buf->writeByte('[');
        sizeToBuffer(t->lwr);
        buf->writestring(" .. ");
        sizeToBuffer(t->upr);
        buf->writeByte(']');
    }

    void visit(TypeNull *)
    {
        buf->writestring("typeof(null)");
    }

    void visit(TypeMixin *t)
    {
        buf->writestring("mixin(");
        argsToBuffer(t->exps);
        buf->writeByte(')');
    }

    void visit(TypeNoreturn *)
    {
        buf->writestring("noreturn");
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(Dsymbol *s)
    {
        buf->writestring(s->toChars());
    }

    void visit(StaticAssert *s)
    {
        buf->writestring(s->kind());
        buf->writeByte('(');
        s->exp->accept(this);
        if (s->msg)
        {
            buf->writestring(", ");
            s->msg->accept(this);
        }
        buf->writestring(");");
        buf->writenl();
    }

    void visit(DebugSymbol *s)
    {
        buf->writestring("debug = ");
        if (s->ident)
            buf->writestring(s->ident->toChars());
        else
            buf->printf("%u", s->level);
        buf->writestring(";");
        buf->writenl();
    }

    void visit(VersionSymbol *s)
    {
        buf->writestring("version = ");
        if (s->ident)
            buf->writestring(s->ident->toChars());
        else
            buf->printf("%u", s->level);
        buf->writestring(";");
        buf->writenl();
    }

    void visit(EnumMember *em)
    {
        if (em->type)
            typeToBuffer(em->type, em->ident);
        else
            buf->writestring(em->ident->toChars());
        if (em->value())
        {
            buf->writestring(" = ");
            em->value()->accept(this);
        }
    }

    void visit(Import *imp)
    {
        if (hgs->hdrgen && imp->id == Id::object)
            return;         // object is imported by default

        if (imp->isstatic)
            buf->writestring("static ");
        buf->writestring("import ");
        if (imp->aliasId)
        {
            buf->printf("%s = ", imp->aliasId->toChars());
        }
        if (imp->packages && imp->packages->length)
        {
            for (size_t i = 0; i < imp->packages->length; i++)
            {
                Identifier *pid = (*imp->packages)[i];
                buf->printf("%s.", pid->toChars());
            }
        }
        buf->printf("%s", imp->id->toChars());
        if (imp->names.length)
        {
            buf->writestring(" : ");
            for (size_t i = 0; i < imp->names.length; i++)
            {
                if (i)
                    buf->writestring(", ");

                Identifier *name = imp->names[i];
                Identifier *alias = imp->aliases[i];
                if (alias)
                    buf->printf("%s = %s", alias->toChars(), name->toChars());
                else
                    buf->printf("%s", name->toChars());
            }
        }
        buf->printf(";");
        buf->writenl();
    }

    void visit(AliasThis *d)
    {
        buf->writestring("alias ");
        buf->writestring(d->ident->toChars());
        buf->writestring(" this;\n");
    }

    void visit(AttribDeclaration *d)
    {
        if (!d->decl)
        {
            buf->writeByte(';');
            buf->writenl();
            return;
        }

        if (d->decl->length == 0)
            buf->writestring("{}");
        else if (hgs->hdrgen && d->decl->length == 1 && (*d->decl)[0]->isUnitTestDeclaration())
        {
            // hack for bugzilla 8081
            buf->writestring("{}");
        }
        else if (d->decl->length == 1)
        {
            ((*d->decl)[0])->accept(this);
            return;
        }
        else
        {
            buf->writenl();
            buf->writeByte('{');
            buf->writenl();
            buf->level++;
            for (size_t i = 0; i < d->decl->length; i++)
            {
                Dsymbol *de = (*d->decl)[i];
                de->accept(this);
            }
            buf->level--;
            buf->writeByte('}');
        }
        buf->writenl();
    }

    void visit(StorageClassDeclaration *d)
    {
        if (stcToBuffer(buf, d->stc))
            buf->writeByte(' ');
        visit((AttribDeclaration *)d);
    }

    void visit(DeprecatedDeclaration *d)
    {
        buf->writestring("deprecated(");
        d->msg->accept(this);
        buf->writestring(") ");
        visit((AttribDeclaration *)d);
    }

    void visit(LinkDeclaration *d)
    {
        const char *p;

        switch (d->linkage)
        {
            case LINKd:             p = "D";                break;
            case LINKc:             p = "C";                break;
            case LINKcpp:           p = "C++";              break;
            case LINKwindows:       p = "Windows";          break;
            case LINKobjc:          p = "Objective-C";      break;
            default:
                assert(0);
                break;
        }
        buf->writestring("extern (");
        buf->writestring(p);
        buf->writestring(") ");
        visit((AttribDeclaration *)d);
    }

    void visit(CPPMangleDeclaration *d)
    {
        const char *p;

        switch (d->cppmangle)
        {
            case CPPMANGLEclass:    p = "class";            break;
            case CPPMANGLEstruct:   p = "struct";           break;
            default:
                assert(0);
                break;
        }
        buf->writestring("extern (C++, ");
        buf->writestring(p);
        buf->writestring(") ");
        visit((AttribDeclaration *)d);
    }

    void visit(ProtDeclaration *d)
    {
        protectionToBuffer(buf, d->protection);
        buf->writeByte(' ');
        visit((AttribDeclaration *)d);
    }

    void visit(AlignDeclaration *d)
    {
        if (!d->ealign)
            buf->printf("align ");
        else
            buf->printf("align (%s)", d->ealign->toChars());
        visit((AttribDeclaration *)d);
    }

    void visit(AnonDeclaration *d)
    {
        buf->printf(d->isunion ? "union" : "struct");
        buf->writenl();
        buf->writestring("{");
        buf->writenl();
        buf->level++;
        if (d->decl)
        {
            for (size_t i = 0; i < d->decl->length; i++)
            {
                Dsymbol *de = (*d->decl)[i];
                de->accept(this);
            }
        }
        buf->level--;
        buf->writestring("}");
        buf->writenl();
    }

    void visit(PragmaDeclaration *d)
    {
        buf->printf("pragma (%s", d->ident->toChars());
        if (d->args && d->args->length)
        {
            buf->writestring(", ");
            argsToBuffer(d->args);
        }
        buf->writeByte(')');
        visit((AttribDeclaration *)d);
    }

    void visit(ConditionalDeclaration *d)
    {
        d->condition->accept(this);
        if (d->decl || d->elsedecl)
        {
            buf->writenl();
            buf->writeByte('{');
            buf->writenl();
            buf->level++;
            if (d->decl)
            {
                for (size_t i = 0; i < d->decl->length; i++)
                {
                    Dsymbol *de = (*d->decl)[i];
                    de->accept(this);
                }
            }
            buf->level--;
            buf->writeByte('}');
            if (d->elsedecl)
            {
                buf->writenl();
                buf->writestring("else");
                buf->writenl();
                buf->writeByte('{');
                buf->writenl();
                buf->level++;
                for (size_t i = 0; i < d->elsedecl->length; i++)
                {
                    Dsymbol *de = (*d->elsedecl)[i];
                    de->accept(this);
                }
                buf->level--;
                buf->writeByte('}');
            }
        }
        else
            buf->writeByte(':');
        buf->writenl();
    }

    void visit(ForwardingStatement *s)
    {
        s->statement->accept(this);
    }

    void visit(StaticForeachDeclaration *s)
    {
        buf->writestring("static ");
        if (s->sfe->aggrfe)
        {
            foreachWithoutBody(s->sfe->aggrfe);
        }
        else
        {
            assert(s->sfe->rangefe);
            foreachRangeWithoutBody(s->sfe->rangefe);
        }
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        visit((AttribDeclaration *)s);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(CompileDeclaration *d)
    {
        buf->writestring("mixin(");
        argsToBuffer(d->exps);
        buf->writestring(");");
        buf->writenl();
    }

    void visit(UserAttributeDeclaration *d)
    {
        buf->writestring("@(");
        argsToBuffer(d->atts);
        buf->writeByte(')');
        visit((AttribDeclaration *)d);
    }

    void visit(TemplateDeclaration *d)
    {
        if ((hgs->hdrgen || hgs->fullDump) && visitEponymousMember(d))
            return;

        if (hgs->ddoc)
            buf->writestring(d->kind());
        else
            buf->writestring("template");
        buf->writeByte(' ');
        buf->writestring(d->ident->toChars());
        buf->writeByte('(');
        visitTemplateParameters(hgs->ddoc ? d->origParameters : d->parameters);
        buf->writeByte(')');
        visitTemplateConstraint(d->constraint);

        if (hgs->hdrgen || hgs->fullDump)
        {
            hgs->tpltMember++;
            buf->writenl();
            buf->writeByte('{');
            buf->writenl();
            buf->level++;
            for (size_t i = 0; i < d->members->length; i++)
            {
                Dsymbol *s = (*d->members)[i];
                s->accept(this);
            }
            buf->level--;
            buf->writeByte('}');
            buf->writenl();
            hgs->tpltMember--;
        }
    }

    bool visitEponymousMember(TemplateDeclaration *d)
    {
        if (!d->members || d->members->length != 1)
            return false;

        Dsymbol *onemember = (*d->members)[0];
        if (onemember->ident != d->ident)
            return false;

        if (FuncDeclaration *fd = onemember->isFuncDeclaration())
        {
            assert(fd->type);
            if (stcToBuffer(buf, fd->storage_class))
                buf->writeByte(' ');
            functionToBufferFull((TypeFunction *)fd->type, buf, d->ident, hgs, d);
            visitTemplateConstraint(d->constraint);

            hgs->tpltMember++;
            bodyToBuffer(fd);
            hgs->tpltMember--;
            return true;
        }
        if (AggregateDeclaration *ad = onemember->isAggregateDeclaration())
        {
            buf->writestring(ad->kind());
            buf->writeByte(' ');
            buf->writestring(ad->ident->toChars());
            buf->writeByte('(');
            visitTemplateParameters(hgs->ddoc ? d->origParameters : d->parameters);
            buf->writeByte(')');
            visitTemplateConstraint(d->constraint);
            visitBaseClasses(ad->isClassDeclaration());

            hgs->tpltMember++;
            if (ad->members)
            {
                buf->writenl();
                buf->writeByte('{');
                buf->writenl();
                buf->level++;
                for (size_t i = 0; i < ad->members->length; i++)
                {
                    Dsymbol *s = (*ad->members)[i];
                    s->accept(this);
                }
                buf->level--;
                buf->writeByte('}');
            }
            else
                buf->writeByte(';');
            buf->writenl();
            hgs->tpltMember--;
            return true;
        }
        if (VarDeclaration *vd = onemember->isVarDeclaration())
        {
            if (d->constraint)
                return false;

            if (stcToBuffer(buf, vd->storage_class))
                buf->writeByte(' ');
            if (vd->type)
                typeToBuffer(vd->type, vd->ident);
            else
                buf->writestring(vd->ident->toChars());

            buf->writeByte('(');
            visitTemplateParameters(hgs->ddoc ? d->origParameters : d->parameters);
            buf->writeByte(')');

            if (vd->_init)
            {
                buf->writestring(" = ");
                ExpInitializer *ie = vd->_init->isExpInitializer();
                if (ie && (ie->exp->op == TOKconstruct || ie->exp->op == TOKblit))
                    ((AssignExp *)ie->exp)->e2->accept(this);
                else
                    vd->_init->accept(this);
            }
            buf->writeByte(';');
            buf->writenl();
            return true;
        }

        return false;
    }
    void visitTemplateParameters(TemplateParameters *parameters)
    {
        if (!parameters || !parameters->length)
            return;
        for (size_t i = 0; i < parameters->length; i++)
        {
            TemplateParameter *p = (*parameters)[i];
            if (i)
                buf->writestring(", ");
            p->accept(this);
        }
    }
    void visitTemplateConstraint(Expression *constraint)
    {
        if (!constraint)
            return;
        buf->writestring(" if (");
        constraint->accept(this);
        buf->writeByte(')');
    }

    void visit(TemplateInstance *ti)
    {
        buf->writestring(ti->name->toChars());
        tiargsToBuffer(ti);

        if (hgs->fullDump)
        {
            buf->writenl();
            if (ti->aliasdecl)
            {
                // the ti.aliasDecl is the instantiated body
                // if we have it, print it.
                ti->aliasdecl->accept(this);
            }
        }
    }

    void visit(TemplateMixin *tm)
    {
        buf->writestring("mixin ");

        typeToBuffer(tm->tqual, NULL);
        tiargsToBuffer(tm);

        if (tm->ident && memcmp(tm->ident->toChars(), "__mixin", 7) != 0)
        {
            buf->writeByte(' ');
            buf->writestring(tm->ident->toChars());
        }
        buf->writeByte(';');
        buf->writenl();
    }

    void tiargsToBuffer(TemplateInstance *ti)
    {
        buf->writeByte('!');
        if (ti->nest)
        {
            buf->writestring("(...)");
            return;
        }
        if (!ti->tiargs)
        {
            buf->writestring("()");
            return;
        }

        if (ti->tiargs->length == 1)
        {
            RootObject *oarg = (*ti->tiargs)[0];
            if (Type *t = isType(oarg))
            {
                if (t->equals(Type::tstring) ||
                    t->equals(Type::twstring) ||
                    t->equals(Type::tdstring) ||
                    (t->mod == 0 &&
                     (t->isTypeBasic() ||
                      (t->ty == Tident && ((TypeIdentifier *)t)->idents.length == 0))))
                {
                    buf->writestring(t->toChars());
                    return;
                }
            }
            else if (Expression *e = isExpression(oarg))
            {
                if (e->op == TOKint64 ||
                    e->op == TOKfloat64 ||
                    e->op == TOKnull ||
                    e->op == TOKstring ||
                    e->op == TOKthis)
                {
                    buf->writestring(e->toChars());
                    return;
                }
            }
        }
        buf->writeByte('(');
        ti->nest++;
        for (size_t i = 0; i < ti->tiargs->length; i++)
        {
            RootObject *arg = (*ti->tiargs)[i];
            if (i)
                buf->writestring(", ");
            objectToBuffer(arg);
        }
        ti->nest--;
        buf->writeByte(')');
    }

    /****************************************
     * This makes a 'pretty' version of the template arguments.
     * It's analogous to genIdent() which makes a mangled version.
     */
    void objectToBuffer(RootObject *oarg)
    {
        //printf("objectToBuffer()\n");

        /* The logic of this should match what genIdent() does. The _dynamic_cast()
         * function relies on all the pretty strings to be unique for different classes
         * (see Bugzilla 7375).
         * Perhaps it would be better to demangle what genIdent() does.
         */
        if (Type *t = isType(oarg))
        {
            //printf("\tt: %s ty = %d\n", t->toChars(), t->ty);
            typeToBuffer(t, NULL);
        }
        else if (Expression *e = isExpression(oarg))
        {
            if (e->op == TOKvar)
                e = e->optimize(WANTvalue);         // added to fix Bugzilla 7375
            e->accept(this);
        }
        else if (Dsymbol *s = isDsymbol(oarg))
        {
            const char *p = s->ident ? s->ident->toChars() : s->toChars();
            buf->writestring(p);
        }
        else if (Tuple *v = isTuple(oarg))
        {
            Objects *args = &v->objects;
            for (size_t i = 0; i < args->length; i++)
            {
                RootObject *arg = (*args)[i];
                if (i)
                    buf->writestring(", ");
                objectToBuffer(arg);
            }
        }
        else if (Parameter *p = isParameter(oarg))
        {
            p->accept(this);
        }
        else if (!oarg)
        {
            buf->writestring("NULL");
        }
        else
        {
            assert(0);
        }
    }

    void visit(EnumDeclaration *d)
    {
        EnumDeclaration *oldInEnumDecl = inEnumDecl;
        inEnumDecl = d;
        buf->writestring("enum ");
        if (d->ident)
        {
            buf->writestring(d->ident->toChars());
            buf->writeByte(' ');
        }
        if (d->memtype)
        {
            buf->writestring(": ");
            typeToBuffer(d->memtype, NULL);
        }
        if (!d->members)
        {
            buf->writeByte(';');
            buf->writenl();
            inEnumDecl = oldInEnumDecl;
            return;
        }
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        for (size_t i = 0; i < d->members->length; i++)
        {
            EnumMember *em = (*d->members)[i]->isEnumMember();
            if (!em)
                continue;
            em->accept(this);
            buf->writeByte(',');
            buf->writenl();
        }
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
        inEnumDecl = oldInEnumDecl;
    }

    void visit(Nspace *d)
    {
        buf->writestring("extern (C++, ");
        buf->writestring(d->ident->toChars());
        buf->writeByte(')');
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        for (size_t i = 0; i < d->members->length; i++)
        {
            Dsymbol *s = (*d->members)[i];
            s->accept(this);
        }
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(StructDeclaration *d)
    {
        buf->printf("%s ", d->kind());
        if (!d->isAnonymous())
            buf->writestring(d->toChars());
        if (!d->members)
        {
            buf->writeByte(';');
            buf->writenl();
            return;
        }
        buf->writenl();
        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        for (size_t i = 0; i < d->members->length; i++)
        {
            Dsymbol *s = (*d->members)[i];
            s->accept(this);
        }
        buf->level--;
        buf->writeByte('}');
        buf->writenl();
    }

    void visit(ClassDeclaration *d)
    {
        if (!d->isAnonymous())
        {
            buf->writestring(d->kind());
            buf->writeByte(' ');
            buf->writestring(d->ident->toChars());
        }
        visitBaseClasses(d);
        if (d->members)
        {
            buf->writenl();
            buf->writeByte('{');
            buf->writenl();
            buf->level++;
            for (size_t i = 0; i < d->members->length; i++)
            {
                Dsymbol *s = (*d->members)[i];
                s->accept(this);
            }
            buf->level--;
            buf->writeByte('}');
        }
        else
            buf->writeByte(';');
        buf->writenl();
    }

    void visitBaseClasses(ClassDeclaration *d)
    {
        if (!d || !d->baseclasses->length)
            return;

        buf->writestring(" : ");
        for (size_t i = 0; i < d->baseclasses->length; i++)
        {
            if (i)
                buf->writestring(", ");
            BaseClass *b = (*d->baseclasses)[i];
            typeToBuffer(b->type, NULL);
        }
    }

    void visit(AliasDeclaration *d)
    {
        if (d->storage_class & STClocal)
            return;
        buf->writestring("alias ");
        if (d->aliassym)
        {
            buf->writestring(d->ident->toChars());
            buf->writestring(" = ");
            if (stcToBuffer(buf, d->storage_class))
                buf->writeByte(' ');
            d->aliassym->accept(this);
        }
        else if (d->type->ty == Tfunction)
        {
            if (stcToBuffer(buf, d->storage_class))
                buf->writeByte(' ');
            typeToBuffer(d->type, d->ident);
        }
        else
        {
            declstring = (d->ident == Id::string || d->ident == Id::wstring || d->ident == Id::dstring);
            buf->writestring(d->ident->toChars());
            buf->writestring(" = ");
            if (stcToBuffer(buf, d->storage_class))
                buf->writeByte(' ');
            typeToBuffer(d->type, NULL);
            declstring = false;
        }
        buf->writeByte(';');
        buf->writenl();
    }

    void visit(VarDeclaration *d)
    {
        if (d->storage_class & STClocal)
            return;
        visitVarDecl(d, false);
        buf->writeByte(';');
        buf->writenl();
    }
    void visitVarDecl(VarDeclaration *v, bool anywritten)
    {
        if (anywritten)
        {
            buf->writestring(", ");
            buf->writestring(v->ident->toChars());
        }
        else
        {
            if (stcToBuffer(buf, v->storage_class))
                buf->writeByte(' ');
            if (v->type)
                typeToBuffer(v->type, v->ident);
            else
                buf->writestring(v->ident->toChars());
        }
        if (v->_init)
        {
            buf->writestring(" = ");
            ExpInitializer *ie = v->_init->isExpInitializer();
            if (ie && (ie->exp->op == TOKconstruct || ie->exp->op == TOKblit))
                ((AssignExp *)ie->exp)->e2->accept(this);
            else
                v->_init->accept(this);
        }
    }

    void visit(FuncDeclaration *f)
    {
        //printf("FuncDeclaration::toCBuffer() '%s'\n", f->toChars());

        if (stcToBuffer(buf, f->storage_class))
            buf->writeByte(' ');
        TypeFunction *tf = (TypeFunction *)f->type;
        typeToBuffer(tf, f->ident);
        if (hgs->hdrgen)
        {
            // if the return type is missing (e.g. ref functions or auto)
            if (!tf->next || f->storage_class & STCauto)
            {
                hgs->autoMember++;
                bodyToBuffer(f);
                hgs->autoMember--;
            }
            else if (hgs->tpltMember == 0 && global.params.hdrStripPlainFunctions)
            {
                buf->writeByte(';');
                buf->writenl();
            }
            else
                bodyToBuffer(f);
        }
        else
            bodyToBuffer(f);
    }

    void bodyToBuffer(FuncDeclaration *f)
    {
        if (!f->fbody || (hgs->hdrgen && global.params.hdrStripPlainFunctions && !hgs->autoMember && !hgs->tpltMember))
        {
            buf->writeByte(';');
            buf->writenl();
            return;
        }

        int savetlpt = hgs->tpltMember;
        int saveauto = hgs->autoMember;
        hgs->tpltMember = 0;
        hgs->autoMember = 0;
        buf->writenl();
        bool requireDo = false;
        // in{}
        if (f->frequires)
        {
            for (size_t i = 0; i < f->frequires->length; i++)
            {
                Statement *frequire = (*f->frequires)[i];
                buf->writestring("in");
                if (ExpStatement *es = frequire->isExpStatement())
                {
                    assert(es->exp && es->exp->op == TOKassert);
                    buf->writestring(" (");
                    ((AssertExp *)es->exp)->e1->accept(this);
                    buf->writeByte(')');
                    buf->writenl();
                    requireDo = false;
                }
                else
                {
                    buf->writenl();
                    frequire->accept(this);
                    requireDo = true;
                }
            }
        }

        // out{}
        if (f->fensures)
        {
            for (size_t i = 0; i < f->fensures->length; i++)
            {
                Ensure fensure = (*f->fensures)[i];
                buf->writestring("out");
                if (ExpStatement *es = fensure.ensure->isExpStatement())
                {
                    assert(es->exp && es->exp->op == TOKassert);
                    buf->writestring(" (");
                    if (fensure.id)
                    {
                        buf->writestring(fensure.id->toChars());
                    }
                    buf->writestring("; ");
                    ((AssertExp *)es->exp)->e1->accept(this);
                    buf->writeByte(')');
                    buf->writenl();
                    requireDo = false;
                }
                else
                {
                    if (fensure.id)
                    {
                        buf->writeByte('(');
                        buf->writestring(fensure.id->toChars());
                        buf->writeByte(')');
                    }
                    buf->writenl();
                    fensure.ensure->accept(this);
                    requireDo = true;
                }
            }
        }

        if (requireDo)
        {
            buf->writestring("body");
            buf->writenl();
        }

        buf->writeByte('{');
        buf->writenl();
        buf->level++;
        f->fbody->accept(this);
        buf->level--;
        buf->writeByte('}');
        buf->writenl();

        hgs->tpltMember = savetlpt;
        hgs->autoMember = saveauto;
    }

    void visit(FuncLiteralDeclaration *f)
    {
        if (f->type->ty == Terror)
        {
            buf->writestring("__error");
            return;
        }

        if (f->tok != TOKreserved)
        {
            buf->writestring(f->kind());
            buf->writeByte(' ');
        }

        TypeFunction *tf = (TypeFunction *)f->type;
        // Don't print tf->mod, tf->trust, and tf->linkage
        if (!f->inferRetType && tf->next)
            typeToBuffer(tf->next, NULL);
        parametersToBuffer(tf->parameterList.parameters, tf->parameterList.varargs);

        CompoundStatement *cs = f->fbody->isCompoundStatement();
        Statement *s1;
        if (f->semanticRun >= PASSsemantic3done && cs)
        {
            s1 = (*cs->statements)[cs->statements->length - 1];
        }
        else
            s1 = !cs ? f->fbody : NULL;
        ReturnStatement *rs = s1 ? s1->isReturnStatement() : NULL;
        if (rs && rs->exp)
        {
            buf->writestring(" => ");
            rs->exp->accept(this);
        }
        else
        {
            hgs->tpltMember++;
            bodyToBuffer(f);
            hgs->tpltMember--;
        }
    }

    void visit(PostBlitDeclaration *d)
    {
        if (stcToBuffer(buf, d->storage_class))
             buf->writeByte(' ');
        buf->writestring("this(this)");
        bodyToBuffer(d);
    }

    void visit(DtorDeclaration *d)
    {
        if (d->storage_class & STCtrusted)
            buf->writestring("@trusted ");
        if (d->storage_class & STCsafe)
            buf->writestring("@safe ");
        if (d->storage_class & STCnogc)
            buf->writestring("@nogc ");
        if (d->storage_class & STCdisable)
            buf->writestring("@disable ");

        buf->writestring("~this()");
        bodyToBuffer(d);
    }

    void visit(StaticCtorDeclaration *d)
    {
        if (stcToBuffer(buf, d->storage_class & ~STCstatic))
            buf->writeByte(' ');
        if (d->isSharedStaticCtorDeclaration())
            buf->writestring("shared ");
        buf->writestring("static this()");
        if (hgs->hdrgen && !hgs->tpltMember)
        {
            buf->writeByte(';');
            buf->writenl();
        }
        else
            bodyToBuffer(d);
    }

    void visit(StaticDtorDeclaration *d)
    {
        if (hgs->hdrgen)
            return;
        if (stcToBuffer(buf, d->storage_class & ~STCstatic))
            buf->writeByte(' ');
        if (d->isSharedStaticDtorDeclaration())
            buf->writestring("shared ");
        buf->writestring("static ~this()");
        bodyToBuffer(d);
    }

    void visit(InvariantDeclaration *d)
    {
        if (hgs->hdrgen)
            return;
        if (stcToBuffer(buf, d->storage_class))
            buf->writeByte(' ');
        buf->writestring("invariant");
        if (ExpStatement *es = d->fbody->isExpStatement())
        {
            assert(es->exp && es->exp->op == TOKassert);
            buf->writestring(" (");
            ((AssertExp *)es->exp)->e1->accept(this);
            buf->writestring(");");
            buf->writenl();
        }
        else
        {
            bodyToBuffer(d);
        }
    }

    void visit(UnitTestDeclaration *d)
    {
        if (hgs->hdrgen)
            return;
        if (stcToBuffer(buf, d->storage_class))
            buf->writeByte(' ');
        buf->writestring("unittest");
        bodyToBuffer(d);
    }

    void visit(NewDeclaration *d)
    {
        if (stcToBuffer(buf, d->storage_class & ~STCstatic))
            buf->writeByte(' ');
        buf->writestring("new");
        parametersToBuffer(d->parameters, d->varargs);
        bodyToBuffer(d);
    }

    void visit(DeleteDeclaration *d)
    {
        if (stcToBuffer(buf, d->storage_class & ~STCstatic))
            buf->writeByte(' ');
        buf->writestring("delete");
        parametersToBuffer(d->parameters, 0);
        bodyToBuffer(d);
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(ErrorInitializer *)
    {
        buf->writestring("__error__");
    }

    void visit(VoidInitializer *)
    {
        buf->writestring("void");
    }

    void visit(StructInitializer *si)
    {
        //printf("StructInitializer::toCBuffer()\n");
        buf->writeByte('{');
        for (size_t i = 0; i < si->field.length; i++)
        {
            if (i)
                buf->writestring(", ");
            if (Identifier *id = si->field[i])
            {
                buf->writestring(id->toChars());
                buf->writeByte(':');
            }
            if (Initializer *iz = si->value[i])
                iz->accept(this);
        }
        buf->writeByte('}');
    }

    void visit(ArrayInitializer *ai)
    {
        buf->writeByte('[');
        for (size_t i = 0; i < ai->index.length; i++)
        {
            if (i)
                buf->writestring(", ");
            if (Expression *ex = ai->index[i])
            {
                ex->accept(this);
                buf->writeByte(':');
            }
            if (Initializer *iz = ai->value[i])
                iz->accept(this);
        }
        buf->writeByte(']');
    }

    void visit(ExpInitializer *ei)
    {
        ei->exp->accept(this);
    }

    ////////////////////////////////////////////////////////////////////////////

    /**************************************************
     * Write out argument list to buf.
     */
    void argsToBuffer(Expressions *expressions, Expression *basis = NULL)
    {
        if (!expressions || !expressions->length)
            return;

        for (size_t i = 0; i < expressions->length; i++)
        {
            Expression *el = (*expressions)[i];
            if (i)
                buf->writestring(", ");
            if (!el)
                el = basis;
            if (el)
                expToBuffer(el, PREC_assign);
        }
    }

    void sizeToBuffer(Expression *e)
    {
        if (e->type == Type::tsize_t)
        {
            Expression *ex = (e->op == TOKcast ? ((CastExp *)e)->e1 : e);
            ex = ex->optimize(WANTvalue);

            dinteger_t uval = ex->op == TOKint64 ? ex->toInteger() : (dinteger_t)-1;
            if ((sinteger_t)uval >= 0)
            {
                dinteger_t sizemax;
                if (target.ptrsize == 8)
                    sizemax = 0xFFFFFFFFFFFFFFFFULL;
                else if (target.ptrsize == 4)
                    sizemax = 0xFFFFFFFFUL;
                else if (target.ptrsize == 2)
                    sizemax = 0xFFFFUL;
                else
                    assert(0);
                if (uval <= sizemax && uval <= 0x7FFFFFFFFFFFFFFFULL)
                {
                    buf->printf("%llu", uval);
                    return;
                }
            }
        }
        expToBuffer(e, PREC_assign);
    }

    /**************************************************
     * Write expression out to buf, but wrap it
     * in ( ) if its precedence is less than pr.
     */
    void expToBuffer(Expression *e, PREC pr)
    {
        assert(precedence[e->op] != PREC_zero);
        assert(pr != PREC_zero);

        //if (precedence[e->op] == 0) e->print();
        /* Despite precedence, we don't allow a<b<c expressions.
         * They must be parenthesized.
         */
        if (precedence[e->op] < pr ||
            (pr == PREC_rel && precedence[e->op] == pr))
        {
            buf->writeByte('(');
            e->accept(this);
            buf->writeByte(')');
        }
        else
            e->accept(this);
    }

    void visit(Expression *e)
    {
        buf->writestring(Token::toChars(e->op));
    }

    void visit(IntegerExp *e)
    {
        dinteger_t v = e->toInteger();

        if (e->type)
        {
            Type *t = e->type;
        L1:
            switch (t->ty)
            {
                case Tenum:
                {
                    TypeEnum *te = (TypeEnum *)t;
                    if (hgs->fullDump)
                    {
                        EnumDeclaration *sym = te->sym;
                        if (inEnumDecl != sym)
                        {
                            for (size_t i = 0; i < sym->members->length; i++)
                            {
                                EnumMember *em = (EnumMember *)(*sym->members)[i];
                                if (em->value()->toInteger() == v)
                                {
                                    buf->printf("%s.%s", sym->toChars(), em->ident->toChars());
                                    return;
                                }
                            }
                        }
                    }
                    buf->printf("cast(%s)", te->sym->toChars());
                    t = te->sym->memtype;
                    goto L1;
                }

                case Twchar:        // BUG: need to cast(wchar)
                case Tdchar:        // BUG: need to cast(dchar)
                    if ((uinteger_t)v > 0xFF)
                    {
                        buf->printf("'\\U%08x'", v);
                        break;
                    }
                    /* fall through */
                case Tchar:
                {
                    size_t o = buf->length();
                    if (v == '\'')
                        buf->writestring("'\\''");
                    else if (isprint((int)v) && v != '\\')
                        buf->printf("'%c'", (int)v);
                    else
                        buf->printf("'\\x%02x'", (int)v);
                    if (hgs->ddoc)
                        escapeDdocString(buf, o);
                    break;
                }

                case Tint8:
                    buf->writestring("cast(byte)");
                    goto L2;

                case Tint16:
                    buf->writestring("cast(short)");
                    goto L2;

                case Tint32:
                L2:
                    buf->printf("%d", (int)v);
                    break;

                case Tuns8:
                    buf->writestring("cast(ubyte)");
                    goto L3;

                case Tuns16:
                    buf->writestring("cast(ushort)");
                    goto L3;

                case Tuns32:
                L3:
                    buf->printf("%uu", (unsigned)v);
                    break;

                case Tint64:
                    buf->printf("%lldL", v);
                    break;

                case Tuns64:
                L4:
                    buf->printf("%lluLU", v);
                    break;

                case Tbool:
                    buf->writestring(v ? "true" : "false");
                    break;

                case Tpointer:
                    buf->writestring("cast(");
                    buf->writestring(t->toChars());
                    buf->writeByte(')');
                    if (target.ptrsize == 8)
                        goto L4;
                    else if (target.ptrsize == 4 ||
                             target.ptrsize == 2)
                        goto L3;
                    else
                        assert(0);

                case Tvoid:
                    buf->writestring("cast(void)0");
                    break;

                default:
                    /* This can happen if errors, such as
                     * the type is painted on like in fromConstInitializer().
                     */
                    if (!global.errors)
                    {
                        assert(0);
                    }
                    break;
            }
        }
        else if (v & 0x8000000000000000LL)
            buf->printf("0x%llx", v);
        else
            buf->printf("%lld", v);
    }

    void visit(ErrorExp *)
    {
        buf->writestring("__error");
    }

    void floatToBuffer(Type *type, real_t value)
    {
        /** sizeof(value)*3 is because each byte of mantissa is max
        of 256 (3 characters). The string will be "-M.MMMMe-4932".
        (ie, 8 chars more than mantissa). Plus one for trailing \0.
        Plus one for rounding. */
        const size_t BUFFER_LEN = sizeof(value) * 3 + 8 + 1 + 1;
        char buffer[BUFFER_LEN];
        memset(buffer, 0, BUFFER_LEN);
        CTFloat::sprint(buffer, 'g', value);
        assert(strlen(buffer) < BUFFER_LEN);

        if (hgs->hdrgen)
        {
            real_t r = CTFloat::parse(buffer);
            if (r != value)                     // if exact duplication
                CTFloat::sprint(buffer, 'a', value);
        }
        buf->writestring(buffer);

        if (type)
        {
            Type *t = type->toBasetype();
            switch (t->ty)
            {
                case Tfloat32:
                case Timaginary32:
                case Tcomplex32:
                    buf->writeByte('F');
                    break;

                case Tfloat80:
                case Timaginary80:
                case Tcomplex80:
                    buf->writeByte('L');
                    break;

                default:
                    break;
            }
            if (t->isimaginary())
                buf->writeByte('i');
        }
    }

    void visit(RealExp *e)
    {
        floatToBuffer(e->type, e->value);
    }

    void visit(ComplexExp *e)
    {
        /* Print as:
         *  (re+imi)
         */
        buf->writeByte('(');
        floatToBuffer(e->type, creall(e->value));
        buf->writeByte('+');
        floatToBuffer(e->type, cimagl(e->value));
        buf->writestring("i)");
    }

    void visit(IdentifierExp *e)
    {
        if (hgs->hdrgen || hgs->ddoc)
            buf->writestring(e->ident->toHChars2());
        else
            buf->writestring(e->ident->toChars());
    }

    void visit(DsymbolExp *e)
    {
        buf->writestring(e->s->toChars());
    }

    void visit(ThisExp *)
    {
        buf->writestring("this");
    }

    void visit(SuperExp *)
    {
        buf->writestring("super");
    }

    void visit(NullExp *)
    {
        buf->writestring("null");
    }

    void visit(StringExp *e)
    {
        buf->writeByte('"');
        size_t o = buf->length();
        for (size_t i = 0; i < e->len; i++)
        {
            unsigned c = e->charAt(i);
            switch (c)
            {
                case '"':
                case '\\':
                    buf->writeByte('\\');
                    /* fall through */
                default:
                    if (c <= 0xFF)
                    {
                        if (c <= 0x7F && isprint(c))
                            buf->writeByte(c);
                        else
                            buf->printf("\\x%02x", c);
                    }
                    else if (c <= 0xFFFF)
                        buf->printf("\\x%02x\\x%02x", c & 0xFF, c >> 8);
                    else
                        buf->printf("\\x%02x\\x%02x\\x%02x\\x%02x",
                            c & 0xFF, (c >> 8) & 0xFF, (c >> 16) & 0xFF, c >> 24);
                    break;
            }
        }
        if (hgs->ddoc)
            escapeDdocString(buf, o);
        buf->writeByte('"');
        if (e->postfix)
            buf->writeByte(e->postfix);
    }

    void visit(ArrayLiteralExp *e)
    {
        buf->writeByte('[');
        argsToBuffer(e->elements, e->basis);
        buf->writeByte(']');
    }

    void visit(AssocArrayLiteralExp *e)
    {
        buf->writeByte('[');
        for (size_t i = 0; i < e->keys->length; i++)
        {
            Expression *key = (*e->keys)[i];
            Expression *value = (*e->values)[i];

            if (i)
                buf->writestring(", ");
            expToBuffer(key, PREC_assign);
            buf->writeByte(':');
            expToBuffer(value, PREC_assign);
        }
        buf->writeByte(']');
    }

    void visit(StructLiteralExp *e)
    {
        buf->writestring(e->sd->toChars());
        buf->writeByte('(');

        // CTFE can generate struct literals that contain an AddrExp pointing
        // to themselves, need to avoid infinite recursion:
        // struct S { this(int){ this.s = &this; } S* s; }
        // const foo = new S(0);
        if (e->stageflags & stageToCBuffer)
            buf->writestring("<recursion>");
        else
        {
            int old = e->stageflags;
            e->stageflags |= stageToCBuffer;
            argsToBuffer(e->elements);
            e->stageflags = old;
        }

        buf->writeByte(')');
    }

    void visit(TypeExp *e)
    {
        typeToBuffer(e->type, NULL);
    }

    void visit(ScopeExp *e)
    {
        if (e->sds->isTemplateInstance())
        {
            e->sds->accept(this);
        }
        else if (hgs != NULL && hgs->ddoc)
        {
            // fixes bug 6491
            Module *m = e->sds->isModule();
            if (m)
                buf->writestring(m->md->toChars());
            else
                buf->writestring(e->sds->toChars());
        }
        else
        {
            buf->writestring(e->sds->kind());
            buf->writeByte(' ');
            buf->writestring(e->sds->toChars());
        }
    }

    void visit(TemplateExp *e)
    {
        buf->writestring(e->td->toChars());
    }

    void visit(NewExp *e)
    {
        if (e->thisexp)
        {
            expToBuffer(e->thisexp, PREC_primary);
            buf->writeByte('.');
        }
        buf->writestring("new ");
        if (e->newargs && e->newargs->length)
        {
            buf->writeByte('(');
            argsToBuffer(e->newargs);
            buf->writeByte(')');
        }
        typeToBuffer(e->newtype, NULL);
        if (e->arguments && e->arguments->length)
        {
            buf->writeByte('(');
            argsToBuffer(e->arguments);
            buf->writeByte(')');
        }
    }

    void visit(NewAnonClassExp *e)
    {
        if (e->thisexp)
        {
            expToBuffer(e->thisexp, PREC_primary);
            buf->writeByte('.');
        }
        buf->writestring("new");
        if (e->newargs && e->newargs->length)
        {
            buf->writeByte('(');
            argsToBuffer(e->newargs);
            buf->writeByte(')');
        }
        buf->writestring(" class ");
        if (e->arguments && e->arguments->length)
        {
            buf->writeByte('(');
            argsToBuffer(e->arguments);
            buf->writeByte(')');
        }
        if (e->cd)
            e->cd->accept(this);
    }

    void visit(SymOffExp *e)
    {
        if (e->offset)
            buf->printf("(& %s+%u)", e->var->toChars(), e->offset);
        else if (e->var->isTypeInfoDeclaration())
            buf->printf("%s", e->var->toChars());
        else
            buf->printf("& %s", e->var->toChars());
    }

    void visit(VarExp *e)
    {
        buf->writestring(e->var->toChars());
    }

    void visit(OverExp *e)
    {
        buf->writestring(e->vars->ident->toChars());
    }

    void visit(TupleExp *e)
    {
        if (e->e0)
        {
            buf->writeByte('(');
            e->e0->accept(this);
            buf->writestring(", tuple(");
            argsToBuffer(e->exps);
            buf->writestring("))");
        }
        else
        {
            buf->writestring("tuple(");
            argsToBuffer(e->exps);
            buf->writeByte(')');
        }
    }

    void visit(FuncExp *e)
    {
        e->fd->accept(this);
        //buf->writestring(e->fd->toChars());
    }

    void visit(DeclarationExp *e)
    {
        /* Normal dmd execution won't reach here - regular variable declarations
         * are handled in visit(ExpStatement), so here would be used only when
         * we'll directly call Expression::toChars() for debugging.
         */
        if (VarDeclaration *v = e->declaration->isVarDeclaration())
        {
            // For debugging use:
            // - Avoid printing newline.
            // - Intentionally use the format (Type var;)
            //   which isn't correct as regular D code.
            buf->writeByte('(');
            visitVarDecl(v, false);
            buf->writeByte(';');
            buf->writeByte(')');
        }
        else
            e->declaration->accept(this);
    }

    void visit(TypeidExp *e)
    {
        buf->writestring("typeid(");
        objectToBuffer(e->obj);
        buf->writeByte(')');
    }

    void visit(TraitsExp *e)
    {
        buf->writestring("__traits(");
        if (e->ident)
            buf->writestring(e->ident->toChars());
        if (e->args)
        {
            for (size_t i = 0; i < e->args->length; i++)
            {
                RootObject *arg = (*e->args)[i];
                buf->writestring(", ");
                objectToBuffer(arg);
            }
        }
        buf->writeByte(')');
    }

    void visit(HaltExp *)
    {
        buf->writestring("halt");
    }

    void visit(IsExp *e)
    {
        buf->writestring("is(");
        typeToBuffer(e->targ, e->id);
        if (e->tok2 != TOKreserved)
        {
            buf->printf(" %s %s", Token::toChars(e->tok), Token::toChars(e->tok2));
        }
        else if (e->tspec)
        {
            if (e->tok == TOKcolon)
                buf->writestring(" : ");
            else
                buf->writestring(" == ");
            typeToBuffer(e->tspec, NULL);
        }
        if (e->parameters && e->parameters->length)
        {
            buf->writestring(", ");
            visitTemplateParameters(e->parameters);
        }
        buf->writeByte(')');
    }

    void visit(UnaExp *e)
    {
        buf->writestring(Token::toChars(e->op));
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(BinExp *e)
    {
        expToBuffer(e->e1, precedence[e->op]);
        buf->writeByte(' ');
        buf->writestring(Token::toChars(e->op));
        buf->writeByte(' ');
        expToBuffer(e->e2, (PREC)(precedence[e->op] + 1));
    }

    void visit(CompileExp *e)
    {
        buf->writestring("mixin(");
        argsToBuffer(e->exps);
        buf->writeByte(')');
    }

    void visit(ImportExp *e)
    {
        buf->writestring("import(");
        expToBuffer(e->e1, PREC_assign);
        buf->writeByte(')');
    }

    void visit(AssertExp *e)
    {
        buf->writestring("assert(");
        expToBuffer(e->e1, PREC_assign);
        if (e->msg)
        {
            buf->writestring(", ");
            expToBuffer(e->msg, PREC_assign);
        }
        buf->writeByte(')');
    }

    void visit(DotIdExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        buf->writestring(e->ident->toChars());
    }

    void visit(DotTemplateExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        buf->writestring(e->td->toChars());
    }

    void visit(DotVarExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        buf->writestring(e->var->toChars());
    }

    void visit(DotTemplateInstanceExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        e->ti->accept(this);
    }

    void visit(DelegateExp *e)
    {
        buf->writeByte('&');
        if (!e->func->isNested())
        {
            expToBuffer(e->e1, PREC_primary);
            buf->writeByte('.');
        }
        buf->writestring(e->func->toChars());
    }

    void visit(DotTypeExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        buf->writestring(e->sym->toChars());
    }

    void visit(CallExp *e)
    {
        if (e->e1->op == TOKtype)
        {
            /* Avoid parens around type to prevent forbidden cast syntax:
             *   (sometype)(arg1)
             * This is ok since types in constructor calls
             * can never depend on parens anyway
             */
            e->e1->accept(this);
        }
        else
            expToBuffer(e->e1, precedence[e->op]);
        buf->writeByte('(');
        argsToBuffer(e->arguments);
        buf->writeByte(')');
    }

    void visit(PtrExp *e)
    {
        buf->writeByte('*');
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(DeleteExp *e)
    {
        buf->writestring("delete ");
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(CastExp *e)
    {
        buf->writestring("cast(");
        if (e->to)
            typeToBuffer(e->to, NULL);
        else
        {
            MODtoBuffer(buf, e->mod);
        }
        buf->writeByte(')');
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(VectorExp *e)
    {
        buf->writestring("cast(");
        typeToBuffer(e->to, NULL);
        buf->writeByte(')');
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(VectorArrayExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writestring(".array");
    }

    void visit(SliceExp *e)
    {
        expToBuffer(e->e1, precedence[e->op]);
        buf->writeByte('[');
        if (e->upr || e->lwr)
        {
            if (e->lwr)
                sizeToBuffer(e->lwr);
            else
                buf->writeByte('0');
            buf->writestring("..");
            if (e->upr)
                sizeToBuffer(e->upr);
            else
                buf->writeByte('$');
        }
        buf->writeByte(']');
    }

    void visit(ArrayLengthExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writestring(".length");
    }

    void visit(IntervalExp *e)
    {
        expToBuffer(e->lwr, PREC_assign);
        buf->writestring("..");
        expToBuffer(e->upr, PREC_assign);
    }

    void visit(DelegatePtrExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writestring(".ptr");
    }

    void visit(DelegateFuncptrExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writestring(".funcptr");
    }

    void visit(ArrayExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('[');
        argsToBuffer(e->arguments);
        buf->writeByte(']');
    }

    void visit(DotExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('.');
        expToBuffer(e->e2, PREC_primary);
    }

    void visit(IndexExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writeByte('[');
        sizeToBuffer(e->e2);
        buf->writeByte(']');
    }

    void visit(PostExp *e)
    {
        expToBuffer(e->e1, precedence[e->op]);
        buf->writestring(Token::toChars(e->op));
    }

    void visit(PreExp *e)
    {
        buf->writestring(Token::toChars(e->op));
        expToBuffer(e->e1, precedence[e->op]);
    }

    void visit(RemoveExp *e)
    {
        expToBuffer(e->e1, PREC_primary);
        buf->writestring(".remove(");
        expToBuffer(e->e2, PREC_assign);
        buf->writeByte(')');
    }

    void visit(CondExp *e)
    {
        expToBuffer(e->econd, PREC_oror);
        buf->writestring(" ? ");
        expToBuffer(e->e1, PREC_expr);
        buf->writestring(" : ");
        expToBuffer(e->e2, PREC_cond);
    }

    void visit(DefaultInitExp *e)
    {
        buf->writestring(Token::toChars(e->subop));
    }

    void visit(ClassReferenceExp *e)
    {
        buf->writestring(e->value->toChars());
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(TemplateTypeParameter *tp)
    {
        buf->writestring(tp->ident->toChars());
        if (tp->specType)
        {
            buf->writestring(" : ");
            typeToBuffer(tp->specType, NULL);
        }
        if (tp->defaultType)
        {
            buf->writestring(" = ");
            typeToBuffer(tp->defaultType, NULL);
        }
    }

    void visit(TemplateThisParameter *tp)
    {
        buf->writestring("this ");
        visit((TemplateTypeParameter *)tp);
    }

    void visit(TemplateAliasParameter *tp)
    {
        buf->writestring("alias ");
        if (tp->specType)
            typeToBuffer(tp->specType, tp->ident);
        else
            buf->writestring(tp->ident->toChars());
        if (tp->specAlias)
        {
            buf->writestring(" : ");
            objectToBuffer(tp->specAlias);
        }
        if (tp->defaultAlias)
        {
            buf->writestring(" = ");
            objectToBuffer(tp->defaultAlias);
        }
    }

    void visit(TemplateValueParameter *tp)
    {
        typeToBuffer(tp->valType, tp->ident);
        if (tp->specValue)
        {
            buf->writestring(" : ");
            tp->specValue->accept(this);
        }
        if (tp->defaultValue)
        {
            buf->writestring(" = ");
            tp->defaultValue->accept(this);
        }
    }

    void visit(TemplateTupleParameter *tp)
    {
        buf->writestring(tp->ident->toChars());
        buf->writestring("...");
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(DebugCondition *c)
    {
        if (c->ident)
            buf->printf("debug (%s)", c->ident->toChars());
        else
            buf->printf("debug (%u)", c->level);
    }

    void visit(VersionCondition *c)
    {
        if (c->ident)
            buf->printf("version (%s)", c->ident->toChars());
        else
            buf->printf("version (%u)", c->level);
    }

    void visit(StaticIfCondition *c)
    {
        buf->writestring("static if (");
        c->exp->accept(this);
        buf->writeByte(')');
    }

    ////////////////////////////////////////////////////////////////////////////

    void visit(Parameter *p)
    {
        if (p->userAttribDecl)
        {
            buf->writestring("@");
            bool isAnonymous = p->userAttribDecl->atts->length > 0
                && (*p->userAttribDecl->atts)[0]->op != TOKcall;
            if (isAnonymous)
                buf->writestring("(");
            argsToBuffer(p->userAttribDecl->atts);
            if (isAnonymous)
                buf->writestring(")");
            buf->writestring(" ");
        }
        if (p->storageClass & STCauto)
            buf->writestring("auto ");

        if (p->storageClass & STCreturn)
            buf->writestring("return ");

        if (p->storageClass & STCout)
            buf->writestring("out ");
        else if (p->storageClass & STCref)
            buf->writestring("ref ");
        else if (p->storageClass & STCin)
            buf->writestring("in ");
        else if (p->storageClass & STClazy)
            buf->writestring("lazy ");
        else if (p->storageClass & STCalias)
            buf->writestring("alias ");

        StorageClass stc = p->storageClass;
        if (p->type && p->type->mod & MODshared)
            stc &= ~STCshared;

        if (stcToBuffer(buf, stc & (STCconst | STCimmutable | STCwild | STCshared | STCscope | STCscopeinferred)))
            buf->writeByte(' ');

        if (p->storageClass & STCalias)
        {
            if (p->ident)
                buf->writestring(p->ident->toChars());
        }
        else if (p->type->ty == Tident &&
                 strlen(((TypeIdentifier *)p->type)->ident->toChars()) > 3 &&
                 startswith (((TypeIdentifier *)p->type)->ident->toChars(), "__T"))
        {
            // print parameter name, instead of undetermined type parameter
            buf->writestring(p->ident->toChars());
        }
        else
            typeToBuffer(p->type, p->ident);
        if (p->defaultArg)
        {
            buf->writestring(" = ");
            p->defaultArg->accept(this);
        }
    }

    void parametersToBuffer(Parameters *parameters, int varargs)
    {
        buf->writeByte('(');
        if (parameters)
        {
            size_t dim = Parameter::dim(parameters);
            for (size_t i = 0; i < dim; i++)
            {
                if (i)
                    buf->writestring(", ");
                Parameter *fparam = Parameter::getNth(parameters, i);
                fparam->accept(this);
            }
            if (varargs)
            {
                if (parameters->length && varargs == 1)
                    buf->writestring(", ");
                buf->writestring("...");
            }
        }
        buf->writeByte(')');
    }

    void visit(Module *m)
    {
        if (m->md)
        {
            if (m->userAttribDecl)
            {
                buf->writestring("@(");
                argsToBuffer(m->userAttribDecl->atts);
                buf->writeByte(')');
                buf->writenl();
            }
            if (m->md->isdeprecated)
            {
                if (m->md->msg)
                {
                    buf->writestring("deprecated(");
                    m->md->msg->accept(this);
                    buf->writestring(") ");
                }
                else
                    buf->writestring("deprecated ");
            }

            buf->writestring("module ");
            buf->writestring(m->md->toChars());
            buf->writeByte(';');
            buf->writenl();
        }
        for (size_t i = 0; i < m->members->length; i++)
        {
            Dsymbol *s = (*m->members)[i];
            s->accept(this);
        }
    }
};

void toCBuffer(Statement *s, OutBuffer *buf, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    s->accept(&v);
}

void toCBuffer(Type *t, OutBuffer *buf, Identifier *ident, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    v.typeToBuffer(t, ident);
}

void toCBuffer(Dsymbol *s, OutBuffer *buf, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    s->accept(&v);
}

// used from TemplateInstance::toChars() and TemplateMixin::toChars()
void toCBufferInstance(TemplateInstance *ti, OutBuffer *buf, bool qualifyTypes)
{
    HdrGenState hgs;
    hgs.fullQual = qualifyTypes;
    PrettyPrintVisitor v(buf, &hgs);
    v.visit(ti);
}

void toCBuffer(Initializer *iz, OutBuffer *buf, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    iz->accept(&v);
}

bool stcToBuffer(OutBuffer *buf, StorageClass stc)
{
    bool result = false;
    if ((stc & (STCreturn | STCscope)) == (STCreturn | STCscope))
        stc &= ~STCscope;
    if (stc & STCscopeinferred)
        stc &= ~(STCscope | STCscopeinferred);
    while (stc)
    {
        const char *p = stcToChars(stc);
        if (!p)
            break;
        if (!result)
            result = true;
        else
            buf->writeByte(' ');
        buf->writestring(p);
    }
    return result;
}

/*************************************************
 * Pick off one of the storage classes from stc,
 * and return a pointer to a string representation of it.
 * stc is reduced by the one picked.
 */
const char *stcToChars(StorageClass& stc)
{
    struct SCstring
    {
        StorageClass stc;
        TOK tok;
        const char *id;
    };

    static SCstring table[] =
    {
        { STCauto,         TOKauto,     NULL },
        { STCscope,        TOKscope,    NULL },
        { STCstatic,       TOKstatic,   NULL },
        { STCextern,       TOKextern,   NULL },
        { STCconst,        TOKconst,    NULL },
        { STCfinal,        TOKfinal,    NULL },
        { STCabstract,     TOKabstract, NULL },
        { STCsynchronized, TOKsynchronized, NULL },
        { STCdeprecated,   TOKdeprecated, NULL },
        { STCoverride,     TOKoverride, NULL },
        { STClazy,         TOKlazy,     NULL },
        { STCalias,        TOKalias,    NULL },
        { STCout,          TOKout,      NULL },
        { STCin,           TOKin,       NULL },
        { STCmanifest,     TOKenum,     NULL },
        { STCimmutable,    TOKimmutable, NULL },
        { STCshared,       TOKshared,   NULL },
        { STCnothrow,      TOKnothrow,  NULL },
        { STCwild,         TOKwild,     NULL },
        { STCpure,         TOKpure,     NULL },
        { STCref,          TOKref,      NULL },
        { STCtls,          TOKreserved, NULL },
        { STCgshared,      TOKgshared,  NULL },
        { STCnogc,         TOKat,       "@nogc" },
        { STCproperty,     TOKat,       "@property" },
        { STCsafe,         TOKat,       "@safe" },
        { STCtrusted,      TOKat,       "@trusted" },
        { STCsystem,       TOKat,       "@system" },
        { STCdisable,      TOKat,       "@disable" },
        { STCfuture,       TOKat,       "@__future" },
        { STClocal,        TOKat,       "__local" },
        { 0,               TOKreserved, NULL }
    };

    for (int i = 0; table[i].stc; i++)
    {
        StorageClass tbl = table[i].stc;
        assert(tbl & STCStorageClass);
        if (stc & tbl)
        {
            stc &= ~tbl;
            if (tbl == STCtls)  // TOKtls was removed
                return "__thread";

            TOK tok = table[i].tok;
            if (tok == TOKat)
                return table[i].id;
            else
                return Token::toChars(tok);
        }
    }
    //printf("stc = %llx\n", stc);
    return NULL;
}

void trustToBuffer(OutBuffer *buf, TRUST trust)
{
    const char *p = trustToChars(trust);
    if (p)
        buf->writestring(p);
}

const char *trustToChars(TRUST trust)
{
    switch (trust)
    {
        case TRUSTdefault:  return NULL;
        case TRUSTsystem:   return "@system";
        case TRUSTtrusted:  return "@trusted";
        case TRUSTsafe:     return "@safe";
        default:            assert(0);
    }
    return NULL;    // never reached
}

void linkageToBuffer(OutBuffer *buf, LINK linkage)
{
    const char *p = linkageToChars(linkage);
    if (p)
    {
        buf->writestring("extern (");
        buf->writestring(p);
        buf->writeByte(')');
    }
}

const char *linkageToChars(LINK linkage)
{
    switch (linkage)
    {
        case LINKdefault:   return NULL;
        case LINKd:         return "D";
        case LINKc:         return "C";
        case LINKcpp:       return "C++";
        case LINKwindows:   return "Windows";
        case LINKobjc:      return "Objective-C";
        case LINKsystem:    return "System";
        default:            assert(0);
    }
    return NULL;    // never reached
}

void protectionToBuffer(OutBuffer *buf, Prot prot)
{
    const char *p = protectionToChars(prot.kind);
    if (p)
        buf->writestring(p);

    if (prot.kind == Prot::package_ && prot.pkg)
    {
        buf->writeByte('(');
        buf->writestring(prot.pkg->toPrettyChars(true));
        buf->writeByte(')');
    }
}

const char *protectionToChars(Prot::Kind kind)
{
    switch (kind)
    {
        case Prot::undefined: return NULL;
        case Prot::none:      return "none";
        case Prot::private_:   return "private";
        case Prot::package_:   return "package";
        case Prot::protected_: return "protected";
        case Prot::public_:    return "public";
        case Prot::export_:    return "export";
        default:            assert(0);
    }
    return NULL;    // never reached
}

// Print the full function signature with correct ident, attributes and template args
void functionToBufferFull(TypeFunction *tf, OutBuffer *buf, Identifier *ident,
        HdrGenState* hgs, TemplateDeclaration *td)
{
    //printf("TypeFunction::toCBuffer() this = %p\n", this);
    PrettyPrintVisitor v(buf, hgs);
    v.visitFuncIdentWithPrefix(tf, ident, td);
}

// ident is inserted before the argument list and will be "function" or "delegate" for a type
void functionToBufferWithIdent(TypeFunction *tf, OutBuffer *buf, const char *ident)
{
    HdrGenState hgs;
    PrettyPrintVisitor v(buf, &hgs);
    v.visitFuncIdentWithPostfix(tf, ident);
}

void toCBuffer(Expression *e, OutBuffer *buf, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    e->accept(&v);
}

/**************************************************
 * Write out argument types to buf.
 */
void argExpTypesToCBuffer(OutBuffer *buf, Expressions *arguments)
{
    if (!arguments || !arguments->length)
        return;

    HdrGenState hgs;
    PrettyPrintVisitor v(buf, &hgs);
    for (size_t i = 0; i < arguments->length; i++)
    {
        Expression *arg = (*arguments)[i];
        if (i)
            buf->writestring(", ");
        v.typeToBuffer(arg->type, NULL);
    }
}

void toCBuffer(TemplateParameter *tp, OutBuffer *buf, HdrGenState *hgs)
{
    PrettyPrintVisitor v(buf, hgs);
    tp->accept(&v);
}

void arrayObjectsToBuffer(OutBuffer *buf, Objects *objects)
{
    if (!objects || !objects->length)
        return;

    HdrGenState hgs;
    PrettyPrintVisitor v(buf, &hgs);
    for (size_t i = 0; i < objects->length; i++)
    {
        RootObject *o = (*objects)[i];
        if (i)
            buf->writestring(", ");
        v.objectToBuffer(o);
    }
}

/*************************************************************
 * Pretty print function parameters.
 * Params:
 *  parameters = parameters to print, such as TypeFunction.parameters.
 *  varargs = kind of varargs, see TypeFunction.varargs.
 * Returns: Null-terminated string representing parameters.
 */
const char *parametersTypeToChars(ParameterList pl)
{
    OutBuffer buf;
    HdrGenState hgs;
    PrettyPrintVisitor v(&buf, &hgs);
    v.parametersToBuffer(pl.parameters, pl.varargs);
    return buf.extractChars();
}

/*************************************************************
 * Pretty print function parameter.
 * Params:
 *  parameter = parameter to print.
 *  tf = TypeFunction which holds parameter.
 *  fullQual = whether to fully qualify types.
 * Returns: Null-terminated string representing parameters.
 */
const char *parameterToChars(Parameter *parameter, TypeFunction *tf, bool fullQual)
{
    OutBuffer buf;
    HdrGenState hgs;
    hgs.fullQual = fullQual;
    PrettyPrintVisitor v(&buf, &hgs);

    parameter->accept(&v);
    if (tf->parameterList.varargs == 2 && parameter == tf->parameterList[tf->parameterList.parameters->length - 1])
    {
        buf.writestring("...");
    }
    return buf.extractChars();
}
