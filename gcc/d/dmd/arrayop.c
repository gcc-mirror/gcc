
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/arrayop.c
 */

#include "root/dsystem.h"
#include "root/rmem.h"
#include "root/aav.h"

#include "mars.h"
#include "expression.h"
#include "statement.h"
#include "mtype.h"
#include "declaration.h"
#include "scope.h"
#include "id.h"
#include "module.h"
#include "init.h"
#include "tokens.h"

void buildArrayIdent(Expression *e, OutBuffer *buf, Expressions *arguments);
Expression *buildArrayLoop(Expression *e, Parameters *fparams);

/**************************************
 * Hash table of array op functions already generated or known about.
 */

AA *arrayfuncs;

/**************************************
 * Structure to contain information needed to insert an array op call
 */

FuncDeclaration *buildArrayOp(Identifier *ident, BinExp *exp, Scope *sc)
{
    Parameters *fparams = new Parameters();
    Expression *loopbody = buildArrayLoop(exp, fparams);

    /* Construct the function body:
     *  foreach (i; 0 .. p.length)    for (size_t i = 0; i < p.length; i++)
     *      loopbody;
     *  return p;
     */

    Parameter *p = (*fparams)[0];
    // foreach (i; 0 .. p.length)
    Statement *s1 = new ForeachRangeStatement(Loc(), TOKforeach,
        new Parameter(0, NULL, Id::p, NULL, NULL),
        new IntegerExp(Loc(), 0, Type::tsize_t),
        new ArrayLengthExp(Loc(), new IdentifierExp(Loc(), p->ident)),
        new ExpStatement(Loc(), loopbody),
        Loc());
    //printf("%s\n", s1->toChars());
    Statement *s2 = new ReturnStatement(Loc(), new IdentifierExp(Loc(), p->ident));
    //printf("s2: %s\n", s2->toChars());
    Statement *fbody = new CompoundStatement(Loc(), s1, s2);

    // Built-in array ops should be @trusted, pure, nothrow and nogc
    StorageClass stc = STCtrusted | STCpure | STCnothrow | STCnogc;

    /* Construct the function
     */
    TypeFunction *ftype = new TypeFunction(ParameterList(fparams), exp->e1->type, LINKc, stc);
    //printf("fd: %s %s\n", ident->toChars(), ftype->toChars());
    FuncDeclaration *fd = new FuncDeclaration(Loc(), Loc(), ident, STCundefined, ftype);
    fd->fbody = fbody;
    fd->protection = Prot(Prot::public_);
    fd->linkage = LINKc;
    fd->isArrayOp = 1;

    sc->_module->importedFrom->members->push(fd);

    sc = sc->push();
    sc->parent = sc->_module->importedFrom;
    sc->stc = 0;
    sc->linkage = LINKc;
    dsymbolSemantic(fd, sc);
    semantic2(fd, sc);
    unsigned errors = global.startGagging();
    semantic3(fd, sc);
    if (global.endGagging(errors))
    {
        fd->type = Type::terror;
        fd->errors = true;
        fd->fbody = NULL;
    }
    sc->pop();

    return fd;
}

/**********************************************
 * Check that there are no uses of arrays without [].
 */
bool isArrayOpValid(Expression *e)
{
    if (e->op == TOKslice)
        return true;
    if (e->op == TOKarrayliteral)
    {
        Type *t = e->type->toBasetype();
        while (t->ty == Tarray || t->ty == Tsarray)
            t = t->nextOf()->toBasetype();
        return (t->ty != Tvoid);
    }
    Type *tb = e->type->toBasetype();
    if (tb->ty == Tarray || tb->ty == Tsarray)
    {
        if (isUnaArrayOp(e->op))
        {
             return isArrayOpValid(((UnaExp *)e)->e1);
        }
        if (isBinArrayOp(e->op) ||
            isBinAssignArrayOp(e->op) ||
            e->op == TOKassign)
        {
            BinExp *be = (BinExp *)e;
            return isArrayOpValid(be->e1) && isArrayOpValid(be->e2);
        }
        if (e->op == TOKconstruct)
        {
            BinExp *be = (BinExp *)e;
            return be->e1->op == TOKslice && isArrayOpValid(be->e2);
        }
        if (e->op == TOKcall)
        {
             return false; // TODO: Decide if [] is required after arrayop calls.
        }
        else
        {
            return false;
        }
    }
    return true;
}

bool isNonAssignmentArrayOp(Expression *e)
{
    if (e->op == TOKslice)
        return isNonAssignmentArrayOp(((SliceExp *)e)->e1);

    Type *tb = e->type->toBasetype();
    if (tb->ty == Tarray || tb->ty == Tsarray)
    {
        return (isUnaArrayOp(e->op) || isBinArrayOp(e->op));
    }
    return false;
}

bool checkNonAssignmentArrayOp(Expression *e, bool suggestion)
{
    if (isNonAssignmentArrayOp(e))
    {
        const char *s = "";
        if (suggestion)
            s = " (possible missing [])";
        e->error("array operation %s without destination memory not allowed%s", e->toChars(), s);
        return true;
    }
    return false;
}

/***********************************
 * Construct the array operation expression.
 */

Expression *arrayOp(BinExp *e, Scope *sc)
{
    //printf("BinExp::arrayOp() %s\n", toChars());

    Type *tb = e->type->toBasetype();
    assert(tb->ty == Tarray || tb->ty == Tsarray);
    Type *tbn = tb->nextOf()->toBasetype();
    if (tbn->ty == Tvoid)
    {
        e->error("cannot perform array operations on void[] arrays");
        return new ErrorExp();
    }
    if (!isArrayOpValid(e))
    {
        e->error("invalid array operation %s (possible missing [])", e->toChars());
        return new ErrorExp();
    }

    Expressions *arguments = new Expressions();

    /* The expression to generate an array operation for is mangled
     * into a name to use as the array operation function name.
     * Mangle in the operands and operators in RPN order, and type.
     */
    OutBuffer buf;
    buf.writestring("_array");
    buildArrayIdent(e, &buf, arguments);
    buf.writeByte('_');

    /* Append deco of array element type
     */
    buf.writestring(e->type->toBasetype()->nextOf()->toBasetype()->mutableOf()->deco);

    char *name = buf.peekChars();
    Identifier *ident = Identifier::idPool(name);

    FuncDeclaration **pFd = (FuncDeclaration **)dmd_aaGet(&arrayfuncs, (void *)ident);
    FuncDeclaration *fd = *pFd;

    if (!fd)
        fd = buildArrayOp(ident, e, sc);

    if (fd && fd->errors)
    {
        const char *fmt;
        if (tbn->ty == Tstruct || tbn->ty == Tclass)
            fmt = "invalid array operation '%s' because %s doesn't support necessary arithmetic operations";
        else if (!tbn->isscalar())
            fmt = "invalid array operation '%s' because %s is not a scalar type";
        else
            fmt = "invalid array operation '%s' for element type %s";

        e->error(fmt, e->toChars(), tbn->toChars());
        return new ErrorExp();
    }

    *pFd = fd;

    Expression *ev = new VarExp(e->loc, fd);
    Expression *ec = new CallExp(e->loc, ev, arguments);

    return expressionSemantic(ec, sc);
}

Expression *arrayOp(BinAssignExp *e, Scope *sc)
{
    //printf("BinAssignExp::arrayOp() %s\n", toChars());

    /* Check that the elements of e1 can be assigned to
     */
    Type *tn = e->e1->type->toBasetype()->nextOf();

    if (tn && (!tn->isMutable() || !tn->isAssignable()))
    {
        e->error("slice %s is not mutable", e->e1->toChars());
        return new ErrorExp();
    }
    if (e->e1->op == TOKarrayliteral)
    {
        return e->e1->modifiableLvalue(sc, e->e1);
    }

    return arrayOp((BinExp *)e, sc);
}

/******************************************
 * Construct the identifier for the array operation function,
 * and build the argument list to pass to it.
 */

void buildArrayIdent(Expression *e, OutBuffer *buf, Expressions *arguments)
{
    class BuildArrayIdentVisitor : public Visitor
    {
        OutBuffer *buf;
        Expressions *arguments;
    public:
        BuildArrayIdentVisitor(OutBuffer *buf, Expressions *arguments)
            : buf(buf), arguments(arguments)
        {
        }

        void visit(Expression *e)
        {
            buf->writestring("Exp");
            arguments->shift(e);
        }

        void visit(CastExp *e)
        {
            Type *tb = e->type->toBasetype();
            if (tb->ty == Tarray || tb->ty == Tsarray)
            {
                e->e1->accept(this);
            }
            else
                visit((Expression *)e);
        }

        void visit(ArrayLiteralExp *e)
        {
            buf->writestring("Slice");
            arguments->shift(e);
        }

        void visit(SliceExp *e)
        {
            buf->writestring("Slice");
            arguments->shift(e);
        }

        void visit(AssignExp *e)
        {
            /* Evaluate assign expressions right to left
             */
            e->e2->accept(this);
            e->e1->accept(this);
            buf->writestring("Assign");
        }

        void visit(BinAssignExp *e)
        {
            /* Evaluate assign expressions right to left
             */
            e->e2->accept(this);
            e->e1->accept(this);
            const char *s;
            switch(e->op)
            {
            case TOKaddass: s = "Addass"; break;
            case TOKminass: s = "Minass"; break;
            case TOKmulass: s = "Mulass"; break;
            case TOKdivass: s = "Divass"; break;
            case TOKmodass: s = "Modass"; break;
            case TOKxorass: s = "Xorass"; break;
            case TOKandass: s = "Andass"; break;
            case TOKorass:  s = "Orass";  break;
            case TOKpowass: s = "Powass"; break;
            default: assert(0);
            }
            buf->writestring(s);
        }

        void visit(NegExp *e)
        {
            e->e1->accept(this);
            buf->writestring("Neg");
        }

        void visit(ComExp *e)
        {
            e->e1->accept(this);
            buf->writestring("Com");
        }

        void visit(BinExp *e)
        {
            /* Evaluate assign expressions left to right
             */
            const char *s = NULL;
            switch(e->op)
            {
            case TOKadd: s = "Add"; break;
            case TOKmin: s = "Min"; break;
            case TOKmul: s = "Mul"; break;
            case TOKdiv: s = "Div"; break;
            case TOKmod: s = "Mod"; break;
            case TOKxor: s = "Xor"; break;
            case TOKand: s = "And"; break;
            case TOKor:  s = "Or";  break;
            case TOKpow: s = "Pow"; break;
            default: break;
            }
            if (s)
            {
                Type *tb = e->type->toBasetype();
                Type *t1 = e->e1->type->toBasetype();
                Type *t2 = e->e2->type->toBasetype();
                e->e1->accept(this);
                if (t1->ty == Tarray &&
                    ((t2->ty == Tarray && !t1->equivalent(tb)) ||
                     (t2->ty != Tarray && !t1->nextOf()->equivalent(e->e2->type))))
                {
                    // Bugzilla 12780: if A is narrower than B
                    //  A[] op B[]
                    //  A[] op B
                    buf->writestring("Of");
                    buf->writestring(t1->nextOf()->mutableOf()->deco);
                }
                e->e2->accept(this);
                if (t2->ty == Tarray &&
                    ((t1->ty == Tarray && !t2->equivalent(tb)) ||
                     (t1->ty != Tarray && !t2->nextOf()->equivalent(e->e1->type))))
                {
                    // Bugzilla 12780: if B is narrower than A:
                    //  A[] op B[]
                    //  A op B[]
                    buf->writestring("Of");
                    buf->writestring(t2->nextOf()->mutableOf()->deco);
                }
                buf->writestring(s);
            }
            else
                visit((Expression *)e);
        }
    };

    BuildArrayIdentVisitor v(buf, arguments);
    e->accept(&v);
}

/******************************************
 * Construct the inner loop for the array operation function,
 * and build the parameter list.
 */

Expression *buildArrayLoop(Expression *e, Parameters *fparams)
{
    class BuildArrayLoopVisitor : public Visitor
    {
        Parameters *fparams;
        Expression *result;

    public:
        BuildArrayLoopVisitor(Parameters *fparams)
            : fparams(fparams), result(NULL)
        {
        }

        void visit(Expression *e)
        {
            Identifier *id = Identifier::generateId("c", fparams->length);
            Parameter *param = new Parameter(0, e->type, id, NULL, NULL);
            fparams->shift(param);
            result = new IdentifierExp(Loc(), id);
        }

        void visit(CastExp *e)
        {
            Type *tb = e->type->toBasetype();
            if (tb->ty == Tarray || tb->ty == Tsarray)
            {
                e->e1->accept(this);
            }
            else
                visit((Expression *)e);
        }

        void visit(ArrayLiteralExp *e)
        {
            Identifier *id = Identifier::generateId("p", fparams->length);
            Parameter *param = new Parameter(STCconst, e->type, id, NULL, NULL);
            fparams->shift(param);
            Expression *ie = new IdentifierExp(Loc(), id);
            Expression *index = new IdentifierExp(Loc(), Id::p);
            result = new ArrayExp(Loc(), ie, index);
        }

        void visit(SliceExp *e)
        {
            Identifier *id = Identifier::generateId("p", fparams->length);
            Parameter *param = new Parameter(STCconst, e->type, id, NULL, NULL);
            fparams->shift(param);
            Expression *ie = new IdentifierExp(Loc(), id);
            Expression *index = new IdentifierExp(Loc(), Id::p);
            result = new ArrayExp(Loc(), ie, index);
        }

        void visit(AssignExp *e)
        {
            /* Evaluate assign expressions right to left
             */
            Expression *ex2 = buildArrayLoop(e->e2);
            /* Need the cast because:
             *   b = c + p[i];
             * where b is a byte fails because (c + p[i]) is an int
             * which cannot be implicitly cast to byte.
             */
            ex2 = new CastExp(Loc(), ex2, e->e1->type->nextOf());
            Expression *ex1 = buildArrayLoop(e->e1);
            Parameter *param = (*fparams)[0];
            param->storageClass = 0;
            result = new AssignExp(Loc(), ex1, ex2);
        }

        void visit(BinAssignExp *e)
        {
            /* Evaluate assign expressions right to left
             */
            Expression *ex2 = buildArrayLoop(e->e2);
            Expression *ex1 = buildArrayLoop(e->e1);
            Parameter *param = (*fparams)[0];
            param->storageClass = 0;
            switch(e->op)
            {
            case TOKaddass: result = new AddAssignExp(e->loc, ex1, ex2); return;
            case TOKminass: result = new MinAssignExp(e->loc, ex1, ex2); return;
            case TOKmulass: result = new MulAssignExp(e->loc, ex1, ex2); return;
            case TOKdivass: result = new DivAssignExp(e->loc, ex1, ex2); return;
            case TOKmodass: result = new ModAssignExp(e->loc, ex1, ex2); return;
            case TOKxorass: result = new XorAssignExp(e->loc, ex1, ex2); return;
            case TOKandass: result = new AndAssignExp(e->loc, ex1, ex2); return;
            case TOKorass:  result = new OrAssignExp(e->loc, ex1, ex2); return;
            case TOKpowass: result = new PowAssignExp(e->loc, ex1, ex2); return;
            default:
                assert(0);
            }
        }

        void visit(NegExp *e)
        {
            Expression *ex1 = buildArrayLoop(e->e1);
            result = new NegExp(Loc(), ex1);
        }

        void visit(ComExp *e)
        {
            Expression *ex1 = buildArrayLoop(e->e1);
            result = new ComExp(Loc(), ex1);
        }

        void visit(BinExp *e)
        {
            if (isBinArrayOp(e->op))
            {
                /* Evaluate assign expressions left to right
                 */
                BinExp *be = (BinExp *)e->copy();
                be->e1 = buildArrayLoop(be->e1);
                be->e2 = buildArrayLoop(be->e2);
                be->type = NULL;
                result = be;
                return;
            }
            else
            {
                visit((Expression *)e);
                return;
            }
        }

        Expression *buildArrayLoop(Expression *e)
        {
            e->accept(this);
            return result;
        }
    };

    BuildArrayLoopVisitor v(fparams);
    return v.buildArrayLoop(e);
}

/***********************************************
 * Test if expression is a unary array op.
 */

bool isUnaArrayOp(TOK op)
{
    switch (op)
    {
    case TOKneg:
    case TOKtilde:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if expression is a binary array op.
 */

bool isBinArrayOp(TOK op)
{
    switch (op)
    {
    case TOKadd:
    case TOKmin:
    case TOKmul:
    case TOKdiv:
    case TOKmod:
    case TOKxor:
    case TOKand:
    case TOKor:
    case TOKpow:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if expression is a binary assignment array op.
 */

bool isBinAssignArrayOp(TOK op)
{
    switch (op)
    {
    case TOKaddass:
    case TOKminass:
    case TOKmulass:
    case TOKdivass:
    case TOKmodass:
    case TOKxorass:
    case TOKandass:
    case TOKorass:
    case TOKpowass:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if operand is a valid array op operand.
 */

bool isArrayOpOperand(Expression *e)
{
    //printf("Expression::isArrayOpOperand() %s\n", e->toChars());
    if (e->op == TOKslice)
        return true;
    if (e->op == TOKarrayliteral)
    {
        Type *t = e->type->toBasetype();
        while (t->ty == Tarray || t->ty == Tsarray)
            t = t->nextOf()->toBasetype();
        return (t->ty != Tvoid);
    }
    Type *tb = e->type->toBasetype();
    if (tb->ty == Tarray)
    {
        return (isUnaArrayOp(e->op) ||
                isBinArrayOp(e->op) ||
                isBinAssignArrayOp(e->op) ||
                e->op == TOKassign);
    }
    return false;
}
