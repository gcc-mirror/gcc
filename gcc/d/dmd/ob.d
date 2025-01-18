/**
 * Flow analysis for Ownership/Borrowing
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/ob.d, _ob.d)
 * Documentation:  https://dlang.org/phobos/dmd_escape.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/ob.d
 * References:  https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md Argument Ownership and Function Calls
 */

module dmd.ob;

import core.stdc.stdio : printf;
import core.stdc.stdlib;
import core.stdc.string;

import dmd.root.array;
import dmd.rootobject;
import dmd.root.rmem;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expression;

import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.printast;
import dmd.statement;
import dmd.stmtstate;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;
import dmd.visitor.foreachvar;

import dmd.root.bitarray;
import dmd.common.outbuffer;

/**********************************
 * Perform ownership/borrowing checks for funcdecl.
 * Does not modify the AST, just checks for errors.
 */

void oblive(FuncDeclaration funcdecl)
{
    //printf("oblive() %s\n", funcdecl.toChars());
    //printf("fbody: %s\n", funcdecl.fbody.toChars());
    ObState obstate;

    /* Build the flow graph
     */
    setLabelStatementExtraFields(funcdecl.labtab);
    toObNodes(obstate.nodes, funcdecl.fbody);
    insertFinallyBlockCalls(obstate.nodes);
    insertFinallyBlockGotos(obstate.nodes);
    removeUnreachable(obstate.nodes);
    computePreds(obstate.nodes);

    numberNodes(obstate.nodes);
    //foreach (ob; obstate.nodes) ob.print();

    collectVars(funcdecl, obstate.vars);
    allocStates(obstate);
    doDataFlowAnalysis(obstate);

    checkObErrors(obstate);
}

alias ObNodes = Array!(ObNode*);

alias StmtState = dmd.stmtstate.StmtState!ObNode;

/*******************************************
 * Collect the state information.
 */
struct ObState
{
    ObNodes nodes;
    VarDeclarations vars;

    Array!size_t varStack;      /// temporary storage
    Array!bool mutableStack;    /// parallel to varStack[], is type mutable?

    PtrVarState[] varPool;      /// memory pool

    ~this()
    {
        mem.xfree(varPool.ptr);
    }
}

/***********************************************
 * A node in the function's expression graph, and its edges to predecessors and successors.
 */
struct ObNode
{
    Expression exp;     /// expression for the node
    ObNodes preds;      /// predecessors
    ObNodes succs;      /// successors
    ObNode* tryBlock;   /// try-finally block we're inside
    ObType obtype;
    uint index;         /// index of this in obnodes

    PtrVarState[] gen;    /// new states generated for this node
    PtrVarState[] input;  /// variable states on entry to exp
    PtrVarState[] output; /// variable states on exit to exp

    this(ObNode* tryBlock) scope
    {
        this.tryBlock = tryBlock;
    }

    void print()
    {
        printf("%d: %s %s\n", index, obtype.toString.ptr, exp ? exp.toChars() : "-");
        printf("  preds: ");
        foreach (ob; preds)
            printf(" %d", ob.index);
        printf("\n  succs: ");
        foreach (ob; succs)
            printf(" %d", ob.index);
        printf("\n\n");
    }
}


enum ObType : ubyte
{
    goto_,              /// goto one of the succs[]
    return_,            /// returns from function
    retexp,             /// returns expression from function
    throw_,             /// exits with throw
    exit,               /// exits program
    try_,
    finally_,
    fend,
}

string toString(ObType obtype) @safe
{
    return obtype == ObType.goto_     ? "goto  "  :
           obtype == ObType.return_   ? "ret   "  :
           obtype == ObType.retexp    ? "retexp"  :
           obtype == ObType.throw_    ? "throw"   :
           obtype == ObType.exit      ? "exit"    :
           obtype == ObType.try_      ? "try"     :
           obtype == ObType.finally_  ? "finally" :
           obtype == ObType.fend      ? "fend"    :
           "---";
}

/***********
 Pointer variable states:

    Initial     state is not known; ignore for now

    Undefined   not in a usable state

                T* p = void;

    Owner       mutable pointer

                T* p = initializer;

    Borrowed    scope mutable pointer, borrowed from [p]

                T* p = initializer;
                scope T* b = p;

    Readonly    scope const pointer, copied from [p]

                T* p = initializer;
                scope const(T)* cp = p;

 Examples:

    T* p = initializer; // p is owner
    T** pp = &p;        // pp borrows from p

    T* p = initialize;  // p is owner
    T* q = p;           // transfer: q is owner, p is undefined
 */

enum PtrState : ubyte
{
    Initial, Undefined, Owner, Borrowed, Readonly
}

/************
 */
const(char)* PtrStateToChars(PtrState state)
{
    return toString(state).ptr;
}

string toString(PtrState state) @safe
{
    return ["Initial", "Undefined", "Owner", "Borrowed", "Readonly"][state];
}

/******
 * Carries the state of a pointer variable.
 */
struct PtrVarState
{
    BitArray deps;           /// dependencies
    PtrState state;          /// state the pointer variable is in

    void opAssign(const ref PtrVarState pvs)
    {
        state = pvs.state;
        deps = pvs.deps;
    }

    /* Combine `this` and `pvs` into `this`,
     * on the idea that the `this` and the `pvs` paths
     * are being merged
     * Params:
     *  pvs = path to be merged with `this`
     *  vi = variable's index into gen[]
     *  gen = array of variable states
     */
    void combine(ref PtrVarState pvs, size_t vi, PtrVarState[] gen)
    {
        static uint X(PtrState x1, PtrState x2) { return x1 * (PtrState.max + 1) + x2; }

        with (PtrState)
        {
            switch (X(state, pvs.state))
            {
                case X(Initial, Initial):
                    break;

                case X(Initial, Owner    ):
                case X(Initial, Borrowed ):
                case X(Initial, Readonly ):
                    // Transfer state to `this`
                    state = pvs.state;
                    deps = pvs.deps;
                    break;

                case X(Owner,    Initial):
                case X(Borrowed, Initial):
                case X(Readonly, Initial):
                    break;

                case X(Undefined, Initial):
                case X(Undefined, Undefined):
                case X(Undefined, Owner    ):
                case X(Undefined, Borrowed ):
                case X(Undefined, Readonly ):
                    break;

                case X(Owner    , Owner   ):
                    break;

                case X(Borrowed , Borrowed):
                case X(Readonly , Readonly):
                    deps.or(pvs.deps);
                    break;

                default:
                    makeUndefined(vi, gen);
                    break;
            }
        }
    }

    bool opEquals(const ref PtrVarState pvs) const
    {
        return state == pvs.state &&
                deps == pvs.deps;
    }

    /***********************
     * Print a bracketed list of all the variables that depend on 'this'
     * Params:
     *  vars = variables that depend on 'this'
     */
    void print(VarDeclaration[] vars)
    {
        string s = toString(state);
        printf("%.*s [", cast(int)s.length, s.ptr);
        assert(vars.length == deps.length);
        OutBuffer buf;
        depsToBuf(buf, vars);
        auto t = buf[];
        printf("%.*s]\n", cast(int)t.length, t.ptr);
    }

    /*****************************
     * Produce a user-readable comma separated string of the
     * dependencies.
     * Params:
     *  buf = write resulting string here
     *  vars = array from which to get the variable names
     */
    void depsToBuf(ref OutBuffer buf, const VarDeclaration[] vars)
    {
        bool any = false;
        foreach (i; 0 .. deps.length)
        {
            if (deps[i])
            {
                if (any)
                    buf.writestring(", ");
                buf.writestring(vars[i].toString());
                any = true;
            }
        }
    }
}


/*****************************************
 * Set the `.extra` field for LabelStatements in labtab[].
 */
void setLabelStatementExtraFields(DsymbolTable labtab)
{
    if (labtab)
        foreach (keyValue; labtab.tab.asRange)
        {
            //printf("  KV: %s = %s\n", keyValue.key.toChars(), keyValue.value.toChars());
            auto label = cast(LabelDsymbol)keyValue.value;
            if (label.statement)
                label.statement.extra = cast(void*) new ObNode(null);
        }
}

/*****************************************
 * Convert statement into ObNodes.
 */

void toObNodes(ref ObNodes obnodes, Statement s)
{
    ObNode* curblock = new ObNode(null);
    obnodes.push(curblock);

    void visit(Statement s, StmtState* stmtstate)
    {
        if (!s)
            return;

        ObNode* newNode()
        {
            return new ObNode(stmtstate.tryBlock);
        }

        ObNode* nextNodeIs(ObNode* ob)
        {
            obnodes.push(ob);
            curblock = ob;
            return ob;
        }

        ObNode* nextNode()
        {
            return nextNodeIs(newNode());
        }

        ObNode* gotoNextNodeIs(ObNode* ob)
        {
            obnodes.push(ob);
            curblock.succs.push(ob);
            curblock = ob;
            return ob;
        }

        // block_goto(blx, BC.goto_, null)
        ObNode* gotoNextNode()
        {
            return gotoNextNodeIs(newNode());
        }

        /***
         * Doing a goto to dest
         */
        ObNode* gotoDest(ObNode* dest)
        {
            curblock.succs.push(dest);
            return nextNode();
        }

        void visitExp(ExpStatement s)
        {
            curblock.obtype = ObType.goto_;
            curblock.exp = s.exp;
            gotoNextNode();
        }

        void visitDtorExp(DtorExpStatement s)
        {
            visitExp(s);
        }

        void visitCompound(CompoundStatement s)
        {
            if (s.statements)
            {
                foreach (s2; *s.statements)
                {
                    visit(s2, stmtstate);
                }
            }
        }

        void visitCompoundDeclaration(CompoundDeclarationStatement s)
        {
            visitCompound(s);
        }

        void visitUnrolledLoop(UnrolledLoopStatement s)
        {
            StmtState mystate = StmtState(stmtstate, s);
            mystate.breakBlock = newNode();

            gotoNextNode();

            foreach (s2; *s.statements)
            {
                if (s2)
                {
                    mystate.contBlock = newNode();

                    visit(s2, &mystate);

                    gotoNextNodeIs(mystate.contBlock);
                }
            }

            gotoNextNodeIs(mystate.breakBlock);
        }

        void visitScope(ScopeStatement s)
        {
            if (s.statement)
            {
                StmtState mystate = StmtState(stmtstate, s);

                if (mystate.prev.ident)
                    mystate.ident = mystate.prev.ident;

                visit(s.statement, &mystate);

                if (mystate.breakBlock)
                    gotoNextNodeIs(mystate.breakBlock);
            }
        }

        void visitDo(DoStatement s)
        {
            StmtState mystate = StmtState(stmtstate, s);
            mystate.breakBlock = newNode();
            mystate.contBlock = newNode();

            auto bpre = curblock;

            auto ob = newNode();
            obnodes.push(ob);
            curblock.succs.push(ob);
            curblock = ob;
            bpre.succs.push(curblock);

            mystate.contBlock.succs.push(curblock);
            mystate.contBlock.succs.push(mystate.breakBlock);

            visit(s._body, &mystate);

            gotoNextNodeIs(mystate.contBlock);
            mystate.contBlock.exp = s.condition;
            nextNodeIs(mystate.breakBlock);
        }

        void visitFor(ForStatement s)
        {
            //printf("visit(ForStatement)) %u..%u\n", s.loc.linnum, s.endloc.linnum);
            StmtState mystate = StmtState(stmtstate, s);
            mystate.breakBlock = newNode();
            mystate.contBlock = newNode();

            visit(s._init, &mystate);

            auto bcond = gotoNextNode();
            mystate.contBlock.succs.push(bcond);

            if (s.condition)
            {
                bcond.exp = s.condition;
                auto ob = newNode();
                obnodes.push(ob);
                bcond.succs.push(ob);
                bcond.succs.push(mystate.breakBlock);
                curblock = ob;
            }
            else
            {   /* No conditional, it's a straight goto
                 */
                bcond.exp = s.condition;
                bcond.succs.push(nextNode());
            }

            visit(s._body, &mystate);
            /* End of the body goes to the continue block
             */
            curblock.succs.push(mystate.contBlock);
            nextNodeIs(mystate.contBlock);

            if (s.increment)
                curblock.exp = s.increment;

            /* The 'break' block follows the for statement.
             */
            nextNodeIs(mystate.breakBlock);
        }

        void visitIf(IfStatement s)
        {
            StmtState mystate = StmtState(stmtstate, s);

            // bexit is the block that gets control after this IfStatement is done
            auto bexit = mystate.breakBlock ? mystate.breakBlock : newNode();

            curblock.exp = s.condition;

            auto bcond = curblock;
            gotoNextNode();

            visit(s.ifbody, &mystate);
            curblock.succs.push(bexit);

            if (s.elsebody)
            {
                bcond.succs.push(nextNode());

                visit(s.elsebody, &mystate);

                gotoNextNodeIs(bexit);
            }
            else
            {
                bcond.succs.push(bexit);
                nextNodeIs(bexit);
            }
        }

        void visitSwitch(SwitchStatement s)
        {
            StmtState mystate = StmtState(stmtstate, s);

            mystate.switchBlock = curblock;

            /* Block for where "break" goes to
             */
            mystate.breakBlock = newNode();

            /* Block for where "default" goes to.
             * If there is a default statement, then that is where default goes.
             * If not, then do:
             *   default: break;
             * by making the default block the same as the break block.
             */
            mystate.defaultBlock = s.sdefault ? newNode() : mystate.breakBlock;

            const numcases = s.cases ? s.cases.length : 0;

            /* allocate a block for each case
             */
            if (numcases)
                foreach (cs; *s.cases)
                {
                    cs.extra = cast(void*)newNode();
                }

            curblock.exp = s.condition;

            if (s.hasVars)
            {   /* Generate a sequence of if-then-else blocks for the cases.
                 */
                if (numcases)
                    foreach (cs; *s.cases)
                    {
                        auto ecase = newNode();
                        obnodes.push(ecase);
                        ecase.exp = cs.exp;
                        curblock.succs.push(ecase);

                        auto cn = cast(ObNode*)cs.extra;
                        ecase.succs.push(cn);
                        ecase.succs.push(nextNode());
                    }

                /* The final 'else' clause goes to the default
                 */
                curblock.succs.push(mystate.defaultBlock);
                nextNode();

                visit(s._body, &mystate);

                /* Have the end of the switch body fall through to the block
                 * following the switch statement.
                 */
                gotoNextNodeIs(mystate.breakBlock);
                return;
            }

            auto ob = newNode();
            obnodes.push(ob);
            curblock = ob;

            mystate.switchBlock.succs.push(mystate.defaultBlock);

            visit(s._body, &mystate);

            /* Have the end of the switch body fall through to the block
             * following the switch statement.
             */
            gotoNextNodeIs(mystate.breakBlock);
        }

        void visitCase(CaseStatement s)
        {
            auto cb = cast(ObNode*)s.extra;
            cb.tryBlock = stmtstate.tryBlock;
            auto bsw = stmtstate.getSwitchBlock();
            bsw.succs.push(cb);
            gotoNextNodeIs(cb);

            visit(s.statement, stmtstate);
        }

        void visitDefault(DefaultStatement s)
        {
            auto bdefault = stmtstate.getDefaultBlock;
            bdefault.tryBlock = stmtstate.tryBlock;
            gotoNextNodeIs(bdefault);
            visit(s.statement, stmtstate);
        }

        void visitGotoDefault(GotoDefaultStatement s)
        {
            gotoDest(stmtstate.getDefaultBlock);
        }

        void visitGotoCase(GotoCaseStatement s)
        {
            gotoDest(cast(ObNode*)s.cs.extra);
        }

        void visitSwitchError(SwitchErrorStatement s)
        {
            curblock.obtype = ObType.throw_;
            curblock.exp = s.exp;

            nextNode();
        }

        void visitReturn(ReturnStatement s)
        {
            //printf("visitReturn() %s\n", s.toChars());
            curblock.obtype = s.exp && s.exp.type.toBasetype().ty != Tvoid
                        ? ObType.retexp
                        : ObType.return_;
            curblock.exp = s.exp;

            nextNode();
        }

        void visitBreak(BreakStatement s)
        {
            gotoDest(stmtstate.getBreakBlock(s.ident));
        }

        void visitContinue(ContinueStatement s)
        {
            gotoDest(stmtstate.getContBlock(s.ident));
        }

        void visitWith(WithStatement s)
        {
            visit(s._body, stmtstate);
        }

        void visitTryCatch(TryCatchStatement s)
        {
            /* tryblock
             * body
             * breakBlock
             * catches
             * breakBlock2
             */

            StmtState mystate = StmtState(stmtstate, s);
            mystate.breakBlock = newNode();

            auto tryblock = gotoNextNode();

            visit(s._body, &mystate);

            gotoNextNodeIs(mystate.breakBlock);

            // create new break block that follows all the catches
            auto breakBlock2 = newNode();

            gotoDest(breakBlock2);

            foreach (cs; *s.catches)
            {
                /* Each catch block is a successor to tryblock
                 * and the last block of try body
                 */
                StmtState catchState = StmtState(stmtstate, s);

                auto bcatch = curblock;
                tryblock.succs.push(bcatch);
                mystate.breakBlock.succs.push(bcatch);

                nextNode();

                visit(cs.handler, &catchState);

                gotoDest(breakBlock2);
            }

            curblock.succs.push(breakBlock2);
            obnodes.push(breakBlock2);
            curblock = breakBlock2;
        }

        void visitTryFinally(TryFinallyStatement s)
        {
            /* Build this:
             *  1  goto     [2]
             *  2  try_     [3] [5] [7]
             *  3  body
             *  4  goto     [8]
             *  5  finally_ [6]
             *  6  finalbody
             *  7  fend     [8]
             *  8  lastblock
             */

            StmtState bodyState = StmtState(stmtstate, s);

            auto b2 = gotoNextNode();
            b2.obtype = ObType.try_;
            bodyState.tryBlock = b2;

            gotoNextNode();

            visit(s._body, &bodyState);

            auto b4 = gotoNextNode();

            auto b5 = newNode();
            b5.obtype = ObType.finally_;
            nextNodeIs(b5);
            gotoNextNode();

            StmtState finallyState = StmtState(stmtstate, s);
            visit(s.finalbody, &finallyState);

            auto b7 = gotoNextNode();
            b7.obtype = ObType.fend;

            auto b8 = gotoNextNode();

            b2.succs.push(b5);
            b2.succs.push(b7);

            b4.succs.push(b8);
        }

        void visitThrow(ThrowStatement s)
        {
            curblock.obtype = ObType.throw_;
            curblock.exp = s.exp;
            nextNode();
        }

        void visitGoto(GotoStatement s)
        {
            gotoDest(cast(ObNode*)s.label.statement.extra);
        }

        void visitLabel(LabelStatement s)
        {
            StmtState mystate = StmtState(stmtstate, s);
            mystate.ident = s.ident;

            auto ob = cast(ObNode*)s.extra;
            ob.tryBlock = mystate.tryBlock;
            visit(s.statement, &mystate);
        }

        final switch (s.stmt)
        {
            case STMT.Exp:                 visitExp(s.isExpStatement()); break;
            case STMT.DtorExp:             visitDtorExp(s.isDtorExpStatement()); break;
            case STMT.Compound:            visitCompound(s.isCompoundStatement()); break;
            case STMT.CompoundDeclaration: visitCompoundDeclaration(s.isCompoundDeclarationStatement()); break;
            case STMT.UnrolledLoop:        visitUnrolledLoop(s.isUnrolledLoopStatement()); break;
            case STMT.Scope:               visitScope(s.isScopeStatement()); break;
            case STMT.Do:                  visitDo(s.isDoStatement()); break;
            case STMT.For:                 visitFor(s.isForStatement()); break;
            case STMT.If:                  visitIf(s.isIfStatement()); break;
            case STMT.Switch:              visitSwitch(s.isSwitchStatement()); break;
            case STMT.Case:                visitCase(s.isCaseStatement()); break;
            case STMT.Default:             visitDefault(s.isDefaultStatement()); break;
            case STMT.GotoDefault:         visitGotoDefault(s.isGotoDefaultStatement()); break;
            case STMT.GotoCase:            visitGotoCase(s.isGotoCaseStatement()); break;
            case STMT.SwitchError:         visitSwitchError(s.isSwitchErrorStatement()); break;
            case STMT.Return:              visitReturn(s.isReturnStatement()); break;
            case STMT.Break:               visitBreak(s.isBreakStatement()); break;
            case STMT.Continue:            visitContinue(s.isContinueStatement()); break;
            case STMT.With:                visitWith(s.isWithStatement()); break;
            case STMT.TryCatch:            visitTryCatch(s.isTryCatchStatement()); break;
            case STMT.TryFinally:          visitTryFinally(s.isTryFinallyStatement()); break;
            case STMT.Throw:               visitThrow(s.isThrowStatement()); break;
            case STMT.Goto:                visitGoto(s.isGotoStatement()); break;
            case STMT.Label:               visitLabel(s.isLabelStatement()); break;

            case STMT.CompoundAsm:
            case STMT.Asm:
            case STMT.InlineAsm:
            case STMT.GccAsm:

            case STMT.Pragma:
            case STMT.Import:
            case STMT.ScopeGuard:
            case STMT.Error:
                break;          // ignore these

            case STMT.Foreach:
            case STMT.ForeachRange:
            case STMT.Debug:
            case STMT.CaseRange:
            case STMT.StaticForeach:
            case STMT.StaticAssert:
            case STMT.Conditional:
            case STMT.While:
            case STMT.Forwarding:
            case STMT.Mixin:
            case STMT.Peel:
            case STMT.Synchronized:
                debug printf("s: %s\n", toChars(s));
                assert(0);              // should have been rewritten
        }
    }

    StmtState stmtstate;
    visit(s, &stmtstate);
    curblock.obtype = ObType.return_;

    static if (0)
    {
        printf("toObNodes()\n");
        printf("------- before ----------\n");
        numberNodes(obnodes);
        foreach (ob; obnodes) ob.print();
        printf("-------------------------\n");
    }

    assert(stmtstate.breakBlock is null);
    assert(stmtstate.contBlock is null);
    assert(stmtstate.switchBlock is null);
    assert(stmtstate.defaultBlock is null);
    assert(stmtstate.tryBlock is null);
}

/***************************************************
 * Insert finally block calls when doing a goto from
 * inside a try block to outside.
 * Done after blocks are generated because then we know all
 * the edges of the graph, but before the pred's are computed.
 * Params:
 *      obnodes = graph of the function
 */

void insertFinallyBlockCalls(ref ObNodes obnodes)
{
    ObNode* bcret = null;
    ObNode* bcretexp = null;

    enum log = false;

    static if (log)
    {
        printf("insertFinallyBlockCalls()\n");
        printf("------- before ----------\n");
        numberNodes(obnodes);
        foreach (ob; obnodes) ob.print();
        printf("-------------------------\n");
    }

    foreach (ob; obnodes)
    {
        if (!ob.tryBlock)
            continue;

        switch (ob.obtype)
        {
            case ObType.return_:
                // Rewrite into a ObType.goto_ => ObType.return_
                if (!bcret)
                {
                    bcret = new ObNode();
                    bcret.obtype = ob.obtype;
                }
                ob.obtype = ObType.goto_;
                ob.succs.push(bcret);
                goto case_goto;

            case ObType.retexp:
                // Rewrite into a ObType.goto_ => ObType.retexp
                if (!bcretexp)
                {
                    bcretexp = new ObNode();
                    bcretexp.obtype = ob.obtype;
                }
                ob.obtype = ObType.goto_;
                ob.succs.push(bcretexp);
                goto case_goto;

            case ObType.goto_:
                if (ob.succs.length != 1)
                    break;

            case_goto:
            {
                auto target = ob.succs[0];              // destination of goto
                ob.succs.setDim(0);
                auto lasttry = target.tryBlock;
                auto blast = ob;
                for (auto bt = ob.tryBlock; bt != lasttry; bt = bt.tryBlock)
                {
                    assert(bt.obtype == ObType.try_);
                    auto bf = bt.succs[1];
                    assert(bf.obtype == ObType.finally_);
                    auto bfend = bt.succs[2];
                    assert(bfend.obtype == ObType.fend);

                    if (!blast.succs.contains(bf.succs[0]))
                        blast.succs.push(bf.succs[0]);

                    blast = bfend;
                }
                if (!blast.succs.contains(target))
                    blast.succs.push(target);

                break;
            }

            default:
                break;
        }
    }
    if (bcret)
        obnodes.push(bcret);
    if (bcretexp)
        obnodes.push(bcretexp);

    static if (log)
    {
        printf("------- after ----------\n");
        numberNodes(obnodes);
        foreach (ob; obnodes) ob.print();
        printf("-------------------------\n");
    }
}

/***************************************************
 * Remove try-finally scaffolding.
 * Params:
 *      obnodes = nodes for the function
 */

void insertFinallyBlockGotos(ref ObNodes obnodes)
{
    /* Remove all the try_, finally_, lpad and ret nodes.
     * Actually, just make them into no-ops.
     */
    foreach (ob; obnodes)
    {
        ob.tryBlock = null;
        switch (ob.obtype)
        {
            case ObType.try_:
                ob.obtype = ObType.goto_;
                ob.succs.remove(2);     // remove fend
                ob.succs.remove(1);     // remove finally_
                break;

            case ObType.finally_:
                ob.obtype = ObType.goto_;
                break;

            case ObType.fend:
                ob.obtype = ObType.goto_;
                break;

            default:
                break;
        }
    }
}

/*********************************
 * Set the `index` field of each ObNode
 * to its index in the `obnodes[]` array.
 */
void numberNodes(ref ObNodes obnodes) @safe
{
    //printf("numberNodes()\n");
    foreach (i, ob; obnodes)
    {
        //printf("ob = %d, %p\n", i, ob);
        ob.index = cast(uint)i;
    }

    // Verify that nodes do not appear more than once in obnodes[]
    debug
    foreach (i, ob; obnodes)
    {
        assert(ob.index == cast(uint)i);
    }
}


/*********************************
 * Remove unreachable nodes and compress
 * them out of obnodes[].
 * Params:
 *      obnodes = array of nodes
 */
void removeUnreachable(ref ObNodes obnodes)
{
    if (!obnodes.length)
        return;

    /* Mark all nodes as unreachable,
     * temporarilly reusing ObNode.index
     */
    foreach (ob; obnodes)
        ob.index = 0;

    /* Recursively mark ob and all its successors as reachable
     */
    static void mark(ObNode* ob)
    {
        ob.index = 1;
        foreach (succ; ob.succs)
        {
            if (!succ.index)
                mark(succ);
        }
    }

    mark(obnodes[0]);   // first node is entry point

    /* Remove unreachable nodes by shifting the remainder left
     */
    size_t j = 1;
    foreach (i; 1 .. obnodes.length)
    {
        if (obnodes[i].index)
        {
            if (i != j)
                obnodes[j] = obnodes[i];
            ++j;
        }
        else
        {
            obnodes[i].destroy();
        }
    }
    obnodes.setDim(j);
}



/*************************************
 * Compute predecessors.
 */
void computePreds(ref ObNodes obnodes)
{
    foreach (ob; obnodes)
    {
        foreach (succ; ob.succs)
        {
            succ.preds.push(ob);
        }
    }
}

/*******************************
 * Are we interested in tracking variable `v`?
 */
bool isTrackableVar(VarDeclaration v)
{
    //printf("isTrackableVar() %s\n", v.toChars());
    auto tb = v.type.toBasetype();

    /* Assume class references are managed by the GC,
     * don't need to track them
     */
    if (tb.ty == Tclass)
        return false;

    /* Assume types with a destructor are doing their own tracking,
     * such as being a ref counted type
     */
//    if (v.needsScopeDtor())
//        return false;

    /* Not tracking function parameters that are not mutable
     */
    if (v.storage_class & STC.parameter && !tb.hasPointersToMutableFields())
        return false;

    /* Not tracking global variables
     */
    return !v.isDataseg();
}

/*******************************
 * Are we interested in tracking this expression?
 * Returns:
 *      variable if so, null if not
 */
VarDeclaration isTrackableVarExp(Expression e)
{
    if (auto ve = e.isVarExp())
    {
        if (auto v = ve.var.isVarDeclaration())
            if (isTrackableVar(v))
                return v;
    }
    return null;
}


/**************
 * Find the pointer variable declarations in this function,
 * and fill `vars` with them.
 * Params:
 *      funcdecl = function we are in
 *      vars = array to fill in
 */
void collectVars(FuncDeclaration funcdecl, out VarDeclarations vars)
{
    enum log = false;
    if (log)
        printf("----------------collectVars()---------------\n");

    if (funcdecl.parameters)
        foreach (v; (*funcdecl.parameters)[])
        {
            if (isTrackableVar(v))
                vars.push(v);
        }

    void dgVar(VarDeclaration v)
    {
        if (isTrackableVar(v))
            vars.push(v);
    }

    void dgExp(Expression e)
    {
        foreachVar(e, &dgVar);
    }

    foreachExpAndVar(funcdecl.fbody, &dgExp, &dgVar);

    static if (log)
    {
        foreach (i, v; vars[])
        {
            printf("vars[%d] = %s\n", cast(int)i, v.toChars());
        }
    }
}

/***********************************
 * Allocate BitArrays in PtrVarState.
 * Can be allocated much more efficiently by subdividing a single
 * large array of bits
 */
void allocDeps(PtrVarState[] pvss)
{
    //printf("allocDeps()\n");
    foreach (ref pvs; pvss)
    {
        pvs.deps.length = pvss.length;
    }
}


/**************************************
 * Allocate state variables foreach node.
 */
void allocStates(ref ObState obstate)
{
    //printf("---------------allocStates()------------------\n");
    const vlen = obstate.vars.length;
    PtrVarState* p = cast(PtrVarState*) mem.xcalloc(obstate.nodes.length * 3 * vlen, PtrVarState.sizeof);
    obstate.varPool = p[0 .. obstate.nodes.length * 3 * vlen];
    foreach (i, ob; obstate.nodes)
    {
        //printf(" [%d]\n", cast(int)i);
//        ob.kill.length = obstate.vars.length;
//        ob.comb.length = obstate.vars.length;
        ob.gen         = p[0 .. vlen]; p += vlen;
        ob.input       = p[0 .. vlen]; p += vlen;
        ob.output      = p[0 .. vlen]; p += vlen;

        allocDeps(ob.gen);
        allocDeps(ob.input);
        allocDeps(ob.output);
    }
}

/******************************
 * Does v meet the definiton of a `Borrowed` pointer?
 * Returns:
 *      true if it does
 */
bool isBorrowedPtr(VarDeclaration v)
{
    return v.isScope() && !v.isowner &&
        v.type.hasPointersToMutableFields();
}

/******************************
 * Does v meet the definiton of a `Readonly` pointer?
 * Returns:
 *      true if it does
 */
bool isReadonlyPtr(VarDeclaration v)
{
    return v.isScope() && !v.type.hasPointersToMutableFields();
}

/***************************************
 * Compute the gen vector for ob.
 */
void genKill(ref ObState obstate, ObNode* ob)
{
    enum log = false;
    if (log)
        printf("-----------computeGenKill() %d -----------\n", ob.index);

    /***************
     * Assigning result of expression `e` to variable `v`.
     */
    void dgWriteVar(ObNode* ob, VarDeclaration v, Expression e, bool initializer)
    {
        if (log)
            printf("dgWriteVar() %s := %s %d\n", v.toChars(), e.toChars(), initializer);
        const vi = obstate.vars.find(v);
        assert(vi != size_t.max);
        PtrVarState* pvs = &ob.gen[vi];
        readVar(ob, vi, true, ob.gen);
        if (e)
        {
            if (isBorrowedPtr(v))
                pvs.state = PtrState.Borrowed;
            else if (isReadonlyPtr(v))
                pvs.state = PtrState.Readonly;
            else
                pvs.state = PtrState.Owner;
            pvs.deps.zero();

            bool any = false;           // if any variables are assigned to v

            void by(VarDeclaration r)
            {
                const ri = obstate.vars.find(r);
                if (ri != size_t.max && ri != vi)
                {
                    pvs.deps[ri] = true;         // v took from r
                    auto pvsr = &ob.gen[ri];
                    any = true;

                    if (isBorrowedPtr(v))
                    {
                        // v is borrowing from r
                        pvs.state = PtrState.Borrowed;
                    }
                    else if (isReadonlyPtr(v))
                    {
                        pvs.state = PtrState.Readonly;
                    }
                    else
                    {
                        // move r to v, which "consumes" r
                        pvsr.state = PtrState.Undefined;
                        pvsr.deps.zero();
                    }
                }
            }

            escapeLive(e, &by);

            /* Make v an Owner for initializations like:
             *    scope v = malloc();
             */
            if (initializer && !any && isBorrowedPtr(v))
            {
                v.isowner = true;
                pvs.state = PtrState.Owner;
            }
        }
        else
        {
            if (isBorrowedPtr(v))
                pvs.state = PtrState.Borrowed;
            else if (isReadonlyPtr(v))
                pvs.state = PtrState.Readonly;
            else
                pvs.state = PtrState.Owner;
            pvs.deps.zero();
        }
    }

    void dgReadVar(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable)
    {
        if (log)
            printf("dgReadVar() %s %d\n", v.toChars(), mutable);
        const vi = obstate.vars.find(v);
        assert(vi != size_t.max);
        readVar(ob, vi, mutable, ob.gen);
    }

    void foreachExp(ObNode* ob, Expression e)
    {
        extern (C++) final class ExpWalker : Visitor
        {
            alias visit = typeof(super).visit;
            extern (D) void delegate(ObNode*, VarDeclaration, Expression, bool) dgWriteVar;
            extern (D) void delegate(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable) dgReadVar;
            ObNode* ob;
            ObState* obstate;

            extern (D) this(void delegate(ObNode*, VarDeclaration, Expression, bool) dgWriteVar,
                            void delegate(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable) dgReadVar,
                            ObNode* ob, ref ObState obstate) scope
            {
                this.dgWriteVar = dgWriteVar;
                this.dgReadVar  = dgReadVar;
                this.ob = ob;
                this.obstate = &obstate;
            }

            override void visit(Expression e)
            {
                //printf("[%s] %s: %s\n", e.loc.toChars(), EXPtoString(e.op).ptr, e.toChars());
                //assert(0);
            }

            void visitAssign(AssignExp ae, bool initializer)
            {
                ae.e2.accept(this);
                if (auto ve = ae.e1.isVarExp())
                {
                    if (auto v = ve.var.isVarDeclaration())
                        if (isTrackableVar(v))
                            dgWriteVar(ob, v, ae.e2, initializer);
                }
                else
                    ae.e1.accept(this);
            }

            override void visit(AssignExp ae)
            {
                visitAssign(ae, false);
            }

            override void visit(DeclarationExp e)
            {
                void Dsymbol_visit(Dsymbol s)
                {
                    if (auto vd = s.isVarDeclaration())
                    {
                        s = s.toAlias();
                        if (s != vd)
                            return Dsymbol_visit(s);
                        if (!isTrackableVar(vd))
                            return;

                        if (!(vd._init && vd._init.isVoidInitializer()))
                        {
                            auto ei = vd._init ? vd._init.isExpInitializer() : null;
                            if (ei)
                                visitAssign(cast(AssignExp)ei.exp, true);
                            else
                                dgWriteVar(ob, vd, null, false);
                        }
                    }
                    else if (auto td = s.isTupleDeclaration())
                    {
                        td.foreachVar(&Dsymbol_visit);
                    }
                }

                Dsymbol_visit(e.declaration);
            }

            override void visit(VarExp ve)
            {
                //printf("VarExp: %s\n", ve.toChars());
                if (auto v = ve.var.isVarDeclaration())
                    if (isTrackableVar(v))
                    {
                        dgReadVar(ve.loc, ob, v, isMutableRef(ve.type));
                    }
            }

            override void visit(CallExp ce)
            {
                //printf("CallExp() %s\n", ce.toChars());
                ce.e1.accept(this);
                auto t = ce.e1.type.toBasetype();
                auto tf = t.isTypeFunction();
                if (!tf)
                {
                    assert(t.ty == Tdelegate);
                    tf = t.nextOf().isTypeFunction();
                    assert(tf);

                }

                if (auto dve = ce.e1.isDotVarExp())
                {
                    if (!t.isTypeDelegate() && dve.e1.isVarExp())
                    {
                        //printf("dve: %s\n", dve.toChars());

                        void byf(VarDeclaration v)
                        {
                            //printf("byf v: %s\n", v.ident.toChars());
                            if (!isTrackableVar(v))
                                return;

                            const vi = obstate.vars.find(v);
                            if (vi == size_t.max)
                                return;

                            auto fd = dve.var.isFuncDeclaration();
                            if (fd && fd.storage_class & STC.scope_)
                            {
                                // borrow
                                obstate.varStack.push(vi);
                                obstate.mutableStack.push(isMutableRef(dve.e1.type.toBasetype()));
                            }
                            else
                            {
                                // move (i.e. consume arg)
                                makeUndefined(vi, ob.gen);
                            }
                        }

                        escapeLive(dve.e1, &byf);
                    }
                }

                // j=1 if _arguments[] is first argument
                const int j = tf.isDstyleVariadic();
                bool hasOut;
                const varStackSave = obstate.varStack.length;

                foreach (const i, arg; (*ce.arguments)[])
                {
                    if (i - j < tf.parameterList.length &&
                        i >= j)
                    {
                        Parameter p = tf.parameterList[i - j];
                        auto pt = p.type.toBasetype();


                        if (!(p.storageClass & STC.out_ && arg.isVarExp()))
                            arg.accept(this);

                        void by(VarDeclaration v)
                        {
                            //printf("by v: %s\n", v.ident.toChars());
                            if (!isTrackableVar(v))
                                return;

                            const vi = obstate.vars.find(v);
                            if (vi == size_t.max)
                                return;

                            if (p.storageClass & STC.out_)
                            {
                                /// initialize
                                hasOut = true;
                                makeUndefined(vi, ob.gen);
                            }
                            else if (p.storageClass & STC.scope_)
                            {
                                // borrow
                                obstate.varStack.push(vi);
                                obstate.mutableStack.push(isMutableRef(pt));
                            }
                            else
                            {
                                // move (i.e. consume arg)
                                makeUndefined(vi, ob.gen);
                            }
                        }

                        escapeLive(arg, &by);
                    }
                    else // variadic args
                    {
                        arg.accept(this);

                        void byv(VarDeclaration v)
                        {
                            if (!isTrackableVar(v))
                                return;

                            const vi = obstate.vars.find(v);
                            if (vi == size_t.max)
                                return;

                            if (tf.parameterList.stc & STC.scope_)
                            {
                                // borrow
                                obstate.varStack.push(vi);
                                obstate.mutableStack.push(isMutableRef(arg.type) &&
                                        !(tf.parameterList.stc & (STC.const_ | STC.immutable_)));
                            }
                            else
                                // move (i.e. consume arg)
                                makeUndefined(vi, ob.gen);
                        }

                        escapeLive(arg, &byv);
                    }
                }

                /* Do a dummy 'read' of each variable passed to the function,
                 * to detect O/B errors
                 */
                assert(obstate.varStack.length == obstate.mutableStack.length);
                foreach (i; varStackSave .. obstate.varStack.length)
                {
                    const vi = obstate.varStack[i];
                    // auto pvs = &ob.gen[vi];
                    auto v = obstate.vars[vi];
                    //if (pvs.state == PtrState.Undefined)
                        //v.error(ce.loc, "is Undefined, cannot pass to function");

                    dgReadVar(ce.loc, ob, v, obstate.mutableStack[i]);
                }

                /* Pop off stack all variables for this function call
                 */
                obstate.varStack.setDim(varStackSave);
                obstate.mutableStack.setDim(varStackSave);

                if (hasOut)
                    // Initialization of out's only happens after the function call
                    foreach (const i, arg; (*ce.arguments)[])
                    {
                        if (i - j < tf.parameterList.length &&
                            i >= j)
                        {
                            Parameter p = tf.parameterList[i - j];
                            if (p.storageClass & STC.out_)
                            {
                                if (auto v = isTrackableVarExp(arg))
                                    dgWriteVar(ob, v, null, true);
                            }
                        }
                    }
            }

            override void visit(SymOffExp e)
            {
                if (auto v = e.var.isVarDeclaration())
                    if (isTrackableVar(v))
                    {
                        dgReadVar(e.loc, ob, v, isMutableRef(e.type));
                    }
            }

            override void visit(LogicalExp e)
            {
                e.e1.accept(this);

                const vlen = obstate.vars.length;
                auto p = cast(PtrVarState*)mem.xcalloc(vlen, PtrVarState.sizeof);
                PtrVarState[] gen1 = p[0 .. vlen];
                foreach (i, ref pvs; gen1)
                {
                    pvs = ob.gen[i];
                }

                e.e2.accept(this);

                // Merge gen1 into ob.gen
                foreach (i; 0 .. vlen)
                {
                    ob.gen[i].combine(gen1[i], i, ob.gen);
                }

                mem.xfree(p); // should free .deps too
            }

            override void visit(CondExp e)
            {
                e.econd.accept(this);

                const vlen = obstate.vars.length;
                auto p = cast(PtrVarState*)mem.xcalloc(vlen, PtrVarState.sizeof);
                PtrVarState[] gen1 = p[0 .. vlen];
                foreach (i, ref pvs; gen1)
                {
                    pvs = ob.gen[i];
                }

                e.e1.accept(this);

                // Swap gen1 with ob.gen
                foreach (i; 0 .. vlen)
                {
                    gen1[i].deps.swap(ob.gen[i].deps);
                    const state = gen1[i].state;
                    gen1[i].state = ob.gen[i].state;
                    ob.gen[i].state = state;
                }

                e.e2.accept(this);

                /* xxx1 is the state from expression e1, ob.xxx is the state from e2.
                 * Merge xxx1 into ob.xxx to get the state from `e`.
                 */
                foreach (i; 0 .. vlen)
                {
                    ob.gen[i].combine(gen1[i], i, ob.gen);
                }

                mem.xfree(p); // should free .deps too
            }

            override void visit(AddrExp e)
            {
                /* Taking the address of struct literal is normally not
                 * allowed, but CTFE can generate one out of a new expression,
                 * but it'll be placed in static data so no need to check it.
                 */
                if (e.e1.op != EXP.structLiteral)
                    e.e1.accept(this);
            }

            override void visit(UnaExp e)
            {
                e.e1.accept(this);
            }

            override void visit(BinExp e)
            {
                e.e1.accept(this);
                e.e2.accept(this);
            }

            override void visit(ArrayLiteralExp e)
            {
                Type tb = e.type.toBasetype();
                if (tb.ty == Tsarray || tb.ty == Tarray)
                {
                    if (e.basis)
                        e.basis.accept(this);
                    foreach (el; *e.elements)
                    {
                        if (el)
                            el.accept(this);
                    }
                }
            }

            override void visit(StructLiteralExp e)
            {
                if (e.elements)
                {
                    foreach (ex; *e.elements)
                    {
                        if (ex)
                            ex.accept(this);
                    }
                }
            }

            override void visit(AssocArrayLiteralExp e)
            {
                if (e.keys)
                {
                    foreach (i, key; *e.keys)
                    {
                        if (key)
                            key.accept(this);
                        if (auto value = (*e.values)[i])
                            value.accept(this);
                    }
                }
            }

            override void visit(NewExp e)
            {
                if (e.arguments)
                {
                    foreach (ex; *e.arguments)
                    {
                        if (ex)
                            ex.accept(this);
                    }
                }
            }

            override void visit(SliceExp e)
            {
                e.e1.accept(this);
                if (e.lwr)
                    e.lwr.accept(this);
                if (e.upr)
                    e.upr.accept(this);
            }
        }

        if (e)
        {
            scope ExpWalker ew = new ExpWalker(&dgWriteVar, &dgReadVar, ob, obstate);
            e.accept(ew);
        }
    }

    foreachExp(ob, ob.exp);

    if (log)
    {
        printf("  gen:\n");
        foreach (i, ref pvs2; ob.gen[])
        {
            printf("    %s: ", obstate.vars[i].toChars()); pvs2.print(obstate.vars[]);
        }
    }
}

/***************************************
 * Determine the state of a variable based on
 * its type and storage class.
 */
PtrState toPtrState(VarDeclaration v)
{
    /* pointer to mutable:        Owner
     * pointer to mutable, scope: Borrowed
     * pointer to const:          Owner
     * pointer to const, scope:   Readonly
     * ref:                       Borrowed
     * const ref:                 Readonly
     */

    auto t = v.type;
    if (v.isReference())
    {
        return t.hasMutableFields() ? PtrState.Borrowed : PtrState.Readonly;
    }
    if (v.isScope())
    {
        return t.hasPointersToMutableFields() ? PtrState.Borrowed : PtrState.Readonly;
    }
    else
        return PtrState.Owner;
}

/**********************************
 * Does type `t` contain any pointers to mutable?
 */
bool hasPointersToMutableFields(Type t)
{
    auto tb = t.toBasetype();
    if (!tb.isMutable())
        return false;
    if (auto tsa = tb.isTypeSArray())
    {
        return tsa.nextOf().hasPointersToMutableFields();
    }
    if (auto ts = tb.isTypeStruct())
    {
        foreach (v; ts.sym.fields)
        {
            if (v.isReference())
            {
                if (v.type.hasMutableFields())
                    return true;
            }
            else if (v.type.hasPointersToMutableFields())
                return true;
        }
        return false;
    }
    auto tbn = tb.nextOf();
    return tbn && tbn.hasMutableFields();
}

/********************************
 * Does type `t` have any mutable fields?
 */
bool hasMutableFields(Type t)
{
    auto tb = t.toBasetype();
    if (!tb.isMutable())
        return false;
    if (auto tsa = tb.isTypeSArray())
    {
        return tsa.nextOf().hasMutableFields();
    }
    if (auto ts = tb.isTypeStruct())
    {
        foreach (v; ts.sym.fields)
        {
            if (v.type.hasMutableFields())
                return true;
        }
        return false;
    }
    return true;
}



/***************************************
 * Do the data flow analysis (i.e. compute the input[]
 * and output[] vectors for each ObNode).
 */
void doDataFlowAnalysis(ref ObState obstate)
{
    enum log = false;
    if (log)
    {
        printf("-----------------doDataFlowAnalysis()-------------------------\n");
        foreach (ob; obstate.nodes) ob.print();
        printf("------------------------------------------\n");
    }

    if (!obstate.nodes.length)
        return;

    auto startnode = obstate.nodes[0];
    assert(startnode.preds.length == 0);

    /* Set opening state `input[]` for first node
     */
    foreach (i, ref ps; startnode.input)
    {
        auto v = obstate.vars[i];
        auto state = toPtrState(v);
        if (v.isParameter())
        {
            if (v.isOut())
                state = PtrState.Undefined;
            else if (v.isBorrowedPtr())
                state = PtrState.Borrowed;
            else
                state = PtrState.Owner;
        }
        else
            state = PtrState.Undefined;
        ps.state = state;
        ps.deps.zero();
        startnode.gen[i] = ps;
    }

    /* Set all output[]s to Initial
     */
    foreach (ob; obstate.nodes[])
    {
        foreach (ref ps; ob.output)
        {
            ps.state = PtrState.Initial;
            ps.deps.zero();
        }
    }

    const vlen = obstate.vars.length;
    PtrVarState pvs;
    pvs.deps.length = vlen;
    int counter = 0;
    bool changes;
    do
    {
        changes = false;
        assert(++counter <= 1000);      // should converge, but don't hang if it doesn't
        foreach (ob; obstate.nodes[])
        {
            /* Construct ob.gen[] by combining the .output[]s of each ob.preds[]
             * and set ob.input[] to the same state
             */
            if (ob != startnode)
            {
                assert(ob.preds.length);

                foreach (i; 0 .. vlen)
                {
                    ob.gen[i] = ob.preds[0].output[i];
                }

                foreach (j; 1 .. ob.preds.length)
                {
                    foreach (i; 0 .. vlen)
                    {
                        ob.gen[i].combine(ob.preds[j].output[i], i, ob.gen);
                    }
                }

                /* Set ob.input[] to ob.gen[],
                 * if any changes were made we'll have to do another iteration
                 */
                foreach (i; 0 .. vlen)
                {
                    if (ob.gen[i] != ob.input[i])
                    {
                        ob.input[i] = ob.gen[i];
                        changes = true;
                    }
                }
            }

            /* Compute gen[] for node ob
             */
            genKill(obstate, ob);

            foreach (i; 0 .. vlen)
            {
                if (ob.gen[i] != ob.output[i])
                {
                    ob.output[i] = ob.gen[i];
                    changes = true;
                }
            }
        }
    } while (changes);

    static if (log)
    {
        foreach (obi, ob; obstate.nodes)
        {
            printf("%d: %s\n", obi, ob.exp ? ob.exp.toChars() : "".ptr);
            printf("  input:\n");
            foreach (i, ref pvs2; ob.input[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs2.print(obstate.vars[]);
            }

            printf("  gen:\n");
            foreach (i, ref pvs2; ob.gen[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs2.print(obstate.vars[]);
            }
            printf("  output:\n");
            foreach (i, ref pvs2; ob.output[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs2.print(obstate.vars[]);
            }
        }
        printf("\n");
    }
}


/***************************************
 * Check for escaping variables using DIP1000's `escapeByValue`, with `live` set to `true`
 * Params:
 *   e = expression to check
 *   onVar = gets called for each variable escaped through `e`, either by value or by ref
 */
void escapeLive(Expression e, scope void delegate(VarDeclaration) onVar)
{
    scope EscapeByResults er = EscapeByResults(
        (VarDeclaration v, bool) => onVar(v),
        onVar,
        (FuncDeclaration f, bool) {},
        (Expression e, bool) {},
        true,
    );

    escapeByValue(e, er);
}

/***************************************
 * Check for Ownership/Borrowing errors.
 */
void checkObErrors(ref ObState obstate)
{
    enum log = false;
    if (log)
        printf("------------checkObErrors()----------\n");

    void dgWriteVar(ObNode* ob, PtrVarState[] cpvs, VarDeclaration v, Expression e)
    {
        if (log) printf("dgWriteVar(v:%s, e:%s)\n", v.toChars(), e ? e.toChars() : "null");
        const vi = obstate.vars.find(v);
        assert(vi != size_t.max);
        PtrVarState* pvs = &cpvs[vi];
        readVar(ob, vi, true, cpvs);
        if (e)
        {
            if (isBorrowedPtr(v))
                pvs.state = PtrState.Borrowed;
            else if (isReadonlyPtr(v))
                pvs.state = PtrState.Readonly;
            else
            {
                if (pvs.state == PtrState.Owner && v.type.hasPointersToMutableFields())
                    .error(e.loc, "%s `%s` assigning to Owner without disposing of owned value", v.kind, v.toPrettyChars);

                pvs.state = PtrState.Owner;
            }
            pvs.deps.zero();


            void by(VarDeclaration r)   // `v` = `r`
            {
                //printf("  by(%s)\n", r.toChars());
                const ri = obstate.vars.find(r);
                if (ri == size_t.max)
                    return;

                with (PtrState)
                {
                    pvs.deps[ri] = true;         // v took from r
                    auto pvsr = &cpvs[ri];

                    if (pvsr.state == Undefined)
                    {
                        .error(e.loc, "%s `%s` is reading from `%s` which is Undefined", v.kind, v.toPrettyChars, r.toChars());
                    }
                    else if (isBorrowedPtr(v))  // v is going to borrow from r
                    {
                        if (pvsr.state == Readonly)
                            .error(e.loc, "%s `%s` is borrowing from `%s` which is Readonly", v.kind, v.toPrettyChars, r.toChars());

                        pvs.state = Borrowed;
                    }
                    else if (isReadonlyPtr(v))
                    {
                        pvs.state = Readonly;
                    }
                    else
                    {
                        // move from r to v
                        pvsr.state = Undefined;
                        pvsr.deps.zero();
                    }
                }
            }

            escapeLive(e, &by);
        }
        else
        {
            if (isBorrowedPtr(v))
                pvs.state = PtrState.Borrowed;
            else if (isReadonlyPtr(v))
                pvs.state = PtrState.Readonly;
            else
                pvs.state = PtrState.Owner;
            pvs.deps.zero();
        }
    }

    void dgReadVar(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable, PtrVarState[] gen)
    {
        if (log) printf("dgReadVar() %s\n", v.toChars());
        const vi = obstate.vars.find(v);
        assert(vi != size_t.max);
        auto pvs = &gen[vi];
        if (pvs.state == PtrState.Undefined)
            .error(loc, "%s `%s` has undefined state and cannot be read", v.kind, v.toPrettyChars);

        readVar(ob, vi, mutable, gen);
    }

    void foreachExp(ObNode* ob, Expression e, PtrVarState[] cpvs)
    {
        extern (C++) final class ExpWalker : Visitor
        {
            alias visit = typeof(super).visit;
            extern (D) void delegate(ObNode*, PtrVarState[], VarDeclaration, Expression) dgWriteVar;
            extern (D) void delegate(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable, PtrVarState[]) dgReadVar;
            PtrVarState[] cpvs;
            ObNode* ob;
            ObState* obstate;

            extern (D) this(void delegate(const ref Loc loc, ObNode* ob, VarDeclaration v, bool mutable, PtrVarState[]) dgReadVar,
                            void delegate(ObNode*, PtrVarState[], VarDeclaration, Expression) dgWriteVar,
                            PtrVarState[] cpvs, ObNode* ob, ref ObState obstate) scope
            {
                this.dgReadVar  = dgReadVar;
                this.dgWriteVar = dgWriteVar;
                this.cpvs = cpvs;
                this.ob = ob;
                this.obstate = &obstate;
            }

            override void visit(Expression)
            {
            }

            override void visit(DeclarationExp e)
            {
                void Dsymbol_visit(Dsymbol s)
                {
                    if (auto vd = s.isVarDeclaration())
                    {
                        s = s.toAlias();
                        if (s != vd)
                            return Dsymbol_visit(s);
                        if (!isTrackableVar(vd))
                            return;

                        if (vd._init && vd._init.isVoidInitializer())
                            return;

                        auto ei = vd._init ? vd._init.isExpInitializer() : null;
                        if (ei)
                        {
                            auto e = ei.exp;
                            if (auto ae = e.isConstructExp())
                                e = ae.e2;
                            dgWriteVar(ob, cpvs, vd, e);
                        }
                        else
                            dgWriteVar(ob, cpvs, vd, null);
                    }
                    else if (auto td = s.isTupleDeclaration())
                    {
                        td.foreachVar(&Dsymbol_visit);
                    }
                }

                Dsymbol_visit(e.declaration);
            }

            override void visit(AssignExp ae)
            {
                ae.e2.accept(this);
                if (auto ve = ae.e1.isVarExp())
                {
                    if (auto v = ve.var.isVarDeclaration())
                        if (isTrackableVar(v))
                            dgWriteVar(ob, cpvs, v, ae.e2);
                }
                else
                    ae.e1.accept(this);
            }

            override void visit(VarExp ve)
            {
                //printf("VarExp: %s\n", ve.toChars());
                if (auto v = ve.var.isVarDeclaration())
                    if (isTrackableVar(v))
                    {
                        dgReadVar(ve.loc, ob, v, isMutableRef(ve.type), cpvs);
                    }
            }

            override void visit(CallExp ce)
            {
                //printf("CallExp(%s)\n", ce.toChars());
                ce.e1.accept(this);
                auto t = ce.e1.type.toBasetype();
                auto tf = t.isTypeFunction();
                if (!tf)
                {
                    assert(t.ty == Tdelegate);
                    tf = t.nextOf().isTypeFunction();
                    assert(tf);
                }

                // j=1 if _arguments[] is first argument
                const int j = tf.isDstyleVariadic();
                bool hasOut;
                const varStackSave = obstate.varStack.length;

                foreach (const i, arg; (*ce.arguments)[])
                {
                    if (i - j < tf.parameterList.length &&
                        i >= j)
                    {
                        Parameter p = tf.parameterList[i - j];
                        auto pt = p.type.toBasetype();

                        if (!(p.storageClass & STC.out_ && arg.isVarExp()))
                            arg.accept(this);


                        void by(VarDeclaration v)
                        {
                            if (!isTrackableVar(v))
                                return;

                            const vi = obstate.vars.find(v);
                            if (vi == size_t.max)
                                return;

                            auto pvs = &cpvs[vi];

                            if (p.storageClass & STC.out_)
                            {
                                /// initialize
                                hasOut = true;
                                makeUndefined(vi, cpvs);
                            }
                            else if (p.storageClass & STC.scope_)
                            {
                                // borrow
                                obstate.varStack.push(vi);
                                obstate.mutableStack.push(isMutableRef(pt));
                            }
                            else
                            {
                                // move (i.e. consume arg)
                                if (pvs.state != PtrState.Owner)
                                    .error(arg.loc, "%s `%s` is not Owner, cannot consume its value", v.kind, v.toPrettyChars);
                                makeUndefined(vi, cpvs);
                            }
                        }

                        escapeLive(arg, &by);
                    }
                    else // variadic args
                    {
                        arg.accept(this);
                        void byv(VarDeclaration v)
                        {
                            if (!isTrackableVar(v))
                                return;

                            const vi = obstate.vars.find(v);
                            if (vi == size_t.max)
                                return;

                            auto pvs = &cpvs[vi];

                            if (tf.parameterList.stc & STC.scope_)
                            {
                                // borrow
                                obstate.varStack.push(vi);
                                obstate.mutableStack.push(isMutableRef(arg.type) &&
                                        !(tf.parameterList.stc & (STC.const_ | STC.immutable_)));
                            }
                            else
                            {
                                // move (i.e. consume arg)
                                if (pvs.state != PtrState.Owner)
                                    .error(arg.loc, "%s `%s` is not Owner, cannot consume its value", v.kind, v.toPrettyChars);
                                makeUndefined(vi, cpvs);
                            }
                        }

                        escapeLive(arg, &byv);
                    }
                }

                /* Do a dummy 'read' of each variable passed to the function,
                 * to detect O/B errors
                 */
                assert(obstate.varStack.length == obstate.mutableStack.length);
                foreach (i; varStackSave .. obstate.varStack.length)
                {
                    const vi = obstate.varStack[i];
                    auto pvs = &cpvs[vi];
                    auto v = obstate.vars[vi];
                    //if (pvs.state == PtrState.Undefined)
                        //v.error(ce.loc, "is Undefined, cannot pass to function");

                    dgReadVar(ce.loc, ob, v, obstate.mutableStack[i], cpvs);

                    if (pvs.state == PtrState.Owner)
                    {
                        for (size_t k = i + 1; k < obstate.varStack.length;++k)
                        {
                            const vk = obstate.varStack[k];
                            if (vk == vi)
                            {
                                if (obstate.mutableStack[vi] || obstate.mutableStack[vk])
                                {
                                    .error(ce.loc, "%s `%s` is passed as Owner more than once", v.kind, v.toPrettyChars);
                                    break;  // no need to continue
                                }
                            }
                        }
                    }
                }

                /* Pop off stack all variables for this function call
                 */
                obstate.varStack.setDim(varStackSave);
                obstate.mutableStack.setDim(varStackSave);

                if (hasOut)
                    // Initialization of out's only happens after the function call
                    foreach (const i, arg; (*ce.arguments)[])
                    {
                        if (i - j < tf.parameterList.length &&
                            i >= j)
                        {
                            Parameter p = tf.parameterList[i - j];
                            if (p.storageClass & STC.out_)
                            {
                                if (auto v = isTrackableVarExp(arg))
                                {
                                    dgWriteVar(ob, cpvs, v, null);
                                }
                            }
                        }
                    }
            }

            override void visit(SymOffExp e)
            {
                if (auto v = e.var.isVarDeclaration())
                    if (isTrackableVar(v))
                    {
                        dgReadVar(e.loc, ob, v, isMutableRef(e.type), cpvs);
                    }
            }

            override void visit(LogicalExp e)
            {
                e.e1.accept(this);

                const vlen = obstate.vars.length;
                auto p = cast(PtrVarState*)mem.xcalloc(vlen, PtrVarState.sizeof);
                PtrVarState[] out1 = p[0 .. vlen];
                foreach (i, ref pvs; out1)
                {
                    pvs = cpvs[i];
                }

                e.e2.accept(this);

                // Merge out1 into cpvs
                foreach (i; 0 .. vlen)
                {
                    cpvs[i].combine(out1[i], i, cpvs);
                }

                mem.xfree(p); // should free .deps too
            }

            override void visit(CondExp e)
            {
                e.econd.accept(this);

                const vlen = obstate.vars.length;
                auto p = cast(PtrVarState*)mem.xcalloc(vlen, PtrVarState.sizeof);
                PtrVarState[] out1 = p[0 .. vlen];
                foreach (i, ref pvs; out1)
                {
                    pvs = cpvs[i];
                }

                e.e1.accept(this);

                // Swap out1 with cpvs
                foreach (i; 0 .. vlen)
                {
                    out1[i].deps.swap(cpvs[i].deps);
                    const state = out1[i].state;
                    out1[i].state = cpvs[i].state;
                    cpvs[i].state = state;
                }

                e.e2.accept(this);

                // Merge out1 into cpvs
                foreach (i; 0 .. vlen)
                {
                    cpvs[i].combine(out1[i], i, cpvs);
                }

                mem.xfree(p); // should free .deps too
            }

            override void visit(AddrExp e)
            {
                /* Taking the address of struct literal is normally not
                 * allowed, but CTFE can generate one out of a new expression,
                 * but it'll be placed in static data so no need to check it.
                 */
                if (e.e1.op != EXP.structLiteral)
                    e.e1.accept(this);
            }

            override void visit(UnaExp e)
            {
                e.e1.accept(this);
            }

            override void visit(BinExp e)
            {
                e.e1.accept(this);
                e.e2.accept(this);
            }

            override void visit(ArrayLiteralExp e)
            {
                Type tb = e.type.toBasetype();
                if (tb.ty == Tsarray || tb.ty == Tarray)
                {
                    if (e.basis)
                        e.basis.accept(this);
                    foreach (el; *e.elements)
                    {
                        if (el)
                            el.accept(this);
                    }
                }
            }

            override void visit(StructLiteralExp e)
            {
                if (e.elements)
                {
                    foreach (ex; *e.elements)
                    {
                        if (ex)
                            ex.accept(this);
                    }
                }
            }

            override void visit(AssocArrayLiteralExp e)
            {
                if (e.keys)
                {
                    foreach (i, key; *e.keys)
                    {
                        if (key)
                            key.accept(this);
                        if (auto value = (*e.values)[i])
                            value.accept(this);
                    }
                }
            }

            override void visit(NewExp e)
            {
                if (e.arguments)
                {
                    foreach (ex; *e.arguments)
                    {
                        if (ex)
                            ex.accept(this);
                    }
                }
            }

            override void visit(SliceExp e)
            {
                e.e1.accept(this);
                if (e.lwr)
                    e.lwr.accept(this);
                if (e.upr)
                    e.upr.accept(this);
            }
        }

        if (e)
        {
            scope ExpWalker ew = new ExpWalker(&dgReadVar, &dgWriteVar, cpvs, ob, obstate);
            e.accept(ew);
        }
    }

    const vlen = obstate.vars.length;
    auto p = cast(PtrVarState*)mem.xcalloc(vlen, PtrVarState.sizeof);
    PtrVarState[] cpvs = p[0 .. vlen];
    foreach (ref pvs; cpvs)
        pvs.deps.length = vlen;

    foreach (obi, ob; obstate.nodes)
    {
        static if (log)
        {
            printf("%d: %s\n", cast(int) obi, ob.exp ? ob.exp.toChars() : "".ptr);
            printf("  input:\n");
            foreach (i, ref pvs; ob.input[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs.print(obstate.vars[]);
            }
        }

        /* Combine the .output[]s of each ob.preds[] looking for errors
         */
        if (obi)   // skip startnode
        {
            assert(ob.preds.length);

            foreach (i; 0 .. vlen)
            {
                ob.gen[i] = ob.preds[0].output[i];
            }

            foreach (j; 1 .. ob.preds.length)
            {
                foreach (i; 0 .. vlen)
                {
                    auto pvs1 = &ob.gen[i];
                    auto pvs2 = &ob.preds[j].output[i];
                    const s1 = pvs1.state;
                    const s2 = pvs2.state;
                    if (s1 != s2 && (s1 == PtrState.Owner || s2 == PtrState.Owner))
                    {
                        auto v = obstate.vars[i];
                        // Don't worry about non-pointers
                        if (hasPointers(v.type))
                            .error(ob.exp ? ob.exp.loc : v.loc, "%s `%s` is both %s and %s", v.kind, v.toPrettyChars, PtrStateToChars(s1), PtrStateToChars(s2));
                    }
                    pvs1.combine(*pvs2, i, ob.gen);
                }
            }
        }

        /* Prolly should use gen[] instead of cpvs[], or vice versa
         */
        foreach (i, ref pvs; ob.input)
        {
            cpvs[i] = pvs;
        }

        foreachExp(ob, ob.exp, cpvs);

        static if (log)
        {
            printf("  cpvs:\n");
            foreach (i, ref pvs; cpvs[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs.print(obstate.vars[]);
            }
            printf("  output:\n");
            foreach (i, ref pvs; ob.output[])
            {
                printf("    %s: ", obstate.vars[i].toChars()); pvs.print(obstate.vars[]);
            }
        }

        if (ob.obtype == ObType.retexp)
        {
            void by(VarDeclaration r)   // `r` is the rvalue
            {
                const ri = obstate.vars.find(r);
                if (ri == size_t.max)
                    return;
                with (PtrState)
                {
                    auto pvsr = &ob.output[ri];
                    switch (pvsr.state)
                    {
                        case Undefined:
                            .error(ob.exp.loc, "%s `%s` is returned but is Undefined", r.kind, r.toPrettyChars);
                            break;

                        case Owner:
                            pvsr.state = Undefined;     // returning a pointer "consumes" it
                            break;

                        case Borrowed:
                        case Readonly:
                            break;

                        default:
                            assert(0);
                    }
                }
            }
            escapeLive(ob.exp, &by);
        }

        if (ob.obtype == ObType.return_ || ob.obtype == ObType.retexp)
        {
            foreach (i, ref pvs; ob.output[])
            {
                //printf("%s: ", obstate.vars[i].toChars()); pvs.print(obstate.vars[]);
                if (pvs.state == PtrState.Owner)
                {
                    import dmd.typesem : hasPointers;
                    auto v = obstate.vars[i];
                    if (v.type.hasPointers())
                        .error(v.loc, "%s `%s` is not disposed of before return", v.kind, v.toPrettyChars);
                }
            }
        }
    }
}


/***************************************************
 * Read from variable vi.
 * The beginning of the 'scope' of a variable is when it is first read.
 * Hence, when a read is done, instead of when assignment to the variable is done, the O/B rules are enforced.
 * (Also called "non-lexical scoping".)
 */
void readVar(ObNode* ob, const size_t vi, bool mutable, PtrVarState[] gen)
{
    //printf("readVar(v%d)\n", cast(int)vi);
    auto pvso = &gen[vi];
    switch (pvso.state)
    {
        case PtrState.Owner:
            //printf("t: %s\n", t.toChars());
            if (mutable) // if mutable read
            {
                makeChildrenUndefined(vi, gen);
            }
            else // const read
            {
                // If there's a Borrow child, set that to Undefined
                foreach (di; 0 .. gen.length)
                {
                    auto pvsd = &gen[di];
                    if (pvsd.deps[vi] && pvsd.state == PtrState.Borrowed) // if di borrowed vi
                    {
                        makeUndefined(di, gen);
                    }
                }
            }
            break;

        case PtrState.Borrowed:
            /* All children become Undefined
             */
            makeChildrenUndefined(vi, gen);
            break;

        case PtrState.Readonly:
            break;

        case PtrState.Undefined:
            break;

        default:
            break;
    }
}

/********************
 * Recursively make Undefined all who list vi as a dependency
 */
void makeChildrenUndefined(size_t vi, PtrVarState[] gen)
{
    //printf("makeChildrenUndefined(%d)\n", vi);
    foreach (di; 0 .. gen.length)
    {
        if (gen[di].deps[vi])    // if di depends on vi
        {
            if (gen[di].state != PtrState.Undefined)
            {
                gen[di].state = PtrState.Undefined;  // set this first to avoid infinite recursion
                makeChildrenUndefined(di, gen);
                gen[di].deps.zero();
            }
        }
    }
}


/********************
 * Recursively make Undefined vi undefined and all who list vi as a dependency
 * Params:
 *    vi = variable's index
 *    gen = array of the states of variables
 */
void makeUndefined(size_t vi, PtrVarState[] gen)
{
    auto pvs = &gen[vi];
    pvs.state = PtrState.Undefined;  // set this first to avoid infinite recursion
    makeChildrenUndefined(vi, gen);
    pvs.deps.zero();
}

/*************************
 * Is type `t` a reference to a const or a reference to a mutable?
 */
bool isMutableRef(Type t)
{
    auto tb = t.toBasetype();
    return (tb.nextOf() ? tb.nextOf() : tb).isMutable();
}
