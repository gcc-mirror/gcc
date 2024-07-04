/**
 * Find side-effects of expressions.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/sideeffect.d, _sideeffect.d)
 * Documentation:  https://dlang.org/phobos/dmd_sideeffect.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/sideeffect.d
 */

module dmd.sideeffect;

import dmd.astenums;
import dmd.declaration;
import dmd.dscope;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.postordervisitor;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

/**************************************************
 * Front-end expression rewriting should create temporary variables for
 * non trivial sub-expressions in order to:
 *  1. save evaluation order
 *  2. prevent sharing of sub-expression in AST
 */
bool isTrivialExp(Expression e)
{
    extern (C++) final class IsTrivialExp : StoppableVisitor
    {
        alias visit = typeof(super).visit;
    public:
        extern (D) this() scope @safe
        {
        }

        override void visit(Expression e)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=11201
             * CallExp is always non trivial expression,
             * especially for inlining.
             */
            if (e.op == EXP.call)
            {
                stop = true;
                return;
            }
            // stop walking if we determine this expression has side effects
            stop = lambdaHasSideEffect(e);
        }
    }

    scope IsTrivialExp v = new IsTrivialExp();
    return walkPostorder(e, v) == false;
}

/********************************************
 * Determine if Expression has any side effects.
 *
 * Params:
 *   e = the expression
 *   assumeImpureCalls = whether function calls should always be assumed to
 *                       be impure (e.g. debug is allowed to violate purity)
 */
bool hasSideEffect(Expression e, bool assumeImpureCalls = false)
{
    extern (C++) final class LambdaHasSideEffect : StoppableVisitor
    {
        alias visit = typeof(super).visit;
    public:
        extern (D) this() scope @safe
        {
        }

        override void visit(Expression e)
        {
            // stop walking if we determine this expression has side effects
            stop = lambdaHasSideEffect(e, assumeImpureCalls);
        }
    }

    scope LambdaHasSideEffect v = new LambdaHasSideEffect();
    return walkPostorder(e, v);
}

/********************************************
 * Determine if the call of f, or function type or delegate type t1, has any side effects.
 * Returns:
 *      0   has any side effects
 *      1   nothrow + strongly pure
 *      2   nothrow + strongly pure + only immutable indirections in the return
 *          type
 */
int callSideEffectLevel(FuncDeclaration f)
{
    /* https://issues.dlang.org/show_bug.cgi?id=12760
     * https://issues.dlang.org/show_bug.cgi?id=16384
     *
     * ctor calls and invariant calls always have side effects
     */
    if (f.isCtorDeclaration() || f.isInvariantDeclaration())
        return 0;
    assert(f.type.ty == Tfunction);
    TypeFunction tf = cast(TypeFunction)f.type;
    if (!tf.isnothrow)
        return 0;
    final switch (f.isPure())
    {
    case PURE.impure:
    case PURE.fwdref:
    case PURE.weak:
        return 0;

    case PURE.const_:
        return mutabilityOfType(tf.isref, tf.next) == 2 ? 2 : 1;
    }
}

int callSideEffectLevel(Type t)
{
    t = t.toBasetype();
    TypeFunction tf;
    if (t.ty == Tdelegate)
        tf = cast(TypeFunction)(cast(TypeDelegate)t).next;
    else
    {
        assert(t.ty == Tfunction);
        tf = cast(TypeFunction)t;
    }
    if (!tf.isnothrow)  // function can throw
        return 0;

    tf.purityLevel();
    PURE purity = tf.purity;
    if (t.ty == Tdelegate && purity > PURE.weak)
    {
        if (tf.isMutable())
            purity = PURE.weak;
        else if (!tf.isImmutable())
            purity = PURE.const_;
    }

    if (purity == PURE.const_)
        return mutabilityOfType(tf.isref, tf.next) == 2 ? 2 : 1;

    return 0;
}

private bool lambdaHasSideEffect(Expression e, bool assumeImpureCalls = false)
{
    switch (e.op)
    {
    // Sort the cases by most frequently used first
    case EXP.assign:
    case EXP.plusPlus:
    case EXP.minusMinus:
    case EXP.declaration:
    case EXP.construct:
    case EXP.blit:
    case EXP.addAssign:
    case EXP.minAssign:
    case EXP.concatenateAssign:
    case EXP.concatenateElemAssign:
    case EXP.concatenateDcharAssign:
    case EXP.mulAssign:
    case EXP.divAssign:
    case EXP.modAssign:
    case EXP.leftShiftAssign:
    case EXP.rightShiftAssign:
    case EXP.unsignedRightShiftAssign:
    case EXP.andAssign:
    case EXP.orAssign:
    case EXP.xorAssign:
    case EXP.powAssign:
    case EXP.in_:
    case EXP.remove:
    case EXP.assert_:
    case EXP.halt:
    case EXP.throw_:
    case EXP.delete_:
    case EXP.new_:
    case EXP.newAnonymousClass:
    case EXP.loweredAssignExp:
        return true;
    case EXP.call:
        {
            if (assumeImpureCalls)
                return true;

            if (e.type && e.type.ty == Tnoreturn)
                return true;

            CallExp ce = cast(CallExp)e;
            /* Calling a function or delegate that is pure nothrow
             * has no side effects.
             */
            if (ce.e1.type)
            {
                Type t = ce.e1.type.toBasetype();
                if (t.ty == Tdelegate)
                    t = (cast(TypeDelegate)t).next;

                const level = t.ty == Tfunction && (ce.f ? callSideEffectLevel(ce.f) : callSideEffectLevel(ce.e1.type));
                if (level == 0) // 0 means the function has a side effect
                    return true;
            }
            break;
        }
    case EXP.cast_:
        {
            CastExp ce = cast(CastExp)e;
            /* if:
             *  cast(classtype)func()  // because it may throw
             */
            if (ce.to.ty == Tclass && ce.e1.op == EXP.call && ce.e1.type.ty == Tclass)
                return true;
            break;
        }
    default:
        break;
    }
    return false;
}

/***********************************
 * The result of this expression will be discarded.
 * Print error messages if the operation has no side effects (and hence is meaningless).
 * Returns:
 *      true if expression has no side effects
 */
bool discardValue(Expression e)
{
    if (lambdaHasSideEffect(e)) // check side-effect shallowly
        return false;
    switch (e.op)
    {
    case EXP.cast_:
        {
            CastExp ce = cast(CastExp)e;
            if (ce.to.equals(Type.tvoid))
            {
                /*
                 * Don't complain about an expression with no effect if it was cast to void
                 */
                return false;
            }
            break; // complain
        }
    // Assumption that error => no side effect
    case EXP.error:
        return true;
    case EXP.variable:
        {
            VarDeclaration v = (cast(VarExp)e).var.isVarDeclaration();
            if (v && (v.storage_class & STC.temp))
            {
                // https://issues.dlang.org/show_bug.cgi?id=5810
                // Don't complain about an internal generated variable.
                return false;
            }
            break;
        }
    case EXP.call:
        // https://issues.dlang.org/show_bug.cgi?id=24359
        auto ce = e.isCallExp();
        if (const f = ce.f)
        {
            if (f.ident == Id.__equals && ce.arguments && ce.arguments.length == 2)
            {
                return discardValue(new EqualExp(EXP.equal, e.loc, (*ce.arguments)[0], (*ce.arguments)[1]));
            }
        }
        return false;
    case EXP.andAnd:
    case EXP.orOr:
        {
            LogicalExp aae = cast(LogicalExp)e;
            return discardValue(aae.e2);
        }
    case EXP.question:
        {
            CondExp ce = cast(CondExp)e;
            /* https://issues.dlang.org/show_bug.cgi?id=6178
             * https://issues.dlang.org/show_bug.cgi?id=14089
             * Either CondExp::e1 or e2 may have
             * redundant expression to make those types common. For example:
             *
             *  struct S { this(int n); int v; alias v this; }
             *  S[int] aa;
             *  aa[1] = 0;
             *
             * The last assignment statement will be rewitten to:
             *
             *  1 in aa ? aa[1].value = 0 : (aa[1] = 0, aa[1].this(0)).value;
             *
             * The last DotVarExp is necessary to take assigned value.
             *
             *  int value = (aa[1] = 0);    // value = aa[1].value
             *
             * To avoid false error, discardValue() should be called only when
             * the both tops of e1 and e2 have actually no side effects.
             */
            if (!lambdaHasSideEffect(ce.e1) && !lambdaHasSideEffect(ce.e2))
            {
                return discardValue(ce.e1) |
                       discardValue(ce.e2);
            }
            return false;
        }
    case EXP.comma:
        {
            CommaExp ce = cast(CommaExp)e;
            // Don't complain about compiler-generated comma expressions
            if (ce.isGenerated)
                return false;

            // Don't check e1 until we cast(void) the a,b code generation.
            // This is concretely done in expressionSemantic, if a CommaExp has Tvoid as type
            return discardValue(ce.e2);
        }
    case EXP.tuple:
        /* Pass without complaint if any of the tuple elements have side effects.
         * Ideally any tuple elements with no side effects should raise an error,
         * this needs more investigation as to what is the right thing to do.
         */
        if (!hasSideEffect(e))
            break;
        return false;
    case EXP.identity, EXP.notIdentity:
    case EXP.equal, EXP.notEqual:
        /*
            `[side effect] == 0`
            Technically has a side effect but is clearly wrong;
        */
        BinExp tmp = e.isBinExp();
        assert(tmp);

        error(e.loc, "the result of the equality expression `%s` is discarded", e.toChars());
        bool seenSideEffect = false;
        foreach(expr; [tmp.e1, tmp.e2])
        {
            if (hasSideEffect(expr)) {
                errorSupplemental(expr.loc, "note that `%s` may have a side effect", expr.toChars());
                seenSideEffect |= true;
            }
        }
        return !seenSideEffect;
    default:
        break;
    }
    error(e.loc, "`%s` has no effect", e.toChars());
    return true;
}

/**************************************************
 * Build a temporary variable to copy the value of e into.
 * Params:
 *  stc = storage classes will be added to the made temporary variable
 *  name = name for temporary variable
 *  e = original expression
 * Returns:
 *  Newly created temporary variable.
 */
VarDeclaration copyToTemp(StorageClass stc, const char[] name, Expression e)
{
    assert(name[0] == '_' && name[1] == '_');
    auto vd = new VarDeclaration(e.loc, e.type,
        Identifier.generateId(name),
        new ExpInitializer(e.loc, e));
    vd.storage_class = stc | STC.temp | STC.ctfe; // temporary is always CTFEable
    return vd;
}

/**************************************************
 * Build a temporary variable to extract e's evaluation, if e is not trivial.
 * Params:
 *  sc = scope
 *  name = name for temporary variable
 *  e0 = a new side effect part will be appended to it.
 *  e = original expression
 *  alwaysCopy = if true, build new temporary variable even if e is trivial.
 * Returns:
 *  When e is trivial and alwaysCopy == false, e itself is returned.
 *  Otherwise, a new VarExp is returned.
 * Note:
 *  e's lvalue-ness will be handled well by STC.ref_ or STC.rvalue.
 */
Expression extractSideEffect(Scope* sc, const char[] name,
    ref Expression e0, Expression e, bool alwaysCopy = false)
{
    //printf("extractSideEffect(e: %s)\n", e.toChars());

    /* The trouble here is that if CTFE is running, extracting the side effect
     * results in an assignment, and then the interpreter says it cannot evaluate the
     * side effect assignment variable. But we don't have to worry about side
     * effects in function calls anyway, because then they won't CTFE.
     * https://issues.dlang.org/show_bug.cgi?id=17145
     */
    if (!alwaysCopy &&
        ((sc.flags & SCOPE.ctfe) ? !hasSideEffect(e) : isTrivialExp(e)))
        return e;

    auto vd = copyToTemp(0, name, e);
    vd.storage_class |= e.isLvalue() ? STC.ref_ : STC.rvalue;

    e0 = Expression.combine(e0, new DeclarationExp(vd.loc, vd)
                                .expressionSemantic(sc));

    return new VarExp(vd.loc, vd)
           .expressionSemantic(sc);
}
