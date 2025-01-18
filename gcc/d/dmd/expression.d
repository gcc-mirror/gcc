/**
 * Defines the bulk of the classes which represent the AST at the expression level.
 *
 * Specification: ($LINK2 https://dlang.org/spec/expression.html, Expressions)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/expression.d, _expression.d)
 * Documentation:  https://dlang.org/phobos/dmd_expression.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/expression.d
 */

module dmd.expression;

import core.stdc.stdarg;
import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.dcast : implicitConvTo;
import dmd.dclass;
import dmd.declaration;
import dmd.dimport;
import dmd.dmodule;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.root.complex;
import dmd.root.ctfloat;
import dmd.common.outbuffer;
import dmd.root.optional;
import dmd.root.rmem;
import dmd.rootobject;
import dmd.root.string;
import dmd.root.utf;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

enum LOGSEMANTIC = false;

/****************************************
 * Find the last non-comma expression.
 * Params:
 *      e = Expressions connected by commas
 * Returns:
 *      right-most non-comma expression
 */

inout(Expression) lastComma(inout Expression e)
{
    Expression ex = cast()e;
    while (ex.op == EXP.comma)
        ex = (cast(CommaExp)ex).e2;
    return cast(inout)ex;

}

/****************************************
 * Expand tuples in-place.
 *
 * Example:
 *     When there's a call `f(10, pair: AliasSeq!(20, 30), single: 40)`, the input is:
 *         `exps =  [10, (20, 30), 40]`
 *         `names = [null, "pair", "single"]`
 *     The arrays will be modified to:
 *         `exps =  [10, 20, 30, 40]`
 *         `names = [null, "pair", null, "single"]`
 *
 * Params:
 *     exps  = array of Expressions
 *     names = optional array of names corresponding to Expressions
 */
void expandTuples(Expressions* exps, Identifiers* names = null)
{
    //printf("expandTuples()\n");
    if (exps is null)
        return;

    if (names)
    {
        if (exps.length != names.length)
        {
            printf("exps.length = %d, names.length = %d\n", cast(int) exps.length, cast(int) names.length);
            printf("exps = %s, names = %s\n", exps.toChars(), names.toChars());
            if (exps.length > 0)
                printf("%s\n", (*exps)[0].loc.toChars());
            assert(0);
        }
    }

    // At `index`, a tuple of length `length` is expanded. Insert corresponding nulls in `names`.
    void expandNames(size_t index, size_t length)
    {
        if (names)
        {
            if (length == 0)
            {
                names.remove(index);
                return;
            }
            foreach (i; 1 .. length)
            {
                names.insert(index + i, cast(Identifier) null);
            }
        }
    }

    for (size_t i = 0; i < exps.length; i++)
    {
        Expression arg = (*exps)[i];
        if (!arg)
            continue;

        // Look for tuple with 0 members
        if (auto e = arg.isTypeExp())
        {
            if (auto tt = e.type.toBasetype().isTypeTuple())
            {
                if (!tt.arguments || tt.arguments.length == 0)
                {
                    exps.remove(i);
                    expandNames(i, 0);
                    if (i == exps.length)
                        return;
                }
                else // Expand a TypeTuple
                {
                    exps.remove(i);
                    auto texps = new Expressions(tt.arguments.length);
                    foreach (j, a; *tt.arguments)
                        (*texps)[j] = new TypeExp(e.loc, a.type);
                    exps.insert(i, texps);
                    expandNames(i, texps.length);
                }
                i--;
                continue;
            }
        }

        // Inline expand all the tuples
        while (arg.op == EXP.tuple)
        {
            TupleExp te = cast(TupleExp)arg;
            exps.remove(i); // remove arg
            exps.insert(i, te.exps); // replace with tuple contents
            expandNames(i, te.exps.length);
            if (i == exps.length)
                return; // empty tuple, no more arguments
            (*exps)[i] = Expression.combine(te.e0, (*exps)[i]);
            arg = (*exps)[i];
        }
    }
}

/****************************************
 * If `s` is a function template, i.e. the only member of a template
 * and that member is a function, return that template.
 * Params:
 *      s = symbol that might be a function template
 * Returns:
 *      template for that function, otherwise null
 */
TemplateDeclaration getFuncTemplateDecl(Dsymbol s) @safe
{
    FuncDeclaration f = s.isFuncDeclaration();
    if (f && f.parent)
    {
        if (auto ti = f.parent.isTemplateInstance())
        {
            if (!ti.isTemplateMixin() && ti.tempdecl)
            {
                auto td = ti.tempdecl.isTemplateDeclaration();
                if (td.onemember && td.ident == f.ident)
                {
                    return td;
                }
            }
        }
    }
    return null;
}

/************************ TypeDotIdExp ************************************/
/* Things like:
 *      int.size
 *      foo.size
 *      (foo).size
 *      cast(foo).size
 */
DotIdExp typeDotIdExp(const ref Loc loc, Type type, Identifier ident) @safe
{
    return new DotIdExp(loc, new TypeExp(loc, type), ident);
}

/***************************************************
 * Given an Expression, find the variable it really is.
 *
 * For example, `a[index]` is really `a`, and `s.f` is really `s`.
 * Params:
 *      e = Expression to look at
 *      deref = number of dereferences encountered
 * Returns:
 *      variable if there is one, null if not
 */
VarDeclaration expToVariable(Expression e, out int deref)
{
    deref = 0;
    while (1)
    {
        switch (e.op)
        {
            case EXP.variable:
                return e.isVarExp().var.isVarDeclaration();

            case EXP.dotVariable:
                e = e.isDotVarExp().e1;
                if (e.type.toBasetype().isTypeClass())
                    deref++;

                continue;

            case EXP.index:
            {
                e = e.isIndexExp().e1;
                if (!e.type.toBasetype().isTypeSArray())
                    deref++;

                continue;
            }

            case EXP.slice:
            {
                e = e.isSliceExp().e1;
                if (!e.type.toBasetype().isTypeSArray())
                    deref++;

                continue;
            }

            case EXP.super_:
                return e.isSuperExp().var.isVarDeclaration();
            case EXP.this_:
                return e.isThisExp().var.isVarDeclaration();

            // Temporaries for rvalues that need destruction
            // are of form: (T s = rvalue, s). For these cases
            // we can just return var declaration of `s`. However,
            // this is intentionally not calling `Expression.extractLast`
            // because at this point we cannot infer the var declaration
            // of more complex generated comma expressions such as the
            // one for the array append hook.
            case EXP.comma:
            {
                if (auto ve = e.isCommaExp().e2.isVarExp())
                    return ve.var.isVarDeclaration();

                return null;
            }
            default:
                return null;
        }
    }
}

enum OwnedBy : ubyte
{
    code,          // normal code expression in AST
    ctfe,          // value expression for CTFE
    cache,         // constant value cached for CTFE
}

enum WANTvalue  = 0;    // default
enum WANTexpand = 1;    // expand const/immutable variables if possible

/***********************************************************
 * https://dlang.org/spec/expression.html#expression
 */
extern (C++) abstract class Expression : ASTNode
{
    /// Usually, this starts out as `null` and gets set to the final expression type by
    /// `expressionSemantic`. However, for some expressions (such as `TypeExp`,`RealExp`,
    /// `VarExp`), the field can get set to an assigned type before running semantic.
    /// See `expressionSemanticDone`
    Type type;

    Loc loc;        // file location
    const EXP op;   // to minimize use of dynamic_cast
    bool parens;    // if this is a parenthesized expression
    bool rvalue;    // true if this is considered to be an rvalue, even if it is an lvalue

    extern (D) this(const ref Loc loc, EXP op) scope @safe
    {
        //printf("Expression::Expression(op = %d) this = %p\n", op, this);
        this.loc = loc;
        this.op = op;
    }

    /// Returns: class instance size of this expression (implemented manually because `extern(C++)`)
    final size_t size() nothrow @nogc pure @safe const { return expSize[op]; }

    static void _init()
    {
        CTFEExp.cantexp = new CTFEExp(EXP.cantExpression);
        CTFEExp.voidexp = new CTFEExp(EXP.voidExpression);
        CTFEExp.breakexp = new CTFEExp(EXP.break_);
        CTFEExp.continueexp = new CTFEExp(EXP.continue_);
        CTFEExp.gotoexp = new CTFEExp(EXP.goto_);
        CTFEExp.showcontext = new CTFEExp(EXP.showCtfeContext);
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    static void deinitialize()
    {
        CTFEExp.cantexp = CTFEExp.cantexp.init;
        CTFEExp.voidexp = CTFEExp.voidexp.init;
        CTFEExp.breakexp = CTFEExp.breakexp.init;
        CTFEExp.continueexp = CTFEExp.continueexp.init;
        CTFEExp.gotoexp = CTFEExp.gotoexp.init;
        CTFEExp.showcontext = CTFEExp.showcontext.init;
    }

    /*********************************
     * Does *not* do a deep copy.
     */
    extern (D) final Expression copy()
    {
        Expression e;
        if (!size)
        {
            debug
            {
                fprintf(stderr, "No expression copy for: %s\n", toChars());
                printf("op = %d\n", op);
            }
            assert(0);
        }

        // memory never freed, so can use the faster bump-pointer-allocation
        e = cast(Expression)allocmemory(size);
        //printf("Expression::copy(op = %d) e = %p\n", op, e);
        return cast(Expression)memcpy(cast(void*)e, cast(void*)this, size);
    }

    Expression syntaxCopy()
    {
        //printf("Expression::syntaxCopy()\n");
        //print();
        return copy();
    }

    // kludge for template.isExpression()
    override final DYNCAST dyncast() const
    {
        return DYNCAST.expression;
    }

    override const(char)* toChars() const
    {
        return .toChars(this);
    }

    /**********************************
     * Combine e1 and e2 by CommaExp if both are not NULL.
     */
    extern (D) static Expression combine(Expression e1, Expression e2) @safe
    {
        if (e1)
        {
            if (e2)
            {
                e1 = new CommaExp(e1.loc, e1, e2);
                e1.type = e2.type;
            }
        }
        else
            e1 = e2;
        return e1;
    }

    extern (D) static Expression combine(Expression e1, Expression e2, Expression e3) @safe
    {
        return combine(combine(e1, e2), e3);
    }

    extern (D) static Expression combine(Expression e1, Expression e2, Expression e3, Expression e4) @safe
    {
        return combine(combine(e1, e2), combine(e3, e4));
    }

    /**********************************
     * If 'e' is a tree of commas, returns the rightmost expression
     * by stripping off it from the tree. The remained part of the tree
     * is returned via e0.
     * Otherwise 'e' is directly returned and e0 is set to NULL.
     */
    extern (D) static Expression extractLast(Expression e, out Expression e0) @trusted
    {
        if (e.op != EXP.comma)
        {
            return e;
        }

        CommaExp ce = cast(CommaExp)e;
        if (ce.e2.op != EXP.comma)
        {
            e0 = ce.e1;
            return ce.e2;
        }
        else
        {
            e0 = e;

            Expression* pce = &ce.e2;
            while ((cast(CommaExp)(*pce)).e2.op == EXP.comma)
            {
                pce = &(cast(CommaExp)(*pce)).e2;
            }
            assert((*pce).op == EXP.comma);
            ce = cast(CommaExp)(*pce);
            *pce = ce.e1;

            return ce.e2;
        }
    }

    extern (D) static Expressions* arraySyntaxCopy(Expressions* exps)
    {
        Expressions* a = null;
        if (exps)
        {
            a = new Expressions(exps.length);
            foreach (i, e; *exps)
            {
                (*a)[i] = e ? e.syntaxCopy() : null;
            }
        }
        return a;
    }

    dinteger_t toInteger()
    {
        //printf("Expression %s\n", EXPtoString(op).ptr);
        if (!type || !type.isTypeError())
            error(loc, "integer constant expression expected instead of `%s`", toChars());
        return 0;
    }

    uinteger_t toUInteger()
    {
        //printf("Expression %s\n", EXPtoString(op).ptr);
        return cast(uinteger_t)toInteger();
    }

    real_t toReal()
    {
        error(loc, "floating point constant expression expected instead of `%s`", toChars());
        return CTFloat.zero;
    }

    real_t toImaginary()
    {
        error(loc, "floating point constant expression expected instead of `%s`", toChars());
        return CTFloat.zero;
    }

    complex_t toComplex()
    {
        error(loc, "floating point constant expression expected instead of `%s`", toChars());
        return complex_t(CTFloat.zero);
    }

    StringExp toStringExp()
    {
        return null;
    }

    /***************************************
     * Return !=0 if expression is an lvalue.
     */
    bool isLvalue()
    {
        return false;
    }

    /****************************************
     * Check that the expression has a valid type.
     * If not, generates an error "... has no type".
     * Returns:
     *      true if the expression is not valid.
     * Note:
     *      When this function returns true, `checkValue()` should also return true.
     */
    bool checkType()
    {
        return false;
    }

    /****************************************
     * Check that the expression has a valid value.
     * If not, generates an error "... has no value".
     * Returns:
     *      true if the expression is not valid or has void type.
     */
    bool checkValue()
    {
        if (type && type.toBasetype().ty == Tvoid)
        {
            error(loc, "expression `%s` is `void` and has no value", toChars());
            //print(); assert(0);
            if (!global.gag)
                type = Type.terror;
            return true;
        }
        return false;
    }

    extern (D) final bool checkScalar()
    {
        if (op == EXP.error)
            return true;
        if (type.toBasetype().ty == Terror)
            return true;
        if (!type.isScalar())
        {
            error(loc, "`%s` is not a scalar, it is a `%s`", toChars(), type.toChars());
            return true;
        }
        return checkValue();
    }

    extern (D) final bool checkNoBool()
    {
        if (op == EXP.error)
            return true;
        if (type.toBasetype().ty == Terror)
            return true;
        if (type.toBasetype().ty == Tbool)
        {
            error(loc, "operation not allowed on `bool` `%s`", toChars());
            return true;
        }
        return false;
    }

    extern (D) final bool checkIntegral()
    {
        if (op == EXP.error)
            return true;
        if (type.toBasetype().ty == Terror)
            return true;
        if (!type.isIntegral())
        {
            error(loc, "`%s` is not of integral type, it is a `%s`", toChars(), type.toChars());
            return true;
        }
        return checkValue();
    }

    extern (D) final bool checkArithmetic(EXP op)
    {
        if (op == EXP.error)
            return true;
        if (type.toBasetype().ty == Terror)
            return true;
        if (!type.isIntegral() && !type.isFloating())
        {
            // unary aggregate ops error here
            const char* msg = type.isAggregate() ?
                "operator `%s` is not defined for `%s` of type `%s`" :
                "illegal operator `%s` for `%s` of type `%s`";
            error(loc, msg, EXPtoString(op).ptr, toChars(), type.toChars());
            return true;
        }
        return checkValue();
    }

    /*******************************
     * Check whether the expression allows RMW operations, error with rmw operator diagnostic if not.
     * ex is the RHS expression, or NULL if ++/-- is used (for diagnostics)
     * Returns true if error occurs.
     */
    extern (D) final bool checkReadModifyWrite(EXP rmwOp, Expression ex = null)
    {
        //printf("Expression::checkReadModifyWrite() %s %s", toChars(), ex ? ex.toChars() : "");
        if (!type || !type.isShared() || type.isTypeStruct() || type.isTypeClass())
            return false;

        // atomicOp uses opAssign (+=/-=) rather than opOp (++/--) for the CT string literal.
        switch (rmwOp)
        {
        case EXP.plusPlus:
        case EXP.prePlusPlus:
            rmwOp = EXP.addAssign;
            break;
        case EXP.minusMinus:
        case EXP.preMinusMinus:
            rmwOp = EXP.minAssign;
            break;
        default:
            break;
        }

        error(loc, "read-modify-write operations are not allowed for `shared` variables");
        errorSupplemental(loc, "Use `core.atomic.atomicOp!\"%s\"(%s, %s)` instead",
                          EXPtoString(rmwOp).ptr, toChars(), ex ? ex.toChars() : "1");
        return true;
    }

    /******************************
     * Take address of expression.
     */
    final Expression addressOf()
    {
        //printf("Expression::addressOf()\n");
        debug
        {
            assert(op == EXP.error || isLvalue());
        }
        Expression e = new AddrExp(loc, this, type.pointerTo());
        return e;
    }

    /******************************
     * If this is a reference, dereference it.
     */
    final Expression deref()
    {
        //printf("Expression::deref()\n");
        // type could be null if forward referencing an 'auto' variable
        if (type)
            if (auto tr = type.isTypeReference())
            {
                Expression e = new PtrExp(loc, this, tr.next);
                return e;
            }
        return this;
    }

    final int isConst()
    {
        //printf("Expression::isConst(): %s\n", e.toChars());
        switch (op)
        {
        case EXP.int64:
        case EXP.float64:
        case EXP.complex80:
            return 1;
        case EXP.null_:
            return 0;
        case EXP.symbolOffset:
            return 2;
        default:
            return 0;
        }
        assert(0);
    }

    /******
     * Identical, not just equal. I.e. NaNs with different bit patterns are not identical
     */
    bool isIdentical(const Expression e) const
    {
        return equals(e);
    }


    /// Statically evaluate this expression to a `bool` if possible
    /// Returns: an optional thath either contains the value or is empty
    Optional!bool toBool()
    {
        return typeof(return)();
    }

    bool hasCode()
    {
        return true;
    }

    final pure inout nothrow @nogc @trusted
    {
        inout(IntegerExp)   isIntegerExp() { return op == EXP.int64 ? cast(typeof(return))this : null; }
        inout(ErrorExp)     isErrorExp() { return op == EXP.error ? cast(typeof(return))this : null; }
        inout(VoidInitExp)  isVoidInitExp() { return op == EXP.void_ ? cast(typeof(return))this : null; }
        inout(RealExp)      isRealExp() { return op == EXP.float64 ? cast(typeof(return))this : null; }
        inout(ComplexExp)   isComplexExp() { return op == EXP.complex80 ? cast(typeof(return))this : null; }
        inout(IdentifierExp) isIdentifierExp() { return op == EXP.identifier ? cast(typeof(return))this : null; }
        inout(DollarExp)    isDollarExp() { return op == EXP.dollar ? cast(typeof(return))this : null; }
        inout(DsymbolExp)   isDsymbolExp() { return op == EXP.dSymbol ? cast(typeof(return))this : null; }
        inout(ThisExp)      isThisExp() { return op == EXP.this_ ? cast(typeof(return))this : null; }
        inout(SuperExp)     isSuperExp() { return op == EXP.super_ ? cast(typeof(return))this : null; }
        inout(NullExp)      isNullExp() { return op == EXP.null_ ? cast(typeof(return))this : null; }
        inout(StringExp)    isStringExp() { return op == EXP.string_ ? cast(typeof(return))this : null; }
        inout(InterpExp)    isInterpExp() { return op == EXP.interpolated ? cast(typeof(return))this : null; }
        inout(TupleExp)     isTupleExp() { return op == EXP.tuple ? cast(typeof(return))this : null; }
        inout(ArrayLiteralExp) isArrayLiteralExp() { return op == EXP.arrayLiteral ? cast(typeof(return))this : null; }
        inout(AssocArrayLiteralExp) isAssocArrayLiteralExp() { return op == EXP.assocArrayLiteral ? cast(typeof(return))this : null; }
        inout(StructLiteralExp) isStructLiteralExp() { return op == EXP.structLiteral ? cast(typeof(return))this : null; }
        inout(CompoundLiteralExp) isCompoundLiteralExp() { return op == EXP.compoundLiteral ? cast(typeof(return))this : null; }
        inout(TypeExp)      isTypeExp() { return op == EXP.type ? cast(typeof(return))this : null; }
        inout(ScopeExp)     isScopeExp() { return op == EXP.scope_ ? cast(typeof(return))this : null; }
        inout(TemplateExp)  isTemplateExp() { return op == EXP.template_ ? cast(typeof(return))this : null; }
        inout(NewExp) isNewExp() { return op == EXP.new_ ? cast(typeof(return))this : null; }
        inout(NewAnonClassExp) isNewAnonClassExp() { return op == EXP.newAnonymousClass ? cast(typeof(return))this : null; }
        inout(SymOffExp)    isSymOffExp() { return op == EXP.symbolOffset ? cast(typeof(return))this : null; }
        inout(VarExp)       isVarExp() { return op == EXP.variable ? cast(typeof(return))this : null; }
        inout(OverExp)      isOverExp() { return op == EXP.overloadSet ? cast(typeof(return))this : null; }
        inout(FuncExp)      isFuncExp() { return op == EXP.function_ ? cast(typeof(return))this : null; }
        inout(DeclarationExp) isDeclarationExp() { return op == EXP.declaration ? cast(typeof(return))this : null; }
        inout(TypeidExp)    isTypeidExp() { return op == EXP.typeid_ ? cast(typeof(return))this : null; }
        inout(TraitsExp)    isTraitsExp() { return op == EXP.traits ? cast(typeof(return))this : null; }
        inout(HaltExp)      isHaltExp() { return op == EXP.halt ? cast(typeof(return))this : null; }
        inout(IsExp)        isIsExp() { return op == EXP.is_ ? cast(typeof(return))this : null; }
        inout(MixinExp)     isMixinExp() { return op == EXP.mixin_ ? cast(typeof(return))this : null; }
        inout(ImportExp)    isImportExp() { return op == EXP.import_ ? cast(typeof(return))this : null; }
        inout(AssertExp)    isAssertExp() { return op == EXP.assert_ ? cast(typeof(return))this : null; }
        inout(ThrowExp)     isThrowExp() { return op == EXP.throw_ ? cast(typeof(return))this : null; }
        inout(DotIdExp)     isDotIdExp() { return op == EXP.dotIdentifier ? cast(typeof(return))this : null; }
        inout(DotTemplateExp) isDotTemplateExp() { return op == EXP.dotTemplateDeclaration ? cast(typeof(return))this : null; }
        inout(DotVarExp)    isDotVarExp() { return op == EXP.dotVariable ? cast(typeof(return))this : null; }
        inout(DotTemplateInstanceExp) isDotTemplateInstanceExp() { return op == EXP.dotTemplateInstance ? cast(typeof(return))this : null; }
        inout(DelegateExp)  isDelegateExp() { return op == EXP.delegate_ ? cast(typeof(return))this : null; }
        inout(DotTypeExp)   isDotTypeExp() { return op == EXP.dotType ? cast(typeof(return))this : null; }
        inout(CallExp)      isCallExp() { return op == EXP.call ? cast(typeof(return))this : null; }
        inout(AddrExp)      isAddrExp() { return op == EXP.address ? cast(typeof(return))this : null; }
        inout(PtrExp)       isPtrExp() { return op == EXP.star ? cast(typeof(return))this : null; }
        inout(NegExp)       isNegExp() { return op == EXP.negate ? cast(typeof(return))this : null; }
        inout(UAddExp)      isUAddExp() { return op == EXP.uadd ? cast(typeof(return))this : null; }
        inout(ComExp)       isComExp() { return op == EXP.tilde ? cast(typeof(return))this : null; }
        inout(NotExp)       isNotExp() { return op == EXP.not ? cast(typeof(return))this : null; }
        inout(DeleteExp)    isDeleteExp() { return op == EXP.delete_ ? cast(typeof(return))this : null; }
        inout(CastExp)      isCastExp() { return op == EXP.cast_ ? cast(typeof(return))this : null; }
        inout(VectorExp)    isVectorExp() { return op == EXP.vector ? cast(typeof(return))this : null; }
        inout(VectorArrayExp) isVectorArrayExp() { return op == EXP.vectorArray ? cast(typeof(return))this : null; }
        inout(SliceExp)     isSliceExp() { return op == EXP.slice ? cast(typeof(return))this : null; }
        inout(ArrayLengthExp) isArrayLengthExp() { return op == EXP.arrayLength ? cast(typeof(return))this : null; }
        inout(ArrayExp)     isArrayExp() { return op == EXP.array ? cast(typeof(return))this : null; }
        inout(DotExp)       isDotExp() { return op == EXP.dot ? cast(typeof(return))this : null; }
        inout(CommaExp)     isCommaExp() { return op == EXP.comma ? cast(typeof(return))this : null; }
        inout(IntervalExp)  isIntervalExp() { return op == EXP.interval ? cast(typeof(return))this : null; }
        inout(DelegatePtrExp)     isDelegatePtrExp() { return op == EXP.delegatePointer ? cast(typeof(return))this : null; }
        inout(DelegateFuncptrExp) isDelegateFuncptrExp() { return op == EXP.delegateFunctionPointer ? cast(typeof(return))this : null; }
        inout(IndexExp)     isIndexExp() { return op == EXP.index ? cast(typeof(return))this : null; }
        inout(PostExp)      isPostExp()  { return (op == EXP.plusPlus || op == EXP.minusMinus) ? cast(typeof(return))this : null; }
        inout(PreExp)       isPreExp()   { return (op == EXP.prePlusPlus || op == EXP.preMinusMinus) ? cast(typeof(return))this : null; }
        inout(AssignExp)    isAssignExp()    { return op == EXP.assign ? cast(typeof(return))this : null; }
        inout(LoweredAssignExp)    isLoweredAssignExp()    { return op == EXP.loweredAssignExp ? cast(typeof(return))this : null; }
        inout(ConstructExp) isConstructExp() { return op == EXP.construct ? cast(typeof(return))this : null; }
        inout(BlitExp)      isBlitExp()      { return op == EXP.blit ? cast(typeof(return))this : null; }
        inout(AddAssignExp) isAddAssignExp() { return op == EXP.addAssign ? cast(typeof(return))this : null; }
        inout(MinAssignExp) isMinAssignExp() { return op == EXP.minAssign ? cast(typeof(return))this : null; }
        inout(MulAssignExp) isMulAssignExp() { return op == EXP.mulAssign ? cast(typeof(return))this : null; }

        inout(DivAssignExp) isDivAssignExp() { return op == EXP.divAssign ? cast(typeof(return))this : null; }
        inout(ModAssignExp) isModAssignExp() { return op == EXP.modAssign ? cast(typeof(return))this : null; }
        inout(AndAssignExp) isAndAssignExp() { return op == EXP.andAssign ? cast(typeof(return))this : null; }
        inout(OrAssignExp)  isOrAssignExp()  { return op == EXP.orAssign ? cast(typeof(return))this : null; }
        inout(XorAssignExp) isXorAssignExp() { return op == EXP.xorAssign ? cast(typeof(return))this : null; }
        inout(PowAssignExp) isPowAssignExp() { return op == EXP.powAssign ? cast(typeof(return))this : null; }

        inout(ShlAssignExp)  isShlAssignExp()  { return op == EXP.leftShiftAssign ? cast(typeof(return))this : null; }
        inout(ShrAssignExp)  isShrAssignExp()  { return op == EXP.rightShiftAssign ? cast(typeof(return))this : null; }
        inout(UshrAssignExp) isUshrAssignExp() { return op == EXP.unsignedRightShiftAssign ? cast(typeof(return))this : null; }

        inout(CatAssignExp) isCatAssignExp() { return op == EXP.concatenateAssign
                                                ? cast(typeof(return))this
                                                : null; }

        inout(CatElemAssignExp) isCatElemAssignExp() { return op == EXP.concatenateElemAssign
                                                ? cast(typeof(return))this
                                                : null; }

        inout(CatDcharAssignExp) isCatDcharAssignExp() { return op == EXP.concatenateDcharAssign
                                                ? cast(typeof(return))this
                                                : null; }

        inout(AddExp)      isAddExp() { return op == EXP.add ? cast(typeof(return))this : null; }
        inout(MinExp)      isMinExp() { return op == EXP.min ? cast(typeof(return))this : null; }
        inout(CatExp)      isCatExp() { return op == EXP.concatenate ? cast(typeof(return))this : null; }
        inout(MulExp)      isMulExp() { return op == EXP.mul ? cast(typeof(return))this : null; }
        inout(DivExp)      isDivExp() { return op == EXP.div ? cast(typeof(return))this : null; }
        inout(ModExp)      isModExp() { return op == EXP.mod ? cast(typeof(return))this : null; }
        inout(PowExp)      isPowExp() { return op == EXP.pow ? cast(typeof(return))this : null; }
        inout(ShlExp)      isShlExp() { return op == EXP.leftShift ? cast(typeof(return))this : null; }
        inout(ShrExp)      isShrExp() { return op == EXP.rightShift ? cast(typeof(return))this : null; }
        inout(UshrExp)     isUshrExp() { return op == EXP.unsignedRightShift ? cast(typeof(return))this : null; }
        inout(AndExp)      isAndExp() { return op == EXP.and ? cast(typeof(return))this : null; }
        inout(OrExp)       isOrExp() { return op == EXP.or ? cast(typeof(return))this : null; }
        inout(XorExp)      isXorExp() { return op == EXP.xor ? cast(typeof(return))this : null; }
        inout(LogicalExp)  isLogicalExp() { return (op == EXP.andAnd || op == EXP.orOr) ? cast(typeof(return))this : null; }
        //inout(CmpExp)    isCmpExp() { return op == EXP. ? cast(typeof(return))this : null; }
        inout(InExp)       isInExp() { return op == EXP.in_ ? cast(typeof(return))this : null; }
        inout(RemoveExp)   isRemoveExp() { return op == EXP.remove ? cast(typeof(return))this : null; }
        inout(EqualExp)    isEqualExp() { return (op == EXP.equal || op == EXP.notEqual) ? cast(typeof(return))this : null; }
        inout(IdentityExp) isIdentityExp() { return (op == EXP.identity || op == EXP.notIdentity) ? cast(typeof(return))this : null; }
        inout(CondExp)     isCondExp() { return op == EXP.question ? cast(typeof(return))this : null; }
        inout(GenericExp)  isGenericExp() { return op == EXP._Generic ? cast(typeof(return))this : null; }
        inout(DefaultInitExp)    isDefaultInitExp() { return
            (op == EXP.prettyFunction    || op == EXP.functionString ||
             op == EXP.line              || op == EXP.moduleString   ||
             op == EXP.file              || op == EXP.fileFullPath   ) ? cast(typeof(return))this : null; }
        inout(FileInitExp)       isFileInitExp() { return (op == EXP.file || op == EXP.fileFullPath) ? cast(typeof(return))this : null; }
        inout(LineInitExp)       isLineInitExp() { return op == EXP.line ? cast(typeof(return))this : null; }
        inout(ModuleInitExp)     isModuleInitExp() { return op == EXP.moduleString ? cast(typeof(return))this : null; }
        inout(FuncInitExp)       isFuncInitExp() { return op == EXP.functionString ? cast(typeof(return))this : null; }
        inout(PrettyFuncInitExp) isPrettyFuncInitExp() { return op == EXP.prettyFunction ? cast(typeof(return))this : null; }
        inout(ObjcClassReferenceExp) isObjcClassReferenceExp() { return op == EXP.objcClassReference ? cast(typeof(return))this : null; }
        inout(ClassReferenceExp) isClassReferenceExp() { return op == EXP.classReference ? cast(typeof(return))this : null; }
        inout(ThrownExceptionExp) isThrownExceptionExp() { return op == EXP.thrownException ? cast(typeof(return))this : null; }

        inout(UnaExp) isUnaExp() pure inout nothrow @nogc
        {
            return exptab[op] & EXPFLAGS.unary ? cast(typeof(return))this : null;
        }

        inout(BinExp) isBinExp() pure inout nothrow @nogc
        {
            return exptab[op] & EXPFLAGS.binary ? cast(typeof(return))this : null;
        }

        inout(BinAssignExp) isBinAssignExp() pure inout nothrow @nogc
        {
            return exptab[op] & EXPFLAGS.binaryAssign ? cast(typeof(return))this : null;
        }
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A compile-time known integer value
 */
extern (C++) final class IntegerExp : Expression
{
    private dinteger_t value;

    extern (D) this(const ref Loc loc, dinteger_t value, Type type)
    {
        super(loc, EXP.int64);
        //printf("IntegerExp(value = %lld, type = '%s')\n", value, type ? type.toChars() : "");
        assert(type);
        if (!type.isScalar())
        {
            //printf("%s, loc = %d\n", toChars(), loc.linnum);
            if (type.ty != Terror)
                error(loc, "integral constant must be scalar type, not `%s`", type.toChars());
            type = Type.terror;
        }
        this.type = type;
        this.value = normalize(type.toBasetype().ty, value);
    }

    extern (D) this(dinteger_t value)
    {
        super(Loc.initial, EXP.int64);
        this.type = Type.tint32;
        this.value = cast(int)value;
    }

    static IntegerExp create(const ref Loc loc, dinteger_t value, Type type)
    {
        return new IntegerExp(loc, value, type);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto ne = (cast(Expression)o).isIntegerExp())
        {
            if (type.toHeadMutable().equals(ne.type.toHeadMutable()) && value == ne.value)
            {
                return true;
            }
        }
        return false;
    }

    override dinteger_t toInteger()
    {
        // normalize() is necessary until we fix all the paints of 'type'
        return value = normalize(type.toBasetype().ty, value);
    }

    override real_t toReal()
    {
        // normalize() is necessary until we fix all the paints of 'type'
        const ty = type.toBasetype().ty;
        const val = normalize(ty, value);
        value = val;
        return (ty == Tuns64)
            ? real_t(cast(ulong)val)
            : real_t(cast(long)val);
    }

    override real_t toImaginary()
    {
        return CTFloat.zero;
    }

    override complex_t toComplex()
    {
        return complex_t(toReal());
    }

    override Optional!bool toBool()
    {
        bool r = toInteger() != 0;
        return typeof(return)(r);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    dinteger_t getInteger()
    {
        return value;
    }

    extern (D) void setInteger(dinteger_t value)
    {
        this.value = normalize(type.toBasetype().ty, value);
    }

    extern (D) static dinteger_t normalize(TY ty, dinteger_t value)
    {
        /* 'Normalize' the value of the integer to be in range of the type
         */
        dinteger_t result;
        switch (ty)
        {
        case Tbool:
            result = (value != 0);
            break;

        case Tint8:
            result = cast(byte)value;
            break;

        case Tchar:
        case Tuns8:
            result = cast(ubyte)value;
            break;

        case Tint16:
            result = cast(short)value;
            break;

        case Twchar:
        case Tuns16:
            result = cast(ushort)value;
            break;

        case Tint32:
            result = cast(int)value;
            break;

        case Tdchar:
        case Tuns32:
            result = cast(uint)value;
            break;

        case Tint64:
            result = cast(long)value;
            break;

        case Tuns64:
            result = cast(ulong)value;
            break;

        case Tpointer:
            if (target.ptrsize == 8)
                goto case Tuns64;
            if (target.ptrsize == 4)
                goto case Tuns32;
            if (target.ptrsize == 2)
                goto case Tuns16;
            assert(0);

        default:
            break;
        }
        return result;
    }

    override IntegerExp syntaxCopy()
    {
        return this;
    }

    /**
     * Use this instead of creating new instances for commonly used literals
     * such as 0 or 1.
     *
     * Parameters:
     *      v = The value of the expression
     * Returns:
     *      A static instance of the expression, typed as `Tint32`.
     */
    static IntegerExp literal(int v)()
    {
        __gshared IntegerExp theConstant;
        if (!theConstant)
            theConstant = new IntegerExp(v);
        return theConstant;
    }

    /**
     * Use this instead of creating new instances for commonly used bools.
     *
     * Parameters:
     *      b = The value of the expression
     * Returns:
     *      A static instance of the expression, typed as `Type.tbool`.
     */
    static IntegerExp createBool(bool b)
    {
        __gshared IntegerExp trueExp, falseExp;
        if (!trueExp)
        {
            trueExp = new IntegerExp(Loc.initial, 1, Type.tbool);
            falseExp = new IntegerExp(Loc.initial, 0, Type.tbool);
        }
        return b ? trueExp : falseExp;
    }
}

/***********************************************************
 * Use this expression for error recovery.
 *
 * It should behave as a 'sink' to prevent further cascaded error messages.
 */
extern (C++) final class ErrorExp : Expression
{
    extern (D) this()
    {
        super(Loc.initial, EXP.error);
        type = Type.terror;
    }

    static ErrorExp get ()
    {
        if (errorexp is null)
            errorexp = new ErrorExp();

        if (global.errors == 0 && global.gaggedErrors == 0)
        {
            /* Unfortunately, errors can still leak out of gagged errors,
              * and we need to set the error count to prevent bogus code
              * generation. At least give a message.
              */
            .error(Loc.initial, "unknown, please file report at https://github.com/dlang/dmd/issues/new");
        }

        return errorexp;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    extern (C++) __gshared ErrorExp errorexp; // handy shared value
}


/***********************************************************
 * An uninitialized value,
 * generated from void initializers.
 *
 * https://dlang.org/spec/declaration.html#void_init
 */
extern (C++) final class VoidInitExp : Expression
{
    VarDeclaration var; /// the variable from where the void value came from, null if not known
                        /// Useful for error messages

    extern (D) this(VarDeclaration var) @safe
    {
        super(var.loc, EXP.void_);
        this.var = var;
        this.type = var.type;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


/***********************************************************
 * A compile-time known floating point number
 */
extern (C++) final class RealExp : Expression
{
    real_t value;

    extern (D) this(const ref Loc loc, real_t value, Type type) @safe
    {
        super(loc, EXP.float64);
        //printf("RealExp::RealExp(%Lg)\n", value);
        this.value = value;
        this.type = type;
    }

    static RealExp create(const ref Loc loc, real_t value, Type type) @safe
    {
        return new RealExp(loc, value, type);
    }

    /********************************
     * Test to see if two reals are the same.
     * Regard NaN's as equivalent.
     * Regard +0 and -0 as different.
     * Params:
     *      x1 = first operand
     *      x2 = second operand
     * Returns:
     *      true if x1 is x2
     *      else false
     */
    private static bool RealIdentical(real_t x1, real_t x2) @safe
    {
        return (CTFloat.isNaN(x1) && CTFloat.isNaN(x2)) || CTFloat.isIdentical(x1, x2);
    }
    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto ne = (cast(Expression)o).isRealExp())
        {
            if (type.toHeadMutable().equals(ne.type.toHeadMutable()) && RealIdentical(value, ne.value))
            {
                return true;
            }
        }
        return false;
    }

    override bool isIdentical(const Expression e) const
    {
        if (!equals(e))
            return false;
        return CTFloat.isIdentical(value, e.isRealExp().value);
    }

    override dinteger_t toInteger()
    {
        return cast(sinteger_t)toReal();
    }

    override uinteger_t toUInteger()
    {
        return cast(uinteger_t)toReal();
    }

    override real_t toReal()
    {
        return type.isReal() ? value : CTFloat.zero;
    }

    override real_t toImaginary()
    {
        return type.isReal() ? CTFloat.zero : value;
    }

    override complex_t toComplex()
    {
        return complex_t(toReal(), toImaginary());
    }

    override Optional!bool toBool()
    {
        return typeof(return)(!!value);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A compile-time complex number (deprecated)
 */
extern (C++) final class ComplexExp : Expression
{
    complex_t value;

    extern (D) this(const ref Loc loc, complex_t value, Type type) @safe
    {
        super(loc, EXP.complex80);
        this.value = value;
        this.type = type;
        //printf("ComplexExp::ComplexExp(%s)\n", toChars());
    }

    static ComplexExp create(const ref Loc loc, complex_t value, Type type) @safe
    {
        return new ComplexExp(loc, value, type);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto ne = (cast(Expression)o).isComplexExp())
        {
            if (type.toHeadMutable().equals(ne.type.toHeadMutable()) &&
                RealExp.RealIdentical(creall(value), creall(ne.value)) &&
                RealExp.RealIdentical(cimagl(value), cimagl(ne.value)))
            {
                return true;
            }
        }
        return false;
    }

    override bool isIdentical(const Expression e) const
    {
        if (!equals(e))
            return false;
        // equals() regards different NaN values as 'equals'
        auto c = e.isComplexExp();
        return CTFloat.isIdentical(creall(value), creall(c.value)) &&
               CTFloat.isIdentical(cimagl(value), cimagl(c.value));
    }

    override dinteger_t toInteger()
    {
        return cast(sinteger_t)toReal();
    }

    override uinteger_t toUInteger()
    {
        return cast(uinteger_t)toReal();
    }

    override real_t toReal()
    {
        return creall(value);
    }

    override real_t toImaginary()
    {
        return cimagl(value);
    }

    override complex_t toComplex()
    {
        return value;
    }

    override Optional!bool toBool()
    {
        return typeof(return)(!!value);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * An identifier in the context of an expression (as opposed to a declaration)
 *
 * ---
 * int x; // VarDeclaration with Identifier
 * x++; // PostExp with IdentifierExp
 * ---
 */
extern (C++) class IdentifierExp : Expression
{
    Identifier ident;

    extern (D) this(const ref Loc loc, Identifier ident) scope @safe
    {
        super(loc, EXP.identifier);
        this.ident = ident;
    }

    static IdentifierExp create(const ref Loc loc, Identifier ident) @safe
    {
        return new IdentifierExp(loc, ident);
    }

    override final bool isLvalue()
    {
        return !this.rvalue;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The dollar operator used when indexing or slicing an array. E.g `a[$]`, `a[1 .. $]` etc.
 *
 * https://dlang.org/spec/arrays.html#array-length
 */
extern (C++) final class DollarExp : IdentifierExp
{
    extern (D) this(const ref Loc loc)
    {
        super(loc, Id.dollar);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Won't be generated by parser.
 */
extern (C++) final class DsymbolExp : Expression
{
    Dsymbol s;
    bool hasOverloads;

    extern (D) this(const ref Loc loc, Dsymbol s, bool hasOverloads = true) @safe
    {
        super(loc, EXP.dSymbol);
        this.s = s;
        this.hasOverloads = hasOverloads;
    }

    override bool isLvalue()
    {
        return !rvalue;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/expression.html#this
 */
extern (C++) class ThisExp : Expression
{
    VarDeclaration var;

    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.this_);
        //printf("ThisExp::ThisExp() loc = %d\n", loc.linnum);
    }

    this(const ref Loc loc, const EXP tok) @safe
    {
        super(loc, tok);
        //printf("ThisExp::ThisExp() loc = %d\n", loc.linnum);
    }

    override ThisExp syntaxCopy()
    {
        auto r = cast(ThisExp) super.syntaxCopy();
        // require new semantic (possibly new `var` etc.)
        r.type = null;
        r.var = null;
        return r;
    }

    override Optional!bool toBool()
    {
        // `this` is never null (what about structs?)
        return typeof(return)(true);
    }

    override final bool isLvalue()
    {
        // Class `this` should be an rvalue; struct `this` should be an lvalue.
        return !rvalue && type.toBasetype().ty != Tclass;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/expression.html#super
 */
extern (C++) final class SuperExp : ThisExp
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.super_);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A compile-time known `null` value
 *
 * https://dlang.org/spec/expression.html#null
 */
extern (C++) final class NullExp : Expression
{
    extern (D) this(const ref Loc loc, Type type = null) scope @safe
    {
        super(loc, EXP.null_);
        this.type = type;
    }

    override bool equals(const RootObject o) const
    {
        if (auto e = o.isExpression())
        {
            if (e.op == EXP.null_ && type.equals(e.type))
            {
                return true;
            }
        }
        return false;
    }

    override Optional!bool toBool()
    {
        // null in any type is false
        return typeof(return)(false);
    }

    override StringExp toStringExp()
    {
        if (this.type.implicitConvTo(Type.tstring))
        {
            auto se = new StringExp(loc, (cast(char*)mem.xcalloc(1, 1))[0 .. 0]);
            se.type = Type.tstring;
            return se;
        }

        return null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/expression.html#string_literals
 */
extern (C++) final class StringExp : Expression
{
    char postfix = NoPostfix;   // 'c', 'w', 'd'
    OwnedBy ownedByCtfe = OwnedBy.code;
    private union
    {
        char* string;   // if sz == 1
        wchar* wstring; // if sz == 2
        dchar* dstring; // if sz == 4
        ulong* lstring; // if sz == 8
    }                   // (const if ownedByCtfe == OwnedBy.code)
    size_t len;         // number of code units
    ubyte sz = 1;       // 1: char, 2: wchar, 4: dchar

    /**
     *  Whether the string literal's type is fixed
     *  Example:
     *  ---
     *  wstring x = "abc"; // OK, string literal is flexible
     *  wstring y = cast(string) "abc"; // Error: type was committed after cast
     *  ---
     */
    bool committed;

    /// If the string is parsed from a hex string literal
    bool hexString = false;

    enum char NoPostfix = 0;

    extern (D) this(const ref Loc loc, const(void)[] string) scope
    {
        super(loc, EXP.string_);
        this.string = cast(char*)string.ptr; // note that this.string should be const
        this.len = string.length;
        this.sz = 1;                    // work around LDC bug #1286
    }

    extern (D) this(const ref Loc loc, const(void)[] string, size_t len, ubyte sz, char postfix = NoPostfix) scope
    {
        super(loc, EXP.string_);
        this.string = cast(char*)string.ptr; // note that this.string should be const
        this.len = len;
        this.sz = sz;
        this.postfix = postfix;
    }

    static StringExp create(const ref Loc loc, const(char)* s)
    {
        return new StringExp(loc, s.toDString());
    }

    static StringExp create(const ref Loc loc, const(void)* string, size_t len)
    {
        return new StringExp(loc, string[0 .. len]);
    }

    override bool equals(const RootObject o) const
    {
        //printf("StringExp::equals('%s') %s\n", o.toChars(), toChars());
        if (auto e = o.isExpression())
        {
            if (auto se = e.isStringExp())
            {
                return compare(se) == 0;
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
    size_t numberOfCodeUnits(int tynto = 0) const
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
        dchar c;

        switch (sz)
        {
        case 1:
            for (size_t u = 0; u < len;)
            {
                if (const s = utf_decodeChar(string[0 .. len], u, c))
                {
                    error(loc, "%.*s", cast(int)s.length, s.ptr);
                    return 0;
                }
                result += utf_codeLength(encSize, c);
            }
            break;

        case 2:
            for (size_t u = 0; u < len;)
            {
                if (const s = utf_decodeWchar(wstring[0 .. len], u, c))
                {
                    error(loc, "%.*s", cast(int)s.length, s.ptr);
                    return 0;
                }
                result += utf_codeLength(encSize, c);
            }
            break;

        case 4:
            foreach (u; 0 .. len)
            {
                result += utf_codeLength(encSize, dstring[u]);
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
    void writeTo(void* dest, bool zero, int tyto = 0) const
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
                memset(dest + len * sz, 0, sz);
        }
        else
            assert(0);
    }

    /*********************************************
     * Get the code unit at index i
     * Params:
     *  i = index
     * Returns:
     *  code unit at index i
     */
    dchar getCodeUnit(size_t i) const pure
    {
        assert(this.sz <= dchar.sizeof);
        return cast(dchar) getIndex(i);
    }

    /// Returns: integer at index `i`
    dinteger_t getIndex(size_t i) const pure
    {
        assert(i < len);
        final switch (sz)
        {
        case 1:
            return string[i];
        case 2:
            return wstring[i];
        case 4:
            return dstring[i];
        case 8:
            return lstring[i];
        }
    }

    /*********************************************
     * Set the code unit at index i to c
     * Params:
     *  i = index
     *  c = code unit to set it to
     */
    extern (D) void setCodeUnit(size_t i, dchar c)
    {
        return setIndex(i, c);
    }

    extern (D) void setIndex(size_t i, long c)
    {
        assert(i < len);
        final switch (sz)
        {
        case 1:
            string[i] = cast(char)c;
            break;
        case 2:
            wstring[i] = cast(wchar)c;
            break;
        case 4:
            dstring[i] = cast(dchar) c;
            break;
        case 8:
            lstring[i] = c;
            break;
        }
    }

    override StringExp toStringExp()
    {
        return this;
    }


    /**
     * Compare two `StringExp` by length, then value
     *
     * The comparison is not the usual C-style comparison as seen with
     * `strcmp` or `memcmp`, but instead first compare based on the length.
     * This allows both faster lookup and sorting when comparing sparse data.
     *
     * This ordering scheme is relied on by the string-switching feature.
     * Code in Druntime's `core.internal.switch_` relies on this ordering
     * when doing a binary search among case statements.
     *
     * Both `StringExp` should be of the same encoding.
     *
     * Params:
     *   se2 = String expression to compare `this` to
     *
     * Returns:
     *   `0` when `this` is equal to se2, a value greater than `0` if
     *   `this` should be considered greater than `se2`,
     *   and a value less than `0` if `this` is lesser than `se2`.
     */
    int compare(const StringExp se2) const nothrow pure @nogc
    {
        //printf("StringExp::compare()\n");
        const len1 = len;
        const len2 = se2.len;

        assert(this.sz == se2.sz, "Comparing string expressions of different sizes");
        //printf("sz = %d, len1 = %d, len2 = %d\n", sz, cast(int)len1, cast(int)len2);
        if (len1 == len2)
        {
            switch (sz)
            {
            case 1:
                return memcmp(string, se2.string, len1);

            case 2:
                {
                    wchar* s1 = cast(wchar*)string;
                    wchar* s2 = cast(wchar*)se2.string;
                    foreach (u; 0 .. len)
                    {
                        if (s1[u] != s2[u])
                            return s1[u] - s2[u];
                    }
                }
                break;
            case 4:
                {
                    dchar* s1 = cast(dchar*)string;
                    dchar* s2 = cast(dchar*)se2.string;
                    foreach (u; 0 .. len)
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
        return cast(int)(len1 - len2);
    }

    override Optional!bool toBool()
    {
        // Keep the old behaviour for this refactoring
        // Should probably match language spec instead and check for length
        return typeof(return)(true);
    }

    override bool isLvalue()
    {
        /* string literal is rvalue in default, but
         * conversion to reference of static array is only allowed.
         */
        return !rvalue && (type && type.toBasetype().ty == Tsarray);
    }

    /********************************
     * Convert string contents to a 0 terminated string,
     * allocated by mem.xmalloc().
     */
    extern (D) const(char)[] toStringz() const
    {
        auto nbytes = len * sz;
        char* s = cast(char*)mem.xmalloc(nbytes + sz);
        writeTo(s, true);
        return s[0 .. nbytes];
    }

    extern (D) const(char)[] peekString() const
    {
        assert(sz == 1);
        return this.string[0 .. len];
    }

    extern (D) const(wchar)[] peekWstring() const
    {
        assert(sz == 2);
        return this.wstring[0 .. len];
    }

    extern (D) const(dchar)[] peekDstring() const
    {
        assert(sz == 4);
        return this.dstring[0 .. len];
    }

    /*******************
     * Get a slice of the data.
     */
    extern (D) const(ubyte)[] peekData() const
    {
        return cast(const(ubyte)[])this.string[0 .. len * sz];
    }

    /*******************
     * Borrow a slice of the data, so the caller can modify
     * it in-place (!)
     */
    extern (D) ubyte[] borrowData()
    {
        return cast(ubyte[])this.string[0 .. len * sz];
    }

    /***********************
     * Set new string data.
     * `this` becomes the new owner of the data.
     */
    extern (D) void setData(void* s, size_t len, ubyte sz)
    {
        this.string = cast(char*)s;
        this.len = len;
        this.sz = sz;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

extern (C++) final class InterpExp : Expression
{
    char postfix = NoPostfix;   // 'c', 'w', 'd'
    OwnedBy ownedByCtfe = OwnedBy.code;
    InterpolatedSet* interpolatedSet;

    enum char NoPostfix = 0;

    extern (D) this(const ref Loc loc, InterpolatedSet* set, char postfix = NoPostfix) scope @safe
    {
        super(loc, EXP.interpolated);
        this.interpolatedSet = set;
        this.postfix = postfix;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}


/***********************************************************
 * A sequence of expressions
 *
 * ---
 * alias AliasSeq(T...) = T;
 * alias Tup = AliasSeq!(3, int, "abc");
 * ---
 */
extern (C++) final class TupleExp : Expression
{
    /* Tuple-field access may need to take out its side effect part.
     * For example:
     *      foo().tupleof
     * is rewritten as:
     *      (ref __tup = foo(); tuple(__tup.field0, __tup.field1, ...))
     * The declaration of temporary variable __tup will be stored in TupleExp.e0.
     */
    Expression e0;

    Expressions* exps;

    extern (D) this(const ref Loc loc, Expression e0, Expressions* exps) @safe
    {
        super(loc, EXP.tuple);
        //printf("TupleExp(this = %p)\n", this);
        this.e0 = e0;
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, Expressions* exps) @safe
    {
        super(loc, EXP.tuple);
        //printf("TupleExp(this = %p)\n", this);
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, TupleDeclaration tup)
    {
        super(loc, EXP.tuple);
        this.exps = new Expressions();

        this.exps.reserve(tup.objects.length);
        foreach (o; *tup.objects)
        {
            if (Dsymbol s = getDsymbol(o))
            {
                /* If tuple element represents a symbol, translate to DsymbolExp
                 * to supply implicit 'this' if needed later.
                 */
                Expression e = new DsymbolExp(loc, s);
                this.exps.push(e);
            }
            else if (auto eo = o.isExpression())
            {
                auto e = eo.copy();
                e.loc = loc;    // https://issues.dlang.org/show_bug.cgi?id=15669
                this.exps.push(e);
            }
            else if (auto t = o.isType())
            {
                Expression e = new TypeExp(loc, t);
                this.exps.push(e);
            }
            else
            {
                error(loc, "`%s` is not an expression", o.toChars());
            }
        }
    }

    static TupleExp create(const ref Loc loc, Expressions* exps) @safe
    {
        return new TupleExp(loc, exps);
    }

    override TupleExp syntaxCopy()
    {
        return new TupleExp(loc, e0 ? e0.syntaxCopy() : null, arraySyntaxCopy(exps));
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto e = o.isExpression())
            if (auto te = e.isTupleExp())
            {
                if (exps.length != te.exps.length)
                    return false;
                if (e0 && !e0.equals(te.e0) || !e0 && te.e0)
                    return false;
                foreach (i, e1; *exps)
                {
                    auto e2 = (*te.exps)[i];
                    if (!e1.equals(e2))
                        return false;
                }
                return true;
            }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * [ e1, e2, e3, ... ]
 *
 * https://dlang.org/spec/expression.html#array_literals
 */
extern (C++) final class ArrayLiteralExp : Expression
{
    OwnedBy ownedByCtfe = OwnedBy.code;
    bool onstack = false;

    /** If !is null, elements[] can be sparse and basis is used for the
     * "default" element value. In other words, non-null elements[i] overrides
     * this 'basis' value.
     */
    Expression basis;

    Expressions* elements;

    extern (D) this(const ref Loc loc, Type type, Expressions* elements) @safe
    {
        super(loc, EXP.arrayLiteral);
        this.type = type;
        this.elements = elements;
    }

    extern (D) this(const ref Loc loc, Type type, Expression e)
    {
        super(loc, EXP.arrayLiteral);
        this.type = type;
        elements = new Expressions();
        elements.push(e);
    }

    extern (D) this(const ref Loc loc, Type type, Expression basis, Expressions* elements) @safe
    {
        super(loc, EXP.arrayLiteral);
        this.type = type;
        this.basis = basis;
        this.elements = elements;
    }

    static ArrayLiteralExp create(const ref Loc loc, Expressions* elements) @safe
    {
        return new ArrayLiteralExp(loc, null, elements);
    }

    override ArrayLiteralExp syntaxCopy()
    {
        return new ArrayLiteralExp(loc,
            null,
            basis ? basis.syntaxCopy() : null,
            arraySyntaxCopy(elements));
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        auto e = o.isExpression();
        if (!e)
            return false;
        if (auto ae = e.isArrayLiteralExp())
        {
            if (elements.length != ae.elements.length)
                return false;
            if (elements.length == 0 && !type.equals(ae.type))
            {
                return false;
            }

            foreach (i, e1; *elements)
            {
                auto e2 = (*ae.elements)[i];
                auto e1x = e1 ? e1 : basis;
                auto e2x = e2 ? e2 : ae.basis;

                if (e1x != e2x && (!e1x || !e2x || !e1x.equals(e2x)))
                    return false;
            }
            return true;
        }
        return false;
    }

    Expression getElement(size_t i) // use opIndex instead
    {
        return this[i];
    }

    extern (D) Expression opIndex(size_t i)
    {
        auto el = (*elements)[i];
        return el ? el : basis;
    }

    override Optional!bool toBool()
    {
        size_t dim = elements ? elements.length : 0;
        return typeof(return)(dim != 0);
    }

    override StringExp toStringExp()
    {
        TY telem = type.nextOf().toBasetype().ty;
        if (telem.isSomeChar || (telem == Tvoid && (!elements || elements.length == 0)))
        {
            ubyte sz = 1;
            if (telem == Twchar)
                sz = 2;
            else if (telem == Tdchar)
                sz = 4;

            OutBuffer buf;
            if (elements)
            {
                foreach (i; 0 .. elements.length)
                {
                    auto ch = this[i];
                    if (ch.op != EXP.int64)
                        return null;
                    if (sz == 1)
                        buf.writeByte(cast(uint)ch.toInteger());
                    else if (sz == 2)
                        buf.writeword(cast(uint)ch.toInteger());
                    else
                        buf.write4(cast(uint)ch.toInteger());
                }
            }
            char prefix;
            if (sz == 1)
            {
                prefix = 'c';
                buf.writeByte(0);
            }
            else if (sz == 2)
            {
                prefix = 'w';
                buf.writeword(0);
            }
            else
            {
                prefix = 'd';
                buf.write4(0);
            }

            const size_t len = buf.length / sz - 1;
            auto se = new StringExp(loc, buf.extractSlice()[0 .. len * sz], len, sz, prefix);
            se.sz = sz;
            se.type = type;
            return se;
        }
        return null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * [ key0 : value0, key1 : value1, ... ]
 *
 * https://dlang.org/spec/expression.html#associative_array_literals
 */
extern (C++) final class AssocArrayLiteralExp : Expression
{
    OwnedBy ownedByCtfe = OwnedBy.code;

    Expressions* keys;
    Expressions* values;
    /// Lower to core.internal.newaa for static initializaton
    Expression lowering;

    extern (D) this(const ref Loc loc, Expressions* keys, Expressions* values) @safe
    {
        super(loc, EXP.assocArrayLiteral);
        assert(keys.length == values.length);
        this.keys = keys;
        this.values = values;
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        auto e = o.isExpression();
        if (!e)
            return false;
        if (auto ae = e.isAssocArrayLiteralExp())
        {
            if (keys.length != ae.keys.length)
                return false;
            size_t count = 0;
            foreach (i, key; *keys)
            {
                foreach (j, akey; *ae.keys)
                {
                    if (key.equals(akey))
                    {
                        if (!(*values)[i].equals((*ae.values)[j]))
                            return false;
                        ++count;
                    }
                }
            }
            return count == keys.length;
        }
        return false;
    }

    override AssocArrayLiteralExp syntaxCopy()
    {
        return new AssocArrayLiteralExp(loc, arraySyntaxCopy(keys), arraySyntaxCopy(values));
    }

    override Optional!bool toBool()
    {
        size_t dim = keys.length;
        return typeof(return)(dim != 0);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * sd( e1, e2, e3, ... )
 */
extern (C++) final class StructLiteralExp : Expression
{
    StructDeclaration sd;   /// which aggregate this is for
    Expressions* elements;  /// parallels sd.fields[] with null entries for fields to skip
    Type stype;             /// final type of result (can be different from sd's type)

    // `inlineCopy` is only used temporarily in the `inline.d` pass,
    // while `sym` is only used in `e2ir/s2ir/tocsym` which comes after
    union
    {
        void* sym;            /// back end symbol to initialize with literal (used as a Symbol*)

        /// those fields need to prevent a infinite recursion when one field of struct initialized with 'this' pointer.
        StructLiteralExp inlinecopy;
    }

    /** pointer to the origin instance of the expression.
     * once a new expression is created, origin is set to 'this'.
     * anytime when an expression copy is created, 'origin' pointer is set to
     * 'origin' pointer value of the original expression.
     */
    StructLiteralExp origin;


    /** anytime when recursive function is calling, 'stageflags' marks with bit flag of
     * current stage and unmarks before return from this function.
     * 'inlinecopy' uses similar 'stageflags' and from multiple evaluation 'doInline'
     * (with infinite recursion) of this expression.
     */
    enum StageFlags : ubyte
    {
        none              = 0x0,
        scrub             = 0x1,  /// scrubReturnValue is running
        searchPointers    = 0x2,  /// hasNonConstPointers is running
        optimize          = 0x4,  /// optimize is running
        apply             = 0x8,  /// apply is running
        inlineScan        = 0x10, /// inlineScan is running
        toCBuffer         = 0x20 /// toCBuffer is running
    }
    StageFlags stageflags;

    bool useStaticInit;     /// if this is true, use the StructDeclaration's init symbol
    bool isOriginal = false; /// used when moving instances to indicate `this is this.origin`
    OwnedBy ownedByCtfe = OwnedBy.code;

    extern (D) this(const ref Loc loc, StructDeclaration sd, Expressions* elements, Type stype = null) @safe
    {
        super(loc, EXP.structLiteral);
        this.sd = sd;
        if (!elements)
            elements = new Expressions();
        this.elements = elements;
        this.stype = stype;
        this.origin = this;
        //printf("StructLiteralExp::StructLiteralExp(%s)\n", toChars());
    }

    static StructLiteralExp create(const ref Loc loc, StructDeclaration sd, void* elements, Type stype = null)
    {
        return new StructLiteralExp(loc, sd, cast(Expressions*)elements, stype);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        auto e = o.isExpression();
        if (!e)
            return false;
        if (auto se = e.isStructLiteralExp())
        {
            if (!type.equals(se.type))
                return false;
            if (elements.length != se.elements.length)
                return false;
            foreach (i, e1; *elements)
            {
                auto e2 = (*se.elements)[i];
                if (e1 != e2 && (!e1 || !e2 || !e1.equals(e2)))
                    return false;
            }
            return true;
        }
        return false;
    }

    override StructLiteralExp syntaxCopy()
    {
        auto exp = new StructLiteralExp(loc, sd, arraySyntaxCopy(elements), type ? type : stype);
        exp.origin = this;
        return exp;
    }

    /**************************************
     * Gets expression at offset of type.
     * Returns NULL if not found.
     */
    extern (D) Expression getField(Type type, uint offset)
    {
        //printf("StructLiteralExp::getField(this = %s, type = %s, offset = %u)\n",
        //  /*toChars()*/"", type.toChars(), offset);
        Expression e = null;
        int i = getFieldIndex(type, offset);

        if (i != -1)
        {
            //printf("\ti = %d\n", i);
            if (i >= sd.nonHiddenFields())
                return null;

            assert(i < elements.length);
            e = (*elements)[i];
            if (e)
            {
                //printf("e = %s, e.type = %s\n", e.toChars(), e.type.toChars());

                /* If type is a static array, and e is an initializer for that array,
                 * then the field initializer should be an array literal of e.
                 */
                auto tsa = type.isTypeSArray();
                if (tsa && e.type.castMod(0) != type.castMod(0))
                {
                    const length = cast(size_t)tsa.dim.toInteger();
                    auto z = new Expressions(length);
                    foreach (ref q; *z)
                        q = e.copy();
                    e = new ArrayLiteralExp(loc, type, z);
                }
                else
                {
                    e = e.copy();
                    e.type = type;
                }
                if (useStaticInit && e.type.needsNested())
                    if (auto se = e.isStructLiteralExp())
                    {
                        se.useStaticInit = true;
                    }
            }
        }
        return e;
    }

    /************************************
     * Get index of field.
     * Returns -1 if not found.
     */
    extern (D) int getFieldIndex(Type type, uint offset)
    {
        /* Find which field offset is by looking at the field offsets
         */
        if (elements.length)
        {
            const sz = type.size();
            if (sz == SIZE_INVALID)
                return -1;
            foreach (i, v; sd.fields)
            {
                if (offset == v.offset && sz == v.type.size())
                {
                    /* context fields might not be filled. */
                    if (i >= sd.nonHiddenFields())
                        return cast(int)i;
                    if (auto e = (*elements)[i])
                    {
                        return cast(int)i;
                    }
                    break;
                }
            }
        }
        return -1;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * C11 6.5.2.5
 * ( type-name ) { initializer-list }
 */
extern (C++) final class CompoundLiteralExp : Expression
{
    Initializer initializer; /// initializer-list

    extern (D) this(const ref Loc loc, Type type_name, Initializer initializer) @safe
    {
        super(loc, EXP.compoundLiteral);
        super.type = type_name;
        this.initializer = initializer;
        //printf("CompoundLiteralExp::CompoundLiteralExp(%s)\n", toChars());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Mainly just a placeholder
 */
extern (C++) final class TypeExp : Expression
{
    extern (D) this(const ref Loc loc, Type type) @safe
    {
        super(loc, EXP.type);
        //printf("TypeExp::TypeExp(%s)\n", type.toChars());
        this.type = type;
    }

    override TypeExp syntaxCopy()
    {
        return new TypeExp(loc, type.syntaxCopy());
    }

    override bool checkType()
    {
        error(loc, "type `%s` is not an expression", toChars());
        return true;
    }

    override bool checkValue()
    {
        error(loc, "type `%s` has no value", toChars());
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Mainly just a placeholder of
 *  Package, Module, Nspace, and TemplateInstance (including TemplateMixin)
 *
 * A template instance that requires IFTI:
 *      foo!tiargs(fargs)       // foo!tiargs
 * is left until CallExp::semantic() or resolveProperties()
 */
extern (C++) final class ScopeExp : Expression
{
    ScopeDsymbol sds;

    extern (D) this(const ref Loc loc, ScopeDsymbol sds) @safe
    {
        super(loc, EXP.scope_);
        //printf("ScopeExp::ScopeExp(sds = '%s')\n", sds.toChars());
        //static int count; if (++count == 38) *(char*)0=0;
        this.sds = sds;
        assert(!sds.isTemplateDeclaration());   // instead, you should use TemplateExp
    }

    override ScopeExp syntaxCopy()
    {
        return new ScopeExp(loc, sds.syntaxCopy(null));
    }

    override bool checkType()
    {
        if (sds.isPackage())
        {
            error(loc, "%s `%s` has no type", sds.kind(), sds.toChars());
            return true;
        }
        if (auto ti = sds.isTemplateInstance())
        {
            //assert(ti.needsTypeInference(sc));
            if (ti.tempdecl &&
                ti.semantictiargsdone &&
                ti.semanticRun == PASS.initial)
            {
                error(loc, "partial %s `%s` has no type", sds.kind(), toChars());
                return true;
            }
        }
        return false;
    }

    override bool checkValue()
    {
        error(loc, "%s `%s` has no value", sds.kind(), sds.toChars());
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Mainly just a placeholder
 */
extern (C++) final class TemplateExp : Expression
{
    TemplateDeclaration td;
    FuncDeclaration fd;

    extern (D) this(const ref Loc loc, TemplateDeclaration td, FuncDeclaration fd = null) @safe
    {
        super(loc, EXP.template_);
        //printf("TemplateExp(): %s\n", td.toChars());
        this.td = td;
        this.fd = fd;
    }

    override bool isLvalue()
    {
        return fd !is null;
    }

    override bool checkType()
    {
        error(loc, "%s `%s` has no type", td.kind(), toChars());
        return true;
    }

    override bool checkValue()
    {
        error(loc, "%s `%s` has no value", td.kind(), toChars());
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * newtype(arguments)
 */
extern (C++) final class NewExp : Expression
{
    Expression thisexp;         // if !=null, 'this' for class being allocated
    Type newtype;
    Expressions* arguments;     // Array of Expression's
    Identifiers* names;         // Array of names corresponding to expressions

    Expression argprefix;       // expression to be evaluated just before arguments[]
    CtorDeclaration member;     // constructor function
    bool onstack;               // allocate on stack
    bool thrownew;              // this NewExp is the expression of a ThrowStatement

    Expression lowering;        // lowered druntime hook: `_d_new{class,itemT}`

    /// Puts the `arguments` and `names` into an `ArgumentList` for easily passing them around.
    /// The fields are still separate for backwards compatibility
    extern (D) ArgumentList argumentList() { return ArgumentList(arguments, names); }

    extern (D) this(const ref Loc loc, Expression thisexp, Type newtype, Expressions* arguments, Identifiers* names = null) @safe
    {
        super(loc, EXP.new_);
        this.thisexp = thisexp;
        this.newtype = newtype;
        this.arguments = arguments;
        this.names = names;
    }

    static NewExp create(const ref Loc loc, Expression thisexp, Type newtype, Expressions* arguments) @safe
    {
        return new NewExp(loc, thisexp, newtype, arguments);
    }

    override NewExp syntaxCopy()
    {
        return new NewExp(loc,
            thisexp ? thisexp.syntaxCopy() : null,
            newtype.syntaxCopy(),
            arraySyntaxCopy(arguments),
            names ? names.copy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * class baseclasses { } (arguments)
 */
extern (C++) final class NewAnonClassExp : Expression
{
    Expression thisexp;     // if !=null, 'this' for class being allocated
    ClassDeclaration cd;    // class being instantiated
    Expressions* arguments; // Array of Expression's to call class constructor

    extern (D) this(const ref Loc loc, Expression thisexp, ClassDeclaration cd, Expressions* arguments) @safe
    {
        super(loc, EXP.newAnonymousClass);
        this.thisexp = thisexp;
        this.cd = cd;
        this.arguments = arguments;
    }

    override NewAnonClassExp syntaxCopy()
    {
        return new NewAnonClassExp(loc, thisexp ? thisexp.syntaxCopy() : null, cd.syntaxCopy(null), arraySyntaxCopy(arguments));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class SymbolExp : Expression
{
    Declaration var;
    Dsymbol originalScope; // original scope before inlining
    bool hasOverloads;

    extern (D) this(const ref Loc loc, EXP op, Declaration var, bool hasOverloads) @safe
    {
        super(loc, op);
        assert(var);
        this.var = var;
        this.hasOverloads = hasOverloads;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Offset from symbol
 */
extern (C++) final class SymOffExp : SymbolExp
{
    dinteger_t offset;

    extern (D) this(const ref Loc loc, Declaration var, dinteger_t offset, bool hasOverloads = true)
    {
        if (auto v = var.isVarDeclaration())
        {
            // FIXME: This error report will never be handled anyone.
            // It should be done before the SymOffExp construction.
            if (v.needThis())
            {
                auto t = v.isThis();
                assert(t);
                .error(loc, "taking the address of non-static variable `%s` requires an instance of `%s`", v.toChars(), t.toChars());
            }
            hasOverloads = false;
        }
        super(loc, EXP.symbolOffset, var, hasOverloads);
        this.offset = offset;
    }

    override Optional!bool toBool()
    {
        return typeof(return)(true);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Variable
 */
extern (C++) final class VarExp : SymbolExp
{
    bool delegateWasExtracted;
    extern (D) this(const ref Loc loc, Declaration var, bool hasOverloads = true) @safe
    {
        if (var.isVarDeclaration())
            hasOverloads = false;

        super(loc, EXP.variable, var, hasOverloads);
        //printf("VarExp(this = %p, '%s', loc = %s)\n", this, var.toChars(), loc.toChars());
        //if (strcmp(var.ident.toChars(), "func") == 0) assert(0);
        this.type = var.type;
    }

    static VarExp create(const ref Loc loc, Declaration var, bool hasOverloads = true) @safe
    {
        return new VarExp(loc, var, hasOverloads);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto ne = o.isExpression().isVarExp())
        {
            if (type.toHeadMutable().equals(ne.type.toHeadMutable()) && var == ne.var)
            {
                return true;
            }
        }
        return false;
    }

    override bool isLvalue()
    {
        if (rvalue || var.storage_class & (STC.lazy_ | STC.rvalue | STC.manifest))
            return false;
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Overload Set
 */
extern (C++) final class OverExp : Expression
{
    OverloadSet vars;

    extern (D) this(const ref Loc loc, OverloadSet s)
    {
        super(loc, EXP.overloadSet);
        //printf("OverExp(this = %p, '%s')\n", this, var.toChars());
        vars = s;
        type = Type.tvoid;
    }

    override bool isLvalue()
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Function/Delegate literal
 */

extern (C++) final class FuncExp : Expression
{
    FuncLiteralDeclaration fd;
    TemplateDeclaration td;
    TOK tok;  // TOK.reserved, TOK.delegate_, TOK.function_

    extern (D) this(const ref Loc loc, Dsymbol s)
    {
        super(loc, EXP.function_);
        this.td = s.isTemplateDeclaration();
        this.fd = s.isFuncLiteralDeclaration();
        if (td)
        {
            assert(td.literal);
            assert(td.members && td.members.length == 1);
            fd = (*td.members)[0].isFuncLiteralDeclaration();
        }
        tok = fd.tok; // save original kind of function/delegate/(infer)
        assert(fd.fbody);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        auto e = o.isExpression();
        if (!e)
            return false;
        if (auto fe = e.isFuncExp())
        {
            return fd == fe.fd;
        }
        return false;
    }

    override FuncExp syntaxCopy()
    {
        if (td)
            return new FuncExp(loc, td.syntaxCopy(null));
        if (fd.semanticRun == PASS.initial)
            return new FuncExp(loc, fd.syntaxCopy(null));
        // https://issues.dlang.org/show_bug.cgi?id=13481
        // Prevent multiple semantic analysis of lambda body.
        return new FuncExp(loc, fd);
    }

    override const(char)* toChars() const
    {
        return fd.toChars();
    }

    override bool checkType()
    {
        if (td)
        {
            error(loc, "template lambda has no type");
            return true;
        }
        return false;
    }

    override bool checkValue()
    {
        if (td)
        {
            error(loc, "template lambda has no value");
            return true;
        }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Declaration of a symbol
 *
 * D grammar allows declarations only as statements. However in AST representation
 * it can be part of any expression. This is used, for example, during internal
 * syntax re-writes to inject hidden symbols.
 */
extern (C++) final class DeclarationExp : Expression
{
    Dsymbol declaration;

    extern (D) this(const ref Loc loc, Dsymbol declaration) @safe
    {
        super(loc, EXP.declaration);
        this.declaration = declaration;
    }

    override DeclarationExp syntaxCopy()
    {
        return new DeclarationExp(loc, declaration.syntaxCopy(null));
    }

    override bool hasCode()
    {
        if (auto vd = declaration.isVarDeclaration())
        {
            return !(vd.storage_class & (STC.manifest | STC.static_));
        }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * typeid(int)
 */
extern (C++) final class TypeidExp : Expression
{
    RootObject obj;

    extern (D) this(const ref Loc loc, RootObject o) @safe
    {
        super(loc, EXP.typeid_);
        this.obj = o;
    }

    override TypeidExp syntaxCopy()
    {
        return new TypeidExp(loc, objectSyntaxCopy(obj));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * __traits(identifier, args...)
 */
extern (C++) final class TraitsExp : Expression
{
    Identifier ident;
    Objects* args;

    extern (D) this(const ref Loc loc, Identifier ident, Objects* args) @safe
    {
        super(loc, EXP.traits);
        this.ident = ident;
        this.args = args;
    }

    override TraitsExp syntaxCopy()
    {
        return new TraitsExp(loc, ident, TemplateInstance.arraySyntaxCopy(args));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Generates a halt instruction
 *
 * `assert(0)` gets rewritten to this with `CHECKACTION.halt`
 */
extern (C++) final class HaltExp : Expression
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.halt);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * is(targ id tok tspec)
 * is(targ id == tok2)
 */
extern (C++) final class IsExp : Expression
{
    Type targ;
    Identifier id;      // can be null
    Type tspec;         // can be null
    TemplateParameters* parameters;
    TOK tok;            // ':' or '=='
    TOK tok2;           // 'struct', 'union', etc.

    extern (D) this(const ref Loc loc, Type targ, Identifier id, TOK tok, Type tspec, TOK tok2, TemplateParameters* parameters) scope @safe
    {
        super(loc, EXP.is_);
        this.targ = targ;
        this.id = id;
        this.tok = tok;
        this.tspec = tspec;
        this.tok2 = tok2;
        this.parameters = parameters;
    }

    override IsExp syntaxCopy()
    {
        // This section is identical to that in TemplateDeclaration::syntaxCopy()
        TemplateParameters* p = null;
        if (parameters)
        {
            p = new TemplateParameters(parameters.length);
            foreach (i, el; *parameters)
                (*p)[i] = el.syntaxCopy();
        }
        return new IsExp(loc, targ.syntaxCopy(), id, tok, tspec ? tspec.syntaxCopy() : null, tok2, p);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Base class for unary operators
 *
 * https://dlang.org/spec/expression.html#unary-expression
 */
extern (C++) abstract class UnaExp : Expression
{
    Expression e1;

    extern (D) this(const ref Loc loc, EXP op, Expression e1) scope @safe
    {
        super(loc, op);
        this.e1 = e1;
    }

    override UnaExp syntaxCopy()
    {
        UnaExp e = cast(UnaExp)copy();
        e.type = null;
        e.e1 = e.e1.syntaxCopy();
        return e;
    }

    /*********************
     * Mark the operand as will never be dereferenced,
     * which is useful info for @safe checks.
     * Do before semantic() on operands rewrites them.
     */
    final void setNoderefOperand()
    {
        if (auto edi = e1.isDotIdExp())
            edi.noderef = true;

    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Base class for binary operators
 */
extern (C++) abstract class BinExp : Expression
{
    Expression e1;
    Expression e2;
    Type att1;      // Save alias this type to detect recursion
    Type att2;      // Save alias this type to detect recursion

    extern (D) this(const ref Loc loc, EXP op, Expression e1, Expression e2) scope @safe
    {
        super(loc, op);
        this.e1 = e1;
        this.e2 = e2;
    }

    override BinExp syntaxCopy()
    {
        BinExp e = cast(BinExp)copy();
        e.type = null;
        e.e1 = e.e1.syntaxCopy();
        e.e2 = e.e2.syntaxCopy();
        return e;
    }

    extern (D) final bool checkIntegralBin()
    {
        bool r1 = e1.checkIntegral();
        bool r2 = e2.checkIntegral();
        return (r1 || r2);
    }

    extern (D) final bool checkArithmeticBin()
    {
        bool r1 = e1.checkArithmetic(this.op);
        bool r2 = e2.checkArithmetic(this.op);
        return (r1 || r2);
    }

    /*********************
     * Mark the operands as will never be dereferenced,
     * which is useful info for @safe checks.
     * Do before semantic() on operands rewrites them.
     */
    final void setNoderefOperands()
    {
        if (auto edi = e1.isDotIdExp())
            edi.noderef = true;
        if (auto edi = e2.isDotIdExp())
            edi.noderef = true;

    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Binary operator assignment, `+=` `-=` `*=` etc.
 */
extern (C++) class BinAssignExp : BinExp
{
    extern (D) this(const ref Loc loc, EXP op, Expression e1, Expression e2) scope @safe
    {
        super(loc, op, e1, e2);
    }

    override final bool isLvalue()
    {
        return !rvalue;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A string mixin, `mixin("x")`
 *
 * https://dlang.org/spec/expression.html#mixin_expressions
 */
extern (C++) final class MixinExp : Expression
{
    Expressions* exps;

    extern (D) this(const ref Loc loc, Expressions* exps) @safe
    {
        super(loc, EXP.mixin_);
        this.exps = exps;
    }

    override MixinExp syntaxCopy()
    {
        return new MixinExp(loc, arraySyntaxCopy(exps));
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        auto e = o.isExpression();
        if (!e)
            return false;
        if (auto ce = e.isMixinExp())
        {
            if (exps.length != ce.exps.length)
                return false;
            foreach (i, e1; *exps)
            {
                auto e2 = (*ce.exps)[i];
                if (e1 != e2 && (!e1 || !e2 || !e1.equals(e2)))
                    return false;
            }
            return true;
        }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * An import expression, `import("file.txt")`
 *
 * Not to be confused with module imports, `import std.stdio`, which is an `ImportStatement`
 *
 * https://dlang.org/spec/expression.html#import_expressions
 */
extern (C++) final class ImportExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.import_, e);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * An assert expression, `assert(x == y)`
 *
 * https://dlang.org/spec/expression.html#assert_expressions
 */
extern (C++) final class AssertExp : UnaExp
{
    Expression msg;

    extern (D) this(const ref Loc loc, Expression e, Expression msg = null) @safe
    {
        super(loc, EXP.assert_, e);
        this.msg = msg;
    }

    override AssertExp syntaxCopy()
    {
        return new AssertExp(loc, e1.syntaxCopy(), msg ? msg.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `throw <e1>` as proposed by DIP 1034.
 *
 * Replacement for the deprecated `ThrowStatement` that can be nested
 * in other expression.
 */
extern (C++) final class ThrowExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.throw_, e);
    }

    override ThrowExp syntaxCopy()
    {
        return new ThrowExp(loc, e1.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DotIdExp : UnaExp
{
    Identifier ident;
    bool noderef;       // true if the result of the expression will never be dereferenced
    bool wantsym;       // do not replace Symbol with its initializer during semantic()
    bool arrow;         // ImportC: if -> instead of .

    extern (D) this(const ref Loc loc, Expression e, Identifier ident) @safe
    {
        super(loc, EXP.dotIdentifier, e);
        this.ident = ident;
    }

    static DotIdExp create(const ref Loc loc, Expression e, Identifier ident) @safe
    {
        return new DotIdExp(loc, e, ident);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Mainly just a placeholder
 */
extern (C++) final class DotTemplateExp : UnaExp
{
    TemplateDeclaration td;

    extern (D) this(const ref Loc loc, Expression e, TemplateDeclaration td) @safe
    {
        super(loc, EXP.dotTemplateDeclaration, e);
        this.td = td;
    }

    override bool checkType()
    {
        error(loc, "%s `%s` has no type", td.kind(), toChars());
        return true;
    }

    override bool checkValue()
    {
        error(loc, "%s `%s` has no value", td.kind(), toChars());
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DotVarExp : UnaExp
{
    Declaration var;
    bool hasOverloads;

    extern (D) this(const ref Loc loc, Expression e, Declaration var, bool hasOverloads = true) @safe
    {
        if (var.isVarDeclaration())
            hasOverloads = false;

        super(loc, EXP.dotVariable, e);
        //printf("DotVarExp()\n");
        this.var = var;
        this.hasOverloads = hasOverloads;
    }

    override bool isLvalue()
    {
        if (rvalue)
            return false;
        if (e1.op != EXP.structLiteral)
            return true;
        auto vd = var.isVarDeclaration();
        return !(vd && vd.isField());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * foo.bar!(args)
 */
extern (C++) final class DotTemplateInstanceExp : UnaExp
{
    TemplateInstance ti;

    extern (D) this(const ref Loc loc, Expression e, Identifier name, Objects* tiargs)
    {
        super(loc, EXP.dotTemplateInstance, e);
        //printf("DotTemplateInstanceExp()\n");
        this.ti = new TemplateInstance(loc, name, tiargs);
    }

    extern (D) this(const ref Loc loc, Expression e, TemplateInstance ti) @safe
    {
        super(loc, EXP.dotTemplateInstance, e);
        this.ti = ti;
    }

    override DotTemplateInstanceExp syntaxCopy()
    {
        return new DotTemplateInstanceExp(loc, e1.syntaxCopy(), ti.name, TemplateInstance.arraySyntaxCopy(ti.tiargs));
    }

    override bool checkType()
    {
        // Same logic as ScopeExp.checkType()
        if (ti.tempdecl &&
            ti.semantictiargsdone &&
            ti.semanticRun == PASS.initial)
        {
            error(loc, "partial %s `%s` has no type", ti.kind(), toChars());
            return true;
        }
        return false;
    }

    override bool checkValue()
    {
        if (ti.tempdecl &&
            ti.semantictiargsdone &&
            ti.semanticRun == PASS.initial)

            error(loc, "partial %s `%s` has no value", ti.kind(), toChars());
        else
            error(loc, "%s `%s` has no value", ti.kind(), ti.toChars());
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DelegateExp : UnaExp
{
    FuncDeclaration func;
    bool hasOverloads;
    VarDeclaration vthis2;  // container for multi-context

    extern (D) this(const ref Loc loc, Expression e, FuncDeclaration f, bool hasOverloads = true, VarDeclaration vthis2 = null) @safe
    {
        super(loc, EXP.delegate_, e);
        this.func = f;
        this.hasOverloads = hasOverloads;
        this.vthis2 = vthis2;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DotTypeExp : UnaExp
{
    Dsymbol sym;        // symbol that represents a type

    extern (D) this(const ref Loc loc, Expression e, Dsymbol s) @safe
    {
        super(loc, EXP.dotType, e);
        this.sym = s;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * The arguments of a function call
 *
 * Contains a list of expressions. If it is a named argument, the `names`
 * list has a non-null entry at the same index.
 */
struct ArgumentList
{
    Expressions* arguments; // function arguments
    Identifiers* names;     // named argument identifiers

    size_t length() const @nogc nothrow pure @safe { return arguments ? arguments.length : 0; }

    /// Returns: whether this argument list contains any named arguments
    bool hasNames() const @nogc nothrow pure @safe
    {
        if (names is null)
            return false;
        foreach (name; *names)
            if (name !is null)
                return true;

        return false;
    }
}

/***********************************************************
 */
extern (C++) final class CallExp : UnaExp
{
    Expressions* arguments; // function arguments
    Identifiers* names;     // named argument identifiers
    FuncDeclaration f;      // symbol to call
    bool directcall;        // true if a virtual call is devirtualized
    bool inDebugStatement;  /// true if this was in a debug statement
    bool ignoreAttributes;  /// don't enforce attributes (e.g. call @gc function in @nogc code)
    bool isUfcsRewrite;     /// the first argument was pushed in here by a UFCS rewrite
    VarDeclaration vthis2;  // container for multi-context

    /// Puts the `arguments` and `names` into an `ArgumentList` for easily passing them around.
    /// The fields are still separate for backwards compatibility
    extern (D) ArgumentList argumentList() { return ArgumentList(arguments, names); }

    extern (D) this(const ref Loc loc, Expression e, Expressions* exps, Identifiers* names = null) @safe
    {
        super(loc, EXP.call, e);
        this.arguments = exps;
        this.names = names;
    }

    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.call, e);
    }

    extern (D) this(const ref Loc loc, Expression e, Expression earg1)
    {
        super(loc, EXP.call, e);
        this.arguments = new Expressions();
        if (earg1)
            this.arguments.push(earg1);
    }

    extern (D) this(const ref Loc loc, Expression e, Expression earg1, Expression earg2)
    {
        super(loc, EXP.call, e);
        auto arguments = new Expressions(2);
        (*arguments)[0] = earg1;
        (*arguments)[1] = earg2;
        this.arguments = arguments;
    }

    /***********************************************************
    * Instatiates a new function call expression
    * Params:
    *       loc   = location
    *       fd    = the declaration of the function to call
    *       earg1 = the function argument
    */
    extern(D) this(const ref Loc loc, FuncDeclaration fd, Expression earg1)
    {
        this(loc, new VarExp(loc, fd, false), earg1);
        this.f = fd;
    }

    static CallExp create(const ref Loc loc, Expression e, Expressions* exps) @safe
    {
        return new CallExp(loc, e, exps);
    }

    static CallExp create(const ref Loc loc, Expression e) @safe
    {
        return new CallExp(loc, e);
    }

    static CallExp create(const ref Loc loc, Expression e, Expression earg1)
    {
        return new CallExp(loc, e, earg1);
    }

    /***********************************************************
    * Creates a new function call expression
    * Params:
    *       loc   = location
    *       fd    = the declaration of the function to call
    *       earg1 = the function argument
    */
    static CallExp create(const ref Loc loc, FuncDeclaration fd, Expression earg1)
    {
        return new CallExp(loc, fd, earg1);
    }

    override CallExp syntaxCopy()
    {
        return new CallExp(loc, e1.syntaxCopy(), arraySyntaxCopy(arguments), names ? names.copy() : null);
    }

    override bool isLvalue()
    {
        if (rvalue)
            return false;
        Type tb = e1.type.toBasetype();
        if (tb.ty == Tdelegate || tb.ty == Tpointer)
            tb = tb.nextOf();
        auto tf = tb.isTypeFunction();
        if (tf && tf.isRef)
        {
            if (auto dve = e1.isDotVarExp())
                if (dve.var.isCtorDeclaration())
                    return false;
            return true; // function returns a reference
        }
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Get the called function type from a call expression
 * Params:
 *   ce = function call expression. Must have had semantic analysis done.
 * Returns: called function type, or `null` if error / no semantic analysis done
 */
TypeFunction calledFunctionType(CallExp ce)
{
    Type t = ce.e1.type;
    if (!t)
        return null;
    t = t.toBasetype();
    if (auto tf = t.isTypeFunction())
        return tf;
    if (auto td = t.isTypeDelegate())
        return td.nextOf().isTypeFunction();
    return null;
}

FuncDeclaration isFuncAddress(Expression e, bool* hasOverloads = null) @safe
{
    if (auto ae = e.isAddrExp())
    {
        auto ae1 = ae.e1;
        if (auto ve = ae1.isVarExp())
        {
            if (hasOverloads)
                *hasOverloads = ve.hasOverloads;
            return ve.var.isFuncDeclaration();
        }
        if (auto dve = ae1.isDotVarExp())
        {
            if (hasOverloads)
                *hasOverloads = dve.hasOverloads;
            return dve.var.isFuncDeclaration();
        }
    }
    else
    {
        if (auto soe = e.isSymOffExp())
        {
            if (hasOverloads)
                *hasOverloads = soe.hasOverloads;
            return soe.var.isFuncDeclaration();
        }
        if (auto dge = e.isDelegateExp())
        {
            if (hasOverloads)
                *hasOverloads = dge.hasOverloads;
            return dge.func.isFuncDeclaration();
        }
    }
    return null;
}

/***********************************************************
 * The 'address of' operator, `&p`
 */
extern (C++) final class AddrExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.address, e);
    }

    extern (D) this(const ref Loc loc, Expression e, Type t) @safe
    {
        this(loc, e);
        type = t;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The pointer dereference operator, `*p`
 */
extern (C++) final class PtrExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.star, e);
        //if (e.type)
        //  type = ((TypePointer *)e.type).next;
    }

    extern (D) this(const ref Loc loc, Expression e, Type t) @safe
    {
        super(loc, EXP.star, e);
        type = t;
    }

    override bool isLvalue()
    {
        return !rvalue;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The negation operator, `-x`
 */
extern (C++) final class NegExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.negate, e);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The unary add operator, `+x`
 */
extern (C++) final class UAddExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) scope @safe
    {
        super(loc, EXP.uadd, e);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The bitwise complement operator, `~x`
 */
extern (C++) final class ComExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.tilde, e);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The logical not operator, `!x`
 */
extern (C++) final class NotExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e) @safe
    {
        super(loc, EXP.not, e);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The delete operator, `delete x` (deprecated)
 *
 * https://dlang.org/spec/expression.html#delete_expressions
 */
extern (C++) final class DeleteExp : UnaExp
{
    bool isRAII;        // true if called automatically as a result of scoped destruction

    extern (D) this(const ref Loc loc, Expression e, bool isRAII) @safe
    {
        super(loc, EXP.delete_, e);
        this.isRAII = isRAII;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The type cast operator, `cast(T) x`
 *
 * It's possible to cast to one type while painting to another type
 *
 * https://dlang.org/spec/expression.html#cast_expressions
 */
extern (C++) final class CastExp : UnaExp
{
    Type to;                    // type to cast to
    ubyte mod = cast(ubyte)~0;  // MODxxxxx
    bool trusted; // assume cast is safe

    extern (D) this(const ref Loc loc, Expression e, Type t) @safe
    {
        super(loc, EXP.cast_, e);
        this.to = t;
    }

    /* For cast(const) and cast(immutable)
     */
    extern (D) this(const ref Loc loc, Expression e, ubyte mod) @safe
    {
        super(loc, EXP.cast_, e);
        this.mod = mod;
    }

    override CastExp syntaxCopy()
    {
        return to ? new CastExp(loc, e1.syntaxCopy(), to.syntaxCopy()) : new CastExp(loc, e1.syntaxCopy(), mod);
    }

    override bool isLvalue()
    {
        //printf("e1.type = %s, to.type = %s\n", e1.type.toChars(), to.toChars());
        if (rvalue || !e1.isLvalue())
            return false;
        return (to.ty == Tsarray && (e1.type.ty == Tvector || e1.type.ty == Tsarray)) ||
            e1.type.mutableOf.unSharedOf().equals(to.mutableOf().unSharedOf());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class VectorExp : UnaExp
{
    TypeVector to;      // the target vector type before semantic()
    uint dim = ~0;      // number of elements in the vector
    OwnedBy ownedByCtfe = OwnedBy.code;

    extern (D) this(const ref Loc loc, Expression e, Type t) @trusted
    {
        super(loc, EXP.vector, e);
        assert(t.ty == Tvector);
        to = cast(TypeVector)t;
    }

    static VectorExp create(const ref Loc loc, Expression e, Type t) @safe
    {
        return new VectorExp(loc, e, t);
    }

    override VectorExp syntaxCopy()
    {
        return new VectorExp(loc, e1.syntaxCopy(), to.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * e1.array property for vectors.
 *
 * https://dlang.org/spec/simd.html#properties
 */
extern (C++) final class VectorArrayExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e1) @safe
    {
        super(loc, EXP.vectorArray, e1);
    }

    override bool isLvalue()
    {
        return !rvalue && e1.isLvalue();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * e1 [lwr .. upr]
 *
 * https://dlang.org/spec/expression.html#slice_expressions
 */
extern (C++) final class SliceExp : UnaExp
{
    Expression upr;             // null if implicit 0
    Expression lwr;             // null if implicit [length - 1]

    VarDeclaration lengthVar;

    private extern(D) static struct BitFields
    {
        bool upperIsInBounds;       // true if upr <= e1.length
        bool lowerIsLessThanUpper;  // true if lwr <= upr
        bool arrayop;               // an array operation, rather than a slice
    }
    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, ubyte));

    /************************************************************/
    extern (D) this(const ref Loc loc, Expression e1, IntervalExp ie) @safe
    {
        super(loc, EXP.slice, e1);
        this.upr = ie ? ie.upr : null;
        this.lwr = ie ? ie.lwr : null;
    }

    extern (D) this(const ref Loc loc, Expression e1, Expression lwr, Expression upr) @safe
    {
        super(loc, EXP.slice, e1);
        this.upr = upr;
        this.lwr = lwr;
    }

    override SliceExp syntaxCopy()
    {
        auto se = new SliceExp(loc, e1.syntaxCopy(), lwr ? lwr.syntaxCopy() : null, upr ? upr.syntaxCopy() : null);
        se.lengthVar = this.lengthVar; // bug7871
        return se;
    }

    override bool isLvalue()
    {
        /* slice expression is rvalue in default, but
         * conversion to reference of static array is only allowed.
         */
        return !rvalue && (type && type.toBasetype().ty == Tsarray);
    }

    override Optional!bool toBool()
    {
        return e1.toBool();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `.length` property of an array
 */
extern (C++) final class ArrayLengthExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e1) @safe
    {
        super(loc, EXP.arrayLength, e1);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * e1 [ a0, a1, a2, a3 ,... ]
 *
 * https://dlang.org/spec/expression.html#index_expressions
 */
extern (C++) final class ArrayExp : UnaExp
{
    Expressions* arguments;     // Array of Expression's a0..an

    size_t currentDimension;    // for opDollar
    VarDeclaration lengthVar;

    extern (D) this(const ref Loc loc, Expression e1, Expression index = null)
    {
        super(loc, EXP.array, e1);
        arguments = new Expressions();
        if (index)
            arguments.push(index);
    }

    extern (D) this(const ref Loc loc, Expression e1, Expressions* args) @safe
    {
        super(loc, EXP.array, e1);
        arguments = args;
    }

    override ArrayExp syntaxCopy()
    {
        auto ae = new ArrayExp(loc, e1.syntaxCopy(), arraySyntaxCopy(arguments));
        ae.lengthVar = this.lengthVar; // bug7871
        return ae;
    }

    override bool isLvalue()
    {
        if (rvalue)
            return false;
        if (type && type.toBasetype().ty == Tvoid)
            return false;
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DotExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.dot, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class CommaExp : BinExp
{
    /// This is needed because AssignExp rewrites CommaExp, hence it needs
    /// to trigger the deprecation.
    const bool isGenerated;

    /// Temporary variable to enable / disable deprecation of comma expression
    /// depending on the context.
    /// Since most constructor calls are rewritting, the only place where
    /// false will be passed will be from the parser.
    bool allowCommaExp;


    extern (D) this(const ref Loc loc, Expression e1, Expression e2, bool generated = true) @safe
    {
        super(loc, EXP.comma, e1, e2);
        allowCommaExp = isGenerated = generated;
    }

    override bool isLvalue()
    {
        return !rvalue && e2.isLvalue();
    }

    override Optional!bool toBool()
    {
        return e2.toBool();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /**
     * If the argument is a CommaExp, set a flag to prevent deprecation messages
     *
     * It's impossible to know from CommaExp.semantic if the result will
     * be used, hence when there is a result (type != void), a deprecation
     * message is always emitted.
     * However, some construct can produce a result but won't use it
     * (ExpStatement and for loop increment).  Those should call this function
     * to prevent unwanted deprecations to be emitted.
     *
     * Params:
     *   exp = An expression that discards its result.
     *         If the argument is null or not a CommaExp, nothing happens.
     */
    static void allow(Expression exp) @safe
    {
        if (exp)
            if (auto ce = exp.isCommaExp())
                ce.allowCommaExp = true;
    }
}

/***********************************************************
 * Mainly just a placeholder
 */
extern (C++) final class IntervalExp : Expression
{
    Expression lwr;
    Expression upr;

    extern (D) this(const ref Loc loc, Expression lwr, Expression upr) @safe
    {
        super(loc, EXP.interval);
        this.lwr = lwr;
        this.upr = upr;
    }

    override Expression syntaxCopy()
    {
        return new IntervalExp(loc, lwr.syntaxCopy(), upr.syntaxCopy());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `dg.ptr` property, pointing to the delegate's 'context'
 *
 * c.f.`DelegateFuncptrExp` for the delegate's function pointer `dg.funcptr`
 */
extern (C++) final class DelegatePtrExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e1) @safe
    {
        super(loc, EXP.delegatePointer, e1);
    }

    override bool isLvalue()
    {
        return !rvalue && e1.isLvalue();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `dg.funcptr` property, pointing to the delegate's function
 *
 * c.f.`DelegatePtrExp` for the delegate's function pointer `dg.ptr`
 */
extern (C++) final class DelegateFuncptrExp : UnaExp
{
    extern (D) this(const ref Loc loc, Expression e1) @safe
    {
        super(loc, EXP.delegateFunctionPointer, e1);
    }

    override bool isLvalue()
    {
        return !rvalue && e1.isLvalue();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * e1 [ e2 ]
 */
extern (C++) final class IndexExp : BinExp
{
    VarDeclaration lengthVar;
    bool modifiable = false;    // assume it is an rvalue
    bool indexIsInBounds;       // true if 0 <= e2 && e2 <= e1.length - 1

    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.index, e1, e2);
        //printf("IndexExp::IndexExp('%s')\n", toChars());
    }

    extern (D) this(const ref Loc loc, Expression e1, Expression e2, bool indexIsInBounds) @safe
    {
        super(loc, EXP.index, e1, e2);
        this.indexIsInBounds = indexIsInBounds;
        //printf("IndexExp::IndexExp('%s')\n", toChars());
    }

    override IndexExp syntaxCopy()
    {
        auto ie = new IndexExp(loc, e1.syntaxCopy(), e2.syntaxCopy());
        ie.lengthVar = this.lengthVar; // bug7871
        return ie;
    }

    override bool isLvalue()
    {
        if (rvalue)
            return false;
        auto t1b = e1.type.toBasetype();
        if (t1b.isTypeAArray() || t1b.isTypeSArray() ||
            (e1.isIndexExp() && t1b != t1b.isTypeDArray()))
        {
            return e1.isLvalue();
        }
        return true;
    }

    extern (D) Expression markSettingAAElem()
    {
        if (e1.type.toBasetype().ty == Taarray)
        {
            Type t2b = e2.type.toBasetype();
            if (t2b.ty == Tarray && t2b.nextOf().isMutable())
            {
                error(loc, "associative arrays can only be assigned values with immutable keys, not `%s`", e2.type.toChars());
                return ErrorExp.get();
            }
            modifiable = true;

            if (auto ie = e1.isIndexExp())
            {
                Expression ex = ie.markSettingAAElem();
                if (ex.op == EXP.error)
                    return ex;
                assert(ex == e1);
            }
        }
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The postfix increment/decrement operator, `i++` / `i--`
 */
extern (C++) final class PostExp : BinExp
{
    extern (D) this(EXP op, const ref Loc loc, Expression e)
    {
        super(loc, op, e, IntegerExp.literal!1);
        assert(op == EXP.minusMinus || op == EXP.plusPlus);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The prefix increment/decrement operator, `++i` / `--i`
 */
extern (C++) final class PreExp : UnaExp
{
    extern (D) this(EXP op, const ref Loc loc, Expression e) @safe
    {
        super(loc, op, e);
        assert(op == EXP.preMinusMinus || op == EXP.prePlusPlus);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

enum MemorySet
{
    none            = 0,    // simple assignment
    blockAssign     = 1,    // setting the contents of an array
    referenceInit   = 2,    // setting the reference of STC.ref_ variable
}

/***********************************************************
 * The assignment / initialization operator, `=`
 *
 * Note: operator assignment `op=` has a different base class, `BinAssignExp`
 */
extern (C++) class AssignExp : BinExp
{
    MemorySet memset;

    /************************************************************/
    /* op can be EXP.assign, EXP.construct, or EXP.blit */
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.assign, e1, e2);
    }

    this(const ref Loc loc, EXP tok, Expression e1, Expression e2) @safe
    {
        super(loc, tok, e1, e2);
    }

    override final bool isLvalue()
    {
        // Array-op 'x[] = y[]' should make an rvalue.
        // Setting array length 'x.length = v' should make an rvalue.
        if (e1.op == EXP.slice || e1.op == EXP.arrayLength)
        {
            return false;
        }
        return !rvalue;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * When an assignment expression is lowered to a druntime call
 * this class is used to store the lowering.
 * It essentially behaves the same as an AssignExp, but it is
 * used to not waste space for other AssignExp that are not
 * lowered to anything.
 */
extern (C++) final class LoweredAssignExp : AssignExp
{
    Expression lowering;
    extern (D) this(AssignExp exp, Expression lowering) @safe
    {
        super(exp.loc, EXP.loweredAssignExp, exp.e1, exp.e2);
        this.lowering = lowering;
    }

    override const(char)* toChars() const
    {
        return lowering.toChars();
    }
    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class ConstructExp : AssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.construct, e1, e2);
    }

    // Internal use only. If `v` is a reference variable, the assignment
    // will become a reference initialization automatically.
    extern (D) this(const ref Loc loc, VarDeclaration v, Expression e2) @safe
    {
        auto ve = new VarExp(loc, v);
        assert(v.type && ve.type);

        super(loc, EXP.construct, ve, e2);

        if (v.isReference())
            memset = MemorySet.referenceInit;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A bit-for-bit copy from `e2` to `e1`
 */
extern (C++) final class BlitExp : AssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.blit, e1, e2);
    }

    // Internal use only. If `v` is a reference variable, the assinment
    // will become a reference rebinding automatically.
    extern (D) this(const ref Loc loc, VarDeclaration v, Expression e2) @safe
    {
        auto ve = new VarExp(loc, v);
        assert(v.type && ve.type);

        super(loc, EXP.blit, ve, e2);

        if (v.isReference())
            memset = MemorySet.referenceInit;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x += y`
 */
extern (C++) final class AddAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.addAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x -= y`
 */
extern (C++) final class MinAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.minAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x *= y`
 */
extern (C++) final class MulAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.mulAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x /= y`
 */
extern (C++) final class DivAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.divAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x %= y`
 */
extern (C++) final class ModAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.modAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x &= y`
 */
extern (C++) final class AndAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.andAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x |= y`
 */
extern (C++) final class OrAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.orAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x ^= y`
 */
extern (C++) final class XorAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.xorAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x ^^= y`
 */
extern (C++) final class PowAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.powAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x <<= y`
 */
extern (C++) final class ShlAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.leftShiftAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x >>= y`
 */
extern (C++) final class ShrAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.rightShiftAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `x >>>= y`
 */
extern (C++) final class UshrAssignExp : BinAssignExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.unsignedRightShiftAssign, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `~=` operator.
 *
 * It can have one of the following operators:
 *
 * EXP.concatenateAssign      - appending T[] to T[]
 * EXP.concatenateElemAssign  - appending T to T[]
 * EXP.concatenateDcharAssign - appending dchar to T[]
 *
 * The parser initially sets it to EXP.concatenateAssign, and semantic() later decides which
 * of the three it will be set to.
 */
extern (C++) class CatAssignExp : BinAssignExp
{
    Expression lowering;    // lowered druntime hook `_d_arrayappend{cTX,T}`

    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.concatenateAssign, e1, e2);
    }

    extern (D) this(const ref Loc loc, EXP tok, Expression e1, Expression e2) @safe
    {
        super(loc, tok, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `~=` operator when appending a single element
 */
extern (C++) final class CatElemAssignExp : CatAssignExp
{
    extern (D) this(const ref Loc loc, Type type, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.concatenateElemAssign, e1, e2);
        this.type = type;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `~=` operator when appending a single `dchar`
 */
extern (C++) final class CatDcharAssignExp : CatAssignExp
{
    extern (D) this(const ref Loc loc, Type type, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.concatenateDcharAssign, e1, e2);
        this.type = type;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The addition operator, `x + y`
 *
 * https://dlang.org/spec/expression.html#add_expressions
 */
extern (C++) final class AddExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.add, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The minus operator, `x - y`
 *
 * https://dlang.org/spec/expression.html#add_expressions
 */
extern (C++) final class MinExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.min, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The concatenation operator, `x ~ y`
 *
 * https://dlang.org/spec/expression.html#cat_expressions
 */
extern (C++) final class CatExp : BinExp
{
    Expression lowering;  // call to druntime hook `_d_arraycatnTX`

    extern (D) this(const ref Loc loc, Expression e1, Expression e2) scope @safe
    {
        super(loc, EXP.concatenate, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The multiplication operator, `x * y`
 *
 * https://dlang.org/spec/expression.html#mul_expressions
 */
extern (C++) final class MulExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.mul, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The division operator, `x / y`
 *
 * https://dlang.org/spec/expression.html#mul_expressions
 */
extern (C++) final class DivExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.div, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The modulo operator, `x % y`
 *
 * https://dlang.org/spec/expression.html#mul_expressions
 */
extern (C++) final class ModExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.mod, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The 'power' operator, `x ^^ y`
 *
 * https://dlang.org/spec/expression.html#pow_expressions
 */
extern (C++) final class PowExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.pow, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The 'shift left' operator, `x << y`
 *
 * https://dlang.org/spec/expression.html#shift_expressions
 */
extern (C++) final class ShlExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.leftShift, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The 'shift right' operator, `x >> y`
 *
 * https://dlang.org/spec/expression.html#shift_expressions
 */
extern (C++) final class ShrExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.rightShift, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The 'unsigned shift right' operator, `x >>> y`
 *
 * https://dlang.org/spec/expression.html#shift_expressions
 */
extern (C++) final class UshrExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.unsignedRightShift, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The bitwise 'and' operator, `x & y`
 *
 * https://dlang.org/spec/expression.html#and_expressions
 */
extern (C++) final class AndExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.and, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The bitwise 'or' operator, `x | y`
 *
 * https://dlang.org/spec/expression.html#or_expressions
 */
extern (C++) final class OrExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.or, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The bitwise 'xor' operator, `x ^ y`
 *
 * https://dlang.org/spec/expression.html#xor_expressions
 */
extern (C++) final class XorExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.xor, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The logical 'and' / 'or' operator, `X && Y` / `X || Y`
 *
 * https://dlang.org/spec/expression.html#andand_expressions
 * https://dlang.org/spec/expression.html#oror_expressions
 */
extern (C++) final class LogicalExp : BinExp
{
    extern (D) this(const ref Loc loc, EXP op, Expression e1, Expression e2) @safe
    {
        super(loc, op, e1, e2);
        assert(op == EXP.andAnd || op == EXP.orOr);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A comparison operator, `<` `<=` `>` `>=`
 *
 * `op` is one of:
 *      EXP.lessThan, EXP.lessOrEqual, EXP.greaterThan, EXP.greaterOrEqual
 *
 * https://dlang.org/spec/expression.html#relation_expressions
 */
extern (C++) final class CmpExp : BinExp
{
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, op, e1, e2);
        assert(op == EXP.lessThan || op == EXP.lessOrEqual || op == EXP.greaterThan || op == EXP.greaterOrEqual);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `in` operator, `"a" in ["a": 1]`
 *
 * Note: `x !in y` is rewritten to `!(x in y)` in the parser
 *
 * https://dlang.org/spec/expression.html#in_expressions
 */
extern (C++) final class InExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, EXP.in_, e1, e2);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Associative array removal, `aa.remove(arg)`
 *
 * This deletes the key e1 from the associative array e2
 */
extern (C++) final class RemoveExp : BinExp
{
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.remove, e1, e2);
        type = Type.tbool;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `==` and `!=`
 *
 * EXP.equal and EXP.notEqual
 *
 * https://dlang.org/spec/expression.html#equality_expressions
 */
extern (C++) final class EqualExp : BinExp
{
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, op, e1, e2);
        assert(op == EXP.equal || op == EXP.notEqual);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `is` and `!is`
 *
 * EXP.identity and EXP.notIdentity
 *
 *  https://dlang.org/spec/expression.html#identity_expressions
 */
extern (C++) final class IdentityExp : BinExp
{
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2) @safe
    {
        super(loc, op, e1, e2);
        assert(op == EXP.identity || op == EXP.notIdentity);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The ternary operator, `econd ? e1 : e2`
 *
 * https://dlang.org/spec/expression.html#conditional_expressions
 */
extern (C++) final class CondExp : BinExp
{
    Expression econd;

    extern (D) this(const ref Loc loc, Expression econd, Expression e1, Expression e2) scope @safe
    {
        super(loc, EXP.question, e1, e2);
        this.econd = econd;
    }

    override CondExp syntaxCopy()
    {
        return new CondExp(loc, econd.syntaxCopy(), e1.syntaxCopy(), e2.syntaxCopy());
    }

    override bool isLvalue()
    {
        return !rvalue && e1.isLvalue() && e2.isLvalue();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A special keyword when used as a function's default argument
 *
 * When possible, special keywords are resolved in the parser, but when
 * appearing as a default argument, they result in an expression deriving
 * from this base class that is resolved for each function call.
 *
 * ---
 * const x = __LINE__; // resolved in the parser
 * void foo(string file = __FILE__, int line = __LINE__); // DefaultInitExp
 * ---
 *
 * https://dlang.org/spec/expression.html#specialkeywords
 */
extern (C++) class DefaultInitExp : Expression
{
    /*************************
     * Params:
     *  loc = location
     *  op = EXP.prettyFunction, EXP.functionString, EXP.moduleString,
     *       EXP.line, EXP.file, EXP.fileFullPath
     */
    extern (D) this(const ref Loc loc, EXP op) @safe
    {
        super(loc, op);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `__FILE__` token as a default argument
 */
extern (C++) final class FileInitExp : DefaultInitExp
{
    extern (D) this(const ref Loc loc, EXP tok) @safe
    {
        super(loc, tok);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `__LINE__` token as a default argument
 */
extern (C++) final class LineInitExp : DefaultInitExp
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.line);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `__MODULE__` token as a default argument
 */
extern (C++) final class ModuleInitExp : DefaultInitExp
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.moduleString);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `__FUNCTION__` token as a default argument
 */
extern (C++) final class FuncInitExp : DefaultInitExp
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.functionString);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * The `__PRETTY_FUNCTION__` token as a default argument
 */
extern (C++) final class PrettyFuncInitExp : DefaultInitExp
{
    extern (D) this(const ref Loc loc) @safe
    {
        super(loc, EXP.prettyFunction);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A reference to a class, or an interface. We need this when we
 * point to a base class (we must record what the type is).
 */
extern (C++) final class ClassReferenceExp : Expression
{
    StructLiteralExp value;

    extern (D) this(const ref Loc loc, StructLiteralExp lit, Type type) @safe
    {
        super(loc, EXP.classReference);
        assert(lit && lit.sd && lit.sd.isClassDeclaration());
        this.value = lit;
        this.type = type;
    }

    ClassDeclaration originalClass()
    {
        return value.sd.isClassDeclaration();
    }

    // Return index of the field, or -1 if not found
    int getFieldIndex(Type fieldtype, uint fieldoffset)
    {
        ClassDeclaration cd = originalClass();
        uint fieldsSoFar = 0;
        for (size_t j = 0; j < value.elements.length; j++)
        {
            while (j - fieldsSoFar >= cd.fields.length)
            {
                fieldsSoFar += cd.fields.length;
                cd = cd.baseClass;
            }
            VarDeclaration v2 = cd.fields[j - fieldsSoFar];
            if (fieldoffset == v2.offset && fieldtype.size() == v2.type.size())
            {
                return cast(int)(value.elements.length - fieldsSoFar - cd.fields.length + (j - fieldsSoFar));
            }
        }
        return -1;
    }

    // Return index of the field, or -1 if not found
    // Same as getFieldIndex, but checks for a direct match with the VarDeclaration
    int findFieldIndexByName(VarDeclaration v)
    {
        ClassDeclaration cd = originalClass();
        size_t fieldsSoFar = 0;
        for (size_t j = 0; j < value.elements.length; j++)
        {
            while (j - fieldsSoFar >= cd.fields.length)
            {
                fieldsSoFar += cd.fields.length;
                cd = cd.baseClass;
            }
            VarDeclaration v2 = cd.fields[j - fieldsSoFar];
            if (v == v2)
            {
                return cast(int)(value.elements.length - fieldsSoFar - cd.fields.length + (j - fieldsSoFar));
            }
        }
        return -1;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * This type is only used by the interpreter.
 */
extern (C++) final class CTFEExp : Expression
{
    extern (D) this(EXP tok)
    {
        super(Loc.initial, tok);
        type = Type.tvoid;
    }

    override const(char)* toChars() const
    {
        switch (op)
        {
        case EXP.cantExpression:
            return "<cant>";
        case EXP.voidExpression:
            return "cast(void)0";
        case EXP.showCtfeContext:
            return "<error>";
        case EXP.break_:
            return "<break>";
        case EXP.continue_:
            return "<continue>";
        case EXP.goto_:
            return "<goto>";
        default:
            assert(0);
        }
    }

    extern (D) __gshared CTFEExp cantexp;
    extern (D) __gshared CTFEExp voidexp;
    extern (D) __gshared CTFEExp breakexp;
    extern (D) __gshared CTFEExp continueexp;
    extern (D) __gshared CTFEExp gotoexp;
    /* Used when additional information is needed regarding
     * a ctfe error.
     */
    extern (D) __gshared CTFEExp showcontext;

    extern (D) static bool isCantExp(const Expression e) @safe
    {
        return e && e.op == EXP.cantExpression;
    }

    extern (D) static bool isGotoExp(const Expression e) @safe
    {
        return e && e.op == EXP.goto_;
    }
}

/***********************************************************
 * Fake class which holds the thrown exception.
 * Used for implementing exception handling.
 */
extern (C++) final class ThrownExceptionExp : Expression
{
    ClassReferenceExp thrown;   // the thing being tossed

    extern (D) this(const ref Loc loc, ClassReferenceExp victim) @safe
    {
        super(loc, EXP.thrownException);
        this.thrown = victim;
        this.type = victim.type;
    }

    override const(char)* toChars() const
    {
        return "CTFE ThrownException";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Objective-C class reference expression.
 *
 * Used to get the metaclass of an Objective-C class, `NSObject.Class`.
 */
extern (C++) final class ObjcClassReferenceExp : Expression
{
    ClassDeclaration classDeclaration;

    extern (D) this(const ref Loc loc, ClassDeclaration classDeclaration) @safe
    {
        super(loc, EXP.objcClassReference);
        this.classDeclaration = classDeclaration;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/*******************
 * C11 6.5.1.1 Generic Selection
 * For ImportC
 */
extern (C++) final class GenericExp : Expression
{
    Expression cntlExp; /// controlling expression of a generic selection (not evaluated)
    Types* types;       /// type-names for generic associations (null entry for `default`)
    Expressions* exps;  /// 1:1 mapping of typeNames to exps

    extern (D) this(const ref Loc loc, Expression cntlExp, Types* types, Expressions* exps) @safe
    {
        super(loc, EXP._Generic);
        this.cntlExp = cntlExp;
        this.types = types;
        this.exps = exps;
        assert(types.length == exps.length);  // must be the same and >=1
    }

    override GenericExp syntaxCopy()
    {
        return new GenericExp(loc, cntlExp.syntaxCopy(), Type.arraySyntaxCopy(types), Expression.arraySyntaxCopy(exps));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Verify if the given identifier is _d_array{,set}ctor.
 *
 * Params:
 *  id = the identifier to verify
 *
 * Returns:
 *  `true` if the identifier corresponds to a construction runtime hook,
 *  `false` otherwise.
 */
bool isArrayConstruction(const Identifier id)
{
    import dmd.id : Id;

    return id == Id._d_arrayctor || id == Id._d_arraysetctor;
}

/******************************
 * Provide efficient way to implement isUnaExp(), isBinExp(), isBinAssignExp()
 */
private immutable ubyte[EXP.max + 1] exptab =
() {
    ubyte[EXP.max + 1] tab;
    with (EXPFLAGS)
    {
        foreach (i; Eunary)  { tab[i] |= unary;  }
        foreach (i; Ebinary) { tab[i] |= unary | binary; }
        foreach (i; EbinaryAssign) { tab[i] |= unary | binary | binaryAssign; }
    }
    return tab;
} ();

private enum EXPFLAGS : ubyte
{
    unary = 1,
    binary = 2,
    binaryAssign = 4,
}

private enum Eunary =
    [
        EXP.import_, EXP.assert_, EXP.throw_, EXP.dotIdentifier, EXP.dotTemplateDeclaration,
        EXP.dotVariable, EXP.dotTemplateInstance, EXP.delegate_, EXP.dotType, EXP.call,
        EXP.address, EXP.star, EXP.negate, EXP.uadd, EXP.tilde, EXP.not, EXP.delete_, EXP.cast_,
        EXP.vector, EXP.vectorArray, EXP.slice, EXP.arrayLength, EXP.array, EXP.delegatePointer,
        EXP.delegateFunctionPointer, EXP.preMinusMinus, EXP.prePlusPlus,
    ];

private enum Ebinary =
    [
        EXP.dot, EXP.comma, EXP.index, EXP.minusMinus, EXP.plusPlus, EXP.assign,
        EXP.add, EXP.min, EXP.concatenate, EXP.mul, EXP.div, EXP.mod, EXP.pow, EXP.leftShift,
        EXP.rightShift, EXP.unsignedRightShift, EXP.and, EXP.or, EXP.xor, EXP.andAnd, EXP.orOr,
        EXP.lessThan, EXP.lessOrEqual, EXP.greaterThan, EXP.greaterOrEqual,
        EXP.in_, EXP.remove, EXP.equal, EXP.notEqual, EXP.identity, EXP.notIdentity,
        EXP.question,
        EXP.construct, EXP.blit,
    ];

private enum EbinaryAssign =
    [
        EXP.addAssign, EXP.minAssign, EXP.mulAssign, EXP.divAssign, EXP.modAssign,
        EXP.andAssign, EXP.orAssign, EXP.xorAssign, EXP.powAssign,
        EXP.leftShiftAssign, EXP.rightShiftAssign, EXP.unsignedRightShiftAssign,
        EXP.concatenateAssign, EXP.concatenateElemAssign, EXP.concatenateDcharAssign,
    ];

/// Given a member of the EXP enum, get the class instance size of the corresponding Expression class.
/// Needed because the classes are `extern(C++)`
private immutable ubyte[EXP.max+1] expSize = [
    EXP.reserved: 0,
    EXP.negate: __traits(classInstanceSize, NegExp),
    EXP.cast_: __traits(classInstanceSize, CastExp),
    EXP.null_: __traits(classInstanceSize, NullExp),
    EXP.assert_: __traits(classInstanceSize, AssertExp),
    EXP.array: __traits(classInstanceSize, ArrayExp),
    EXP.call: __traits(classInstanceSize, CallExp),
    EXP.address: __traits(classInstanceSize, AddrExp),
    EXP.type: __traits(classInstanceSize, TypeExp),
    EXP.throw_: __traits(classInstanceSize, ThrowExp),
    EXP.new_: __traits(classInstanceSize, NewExp),
    EXP.delete_: __traits(classInstanceSize, DeleteExp),
    EXP.star: __traits(classInstanceSize, PtrExp),
    EXP.symbolOffset: __traits(classInstanceSize, SymOffExp),
    EXP.variable: __traits(classInstanceSize, VarExp),
    EXP.dotVariable: __traits(classInstanceSize, DotVarExp),
    EXP.dotIdentifier: __traits(classInstanceSize, DotIdExp),
    EXP.dotTemplateInstance: __traits(classInstanceSize, DotTemplateInstanceExp),
    EXP.dotType: __traits(classInstanceSize, DotTypeExp),
    EXP.slice: __traits(classInstanceSize, SliceExp),
    EXP.arrayLength: __traits(classInstanceSize, ArrayLengthExp),
    EXP.dollar: __traits(classInstanceSize, DollarExp),
    EXP.template_: __traits(classInstanceSize, TemplateExp),
    EXP.dotTemplateDeclaration: __traits(classInstanceSize, DotTemplateExp),
    EXP.declaration: __traits(classInstanceSize, DeclarationExp),
    EXP.dSymbol: __traits(classInstanceSize, DsymbolExp),
    EXP.typeid_: __traits(classInstanceSize, TypeidExp),
    EXP.uadd: __traits(classInstanceSize, UAddExp),
    EXP.remove: __traits(classInstanceSize, RemoveExp),
    EXP.newAnonymousClass: __traits(classInstanceSize, NewAnonClassExp),
    EXP.arrayLiteral: __traits(classInstanceSize, ArrayLiteralExp),
    EXP.assocArrayLiteral: __traits(classInstanceSize, AssocArrayLiteralExp),
    EXP.structLiteral: __traits(classInstanceSize, StructLiteralExp),
    EXP.classReference: __traits(classInstanceSize, ClassReferenceExp),
    EXP.thrownException: __traits(classInstanceSize, ThrownExceptionExp),
    EXP.delegatePointer: __traits(classInstanceSize, DelegatePtrExp),
    EXP.delegateFunctionPointer: __traits(classInstanceSize, DelegateFuncptrExp),
    EXP.lessThan: __traits(classInstanceSize, CmpExp),
    EXP.greaterThan: __traits(classInstanceSize, CmpExp),
    EXP.lessOrEqual: __traits(classInstanceSize, CmpExp),
    EXP.greaterOrEqual: __traits(classInstanceSize, CmpExp),
    EXP.equal: __traits(classInstanceSize, EqualExp),
    EXP.notEqual: __traits(classInstanceSize, EqualExp),
    EXP.identity: __traits(classInstanceSize, IdentityExp),
    EXP.notIdentity: __traits(classInstanceSize, IdentityExp),
    EXP.index: __traits(classInstanceSize, IndexExp),
    EXP.is_: __traits(classInstanceSize, IsExp),
    EXP.leftShift: __traits(classInstanceSize, ShlExp),
    EXP.rightShift: __traits(classInstanceSize, ShrExp),
    EXP.leftShiftAssign: __traits(classInstanceSize, ShlAssignExp),
    EXP.rightShiftAssign: __traits(classInstanceSize, ShrAssignExp),
    EXP.unsignedRightShift: __traits(classInstanceSize, UshrExp),
    EXP.unsignedRightShiftAssign: __traits(classInstanceSize, UshrAssignExp),
    EXP.concatenate: __traits(classInstanceSize, CatExp),
    EXP.concatenateAssign: __traits(classInstanceSize, CatAssignExp),
    EXP.concatenateElemAssign: __traits(classInstanceSize, CatElemAssignExp),
    EXP.concatenateDcharAssign: __traits(classInstanceSize, CatDcharAssignExp),
    EXP.add: __traits(classInstanceSize, AddExp),
    EXP.min: __traits(classInstanceSize, MinExp),
    EXP.addAssign: __traits(classInstanceSize, AddAssignExp),
    EXP.minAssign: __traits(classInstanceSize, MinAssignExp),
    EXP.mul: __traits(classInstanceSize, MulExp),
    EXP.div: __traits(classInstanceSize, DivExp),
    EXP.mod: __traits(classInstanceSize, ModExp),
    EXP.mulAssign: __traits(classInstanceSize, MulAssignExp),
    EXP.divAssign: __traits(classInstanceSize, DivAssignExp),
    EXP.modAssign: __traits(classInstanceSize, ModAssignExp),
    EXP.and: __traits(classInstanceSize, AndExp),
    EXP.or: __traits(classInstanceSize, OrExp),
    EXP.xor: __traits(classInstanceSize, XorExp),
    EXP.andAssign: __traits(classInstanceSize, AndAssignExp),
    EXP.orAssign: __traits(classInstanceSize, OrAssignExp),
    EXP.xorAssign: __traits(classInstanceSize, XorAssignExp),
    EXP.assign: __traits(classInstanceSize, AssignExp),
    EXP.not: __traits(classInstanceSize, NotExp),
    EXP.tilde: __traits(classInstanceSize, ComExp),
    EXP.plusPlus: __traits(classInstanceSize, PostExp),
    EXP.minusMinus: __traits(classInstanceSize, PostExp),
    EXP.construct: __traits(classInstanceSize, ConstructExp),
    EXP.blit: __traits(classInstanceSize, BlitExp),
    EXP.dot: __traits(classInstanceSize, DotExp),
    EXP.comma: __traits(classInstanceSize, CommaExp),
    EXP.question: __traits(classInstanceSize, CondExp),
    EXP.andAnd: __traits(classInstanceSize, LogicalExp),
    EXP.orOr: __traits(classInstanceSize, LogicalExp),
    EXP.prePlusPlus: __traits(classInstanceSize, PreExp),
    EXP.preMinusMinus: __traits(classInstanceSize, PreExp),
    EXP.identifier: __traits(classInstanceSize, IdentifierExp),
    EXP.string_: __traits(classInstanceSize, StringExp),
    EXP.interpolated: __traits(classInstanceSize, InterpExp),
    EXP.this_: __traits(classInstanceSize, ThisExp),
    EXP.super_: __traits(classInstanceSize, SuperExp),
    EXP.halt: __traits(classInstanceSize, HaltExp),
    EXP.tuple: __traits(classInstanceSize, TupleExp),
    EXP.error: __traits(classInstanceSize, ErrorExp),
    EXP.void_: __traits(classInstanceSize, VoidInitExp),
    EXP.int64: __traits(classInstanceSize, IntegerExp),
    EXP.float64: __traits(classInstanceSize, RealExp),
    EXP.complex80: __traits(classInstanceSize, ComplexExp),
    EXP.import_: __traits(classInstanceSize, ImportExp),
    EXP.delegate_: __traits(classInstanceSize, DelegateExp),
    EXP.function_: __traits(classInstanceSize, FuncExp),
    EXP.mixin_: __traits(classInstanceSize, MixinExp),
    EXP.in_: __traits(classInstanceSize, InExp),
    EXP.break_: __traits(classInstanceSize, CTFEExp),
    EXP.continue_: __traits(classInstanceSize, CTFEExp),
    EXP.goto_: __traits(classInstanceSize, CTFEExp),
    EXP.scope_: __traits(classInstanceSize, ScopeExp),
    EXP.traits: __traits(classInstanceSize, TraitsExp),
    EXP.overloadSet: __traits(classInstanceSize, OverExp),
    EXP.line: __traits(classInstanceSize, LineInitExp),
    EXP.file: __traits(classInstanceSize, FileInitExp),
    EXP.fileFullPath: __traits(classInstanceSize, FileInitExp),
    EXP.moduleString: __traits(classInstanceSize, ModuleInitExp),
    EXP.functionString: __traits(classInstanceSize, FuncInitExp),
    EXP.prettyFunction: __traits(classInstanceSize, PrettyFuncInitExp),
    EXP.pow: __traits(classInstanceSize, PowExp),
    EXP.powAssign: __traits(classInstanceSize, PowAssignExp),
    EXP.vector: __traits(classInstanceSize, VectorExp),
    EXP.voidExpression: __traits(classInstanceSize, CTFEExp),
    EXP.cantExpression: __traits(classInstanceSize, CTFEExp),
    EXP.showCtfeContext: __traits(classInstanceSize, CTFEExp),
    EXP.objcClassReference: __traits(classInstanceSize, ObjcClassReferenceExp),
    EXP.vectorArray: __traits(classInstanceSize, VectorArrayExp),
    EXP.compoundLiteral: __traits(classInstanceSize, CompoundLiteralExp),
    EXP._Generic: __traits(classInstanceSize, GenericExp),
    EXP.interval: __traits(classInstanceSize, IntervalExp),
    EXP.loweredAssignExp : __traits(classInstanceSize, LoweredAssignExp),
];
