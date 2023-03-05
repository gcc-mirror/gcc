/**
 * Defines the bulk of the classes which represent the AST at the expression level.
 *
 * Specification: ($LINK2 https://dlang.org/spec/expression.html, Expressions)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
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
import dmd.aliasthis;
import dmd.apply;
import dmd.arrayop;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.gluelayer;
import dmd.constfold;
import dmd.ctfeexpr;
import dmd.ctorflow;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.delegatize;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.inline;
import dmd.location;
import dmd.mtype;
import dmd.nspace;
import dmd.objc;
import dmd.opover;
import dmd.optimize;
import dmd.root.complex;
import dmd.root.ctfloat;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.optional;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.root.string;
import dmd.root.utf;
import dmd.safe;
import dmd.sideeffect;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

enum LOGSEMANTIC = false;

void emplaceExp(T : Expression, Args...)(void* p, Args args)
{
    static if (__VERSION__ < 2099)
        const init = typeid(T).initializer;
    else
        const init = __traits(initSymbol, T);
    p[0 .. __traits(classInstanceSize, T)] = init[];
    (cast(T)p).__ctor(args);
}

void emplaceExp(T : UnionExp)(T* p, Expression e)
{
    memcpy(p, cast(void*)e, e.size);
}

/// Return value for `checkModifiable`
enum Modifiable
{
    /// Not modifiable
    no,
    /// Modifiable (the type is mutable)
    yes,
    /// Modifiable because it is initialization
    initialization,
}
/**
 * Specifies how the checkModify deals with certain situations
 */
enum ModifyFlags
{
    /// Issue error messages on invalid modifications of the variable
    none,
    /// No errors are emitted for invalid modifications
    noError = 0x1,
    /// The modification occurs for a subfield of the current variable
    fieldAssign = 0x2,
}

/****************************************
 * Find the first non-comma expression.
 * Params:
 *      e = Expressions connected by commas
 * Returns:
 *      left-most non-comma expression
 */
inout(Expression) firstComma(inout Expression e)
{
    Expression ex = cast()e;
    while (ex.op == EXP.comma)
        ex = (cast(CommaExp)ex).e1;
    return cast(inout)ex;

}

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

/*****************************************
 * Determine if `this` is available by walking up the enclosing
 * scopes until a function is found.
 *
 * Params:
 *      sc = where to start looking for the enclosing function
 * Returns:
 *      Found function if it satisfies `isThis()`, otherwise `null`
 */
FuncDeclaration hasThis(Scope* sc)
{
    //printf("hasThis()\n");
    Dsymbol p = sc.parent;
    while (p && p.isTemplateMixin())
        p = p.parent;
    FuncDeclaration fdthis = p ? p.isFuncDeclaration() : null;
    //printf("fdthis = %p, '%s'\n", fdthis, fdthis ? fdthis.toChars() : "");

    // Go upwards until we find the enclosing member function
    FuncDeclaration fd = fdthis;
    while (1)
    {
        if (!fd)
        {
            return null;
        }
        if (!fd.isNested() || fd.isThis() || (fd.hasDualContext() && fd.isMember2()))
            break;

        Dsymbol parent = fd.parent;
        while (1)
        {
            if (!parent)
                return null;
            TemplateInstance ti = parent.isTemplateInstance();
            if (ti)
                parent = ti.parent;
            else
                break;
        }
        fd = parent.isFuncDeclaration();
    }

    if (!fd.isThis() && !(fd.hasDualContext() && fd.isMember2()))
    {
        return null;
    }

    assert(fd.vthis);
    return fd;

}

/***********************************
 * Determine if a `this` is needed to access `d`.
 * Params:
 *      sc = context
 *      d = declaration to check
 * Returns:
 *      true means a `this` is needed
 */
bool isNeedThisScope(Scope* sc, Declaration d)
{
    if (sc.intypeof == 1)
        return false;

    AggregateDeclaration ad = d.isThis();
    if (!ad)
        return false;
    //printf("d = %s, ad = %s\n", d.toChars(), ad.toChars());

    for (Dsymbol s = sc.parent; s; s = s.toParentLocal())
    {
        //printf("\ts = %s %s, toParent2() = %p\n", s.kind(), s.toChars(), s.toParent2());
        if (AggregateDeclaration ad2 = s.isAggregateDeclaration())
        {
            if (ad2 == ad)
                return false;
            else if (ad2.isNested())
                continue;
            else
                return true;
        }
        if (FuncDeclaration f = s.isFuncDeclaration())
        {
            if (f.isMemberLocal())
                break;
        }
    }
    return true;
}

/******************************
 * check e is exp.opDispatch!(tiargs) or not
 * It's used to switch to UFCS the semantic analysis path
 */
bool isDotOpDispatch(Expression e)
{
    if (auto dtie = e.isDotTemplateInstanceExp())
        return dtie.ti.name == Id.opDispatch;
    return false;
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
extern (C++) void expandTuples(Expressions* exps, Identifiers* names = null)
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
 * Expand alias this tuples.
 */
TupleDeclaration isAliasThisTuple(Expression e)
{
    if (!e.type)
        return null;

    Type t = e.type.toBasetype();
    while (true)
    {
        if (Dsymbol s = t.toDsymbol(null))
        {
            if (auto ad = s.isAggregateDeclaration())
            {
                s = ad.aliasthis ? ad.aliasthis.sym : null;
                if (s && s.isVarDeclaration())
                {
                    TupleDeclaration td = s.isVarDeclaration().toAlias().isTupleDeclaration();
                    if (td && td.isexp)
                        return td;
                }
                if (Type att = t.aliasthisOf())
                {
                    t = att;
                    continue;
                }
            }
        }
        return null;
    }
}

int expandAliasThisTuples(Expressions* exps, size_t starti = 0)
{
    if (!exps || exps.length == 0)
        return -1;

    for (size_t u = starti; u < exps.length; u++)
    {
        Expression exp = (*exps)[u];
        if (TupleDeclaration td = exp.isAliasThisTuple)
        {
            exps.remove(u);
            size_t i;
            td.foreachVar((s)
            {
                auto d = s.isDeclaration();
                auto e = new DotVarExp(exp.loc, exp, d);
                assert(d.type);
                e.type = d.type;
                exps.insert(u + i, e);
                ++i;
            });
            version (none)
            {
                printf("expansion ->\n");
                foreach (e; exps)
                {
                    printf("\texps[%d] e = %s %s\n", i, EXPtoString(e.op), e.toChars());
                }
            }
            return cast(int)u;
        }
    }
    return -1;
}

/****************************************
 * If `s` is a function template, i.e. the only member of a template
 * and that member is a function, return that template.
 * Params:
 *      s = symbol that might be a function template
 * Returns:
 *      template for that function, otherwise null
 */
TemplateDeclaration getFuncTemplateDecl(Dsymbol s)
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

/************************************************
 * If we want the value of this expression, but do not want to call
 * the destructor on it.
 */
Expression valueNoDtor(Expression e)
{
    auto ex = lastComma(e);

    if (auto ce = ex.isCallExp())
    {
        /* The struct value returned from the function is transferred
         * so do not call the destructor on it.
         * Recognize:
         *       ((S _ctmp = S.init), _ctmp).this(...)
         * and make sure the destructor is not called on _ctmp
         * BUG: if ex is a CommaExp, we should go down the right side.
         */
        if (auto dve = ce.e1.isDotVarExp())
        {
            if (dve.var.isCtorDeclaration())
            {
                // It's a constructor call
                if (auto comma = dve.e1.isCommaExp())
                {
                    if (auto ve = comma.e2.isVarExp())
                    {
                        VarDeclaration ctmp = ve.var.isVarDeclaration();
                        if (ctmp)
                        {
                            ctmp.storage_class |= STC.nodtor;
                            assert(!ce.isLvalue());
                        }
                    }
                }
            }
        }
    }
    else if (auto ve = ex.isVarExp())
    {
        auto vtmp = ve.var.isVarDeclaration();
        if (vtmp && (vtmp.storage_class & STC.rvalue))
        {
            vtmp.storage_class |= STC.nodtor;
        }
    }
    return e;
}

/*********************************************
 * If e is an instance of a struct, and that struct has a copy constructor,
 * rewrite e as:
 *    (tmp = e),tmp
 * Input:
 *      sc = just used to specify the scope of created temporary variable
 *      destinationType = the type of the object on which the copy constructor is called;
 *                        may be null if the struct defines a postblit
 */
private Expression callCpCtor(Scope* sc, Expression e, Type destinationType)
{
    if (auto ts = e.type.baseElemOf().isTypeStruct())
    {
        StructDeclaration sd = ts.sym;
        if (sd.postblit || sd.hasCopyCtor)
        {
            /* Create a variable tmp, and replace the argument e with:
             *      (tmp = e),tmp
             * and let AssignExp() handle the construction.
             * This is not the most efficient, ideally tmp would be constructed
             * directly onto the stack.
             */
            auto tmp = copyToTemp(STC.rvalue, "__copytmp", e);
            if (sd.hasCopyCtor && destinationType)
            {
                // https://issues.dlang.org/show_bug.cgi?id=22619
                // If the destination type is inout we can preserve it
                // only if inside an inout function; if we are not inside
                // an inout function, then we will preserve the type of
                // the source
                if (destinationType.hasWild && !(sc.func.storage_class & STC.wild))
                    tmp.type = e.type;
                else
                    tmp.type = destinationType;
            }
            tmp.storage_class |= STC.nodtor;
            tmp.dsymbolSemantic(sc);
            Expression de = new DeclarationExp(e.loc, tmp);
            Expression ve = new VarExp(e.loc, tmp);
            de.type = Type.tvoid;
            ve.type = e.type;
            return Expression.combine(de, ve);
        }
    }
    return e;
}

/************************************************
 * Handle the postblit call on lvalue, or the move of rvalue.
 *
 * Params:
 *   sc = the scope where the expression is encountered
 *   e = the expression the needs to be moved or copied (source)
 *   t = if the struct defines a copy constructor, the type of the destination
 *
 * Returns:
 *  The expression that copy constructs or moves the value.
 */
extern (D) Expression doCopyOrMove(Scope *sc, Expression e, Type t = null)
{
    if (auto ce = e.isCondExp())
    {
        ce.e1 = doCopyOrMove(sc, ce.e1);
        ce.e2 = doCopyOrMove(sc, ce.e2);
    }
    else
    {
        e = e.isLvalue() ? callCpCtor(sc, e, t) : valueNoDtor(e);
    }
    return e;
}

/****************************************************************/
/* A type meant as a union of all the Expression types,
 * to serve essentially as a Variant that will sit on the stack
 * during CTFE to reduce memory consumption.
 */
extern (C++) struct UnionExp
{
    // yes, default constructor does nothing
    extern (D) this(Expression e)
    {
        memcpy(&this, cast(void*)e, e.size);
    }

    /* Extract pointer to Expression
     */
    extern (C++) Expression exp() return
    {
        return cast(Expression)&u;
    }

    /* Convert to an allocated Expression
     */
    extern (C++) Expression copy()
    {
        Expression e = exp();
        //if (e.size > sizeof(u)) printf("%s\n", EXPtoString(e.op).ptr);
        assert(e.size <= u.sizeof);
        switch (e.op)
        {
            case EXP.cantExpression:    return CTFEExp.cantexp;
            case EXP.voidExpression:    return CTFEExp.voidexp;
            case EXP.break_:            return CTFEExp.breakexp;
            case EXP.continue_:         return CTFEExp.continueexp;
            case EXP.goto_:             return CTFEExp.gotoexp;
            default:                    return e.copy();
        }
    }

private:
    // Ensure that the union is suitably aligned.
    align(8) union __AnonStruct__u
    {
        char[__traits(classInstanceSize, Expression)] exp;
        char[__traits(classInstanceSize, IntegerExp)] integerexp;
        char[__traits(classInstanceSize, ErrorExp)] errorexp;
        char[__traits(classInstanceSize, RealExp)] realexp;
        char[__traits(classInstanceSize, ComplexExp)] complexexp;
        char[__traits(classInstanceSize, SymOffExp)] symoffexp;
        char[__traits(classInstanceSize, StringExp)] stringexp;
        char[__traits(classInstanceSize, ArrayLiteralExp)] arrayliteralexp;
        char[__traits(classInstanceSize, AssocArrayLiteralExp)] assocarrayliteralexp;
        char[__traits(classInstanceSize, StructLiteralExp)] structliteralexp;
        char[__traits(classInstanceSize, CompoundLiteralExp)] compoundliteralexp;
        char[__traits(classInstanceSize, NullExp)] nullexp;
        char[__traits(classInstanceSize, DotVarExp)] dotvarexp;
        char[__traits(classInstanceSize, AddrExp)] addrexp;
        char[__traits(classInstanceSize, IndexExp)] indexexp;
        char[__traits(classInstanceSize, SliceExp)] sliceexp;
        char[__traits(classInstanceSize, VectorExp)] vectorexp;
    }

    __AnonStruct__u u;
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
bool RealIdentical(real_t x1, real_t x2)
{
    return (CTFloat.isNaN(x1) && CTFloat.isNaN(x2)) || CTFloat.isIdentical(x1, x2);
}

/************************ TypeDotIdExp ************************************/
/* Things like:
 *      int.size
 *      foo.size
 *      (foo).size
 *      cast(foo).size
 */
DotIdExp typeDotIdExp(const ref Loc loc, Type type, Identifier ident)
{
    return new DotIdExp(loc, new TypeExp(loc, type), ident);
}

/***************************************************
 * Given an Expression, find the variable it really is.
 *
 * For example, `a[index]` is really `a`, and `s.f` is really `s`.
 * Params:
 *      e = Expression to look at
 * Returns:
 *      variable if there is one, null if not
 */
VarDeclaration expToVariable(Expression e)
{
    while (1)
    {
        switch (e.op)
        {
            case EXP.variable:
                return (cast(VarExp)e).var.isVarDeclaration();

            case EXP.dotVariable:
                e = (cast(DotVarExp)e).e1;
                continue;

            case EXP.index:
            {
                IndexExp ei = cast(IndexExp)e;
                e = ei.e1;
                Type ti = e.type.toBasetype();
                if (ti.ty == Tsarray)
                    continue;
                return null;
            }

            case EXP.slice:
            {
                SliceExp ei = cast(SliceExp)e;
                e = ei.e1;
                Type ti = e.type.toBasetype();
                if (ti.ty == Tsarray)
                    continue;
                return null;
            }

            case EXP.this_:
            case EXP.super_:
                return (cast(ThisExp)e).var.isVarDeclaration();

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
    const EXP op;   // to minimize use of dynamic_cast
    ubyte size;     // # of bytes in Expression so we can copy() it
    bool parens;    // if this is a parenthesized expression
    Type type;      // !=null means that semantic() has been run
    Loc loc;        // file location

    extern (D) this(const ref Loc loc, EXP op, int size) scope
    {
        //printf("Expression::Expression(op = %d) this = %p\n", op, this);
        this.loc = loc;
        this.op = op;
        this.size = cast(ubyte)size;
    }

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
    final Expression copy()
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
        OutBuffer buf;
        HdrGenState hgs;
        toCBuffer(this, &buf, &hgs);
        return buf.extractChars();
    }

    static if (__VERSION__ < 2092)
    {
        final void error(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .verror(loc, format, ap);
                va_end(ap);
            }
        }

        final void errorSupplemental(const(char)* format, ...)
        {
            if (type == Type.terror)
                return;

            va_list ap;
            va_start(ap, format);
            .verrorSupplemental(loc, format, ap);
            va_end(ap);
        }

        final void warning(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .vwarning(loc, format, ap);
                va_end(ap);
            }
        }

        final void deprecation(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .vdeprecation(loc, format, ap);
                va_end(ap);
            }
        }
    }
    else
    {
        pragma(printf) final void error(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .verror(loc, format, ap);
                va_end(ap);
            }
        }

        pragma(printf) final void errorSupplemental(const(char)* format, ...)
        {
            if (type == Type.terror)
                return;

            va_list ap;
            va_start(ap, format);
            .verrorSupplemental(loc, format, ap);
            va_end(ap);
        }

        pragma(printf) final void warning(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .vwarning(loc, format, ap);
                va_end(ap);
            }
        }

        pragma(printf) final void deprecation(const(char)* format, ...) const
        {
            if (type != Type.terror)
            {
                va_list ap;
                va_start(ap, format);
                .vdeprecation(loc, format, ap);
                va_end(ap);
            }
        }
    }

    /**********************************
     * Combine e1 and e2 by CommaExp if both are not NULL.
     */
    extern (D) static Expression combine(Expression e1, Expression e2)
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

    extern (D) static Expression combine(Expression e1, Expression e2, Expression e3)
    {
        return combine(combine(e1, e2), e3);
    }

    extern (D) static Expression combine(Expression e1, Expression e2, Expression e3, Expression e4)
    {
        return combine(combine(e1, e2), combine(e3, e4));
    }

    /**********************************
     * If 'e' is a tree of commas, returns the rightmost expression
     * by stripping off it from the tree. The remained part of the tree
     * is returned via e0.
     * Otherwise 'e' is directly returned and e0 is set to NULL.
     */
    extern (D) static Expression extractLast(Expression e, out Expression e0)
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
        error("integer constant expression expected instead of `%s`", toChars());
        return 0;
    }

    uinteger_t toUInteger()
    {
        //printf("Expression %s\n", EXPtoString(op).ptr);
        return cast(uinteger_t)toInteger();
    }

    real_t toReal()
    {
        error("floating point constant expression expected instead of `%s`", toChars());
        return CTFloat.zero;
    }

    real_t toImaginary()
    {
        error("floating point constant expression expected instead of `%s`", toChars());
        return CTFloat.zero;
    }

    complex_t toComplex()
    {
        error("floating point constant expression expected instead of `%s`", toChars());
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

    /*******************************
     * Give error if we're not an lvalue.
     * If we can, convert expression to be an lvalue.
     */
    Expression toLvalue(Scope* sc, Expression e)
    {
        if (!e)
            e = this;
        else if (!loc.isValid())
            loc = e.loc;

        if (e.op == EXP.type)
            error("`%s` is a `%s` definition and cannot be modified", e.type.toChars(), e.type.kind());
        else
            error("`%s` is not an lvalue and cannot be modified", e.toChars());

        return ErrorExp.get();
    }

    Expression modifiableLvalue(Scope* sc, Expression e)
    {
        //printf("Expression::modifiableLvalue() %s, type = %s\n", toChars(), type.toChars());
        // See if this expression is a modifiable lvalue (i.e. not const)
        if (checkModifiable(this, sc) == Modifiable.yes)
        {
            assert(type);
            if (!type.isMutable())
            {
                if (auto dve = this.isDotVarExp())
                {
                    if (isNeedThisScope(sc, dve.var))
                        for (Dsymbol s = sc.func; s; s = s.toParentLocal())
                    {
                        FuncDeclaration ff = s.isFuncDeclaration();
                        if (!ff)
                            break;
                        if (!ff.type.isMutable)
                        {
                            error("cannot modify `%s` in `%s` function", toChars(), MODtoChars(type.mod));
                            return ErrorExp.get();
                        }
                    }
                }
                error("cannot modify `%s` expression `%s`", MODtoChars(type.mod), toChars());
                return ErrorExp.get();
            }
            else if (!type.isAssignable())
            {
                error("cannot modify struct instance `%s` of type `%s` because it contains `const` or `immutable` members",
                    toChars(), type.toChars());
                return ErrorExp.get();
            }
        }
        return toLvalue(sc, e);
    }

    final Expression implicitCastTo(Scope* sc, Type t)
    {
        return .implicitCastTo(this, sc, t);
    }

    final MATCH implicitConvTo(Type t)
    {
        return .implicitConvTo(this, t);
    }

    final Expression castTo(Scope* sc, Type t)
    {
        return .castTo(this, sc, t);
    }

    /****************************************
     * Resolve __FILE__, __LINE__, __MODULE__, __FUNCTION__, __PRETTY_FUNCTION__, __FILE_FULL_PATH__ to loc.
     */
    Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        this.loc = loc;
        return this;
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
            error("expression `%s` is `void` and has no value", toChars());
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
        if (!type.isscalar())
        {
            error("`%s` is not a scalar, it is a `%s`", toChars(), type.toChars());
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
            error("operation not allowed on `bool` `%s`", toChars());
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
        if (!type.isintegral())
        {
            error("`%s` is not of integral type, it is a `%s`", toChars(), type.toChars());
            return true;
        }
        return checkValue();
    }

    extern (D) final bool checkArithmetic()
    {
        if (op == EXP.error)
            return true;
        if (type.toBasetype().ty == Terror)
            return true;
        if (!type.isintegral() && !type.isfloating())
        {
            error("`%s` is not of arithmetic type, it is a `%s`", toChars(), type.toChars());
            return true;
        }
        return checkValue();
    }

    final bool checkDeprecated(Scope* sc, Dsymbol s)
    {
        return s.checkDeprecated(loc, sc);
    }

    extern (D) final bool checkDisabled(Scope* sc, Dsymbol s)
    {
        if (auto d = s.isDeclaration())
        {
            return d.checkDisabled(loc, sc);
        }

        return false;
    }

    /*********************************************
     * Calling function f.
     * Check the purity, i.e. if we're in a pure function
     * we can only call other pure functions.
     * Returns true if error occurs.
     */
    extern (D) final bool checkPurity(Scope* sc, FuncDeclaration f)
    {
        if (!sc.func)
            return false;
        if (sc.func == f)
            return false;
        if (sc.intypeof == 1)
            return false;
        if (sc.flags & (SCOPE.ctfe | SCOPE.debug_))
            return false;

        // If the call has a pure parent, then the called func must be pure.
        if (!f.isPure() && checkImpure(sc))
        {
            error("`pure` %s `%s` cannot call impure %s `%s`",
                sc.func.kind(), sc.func.toPrettyChars(), f.kind(),
                f.toPrettyChars());

            checkOverridenDtor(sc, f, dd => dd.type.toTypeFunction().purity != PURE.impure, "impure");
            return true;
        }
        return false;
    }

    /**
     * Checks whether `f` is a generated `DtorDeclaration` that hides a user-defined one
     * which passes `check` while `f` doesn't (e.g. when the user defined dtor is pure but
     * the generated dtor is not).
     * In that case the method will identify and print all members causing the attribute
     * missmatch.
     *
     * Params:
     *   sc = scope
     *   f  = potential `DtorDeclaration`
     *   check = current check (e.g. whether it's pure)
     *   checkName = the kind of check (e.g. `"pure"`)
     */
    extern (D) final void checkOverridenDtor(Scope* sc, FuncDeclaration f,
                scope bool function(DtorDeclaration) check, const string checkName
    ) {
        auto dd = f.isDtorDeclaration();
        if (!dd || !dd.isGenerated())
            return;

        // DtorDeclaration without parents should fail at an earlier stage
        auto ad = cast(AggregateDeclaration) f.toParent2();
        assert(ad);

        if (ad.userDtors.length)
        {
            if (!check(ad.userDtors[0])) // doesn't match check (e.g. is impure as well)
                return;

            // Sanity check
            assert(!check(ad.fieldDtor));
        }

        dd.loc.errorSupplemental("%s`%s.~this` is %.*s because of the following field's destructors:",
                            dd.isGenerated() ? "generated " : "".ptr,
                            ad.toChars,
                            cast(int) checkName.length, checkName.ptr);

        // Search for the offending fields
        foreach (field; ad.fields)
        {
            // Only structs may define automatically called destructors
            auto ts = field.type.isTypeStruct();
            if (!ts)
            {
                // But they might be part of a static array
                auto ta = field.type.isTypeSArray();
                if (!ta)
                    continue;

                ts = ta.baseElemOf().isTypeStruct();
                if (!ts)
                    continue;
            }

            auto fieldSym = ts.toDsymbol(sc);
            assert(fieldSym); // Resolving ts must succeed because missing defs. should error before

            auto fieldSd = fieldSym.isStructDeclaration();
            assert(fieldSd); // ts is a TypeStruct, this would imply a malformed ASR

            if (fieldSd.dtor && !check(fieldSd.dtor))
            {
                field.loc.errorSupplemental(" - %s %s", field.type.toChars(), field.toChars());

                if (fieldSd.dtor.isGenerated())
                    checkOverridenDtor(sc, fieldSd.dtor, check, checkName);
                else
                    fieldSd.dtor.loc.errorSupplemental("   %.*s `%s.~this` is declared here",
                                            cast(int) checkName.length, checkName.ptr, fieldSd.toChars());
            }
        }
    }

    /*******************************************
     * Accessing variable v.
     * Check for purity and safety violations.
     * Returns true if error occurs.
     */
    extern (D) final bool checkPurity(Scope* sc, VarDeclaration v)
    {
        //printf("v = %s %s\n", v.type.toChars(), v.toChars());
        /* Look for purity and safety violations when accessing variable v
         * from current function.
         */
        if (!sc.func)
            return false;
        if (sc.intypeof == 1)
            return false; // allow violations inside typeof(expression)
        if (sc.flags & (SCOPE.ctfe | SCOPE.debug_))
            return false; // allow violations inside compile-time evaluated expressions and debug conditionals
        if (v.ident == Id.ctfe)
            return false; // magic variable never violates pure and safe
        if (v.isImmutable())
            return false; // always safe and pure to access immutables...
        if (v.isConst() && !v.isReference() && (v.isDataseg() || v.isParameter()) && v.type.implicitConvTo(v.type.immutableOf()))
            return false; // or const global/parameter values which have no mutable indirections
        if (v.storage_class & STC.manifest)
            return false; // ...or manifest constants

        // accessing empty structs is pure
        // https://issues.dlang.org/show_bug.cgi?id=18694
        // https://issues.dlang.org/show_bug.cgi?id=21464
        // https://issues.dlang.org/show_bug.cgi?id=23589
        if (v.type.ty == Tstruct)
        {
            StructDeclaration sd = (cast(TypeStruct)v.type).sym;
            if (sd.members) // not opaque
            {
                if (sd.semanticRun >= PASS.semanticdone)
                    sd.determineSize(v.loc);
                if (sd.hasNoFields)
                    return false;
            }
        }

        bool err = false;
        if (v.isDataseg())
        {
            // https://issues.dlang.org/show_bug.cgi?id=7533
            // Accessing implicit generated __gate is pure.
            if (v.ident == Id.gate)
                return false;

            if (checkImpure(sc))
            {
                error("`pure` %s `%s` cannot access mutable static data `%s`",
                    sc.func.kind(), sc.func.toPrettyChars(), v.toChars());
                err = true;
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

            Dsymbol vparent = v.toParent2();
            for (Dsymbol s = sc.func; !err && s; s = s.toParentP(vparent))
            {
                if (s == vparent)
                    break;

                if (AggregateDeclaration ad = s.isAggregateDeclaration())
                {
                    if (ad.isNested())
                        continue;
                    break;
                }
                FuncDeclaration ff = s.isFuncDeclaration();
                if (!ff)
                    break;
                if (ff.isNested() || ff.isThis())
                {
                    if (ff.type.isImmutable() ||
                        ff.type.isShared() && !MODimplicitConv(ff.type.mod, v.type.mod))
                    {
                        OutBuffer ffbuf;
                        OutBuffer vbuf;
                        MODMatchToBuffer(&ffbuf, ff.type.mod, v.type.mod);
                        MODMatchToBuffer(&vbuf, v.type.mod, ff.type.mod);
                        error("%s%s `%s` cannot access %sdata `%s`",
                            ffbuf.peekChars(), ff.kind(), ff.toPrettyChars(), vbuf.peekChars(), v.toChars());
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
        if (v.storage_class & STC.gshared)
        {
            if (sc.setUnsafe(false, this.loc,
                "`@safe` function `%s` cannot access `__gshared` data `%s`", sc.func, v))
            {
                err = true;
            }
        }

        return err;
    }

    /*
    Check if sc.func is impure or can be made impure.
    Returns true on error, i.e. if sc.func is pure and cannot be made impure.
    */
    private static bool checkImpure(Scope* sc)
    {
        return sc.func && (isRootTraitsCompilesScope(sc)
                ? sc.func.isPureBypassingInference() >= PURE.weak
                : sc.func.setImpure());
    }

    /*********************************************
     * Calling function f.
     * Check the safety, i.e. if we're in a @safe function
     * we can only call @safe or @trusted functions.
     * Returns true if error occurs.
     */
    extern (D) final bool checkSafety(Scope* sc, FuncDeclaration f)
    {
        if (sc.func == f)
            return false;
        if (sc.intypeof == 1)
            return false;
        if (sc.flags & SCOPE.debug_)
            return false;
        if ((sc.flags & SCOPE.ctfe) && sc.func)
            return false;

        if (!sc.func)
        {
            if (sc.varDecl && !f.safetyInprocess && !f.isSafe() && !f.isTrusted())
            {
                if (sc.varDecl.storage_class & STC.safe)
                {
                    error("`@safe` variable `%s` cannot be initialized by calling `@system` function `%s`",
                        sc.varDecl.toChars(), f.toChars());
                    return true;
                }
                else
                {
                    sc.varDecl.storage_class |= STC.system;
                }
            }
            return false;
        }

        if (!f.isSafe() && !f.isTrusted())
        {
            if (isRootTraitsCompilesScope(sc) ? sc.func.isSafeBypassingInference() : sc.func.setUnsafeCall(f))
            {
                if (!loc.isValid()) // e.g. implicitly generated dtor
                    loc = sc.func.loc;

                const prettyChars = f.toPrettyChars();
                error("`@safe` %s `%s` cannot call `@system` %s `%s`",
                    sc.func.kind(), sc.func.toPrettyChars(), f.kind(),
                    prettyChars);
                f.errorSupplementalInferredSafety(/*max depth*/ 10, /*deprecation*/ false);
                .errorSupplemental(f.loc, "`%s` is declared here", prettyChars);

                checkOverridenDtor(sc, f, dd => dd.type.toTypeFunction().trust > TRUST.system, "@system");

                return true;
            }
        }
        else if (f.isSafe() && f.safetyViolation)
        {
            // for dip1000 by default transition, print deprecations for calling functions that will become `@system`
            if (sc.func.isSafeBypassingInference())
            {
                .deprecation(this.loc, "`@safe` function `%s` calling `%s`", sc.func.toChars(), f.toChars());
                errorSupplementalInferredSafety(f, 10, true);
            }
            else if (!sc.func.safetyViolation)
            {
                import dmd.func : AttributeViolation;
                sc.func.safetyViolation = new AttributeViolation(this.loc, null, f, null, null);
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
    extern (D) final bool checkNogc(Scope* sc, FuncDeclaration f)
    {
        if (!sc.func)
            return false;
        if (sc.func == f)
            return false;
        if (sc.intypeof == 1)
            return false;
        if (sc.flags & (SCOPE.ctfe | SCOPE.debug_))
            return false;

        if (!f.isNogc())
        {
            if (isRootTraitsCompilesScope(sc) ? sc.func.isNogcBypassingInference() : sc.func.setGC())
            {
                if (loc.linnum == 0) // e.g. implicitly generated dtor
                    loc = sc.func.loc;

                // Lowered non-@nogc'd hooks will print their own error message inside of nogc.d (NOGCVisitor.visit(CallExp e)),
                // so don't print anything to avoid double error messages.
                if (!(f.ident == Id._d_HookTraceImpl || f.ident == Id._d_arraysetlengthT
                    || f.ident == Id._d_arrayappendT || f.ident == Id._d_arrayappendcTX
                    || f.ident == Id._d_newclassT))
                    error("`@nogc` %s `%s` cannot call non-@nogc %s `%s`",
                        sc.func.kind(), sc.func.toPrettyChars(), f.kind(), f.toPrettyChars());

                checkOverridenDtor(sc, f, dd => dd.type.toTypeFunction().isnogc, "non-@nogc");

                return true;
            }
        }
        return false;
    }

    /********************************************
     * Check that the postblit is callable if t is an array of structs.
     * Returns true if error happens.
     */
    extern (D) final bool checkPostblit(Scope* sc, Type t)
    {
        if (auto ts = t.baseElemOf().isTypeStruct())
        {
            if (global.params.useTypeInfo && Type.dtypeinfo)
            {
                // https://issues.dlang.org/show_bug.cgi?id=11395
                // Require TypeInfo generation for array concatenation
                semanticTypeInfo(sc, t);
            }

            StructDeclaration sd = ts.sym;
            if (sd.postblit)
            {
                if (sd.postblit.checkDisabled(loc, sc))
                    return true;

                //checkDeprecated(sc, sd.postblit);        // necessary?
                checkPurity(sc, sd.postblit);
                checkSafety(sc, sd.postblit);
                checkNogc(sc, sd.postblit);
                //checkAccess(sd, loc, sc, sd.postblit);   // necessary?
                return false;
            }
        }
        return false;
    }

    extern (D) final bool checkRightThis(Scope* sc)
    {
        if (op == EXP.error)
            return true;
        if (op == EXP.variable && type.ty != Terror)
        {
            VarExp ve = cast(VarExp)this;
            if (isNeedThisScope(sc, ve.var))
            {
                //printf("checkRightThis sc.intypeof = %d, ad = %p, func = %p, fdthis = %p\n",
                //        sc.intypeof, sc.getStructClassScope(), func, fdthis);
                error("need `this` for `%s` of type `%s`", ve.var.toChars(), ve.var.type.toChars());
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

        error("read-modify-write operations are not allowed for `shared` variables");
        errorSupplemental("Use `core.atomic.atomicOp!\"%s\"(%s, %s)` instead",
                          EXPtoString(rmwOp).ptr, toChars(), ex ? ex.toChars() : "1");
        return true;
    }

    /************************************************
     * Destructors are attached to VarDeclarations.
     * Hence, if expression returns a temp that needs a destructor,
     * make sure and create a VarDeclaration for that temp.
     */
    Expression addDtorHook(Scope* sc)
    {
        return this;
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

    final Expression optimize(int result, bool keepLvalue = false)
    {
        return Expression_optimize(this, result, keepLvalue);
    }

    // Entry point for CTFE.
    // A compile-time result is required. Give an error if not possible
    final Expression ctfeInterpret()
    {
        return .ctfeInterpret(this);
    }

    final int isConst()
    {
        return .isConst(this);
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

    final pure inout nothrow @nogc @safe
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
        inout(IsExp)        isExp() { return op == EXP.is_ ? cast(typeof(return))this : null; }
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
        inout(DefaultInitExp)    isDefaultInitExp() { return isDefaultInitOp(op) ? cast(typeof(return))this: null; }
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
        super(loc, EXP.int64, __traits(classInstanceSize, IntegerExp));
        //printf("IntegerExp(value = %lld, type = '%s')\n", value, type ? type.toChars() : "");
        assert(type);
        if (!type.isscalar())
        {
            //printf("%s, loc = %d\n", toChars(), loc.linnum);
            if (type.ty != Terror)
                error("integral constant must be scalar type, not `%s`", type.toChars());
            type = Type.terror;
        }
        this.type = type;
        this.value = normalize(type.toBasetype().ty, value);
    }

    extern (D) this(dinteger_t value)
    {
        super(Loc.initial, EXP.int64, __traits(classInstanceSize, IntegerExp));
        this.type = Type.tint32;
        this.value = cast(int)value;
    }

    static IntegerExp create(const ref Loc loc, dinteger_t value, Type type)
    {
        return new IntegerExp(loc, value, type);
    }

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, dinteger_t value, Type type)
    {
        emplaceExp!(IntegerExp)(pue, loc, value, type);
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

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (!e)
            e = this;
        else if (!loc.isValid())
            loc = e.loc;
        e.error("cannot modify constant `%s`", e.toChars());
        return ErrorExp.get();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    dinteger_t getInteger()
    {
        return value;
    }

    void setInteger(dinteger_t value)
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
    private extern (D) this()
    {
        super(Loc.initial, EXP.error, __traits(classInstanceSize, ErrorExp));
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
            .error(Loc.initial, "unknown, please file report on issues.dlang.org");
        }

        return errorexp;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        return this;
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

    extern (D) this(VarDeclaration var)
    {
        super(var.loc, EXP.void_, __traits(classInstanceSize, VoidInitExp));
        this.var = var;
        this.type = var.type;
    }

    override const(char)* toChars() const
    {
        return "void";
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

    extern (D) this(const ref Loc loc, real_t value, Type type)
    {
        super(loc, EXP.float64, __traits(classInstanceSize, RealExp));
        //printf("RealExp::RealExp(%Lg)\n", value);
        this.value = value;
        this.type = type;
    }

    static RealExp create(const ref Loc loc, real_t value, Type type)
    {
        return new RealExp(loc, value, type);
    }

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, real_t value, Type type)
    {
        emplaceExp!(RealExp)(pue, loc, value, type);
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
        return type.isreal() ? value : CTFloat.zero;
    }

    override real_t toImaginary()
    {
        return type.isreal() ? CTFloat.zero : value;
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

    extern (D) this(const ref Loc loc, complex_t value, Type type)
    {
        super(loc, EXP.complex80, __traits(classInstanceSize, ComplexExp));
        this.value = value;
        this.type = type;
        //printf("ComplexExp::ComplexExp(%s)\n", toChars());
    }

    static ComplexExp create(const ref Loc loc, complex_t value, Type type)
    {
        return new ComplexExp(loc, value, type);
    }

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, complex_t value, Type type)
    {
        emplaceExp!(ComplexExp)(pue, loc, value, type);
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (auto ne = (cast(Expression)o).isComplexExp())
        {
            if (type.toHeadMutable().equals(ne.type.toHeadMutable()) && RealIdentical(creall(value), creall(ne.value)) && RealIdentical(cimagl(value), cimagl(ne.value)))
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

    extern (D) this(const ref Loc loc, Identifier ident) scope
    {
        super(loc, EXP.identifier, __traits(classInstanceSize, IdentifierExp));
        this.ident = ident;
    }

    static IdentifierExp create(const ref Loc loc, Identifier ident)
    {
        return new IdentifierExp(loc, ident);
    }

    override final bool isLvalue()
    {
        return true;
    }

    override final Expression toLvalue(Scope* sc, Expression e)
    {
        return this;
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

    extern (D) this(const ref Loc loc, Dsymbol s, bool hasOverloads = true)
    {
        super(loc, EXP.dSymbol, __traits(classInstanceSize, DsymbolExp));
        this.s = s;
        this.hasOverloads = hasOverloads;
    }

    override bool isLvalue()
    {
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        return this;
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

    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.this_, __traits(classInstanceSize, ThisExp));
        //printf("ThisExp::ThisExp() loc = %d\n", loc.linnum);
    }

    this(const ref Loc loc, const EXP tok)
    {
        super(loc, tok, __traits(classInstanceSize, ThisExp));
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
        return type.toBasetype().ty != Tclass;
    }

    override final Expression toLvalue(Scope* sc, Expression e)
    {
        if (type.toBasetype().ty == Tclass)
        {
            // Class `this` is an rvalue; struct `this` is an lvalue.
            return Expression.toLvalue(sc, e);
        }
        return this;
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
    extern (D) this(const ref Loc loc)
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
    extern (D) this(const ref Loc loc, Type type = null) scope
    {
        super(loc, EXP.null_, __traits(classInstanceSize, NullExp));
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
        if (implicitConvTo(Type.tstring))
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
    private union
    {
        char* string;   // if sz == 1
        wchar* wstring; // if sz == 2
        dchar* dstring; // if sz == 4
    }                   // (const if ownedByCtfe == OwnedBy.code)
    size_t len;         // number of code units
    ubyte sz = 1;       // 1: char, 2: wchar, 4: dchar
    ubyte committed;    // !=0 if type is committed
    enum char NoPostfix = 0;
    char postfix = NoPostfix;   // 'c', 'w', 'd'
    OwnedBy ownedByCtfe = OwnedBy.code;

    extern (D) this(const ref Loc loc, const(void)[] string) scope
    {
        super(loc, EXP.string_, __traits(classInstanceSize, StringExp));
        this.string = cast(char*)string.ptr; // note that this.string should be const
        this.len = string.length;
        this.sz = 1;                    // work around LDC bug #1286
    }

    extern (D) this(const ref Loc loc, const(void)[] string, size_t len, ubyte sz, char postfix = NoPostfix) scope
    {
        super(loc, EXP.string_, __traits(classInstanceSize, StringExp));
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

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, const(char)* s)
    {
        emplaceExp!(StringExp)(pue, loc, s.toDString());
    }

    extern (D) static void emplace(UnionExp* pue, const ref Loc loc, const(void)[] string)
    {
        emplaceExp!(StringExp)(pue, loc, string);
    }

    extern (D) static void emplace(UnionExp* pue, const ref Loc loc, const(void)[] string, size_t len, ubyte sz, char postfix)
    {
        emplaceExp!(StringExp)(pue, loc, string, len, sz, postfix);
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
                    error("%.*s", cast(int)s.length, s.ptr);
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
                    error("%.*s", cast(int)s.length, s.ptr);
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
        assert(i < len);
        final switch (sz)
        {
        case 1:
            return string[i];
        case 2:
            return wstring[i];
        case 4:
            return dstring[i];
        }
    }

    /*********************************************
     * Set the code unit at index i to c
     * Params:
     *  i = index
     *  c = code unit to set it to
     */
    void setCodeUnit(size_t i, dchar c)
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
            dstring[i] = c;
            break;
        }
    }

    override StringExp toStringExp()
    {
        return this;
    }

    /****************************************
     * Convert string to char[].
     */
    StringExp toUTF8(Scope* sc)
    {
        if (sz != 1)
        {
            // Convert to UTF-8 string
            committed = 0;
            Expression e = castTo(sc, Type.tchar.arrayOf());
            e = e.optimize(WANTvalue);
            auto se = e.isStringExp();
            assert(se.sz == 1);
            return se;
        }
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
        return (type && type.toBasetype().ty == Tsarray);
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        //printf("StringExp::toLvalue(%s) type = %s\n", toChars(), type ? type.toChars() : NULL);
        return (type && type.toBasetype().ty == Tsarray) ? this : Expression.toLvalue(sc, e);
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        error("cannot modify string literal `%s`", toChars());
        return ErrorExp.get();
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

    extern (D) this(const ref Loc loc, Expression e0, Expressions* exps)
    {
        super(loc, EXP.tuple, __traits(classInstanceSize, TupleExp));
        //printf("TupleExp(this = %p)\n", this);
        this.e0 = e0;
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, Expressions* exps)
    {
        super(loc, EXP.tuple, __traits(classInstanceSize, TupleExp));
        //printf("TupleExp(this = %p)\n", this);
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, TupleDeclaration tup)
    {
        super(loc, EXP.tuple, __traits(classInstanceSize, TupleExp));
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
                error("`%s` is not an expression", o.toChars());
            }
        }
    }

    static TupleExp create(const ref Loc loc, Expressions* exps)
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
    /** If !is null, elements[] can be sparse and basis is used for the
     * "default" element value. In other words, non-null elements[i] overrides
     * this 'basis' value.
     */
    Expression basis;

    Expressions* elements;
    OwnedBy ownedByCtfe = OwnedBy.code;
    bool onstack = false;

    extern (D) this(const ref Loc loc, Type type, Expressions* elements)
    {
        super(loc, EXP.arrayLiteral, __traits(classInstanceSize, ArrayLiteralExp));
        this.type = type;
        this.elements = elements;
    }

    extern (D) this(const ref Loc loc, Type type, Expression e)
    {
        super(loc, EXP.arrayLiteral, __traits(classInstanceSize, ArrayLiteralExp));
        this.type = type;
        elements = new Expressions();
        elements.push(e);
    }

    extern (D) this(const ref Loc loc, Type type, Expression basis, Expressions* elements)
    {
        super(loc, EXP.arrayLiteral, __traits(classInstanceSize, ArrayLiteralExp));
        this.type = type;
        this.basis = basis;
        this.elements = elements;
    }

    static ArrayLiteralExp create(const ref Loc loc, Expressions* elements)
    {
        return new ArrayLiteralExp(loc, null, elements);
    }

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, Expressions* elements)
    {
        emplaceExp!(ArrayLiteralExp)(pue, loc, null, elements);
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

    Expression getElement(size_t i)
    {
        return this[i];
    }

    Expression opIndex(size_t i)
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
    Expressions* keys;
    Expressions* values;

    OwnedBy ownedByCtfe = OwnedBy.code;

    extern (D) this(const ref Loc loc, Expressions* keys, Expressions* values)
    {
        super(loc, EXP.assocArrayLiteral, __traits(classInstanceSize, AssocArrayLiteralExp));
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

enum stageScrub             = 0x1;  /// scrubReturnValue is running
enum stageSearchPointers    = 0x2;  /// hasNonConstPointers is running
enum stageOptimize          = 0x4;  /// optimize is running
enum stageApply             = 0x8;  /// apply is running
enum stageInlineScan        = 0x10; /// inlineScan is running
enum stageToCBuffer         = 0x20; /// toCBuffer is running

/***********************************************************
 * sd( e1, e2, e3, ... )
 */
extern (C++) final class StructLiteralExp : Expression
{
    StructDeclaration sd;   /// which aggregate this is for
    Expressions* elements;  /// parallels sd.fields[] with null entries for fields to skip
    Type stype;             /// final type of result (can be different from sd's type)

    Symbol* sym;            /// back end symbol to initialize with literal

    /** pointer to the origin instance of the expression.
     * once a new expression is created, origin is set to 'this'.
     * anytime when an expression copy is created, 'origin' pointer is set to
     * 'origin' pointer value of the original expression.
     */
    StructLiteralExp origin;

    /// those fields need to prevent a infinite recursion when one field of struct initialized with 'this' pointer.
    StructLiteralExp inlinecopy;

    /** anytime when recursive function is calling, 'stageflags' marks with bit flag of
     * current stage and unmarks before return from this function.
     * 'inlinecopy' uses similar 'stageflags' and from multiple evaluation 'doInline'
     * (with infinite recursion) of this expression.
     */
    int stageflags;

    bool useStaticInit;     /// if this is true, use the StructDeclaration's init symbol
    bool isOriginal = false; /// used when moving instances to indicate `this is this.origin`
    OwnedBy ownedByCtfe = OwnedBy.code;

    extern (D) this(const ref Loc loc, StructDeclaration sd, Expressions* elements, Type stype = null)
    {
        super(loc, EXP.structLiteral, __traits(classInstanceSize, StructLiteralExp));
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
    Expression getField(Type type, uint offset)
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
    int getFieldIndex(Type type, uint offset)
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

    override Expression addDtorHook(Scope* sc)
    {
        /* If struct requires a destructor, rewrite as:
         *    (S tmp = S()),tmp
         * so that the destructor can be hung on tmp.
         */
        if (sd.dtor && sc.func)
        {
            /* Make an identifier for the temporary of the form:
             *   __sl%s%d, where %s is the struct name
             */
            char[10] buf = void;
            const prefix = "__sl";
            const ident = sd.ident.toString;
            const fullLen = prefix.length + ident.length;
            const len = fullLen < buf.length ? fullLen : buf.length;
            buf[0 .. prefix.length] = prefix;
            buf[prefix.length .. len] = ident[0 .. len - prefix.length];

            auto tmp = copyToTemp(0, buf[0 .. len], this);
            Expression ae = new DeclarationExp(loc, tmp);
            Expression e = new CommaExp(loc, ae, new VarExp(loc, tmp));
            e = e.expressionSemantic(sc);
            return e;
        }
        return this;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (sc.flags & SCOPE.Cfile)
            return this;  // C struct literals are lvalues
        else
            return Expression.toLvalue(sc, e);
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

    extern (D) this(const ref Loc loc, Type type_name, Initializer initializer)
    {
        super(loc, EXP.compoundLiteral, __traits(classInstanceSize, CompoundLiteralExp));
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
    extern (D) this(const ref Loc loc, Type type)
    {
        super(loc, EXP.type, __traits(classInstanceSize, TypeExp));
        //printf("TypeExp::TypeExp(%s)\n", type.toChars());
        this.type = type;
    }

    override TypeExp syntaxCopy()
    {
        return new TypeExp(loc, type.syntaxCopy());
    }

    override bool checkType()
    {
        error("type `%s` is not an expression", toChars());
        return true;
    }

    override bool checkValue()
    {
        error("type `%s` has no value", toChars());
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

    extern (D) this(const ref Loc loc, ScopeDsymbol sds)
    {
        super(loc, EXP.scope_, __traits(classInstanceSize, ScopeExp));
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
            error("%s `%s` has no type", sds.kind(), sds.toChars());
            return true;
        }
        if (auto ti = sds.isTemplateInstance())
        {
            //assert(ti.needsTypeInference(sc));
            if (ti.tempdecl &&
                ti.semantictiargsdone &&
                ti.semanticRun == PASS.initial)
            {
                error("partial %s `%s` has no type", sds.kind(), toChars());
                return true;
            }
        }
        return false;
    }

    override bool checkValue()
    {
        error("%s `%s` has no value", sds.kind(), sds.toChars());
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

    extern (D) this(const ref Loc loc, TemplateDeclaration td, FuncDeclaration fd = null)
    {
        super(loc, EXP.template_, __traits(classInstanceSize, TemplateExp));
        //printf("TemplateExp(): %s\n", td.toChars());
        this.td = td;
        this.fd = fd;
    }

    override bool isLvalue()
    {
        return fd !is null;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (!fd)
            return Expression.toLvalue(sc, e);

        assert(sc);
        return symbolToExp(fd, loc, sc, true);
    }

    override bool checkType()
    {
        error("%s `%s` has no type", td.kind(), toChars());
        return true;
    }

    override bool checkValue()
    {
        error("%s `%s` has no value", td.kind(), toChars());
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

    Expression lowering;        // lowered druntime hook: `_d_newclass`

    /// Puts the `arguments` and `names` into an `ArgumentList` for easily passing them around.
    /// The fields are still separate for backwards compatibility
    extern (D) ArgumentList argumentList() { return ArgumentList(arguments, names); }

    extern (D) this(const ref Loc loc, Expression thisexp, Type newtype, Expressions* arguments, Identifiers* names = null)
    {
        super(loc, EXP.new_, __traits(classInstanceSize, NewExp));
        this.thisexp = thisexp;
        this.newtype = newtype;
        this.arguments = arguments;
        this.names = names;
    }

    static NewExp create(const ref Loc loc, Expression thisexp, Type newtype, Expressions* arguments)
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

    extern (D) this(const ref Loc loc, Expression thisexp, ClassDeclaration cd, Expressions* arguments)
    {
        super(loc, EXP.newAnonymousClass, __traits(classInstanceSize, NewAnonClassExp));
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

    extern (D) this(const ref Loc loc, EXP op, int size, Declaration var, bool hasOverloads)
    {
        super(loc, op, size);
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
                .error(loc, "need `this` for address of `%s`", v.toChars());
            hasOverloads = false;
        }
        super(loc, EXP.symbolOffset, __traits(classInstanceSize, SymOffExp), var, hasOverloads);
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
    extern (D) this(const ref Loc loc, Declaration var, bool hasOverloads = true)
    {
        if (var.isVarDeclaration())
            hasOverloads = false;

        super(loc, EXP.variable, __traits(classInstanceSize, VarExp), var, hasOverloads);
        //printf("VarExp(this = %p, '%s', loc = %s)\n", this, var.toChars(), loc.toChars());
        //if (strcmp(var.ident.toChars(), "func") == 0) assert(0);
        this.type = var.type;
    }

    static VarExp create(const ref Loc loc, Declaration var, bool hasOverloads = true)
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
        if (var.storage_class & (STC.lazy_ | STC.rvalue | STC.manifest))
            return false;
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (var.storage_class & STC.manifest)
        {
            error("manifest constant `%s` cannot be modified", var.toChars());
            return ErrorExp.get();
        }
        if (var.storage_class & STC.lazy_ && !delegateWasExtracted)
        {
            error("lazy variable `%s` cannot be modified", var.toChars());
            return ErrorExp.get();
        }
        if (var.ident == Id.ctfe)
        {
            error("cannot modify compiler-generated variable `__ctfe`");
            return ErrorExp.get();
        }
        if (var.ident == Id.dollar) // https://issues.dlang.org/show_bug.cgi?id=13574
        {
            error("cannot modify operator `$`");
            return ErrorExp.get();
        }
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        //printf("VarExp::modifiableLvalue('%s')\n", var.toChars());
        if (var.storage_class & STC.manifest)
        {
            error("cannot modify manifest constant `%s`", toChars());
            return ErrorExp.get();
        }
        // See if this expression is a modifiable lvalue (i.e. not const)
        return Expression.modifiableLvalue(sc, e);
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
        super(loc, EXP.overloadSet, __traits(classInstanceSize, OverExp));
        //printf("OverExp(this = %p, '%s')\n", this, var.toChars());
        vars = s;
        type = Type.tvoid;
    }

    override bool isLvalue()
    {
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        return this;
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
        super(loc, EXP.function_, __traits(classInstanceSize, FuncExp));
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

    extern (D) void genIdent(Scope* sc)
    {
        if (fd.ident == Id.empty)
        {
            const(char)[] s;
            if (fd.fes)
                s = "__foreachbody";
            else if (fd.tok == TOK.reserved)
                s = "__lambda";
            else if (fd.tok == TOK.delegate_)
                s = "__dgliteral";
            else
                s = "__funcliteral";

            DsymbolTable symtab;
            if (FuncDeclaration func = sc.parent.isFuncDeclaration())
            {
                if (func.localsymtab is null)
                {
                    // Inside template constraint, symtab is not set yet.
                    // Initialize it lazily.
                    func.localsymtab = new DsymbolTable();
                }
                symtab = func.localsymtab;
            }
            else
            {
                ScopeDsymbol sds = sc.parent.isScopeDsymbol();
                if (!sds.symtab)
                {
                    // Inside template constraint, symtab may not be set yet.
                    // Initialize it lazily.
                    assert(sds.isTemplateInstance());
                    sds.symtab = new DsymbolTable();
                }
                symtab = sds.symtab;
            }
            assert(symtab);
            Identifier id = Identifier.generateId(s, symtab.length() + 1);
            fd.ident = id;
            if (td)
                td.ident = id;
            symtab.insert(td ? cast(Dsymbol)td : cast(Dsymbol)fd);
        }
    }

    override FuncExp syntaxCopy()
    {
        if (td)
            return new FuncExp(loc, td.syntaxCopy(null));
        else if (fd.semanticRun == PASS.initial)
            return new FuncExp(loc, fd.syntaxCopy(null));
        else // https://issues.dlang.org/show_bug.cgi?id=13481
             // Prevent multiple semantic analysis of lambda body.
            return new FuncExp(loc, fd);
    }

    extern (D) MATCH matchType(Type to, Scope* sc, FuncExp* presult, int flag = 0)
    {

        static MATCH cannotInfer(Expression e, Type to, int flag)
        {
            if (!flag)
                e.error("cannot infer parameter types from `%s`", to.toChars());
            return MATCH.nomatch;
        }

        //printf("FuncExp::matchType('%s'), to=%s\n", type ? type.toChars() : "null", to.toChars());
        if (presult)
            *presult = null;

        TypeFunction tof = null;
        if (to.ty == Tdelegate)
        {
            if (tok == TOK.function_)
            {
                if (!flag)
                    error("cannot match function literal to delegate type `%s`", to.toChars());
                return MATCH.nomatch;
            }
            tof = cast(TypeFunction)to.nextOf();
        }
        else if (to.ty == Tpointer && (tof = to.nextOf().isTypeFunction()) !is null)
        {
            if (tok == TOK.delegate_)
            {
                if (!flag)
                    error("cannot match delegate literal to function pointer type `%s`", to.toChars());
                return MATCH.nomatch;
            }
        }

        if (td)
        {
            if (!tof)
            {
                return cannotInfer(this, to, flag);
            }

            // Parameter types inference from 'tof'
            assert(td._scope);
            TypeFunction tf = fd.type.isTypeFunction();
            //printf("\ttof = %s\n", tof.toChars());
            //printf("\ttf  = %s\n", tf.toChars());
            const dim = tf.parameterList.length;

            if (tof.parameterList.length != dim || tof.parameterList.varargs != tf.parameterList.varargs)
                return cannotInfer(this, to, flag);

            auto tiargs = new Objects();
            tiargs.reserve(td.parameters.length);

            foreach (tp; *td.parameters)
            {
                size_t u = 0;
                foreach (i, p; tf.parameterList)
                {
                    if (auto ti = p.type.isTypeIdentifier())
                        if (ti && ti.ident == tp.ident)
                            break;

                    ++u;
                }
                assert(u < dim);
                Parameter pto = tof.parameterList[u];
                Type t = pto.type;
                if (t.ty == Terror)
                    return cannotInfer(this, to, flag);
                tf.parameterList[u].storageClass = tof.parameterList[u].storageClass;
                tiargs.push(t);
            }

            // Set target of return type inference
            if (!tf.next && tof.next)
                fd.treq = to;

            auto ti = new TemplateInstance(loc, td, tiargs);
            Expression ex = (new ScopeExp(loc, ti)).expressionSemantic(td._scope);

            // Reset inference target for the later re-semantic
            fd.treq = null;

            if (ex.op == EXP.error)
                return MATCH.nomatch;
            if (auto ef = ex.isFuncExp())
                return ef.matchType(to, sc, presult, flag);
            else
                return cannotInfer(this, to, flag);
        }

        if (!tof || !tof.next)
            return MATCH.nomatch;

        assert(type && type != Type.tvoid);
        if (fd.type.ty == Terror)
            return MATCH.nomatch;
        auto tfx = fd.type.isTypeFunction();
        bool convertMatch = (type.ty != to.ty);

        if (fd.inferRetType && tfx.next.implicitConvTo(tof.next) == MATCH.convert)
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

            auto tfy = new TypeFunction(tfx.parameterList, tof.next,
                        tfx.linkage, STC.undefined_);
            tfy.mod = tfx.mod;
            tfy.trust = tfx.trust;
            tfy.isnothrow = tfx.isnothrow;
            tfy.isnogc = tfx.isnogc;
            tfy.purity = tfx.purity;
            tfy.isproperty = tfx.isproperty;
            tfy.isref = tfx.isref;
            tfy.isInOutParam = tfx.isInOutParam;
            tfy.isInOutQual = tfx.isInOutQual;
            tfy.deco = tfy.merge().deco;

            tfx = tfy;
        }
        Type tx;
        if (tok == TOK.delegate_ ||
            tok == TOK.reserved && (type.ty == Tdelegate || type.ty == Tpointer && to.ty == Tdelegate))
        {
            // Allow conversion from implicit function pointer to delegate
            tx = new TypeDelegate(tfx);
            tx.deco = tx.merge().deco;
        }
        else
        {
            assert(tok == TOK.function_ || tok == TOK.reserved && type.ty == Tpointer || fd.errors);
            tx = tfx.pointerTo();
        }
        //printf("\ttx = %s, to = %s\n", tx.toChars(), to.toChars());

        MATCH m = tx.implicitConvTo(to);
        if (m > MATCH.nomatch)
        {
            // MATCH.exact:      exact type match
            // MATCH.constant:      covairiant type match (eg. attributes difference)
            // MATCH.convert:    context conversion
            m = convertMatch ? MATCH.convert : tx.equals(to) ? MATCH.exact : MATCH.constant;

            if (presult)
            {
                (*presult) = cast(FuncExp)copy();
                (*presult).type = to;

                // https://issues.dlang.org/show_bug.cgi?id=12508
                // Tweak function body for covariant returns.
                (*presult).fd.modifyReturns(sc, tof.next);
            }
        }
        else if (!flag)
        {
            auto ts = toAutoQualChars(tx, to);
            error("cannot implicitly convert expression `%s` of type `%s` to `%s`",
                toChars(), ts[0], ts[1]);
        }
        return m;
    }

    override const(char)* toChars() const
    {
        return fd.toChars();
    }

    override bool checkType()
    {
        if (td)
        {
            error("template lambda has no type");
            return true;
        }
        return false;
    }

    override bool checkValue()
    {
        if (td)
        {
            error("template lambda has no value");
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

    extern (D) this(const ref Loc loc, Dsymbol declaration)
    {
        super(loc, EXP.declaration, __traits(classInstanceSize, DeclarationExp));
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

    extern (D) this(const ref Loc loc, RootObject o)
    {
        super(loc, EXP.typeid_, __traits(classInstanceSize, TypeidExp));
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

    extern (D) this(const ref Loc loc, Identifier ident, Objects* args)
    {
        super(loc, EXP.traits, __traits(classInstanceSize, TraitsExp));
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
    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.halt, __traits(classInstanceSize, HaltExp));
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

    extern (D) this(const ref Loc loc, Type targ, Identifier id, TOK tok, Type tspec, TOK tok2, TemplateParameters* parameters) scope
    {
        super(loc, EXP.is_, __traits(classInstanceSize, IsExp));
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
    Type att1;      // Save alias this type to detect recursion

    extern (D) this(const ref Loc loc, EXP op, int size, Expression e1) scope
    {
        super(loc, op, size);
        this.e1 = e1;
    }

    override UnaExp syntaxCopy()
    {
        UnaExp e = cast(UnaExp)copy();
        e.type = null;
        e.e1 = e.e1.syntaxCopy();
        return e;
    }

    /********************************
     * The type for a unary expression is incompatible.
     * Print error message.
     * Returns:
     *  ErrorExp
     */
    final Expression incompatibleTypes()
    {
        if (e1.type.toBasetype() == Type.terror)
            return e1;

        if (e1.op == EXP.type)
        {
            error("incompatible type for `%s(%s)`: cannot use `%s` with types", EXPtoString(op).ptr, e1.toChars(), EXPtoString(op).ptr);
        }
        else
        {
            error("incompatible type for `%s(%s)`: `%s`", EXPtoString(op).ptr, e1.toChars(), e1.type.toChars());
        }
        return ErrorExp.get();
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

    override final Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        e1 = e1.resolveLoc(loc, sc);
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

alias fp_t = UnionExp function(const ref Loc loc, Type, Expression, Expression);
alias fp2_t = bool function(const ref Loc loc, EXP, Expression, Expression);

/***********************************************************
 * Base class for binary operators
 */
extern (C++) abstract class BinExp : Expression
{
    Expression e1;
    Expression e2;
    Type att1;      // Save alias this type to detect recursion
    Type att2;      // Save alias this type to detect recursion

    extern (D) this(const ref Loc loc, EXP op, int size, Expression e1, Expression e2) scope
    {
        super(loc, op, size);
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

    /********************************
     * The types for a binary expression are incompatible.
     * Print error message.
     * Returns:
     *  ErrorExp
     */
    final Expression incompatibleTypes()
    {
        if (e1.type.toBasetype() == Type.terror)
            return e1;
        if (e2.type.toBasetype() == Type.terror)
            return e2;

        // CondExp uses 'a ? b : c' but we're comparing 'b : c'
        const(char)* thisOp = (op == EXP.question) ? ":" : EXPtoString(op).ptr;
        if (e1.op == EXP.type || e2.op == EXP.type)
        {
            error("incompatible types for `(%s) %s (%s)`: cannot use `%s` with types",
                e1.toChars(), thisOp, e2.toChars(), EXPtoString(op).ptr);
        }
        else if (e1.type.equals(e2.type))
        {
            error("incompatible types for `(%s) %s (%s)`: both operands are of type `%s`",
                e1.toChars(), thisOp, e2.toChars(), e1.type.toChars());
        }
        else
        {
            auto ts = toAutoQualChars(e1.type, e2.type);
            error("incompatible types for `(%s) %s (%s)`: `%s` and `%s`",
                e1.toChars(), thisOp, e2.toChars(), ts[0], ts[1]);
        }
        return ErrorExp.get();
    }

    extern (D) final Expression checkOpAssignTypes(Scope* sc)
    {
        // At that point t1 and t2 are the merged types. type is the original type of the lhs.
        Type t1 = e1.type;
        Type t2 = e2.type;

        // T opAssign floating yields a floating. Prevent truncating conversions (float to int).
        // See issue 3841.
        // Should we also prevent double to float (type.isfloating() && type.size() < t2.size()) ?
        if (op == EXP.addAssign || op == EXP.minAssign ||
            op == EXP.mulAssign || op == EXP.divAssign || op == EXP.modAssign ||
            op == EXP.powAssign)
        {
            if ((type.isintegral() && t2.isfloating()))
            {
                warning("`%s %s %s` is performing truncating conversion", type.toChars(), EXPtoString(op).ptr, t2.toChars());
            }
        }

        // generate an error if this is a nonsensical *=,/=, or %=, eg real *= imaginary
        if (op == EXP.mulAssign || op == EXP.divAssign || op == EXP.modAssign)
        {
            // Any multiplication by an imaginary or complex number yields a complex result.
            // r *= c, i*=c, r*=i, i*=i are all forbidden operations.
            const(char)* opstr = EXPtoString(op).ptr;
            if (t1.isreal() && t2.iscomplex())
            {
                error("`%s %s %s` is undefined. Did you mean `%s %s %s.re`?", t1.toChars(), opstr, t2.toChars(), t1.toChars(), opstr, t2.toChars());
                return ErrorExp.get();
            }
            else if (t1.isimaginary() && t2.iscomplex())
            {
                error("`%s %s %s` is undefined. Did you mean `%s %s %s.im`?", t1.toChars(), opstr, t2.toChars(), t1.toChars(), opstr, t2.toChars());
                return ErrorExp.get();
            }
            else if ((t1.isreal() || t1.isimaginary()) && t2.isimaginary())
            {
                error("`%s %s %s` is an undefined operation", t1.toChars(), opstr, t2.toChars());
                return ErrorExp.get();
            }
        }

        // generate an error if this is a nonsensical += or -=, eg real += imaginary
        if (op == EXP.addAssign || op == EXP.minAssign)
        {
            // Addition or subtraction of a real and an imaginary is a complex result.
            // Thus, r+=i, r+=c, i+=r, i+=c are all forbidden operations.
            if ((t1.isreal() && (t2.isimaginary() || t2.iscomplex())) || (t1.isimaginary() && (t2.isreal() || t2.iscomplex())))
            {
                error("`%s %s %s` is undefined (result is complex)", t1.toChars(), EXPtoString(op).ptr, t2.toChars());
                return ErrorExp.get();
            }
            if (type.isreal() || type.isimaginary())
            {
                assert(global.errors || t2.isfloating());
                e2 = e2.castTo(sc, t1);
            }
        }
        if (op == EXP.mulAssign)
        {
            if (t2.isfloating())
            {
                if (t1.isreal())
                {
                    if (t2.isimaginary() || t2.iscomplex())
                    {
                        e2 = e2.castTo(sc, t1);
                    }
                }
                else if (t1.isimaginary())
                {
                    if (t2.isimaginary() || t2.iscomplex())
                    {
                        switch (t1.ty)
                        {
                        case Timaginary32:
                            t2 = Type.tfloat32;
                            break;

                        case Timaginary64:
                            t2 = Type.tfloat64;
                            break;

                        case Timaginary80:
                            t2 = Type.tfloat80;
                            break;

                        default:
                            assert(0);
                        }
                        e2 = e2.castTo(sc, t2);
                    }
                }
            }
        }
        else if (op == EXP.divAssign)
        {
            if (t2.isimaginary())
            {
                if (t1.isreal())
                {
                    // x/iv = i(-x/v)
                    // Therefore, the result is 0
                    e2 = new CommaExp(loc, e2, new RealExp(loc, CTFloat.zero, t1));
                    e2.type = t1;
                    Expression e = new AssignExp(loc, e1, e2);
                    e.type = t1;
                    return e;
                }
                else if (t1.isimaginary())
                {
                    Type t3;
                    switch (t1.ty)
                    {
                    case Timaginary32:
                        t3 = Type.tfloat32;
                        break;

                    case Timaginary64:
                        t3 = Type.tfloat64;
                        break;

                    case Timaginary80:
                        t3 = Type.tfloat80;
                        break;

                    default:
                        assert(0);
                    }
                    e2 = e2.castTo(sc, t3);
                    Expression e = new AssignExp(loc, e1, e2);
                    e.type = t1;
                    return e;
                }
            }
        }
        else if (op == EXP.modAssign)
        {
            if (t2.iscomplex())
            {
                error("cannot perform modulo complex arithmetic");
                return ErrorExp.get();
            }
        }
        return this;
    }

    extern (D) final bool checkIntegralBin()
    {
        bool r1 = e1.checkIntegral();
        bool r2 = e2.checkIntegral();
        return (r1 || r2);
    }

    extern (D) final bool checkArithmeticBin()
    {
        bool r1 = e1.checkArithmetic();
        bool r2 = e2.checkArithmetic();
        return (r1 || r2);
    }

    extern (D) final bool checkSharedAccessBin(Scope* sc)
    {
        const r1 = e1.checkSharedAccess(sc);
        const r2 = e2.checkSharedAccess(sc);
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

    final Expression reorderSettingAAElem(Scope* sc)
    {
        BinExp be = this;

        auto ie = be.e1.isIndexExp();
        if (!ie)
            return be;
        if (ie.e1.type.toBasetype().ty != Taarray)
            return be;

        /* Fix evaluation order of setting AA element
         * https://issues.dlang.org/show_bug.cgi?id=3825
         * Rewrite:
         *     aa[k1][k2][k3] op= val;
         * as:
         *     auto ref __aatmp = aa;
         *     auto ref __aakey3 = k1, __aakey2 = k2, __aakey1 = k3;
         *     auto ref __aaval = val;
         *     __aatmp[__aakey3][__aakey2][__aakey1] op= __aaval;  // assignment
         */

        Expression e0;
        while (1)
        {
            Expression de;
            ie.e2 = extractSideEffect(sc, "__aakey", de, ie.e2);
            e0 = Expression.combine(de, e0);

            auto ie1 = ie.e1.isIndexExp();
            if (!ie1 ||
                ie1.e1.type.toBasetype().ty != Taarray)
            {
                break;
            }
            ie = ie1;
        }
        assert(ie.e1.type.toBasetype().ty == Taarray);

        Expression de;
        ie.e1 = extractSideEffect(sc, "__aatmp", de, ie.e1);
        e0 = Expression.combine(de, e0);

        be.e2 = extractSideEffect(sc, "__aaval", e0, be.e2, true);

        //printf("-e0 = %s, be = %s\n", e0.toChars(), be.toChars());
        return Expression.combine(e0, be);
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
    extern (D) this(const ref Loc loc, EXP op, int size, Expression e1, Expression e2) scope
    {
        super(loc, op, size, e1, e2);
    }

    override final bool isLvalue()
    {
        return true;
    }

    override final Expression toLvalue(Scope* sc, Expression ex)
    {
        // Lvalue-ness will be handled in glue layer.
        return this;
    }

    override final Expression modifiableLvalue(Scope* sc, Expression e)
    {
        // should check e1.checkModifiable() ?
        return toLvalue(sc, this);
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

    extern (D) this(const ref Loc loc, Expressions* exps)
    {
        super(loc, EXP.mixin_, __traits(classInstanceSize, MixinExp));
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.import_, __traits(classInstanceSize, ImportExp), e);
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

    extern (D) this(const ref Loc loc, Expression e, Expression msg = null)
    {
        super(loc, EXP.assert_, __traits(classInstanceSize, AssertExp), e);
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
        super(loc, EXP.throw_, __traits(classInstanceSize, ThrowExp), e);
        this.type = Type.tnoreturn;
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

    extern (D) this(const ref Loc loc, Expression e, Identifier ident)
    {
        super(loc, EXP.dotIdentifier, __traits(classInstanceSize, DotIdExp), e);
        this.ident = ident;
    }

    static DotIdExp create(const ref Loc loc, Expression e, Identifier ident)
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

    extern (D) this(const ref Loc loc, Expression e, TemplateDeclaration td)
    {
        super(loc, EXP.dotTemplateDeclaration, __traits(classInstanceSize, DotTemplateExp), e);
        this.td = td;
    }

    override bool checkType()
    {
        error("%s `%s` has no type", td.kind(), toChars());
        return true;
    }

    override bool checkValue()
    {
        error("%s `%s` has no value", td.kind(), toChars());
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

    extern (D) this(const ref Loc loc, Expression e, Declaration var, bool hasOverloads = true)
    {
        if (var.isVarDeclaration())
            hasOverloads = false;

        super(loc, EXP.dotVariable, __traits(classInstanceSize, DotVarExp), e);
        //printf("DotVarExp()\n");
        this.var = var;
        this.hasOverloads = hasOverloads;
    }

    override bool isLvalue()
    {
        if (e1.op != EXP.structLiteral)
            return true;
        auto vd = var.isVarDeclaration();
        return !(vd && vd.isField());
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        //printf("DotVarExp::toLvalue(%s)\n", toChars());
        if (sc && sc.flags & SCOPE.Cfile)
        {
            /* C11 6.5.2.3-3: A postfix expression followed by the '.' or '->' operator
             * is an lvalue if the first expression is an lvalue.
             */
            if (!e1.isLvalue())
                return Expression.toLvalue(sc, e);
        }
        if (!isLvalue())
            return Expression.toLvalue(sc, e);
        if (e1.op == EXP.this_ && sc.ctorflow.fieldinit.length && !(sc.ctorflow.callSuper & CSX.any_ctor))
        {
            if (VarDeclaration vd = var.isVarDeclaration())
            {
                auto ad = vd.isMember2();
                if (ad && ad.fields.length == sc.ctorflow.fieldinit.length)
                {
                    foreach (i, f; ad.fields)
                    {
                        if (f == vd)
                        {
                            if (!(sc.ctorflow.fieldinit[i].csx & CSX.this_ctor))
                            {
                                /* If the address of vd is taken, assume it is thereby initialized
                                 * https://issues.dlang.org/show_bug.cgi?id=15869
                                 */
                                modifyFieldVar(loc, sc, vd, e1);
                            }
                            break;
                        }
                    }
                }
            }
        }
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        version (none)
        {
            printf("DotVarExp::modifiableLvalue(%s)\n", toChars());
            printf("e1.type = %s\n", e1.type.toChars());
            printf("var.type = %s\n", var.type.toChars());
        }

        return Expression.modifiableLvalue(sc, e);
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
        super(loc, EXP.dotTemplateInstance, __traits(classInstanceSize, DotTemplateInstanceExp), e);
        //printf("DotTemplateInstanceExp()\n");
        this.ti = new TemplateInstance(loc, name, tiargs);
    }

    extern (D) this(const ref Loc loc, Expression e, TemplateInstance ti)
    {
        super(loc, EXP.dotTemplateInstance, __traits(classInstanceSize, DotTemplateInstanceExp), e);
        this.ti = ti;
    }

    override DotTemplateInstanceExp syntaxCopy()
    {
        return new DotTemplateInstanceExp(loc, e1.syntaxCopy(), ti.name, TemplateInstance.arraySyntaxCopy(ti.tiargs));
    }

    bool findTempDecl(Scope* sc)
    {
        static if (LOGSEMANTIC)
        {
            printf("DotTemplateInstanceExp::findTempDecl('%s')\n", toChars());
        }
        if (ti.tempdecl)
            return true;

        Expression e = new DotIdExp(loc, e1, ti.name);
        e = e.expressionSemantic(sc);
        if (e.op == EXP.dot)
            e = (cast(DotExp)e).e2;

        Dsymbol s = null;
        switch (e.op)
        {
        case EXP.overloadSet:
            s = (cast(OverExp)e).vars;
            break;

        case EXP.dotTemplateDeclaration:
            s = (cast(DotTemplateExp)e).td;
            break;

        case EXP.scope_:
            s = (cast(ScopeExp)e).sds;
            break;

        case EXP.dotVariable:
            s = (cast(DotVarExp)e).var;
            break;

        case EXP.variable:
            s = (cast(VarExp)e).var;
            break;

        default:
            return false;
        }
        return ti.updateTempDecl(sc, s);
    }

    override bool checkType()
    {
        // Same logic as ScopeExp.checkType()
        if (ti.tempdecl &&
            ti.semantictiargsdone &&
            ti.semanticRun == PASS.initial)
        {
            error("partial %s `%s` has no type", ti.kind(), toChars());
            return true;
        }
        return false;
    }

    override bool checkValue()
    {
        if (ti.tempdecl &&
            ti.semantictiargsdone &&
            ti.semanticRun == PASS.initial)

            error("partial %s `%s` has no value", ti.kind(), toChars());
        else
            error("%s `%s` has no value", ti.kind(), ti.toChars());
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

    extern (D) this(const ref Loc loc, Expression e, FuncDeclaration f, bool hasOverloads = true, VarDeclaration vthis2 = null)
    {
        super(loc, EXP.delegate_, __traits(classInstanceSize, DelegateExp), e);
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

    extern (D) this(const ref Loc loc, Expression e, Dsymbol s)
    {
        super(loc, EXP.dotType, __traits(classInstanceSize, DotTypeExp), e);
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
    VarDeclaration vthis2;  // container for multi-context

    /// Puts the `arguments` and `names` into an `ArgumentList` for easily passing them around.
    /// The fields are still separate for backwards compatibility
    extern (D) ArgumentList argumentList() { return ArgumentList(arguments, names); }

    extern (D) this(const ref Loc loc, Expression e, Expressions* exps, Identifiers* names = null)
    {
        super(loc, EXP.call, __traits(classInstanceSize, CallExp), e);
        this.arguments = exps;
        this.names = names;
    }

    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.call, __traits(classInstanceSize, CallExp), e);
    }

    extern (D) this(const ref Loc loc, Expression e, Expression earg1)
    {
        super(loc, EXP.call, __traits(classInstanceSize, CallExp), e);
        this.arguments = new Expressions();
        if (earg1)
            this.arguments.push(earg1);
    }

    extern (D) this(const ref Loc loc, Expression e, Expression earg1, Expression earg2)
    {
        super(loc, EXP.call, __traits(classInstanceSize, CallExp), e);
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

    static CallExp create(const ref Loc loc, Expression e, Expressions* exps)
    {
        return new CallExp(loc, e, exps);
    }

    static CallExp create(const ref Loc loc, Expression e)
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
        Type tb = e1.type.toBasetype();
        if (tb.ty == Tdelegate || tb.ty == Tpointer)
            tb = tb.nextOf();
        auto tf = tb.isTypeFunction();
        if (tf && tf.isref)
        {
            if (auto dve = e1.isDotVarExp())
                if (dve.var.isCtorDeclaration())
                    return false;
            return true; // function returns a reference
        }
        return false;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (isLvalue())
            return this;
        return Expression.toLvalue(sc, e);
    }

    override Expression addDtorHook(Scope* sc)
    {
        /* Only need to add dtor hook if it's a type that needs destruction.
         * Use same logic as VarDeclaration::callScopeDtor()
         */

        if (auto tf = e1.type.isTypeFunction())
        {
            if (tf.isref)
                return this;
        }

        Type tv = type.baseElemOf();
        if (auto ts = tv.isTypeStruct())
        {
            StructDeclaration sd = ts.sym;
            if (sd.dtor)
            {
                /* Type needs destruction, so declare a tmp
                 * which the back end will recognize and call dtor on
                 */
                auto tmp = copyToTemp(0, Id.__tmpfordtor.toString(), this);
                auto de = new DeclarationExp(loc, tmp);
                auto ve = new VarExp(loc, tmp);
                Expression e = new CommaExp(loc, de, ve);
                e = e.expressionSemantic(sc);
                return e;
            }
        }
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

FuncDeclaration isFuncAddress(Expression e, bool* hasOverloads = null)
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.address, __traits(classInstanceSize, AddrExp), e);
    }

    extern (D) this(const ref Loc loc, Expression e, Type t)
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.star, __traits(classInstanceSize, PtrExp), e);
        //if (e.type)
        //  type = ((TypePointer *)e.type).next;
    }

    extern (D) this(const ref Loc loc, Expression e, Type t)
    {
        super(loc, EXP.star, __traits(classInstanceSize, PtrExp), e);
        type = t;
    }

    override bool isLvalue()
    {
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        //printf("PtrExp::modifiableLvalue() %s, type %s\n", toChars(), type.toChars());
        Declaration var;
        if (auto se = e1.isSymOffExp())
            var = se.var;
        else if (auto ve = e1.isVarExp())
            var = ve.var;
        if (var && var.type.isFunction_Delegate_PtrToFunction())
        {
            if (var.type.isTypeFunction())
                error("function `%s` is not an lvalue and cannot be modified", var.toChars());
            else
                error("function pointed to by `%s` is not an lvalue and cannot be modified", var.toChars());
            return ErrorExp.get();
        }
        return Expression.modifiableLvalue(sc, e);
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.negate, __traits(classInstanceSize, NegExp), e);
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
    extern (D) this(const ref Loc loc, Expression e) scope
    {
        super(loc, EXP.uadd, __traits(classInstanceSize, UAddExp), e);
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.tilde, __traits(classInstanceSize, ComExp), e);
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
    extern (D) this(const ref Loc loc, Expression e)
    {
        super(loc, EXP.not, __traits(classInstanceSize, NotExp), e);
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

    extern (D) this(const ref Loc loc, Expression e, bool isRAII)
    {
        super(loc, EXP.delete_, __traits(classInstanceSize, DeleteExp), e);
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

    extern (D) this(const ref Loc loc, Expression e, Type t)
    {
        super(loc, EXP.cast_, __traits(classInstanceSize, CastExp), e);
        this.to = t;
    }

    /* For cast(const) and cast(immutable)
     */
    extern (D) this(const ref Loc loc, Expression e, ubyte mod)
    {
        super(loc, EXP.cast_, __traits(classInstanceSize, CastExp), e);
        this.mod = mod;
    }

    override CastExp syntaxCopy()
    {
        return to ? new CastExp(loc, e1.syntaxCopy(), to.syntaxCopy()) : new CastExp(loc, e1.syntaxCopy(), mod);
    }

    override bool isLvalue()
    {
        //printf("e1.type = %s, to.type = %s\n", e1.type.toChars(), to.toChars());
        if (!e1.isLvalue())
            return false;
        return (to.ty == Tsarray && (e1.type.ty == Tvector || e1.type.ty == Tsarray)) ||
            e1.type.mutableOf().unSharedOf().equals(to.mutableOf().unSharedOf());
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (sc && sc.flags & SCOPE.Cfile)
        {
            /* C11 6.5.4-5: A cast does not yield an lvalue.
             */
            return Expression.toLvalue(sc, e);
        }
        if (isLvalue())
            return this;
        return Expression.toLvalue(sc, e);
    }

    override Expression addDtorHook(Scope* sc)
    {
        if (to.toBasetype().ty == Tvoid)        // look past the cast(void)
            e1 = e1.addDtorHook(sc);
        return this;
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

    extern (D) this(const ref Loc loc, Expression e, Type t)
    {
        super(loc, EXP.vector, __traits(classInstanceSize, VectorExp), e);
        assert(t.ty == Tvector);
        to = cast(TypeVector)t;
    }

    static VectorExp create(const ref Loc loc, Expression e, Type t)
    {
        return new VectorExp(loc, e, t);
    }

    // Same as create, but doesn't allocate memory.
    static void emplace(UnionExp* pue, const ref Loc loc, Expression e, Type type)
    {
        emplaceExp!(VectorExp)(pue, loc, e, type);
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
    extern (D) this(const ref Loc loc, Expression e1)
    {
        super(loc, EXP.vectorArray, __traits(classInstanceSize, VectorArrayExp), e1);
    }

    override bool isLvalue()
    {
        return e1.isLvalue();
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        e1 = e1.toLvalue(sc, e);
        return this;
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
    bool upperIsInBounds;       // true if upr <= e1.length
    bool lowerIsLessThanUpper;  // true if lwr <= upr
    bool arrayop;               // an array operation, rather than a slice

    /************************************************************/
    extern (D) this(const ref Loc loc, Expression e1, IntervalExp ie)
    {
        super(loc, EXP.slice, __traits(classInstanceSize, SliceExp), e1);
        this.upr = ie ? ie.upr : null;
        this.lwr = ie ? ie.lwr : null;
    }

    extern (D) this(const ref Loc loc, Expression e1, Expression lwr, Expression upr)
    {
        super(loc, EXP.slice, __traits(classInstanceSize, SliceExp), e1);
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
        return (type && type.toBasetype().ty == Tsarray);
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        //printf("SliceExp::toLvalue(%s) type = %s\n", toChars(), type ? type.toChars() : NULL);
        return (type && type.toBasetype().ty == Tsarray) ? this : Expression.toLvalue(sc, e);
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        error("slice expression `%s` is not a modifiable lvalue", toChars());
        return this;
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
    extern (D) this(const ref Loc loc, Expression e1)
    {
        super(loc, EXP.arrayLength, __traits(classInstanceSize, ArrayLengthExp), e1);
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
        super(loc, EXP.array, __traits(classInstanceSize, ArrayExp), e1);
        arguments = new Expressions();
        if (index)
            arguments.push(index);
    }

    extern (D) this(const ref Loc loc, Expression e1, Expressions* args)
    {
        super(loc, EXP.array, __traits(classInstanceSize, ArrayExp), e1);
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
        if (type && type.toBasetype().ty == Tvoid)
            return false;
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (type && type.toBasetype().ty == Tvoid)
            error("`void`s have no value");
        return this;
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.dot, __traits(classInstanceSize, DotExp), e1, e2);
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


    extern (D) this(const ref Loc loc, Expression e1, Expression e2, bool generated = true)
    {
        super(loc, EXP.comma, __traits(classInstanceSize, CommaExp), e1, e2);
        allowCommaExp = isGenerated = generated;
    }

    override bool isLvalue()
    {
        return e2.isLvalue();
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        e2 = e2.toLvalue(sc, null);
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        e2 = e2.modifiableLvalue(sc, e);
        return this;
    }

    override Optional!bool toBool()
    {
        return e2.toBool();
    }

    override Expression addDtorHook(Scope* sc)
    {
        e2 = e2.addDtorHook(sc);
        return this;
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
    static void allow(Expression exp)
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

    extern (D) this(const ref Loc loc, Expression lwr, Expression upr)
    {
        super(loc, EXP.interval, __traits(classInstanceSize, IntervalExp));
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
    extern (D) this(const ref Loc loc, Expression e1)
    {
        super(loc, EXP.delegatePointer, __traits(classInstanceSize, DelegatePtrExp), e1);
    }

    override bool isLvalue()
    {
        return e1.isLvalue();
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        e1 = e1.toLvalue(sc, e);
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        if (sc.setUnsafe(false, this.loc, "cannot modify delegate pointer in `@safe` code `%s`", this))
        {
            return ErrorExp.get();
        }
        return Expression.modifiableLvalue(sc, e);
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
    extern (D) this(const ref Loc loc, Expression e1)
    {
        super(loc, EXP.delegateFunctionPointer, __traits(classInstanceSize, DelegateFuncptrExp), e1);
    }

    override bool isLvalue()
    {
        return e1.isLvalue();
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        e1 = e1.toLvalue(sc, e);
        return this;
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        if (sc.setUnsafe(false, this.loc, "cannot modify delegate function pointer in `@safe` code `%s`", this))
        {
            return ErrorExp.get();
        }
        return Expression.modifiableLvalue(sc, e);
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

    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.index, __traits(classInstanceSize, IndexExp), e1, e2);
        //printf("IndexExp::IndexExp('%s')\n", toChars());
    }

    extern (D) this(const ref Loc loc, Expression e1, Expression e2, bool indexIsInBounds)
    {
        super(loc, EXP.index, __traits(classInstanceSize, IndexExp), e1, e2);
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
        if (e1.op == EXP.assocArrayLiteral)
            return false;
        if (e1.type.ty == Tsarray ||
            (e1.op == EXP.index && e1.type.ty != Tarray))
        {
            return e1.isLvalue();
        }
        return true;
    }

    override Expression toLvalue(Scope* sc, Expression e)
    {
        if (isLvalue())
            return this;
        return Expression.toLvalue(sc, e);
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        //printf("IndexExp::modifiableLvalue(%s)\n", toChars());
        Expression ex = markSettingAAElem();
        if (ex.op == EXP.error)
            return ex;

        return Expression.modifiableLvalue(sc, e);
    }

    extern (D) Expression markSettingAAElem()
    {
        if (e1.type.toBasetype().ty == Taarray)
        {
            Type t2b = e2.type.toBasetype();
            if (t2b.ty == Tarray && t2b.nextOf().isMutable())
            {
                error("associative arrays can only be assigned values with immutable keys, not `%s`", e2.type.toChars());
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
        super(loc, op, __traits(classInstanceSize, PostExp), e, IntegerExp.literal!1);
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
    extern (D) this(EXP op, const ref Loc loc, Expression e)
    {
        super(loc, op, __traits(classInstanceSize, PreExp), e);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.assign, __traits(classInstanceSize, AssignExp), e1, e2);
    }

    this(const ref Loc loc, EXP tok, Expression e1, Expression e2)
    {
        super(loc, tok, __traits(classInstanceSize, AssignExp), e1, e2);
    }

    override final bool isLvalue()
    {
        // Array-op 'x[] = y[]' should make an rvalue.
        // Setting array length 'x.length = v' should make an rvalue.
        if (e1.op == EXP.slice || e1.op == EXP.arrayLength)
        {
            return false;
        }
        return true;
    }

    override final Expression toLvalue(Scope* sc, Expression ex)
    {
        if (e1.op == EXP.slice || e1.op == EXP.arrayLength)
        {
            return Expression.toLvalue(sc, ex);
        }

        /* In front-end level, AssignExp should make an lvalue of e1.
         * Taking the address of e1 will be handled in low level layer,
         * so this function does nothing.
         */
        return this;
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.construct, e1, e2);
    }

    // Internal use only. If `v` is a reference variable, the assignment
    // will become a reference initialization automatically.
    extern (D) this(const ref Loc loc, VarDeclaration v, Expression e2)
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.blit, e1, e2);
    }

    // Internal use only. If `v` is a reference variable, the assinment
    // will become a reference rebinding automatically.
    extern (D) this(const ref Loc loc, VarDeclaration v, Expression e2)
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.addAssign, __traits(classInstanceSize, AddAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.minAssign, __traits(classInstanceSize, MinAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.mulAssign, __traits(classInstanceSize, MulAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.divAssign, __traits(classInstanceSize, DivAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.modAssign, __traits(classInstanceSize, ModAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.andAssign, __traits(classInstanceSize, AndAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.orAssign, __traits(classInstanceSize, OrAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.xorAssign, __traits(classInstanceSize, XorAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.powAssign, __traits(classInstanceSize, PowAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.leftShiftAssign, __traits(classInstanceSize, ShlAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.rightShiftAssign, __traits(classInstanceSize, ShrAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.unsignedRightShiftAssign, __traits(classInstanceSize, UshrAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.concatenateAssign, __traits(classInstanceSize, CatAssignExp), e1, e2);
    }

    extern (D) this(const ref Loc loc, EXP tok, Expression e1, Expression e2)
    {
        super(loc, tok, __traits(classInstanceSize, CatAssignExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Type type, Expression e1, Expression e2)
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
    extern (D) this(const ref Loc loc, Type type, Expression e1, Expression e2)
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.add, __traits(classInstanceSize, AddExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.min, __traits(classInstanceSize, MinExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2) scope
    {
        super(loc, EXP.concatenate, __traits(classInstanceSize, CatExp), e1, e2);
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        e1 = e1.resolveLoc(loc, sc);
        e2 = e2.resolveLoc(loc, sc);
        return this;
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.mul, __traits(classInstanceSize, MulExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.div, __traits(classInstanceSize, DivExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.mod, __traits(classInstanceSize, ModExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.pow, __traits(classInstanceSize, PowExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.leftShift, __traits(classInstanceSize, ShlExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.rightShift, __traits(classInstanceSize, ShrExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.unsignedRightShift, __traits(classInstanceSize, UshrExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.and, __traits(classInstanceSize, AndExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.or, __traits(classInstanceSize, OrExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.xor, __traits(classInstanceSize, XorExp), e1, e2);
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
    extern (D) this(const ref Loc loc, EXP op, Expression e1, Expression e2)
    {
        super(loc, op, __traits(classInstanceSize, LogicalExp), e1, e2);
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
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, op, __traits(classInstanceSize, CmpExp), e1, e2);
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
    extern (D) this(const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, EXP.in_, __traits(classInstanceSize, InExp), e1, e2);
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
        super(loc, EXP.remove, __traits(classInstanceSize, RemoveExp), e1, e2);
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
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, op, __traits(classInstanceSize, EqualExp), e1, e2);
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
    extern (D) this(EXP op, const ref Loc loc, Expression e1, Expression e2)
    {
        super(loc, op, __traits(classInstanceSize, IdentityExp), e1, e2);
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

    extern (D) this(const ref Loc loc, Expression econd, Expression e1, Expression e2) scope
    {
        super(loc, EXP.question, __traits(classInstanceSize, CondExp), e1, e2);
        this.econd = econd;
    }

    override CondExp syntaxCopy()
    {
        return new CondExp(loc, econd.syntaxCopy(), e1.syntaxCopy(), e2.syntaxCopy());
    }

    override bool isLvalue()
    {
        return e1.isLvalue() && e2.isLvalue();
    }

    override Expression toLvalue(Scope* sc, Expression ex)
    {
        // convert (econd ? e1 : e2) to *(econd ? &e1 : &e2)
        CondExp e = cast(CondExp)copy();
        e.e1 = e1.toLvalue(sc, null).addressOf();
        e.e2 = e2.toLvalue(sc, null).addressOf();
        e.type = type.pointerTo();
        return new PtrExp(loc, e, type);
    }

    override Expression modifiableLvalue(Scope* sc, Expression e)
    {
        if (!e1.isLvalue() && !e2.isLvalue())
        {
            error("conditional expression `%s` is not a modifiable lvalue", toChars());
            return ErrorExp.get();
        }
        e1 = e1.modifiableLvalue(sc, e1);
        e2 = e2.modifiableLvalue(sc, e2);
        return toLvalue(sc, this);
    }

    void hookDtors(Scope* sc)
    {
        extern (C++) final class DtorVisitor : StoppableVisitor
        {
            alias visit = typeof(super).visit;
        public:
            Scope* sc;
            CondExp ce;
            VarDeclaration vcond;
            bool isThen;

            extern (D) this(Scope* sc, CondExp ce)
            {
                this.sc = sc;
                this.ce = ce;
            }

            override void visit(Expression e)
            {
                //printf("(e = %s)\n", e.toChars());
            }

            override void visit(DeclarationExp e)
            {
                auto v = e.declaration.isVarDeclaration();
                if (v && !v.isDataseg())
                {
                    if (v._init)
                    {
                        if (auto ei = v._init.isExpInitializer())
                            walkPostorder(ei.exp, this);
                    }

                    if (v.edtor)
                        walkPostorder(v.edtor, this);

                    if (v.needsScopeDtor())
                    {
                        if (!vcond)
                        {
                            vcond = copyToTemp(STC.volatile_ | STC.const_, "__cond", ce.econd);
                            vcond.dsymbolSemantic(sc);

                            Expression de = new DeclarationExp(ce.econd.loc, vcond);
                            de = de.expressionSemantic(sc);

                            Expression ve = new VarExp(ce.econd.loc, vcond);
                            ce.econd = Expression.combine(de, ve);
                        }

                        //printf("\t++v = %s, v.edtor = %s\n", v.toChars(), v.edtor.toChars());
                        Expression ve = new VarExp(vcond.loc, vcond);
                        if (isThen)
                            v.edtor = new LogicalExp(v.edtor.loc, EXP.andAnd, ve, v.edtor);
                        else
                            v.edtor = new LogicalExp(v.edtor.loc, EXP.orOr, ve, v.edtor);
                        v.edtor = v.edtor.expressionSemantic(sc);
                        //printf("\t--v = %s, v.edtor = %s\n", v.toChars(), v.edtor.toChars());
                    }
                }
            }
        }

        scope DtorVisitor v = new DtorVisitor(sc, this);
        //printf("+%s\n", toChars());
        v.isThen = true;
        walkPostorder(e1, v);
        v.isThen = false;
        walkPostorder(e2, v);
        //printf("-%s\n", toChars());
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/// Returns: if this token is the `op` for a derived `DefaultInitExp` class.
bool isDefaultInitOp(EXP op) pure nothrow @safe @nogc
{
    return  op == EXP.prettyFunction    || op == EXP.functionString ||
            op == EXP.line              || op == EXP.moduleString   ||
            op == EXP.file              || op == EXP.fileFullPath   ;
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
    extern (D) this(const ref Loc loc, EXP op, int size)
    {
        super(loc, op, size);
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
    extern (D) this(const ref Loc loc, EXP tok)
    {
        super(loc, tok, __traits(classInstanceSize, FileInitExp));
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        //printf("FileInitExp::resolve() %s\n", toChars());
        const(char)* s;
        if (op == EXP.fileFullPath)
            s = FileName.toAbsolute(loc.isValid() ? loc.filename : sc._module.srcfile.toChars());
        else
            s = loc.isValid() ? loc.filename : sc._module.ident.toChars();

        Expression e = new StringExp(loc, s.toDString());
        e = e.expressionSemantic(sc);
        e = e.castTo(sc, type);
        return e;
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
    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.line, __traits(classInstanceSize, LineInitExp));
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        Expression e = new IntegerExp(loc, loc.linnum, Type.tint32);
        e = e.castTo(sc, type);
        return e;
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
    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.moduleString, __traits(classInstanceSize, ModuleInitExp));
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        const auto s = (sc.callsc ? sc.callsc : sc)._module.toPrettyChars().toDString();
        Expression e = new StringExp(loc, s);
        e = e.expressionSemantic(sc);
        e = e.castTo(sc, type);
        return e;
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
    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.functionString, __traits(classInstanceSize, FuncInitExp));
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        const(char)* s;
        if (sc.callsc && sc.callsc.func)
            s = sc.callsc.func.Dsymbol.toPrettyChars();
        else if (sc.func)
            s = sc.func.Dsymbol.toPrettyChars();
        else
            s = "";
        Expression e = new StringExp(loc, s.toDString());
        e = e.expressionSemantic(sc);
        e.type = Type.tstring;
        return e;
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
    extern (D) this(const ref Loc loc)
    {
        super(loc, EXP.prettyFunction, __traits(classInstanceSize, PrettyFuncInitExp));
    }

    override Expression resolveLoc(const ref Loc loc, Scope* sc)
    {
        FuncDeclaration fd = (sc.callsc && sc.callsc.func)
                        ? sc.callsc.func
                        : sc.func;

        const(char)* s;
        if (fd)
        {
            const funcStr = fd.Dsymbol.toPrettyChars();
            OutBuffer buf;
            functionToBufferWithIdent(fd.type.isTypeFunction(), &buf, funcStr, fd.isStatic);
            s = buf.extractChars();
        }
        else
        {
            s = "";
        }

        Expression e = new StringExp(loc, s.toDString());
        e = e.expressionSemantic(sc);
        e.type = Type.tstring;
        return e;
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

    extern (D) this(const ref Loc loc, ClassDeclaration classDeclaration)
    {
        super(loc, EXP.objcClassReference,
            __traits(classInstanceSize, ObjcClassReferenceExp));
        this.classDeclaration = classDeclaration;
        type = objc.getRuntimeMetaclass(classDeclaration).getType();
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

    extern (D) this(const ref Loc loc, Expression cntlExp, Types* types, Expressions* exps)
    {
        super(loc, EXP._Generic, __traits(classInstanceSize, GenericExp));
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

/***************************************
 * Parameters:
 *      sc:     scope
 *      flag:   1: do not issue error message for invalid modification
                2: the exp is a DotVarExp and a subfield of the leftmost
                   variable is modified
 * Returns:
 *      Whether the type is modifiable
 */
extern(D) Modifiable checkModifiable(Expression exp, Scope* sc, ModifyFlags flag = ModifyFlags.none)
{
    switch(exp.op)
    {
        case EXP.variable:
            auto varExp = cast(VarExp)exp;

            //printf("VarExp::checkModifiable %s", varExp.toChars());
            assert(varExp.type);
            return varExp.var.checkModify(varExp.loc, sc, null, flag);

        case EXP.dotVariable:
            auto dotVarExp = cast(DotVarExp)exp;

            //printf("DotVarExp::checkModifiable %s %s\n", dotVarExp.toChars(), dotVarExp.type.toChars());
            if (dotVarExp.e1.op == EXP.this_)
                return dotVarExp.var.checkModify(dotVarExp.loc, sc, dotVarExp.e1, flag);

            /* https://issues.dlang.org/show_bug.cgi?id=12764
             * If inside a constructor and an expression of type `this.field.var`
             * is encountered, where `field` is a struct declaration with
             * default construction disabled, we must make sure that
             * assigning to `var` does not imply that `field` was initialized
             */
            if (sc.func && sc.func.isCtorDeclaration())
            {
                // if inside a constructor scope and e1 of this DotVarExp
                // is another DotVarExp, then check if the leftmost expression is a `this` identifier
                if (auto dve = dotVarExp.e1.isDotVarExp())
                {
                    // Iterate the chain of DotVarExp to find `this`
                    // Keep track whether access to fields was limited to union members
                    // s.t. one can initialize an entire struct inside nested unions
                    // (but not its members)
                    bool onlyUnion = true;
                    while (true)
                    {
                        auto v = dve.var.isVarDeclaration();
                        assert(v);

                        // Accessing union member?
                        auto t = v.type.isTypeStruct();
                        if (!t || !t.sym.isUnionDeclaration())
                            onlyUnion = false;

                        // Another DotVarExp left?
                        if (!dve.e1 || dve.e1.op != EXP.dotVariable)
                            break;

                        dve = cast(DotVarExp) dve.e1;
                    }

                    if (dve.e1.op == EXP.this_)
                    {
                        scope v = dve.var.isVarDeclaration();
                        /* if v is a struct member field with no initializer, no default construction
                         * and v wasn't intialized before
                         */
                        if (v && v.isField() && !v._init && !v.ctorinit)
                        {
                            if (auto ts = v.type.isTypeStruct())
                            {
                                if (ts.sym.noDefaultCtor)
                                {
                                    /* checkModify will consider that this is an initialization
                                     * of v while it is actually an assignment of a field of v
                                     */
                                    scope modifyLevel = v.checkModify(dotVarExp.loc, sc, dve.e1, !onlyUnion ? (flag | ModifyFlags.fieldAssign) : flag);
                                    if (modifyLevel == Modifiable.initialization)
                                    {
                                        // https://issues.dlang.org/show_bug.cgi?id=22118
                                        // v is a union type field that was assigned
                                        // a variable, therefore it counts as initialization
                                        if (v.ctorinit)
                                            return Modifiable.initialization;

                                        return Modifiable.yes;
                                    }
                                    return modifyLevel;
                                }
                            }
                        }
                    }
                }
            }

            //printf("\te1 = %s\n", e1.toChars());
            return dotVarExp.e1.checkModifiable(sc, flag);

        case EXP.star:
            auto ptrExp = cast(PtrExp)exp;
            if (auto se = ptrExp.e1.isSymOffExp())
            {
                return se.var.checkModify(ptrExp.loc, sc, null, flag);
            }
            else if (auto ae = ptrExp.e1.isAddrExp())
            {
                return ae.e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.slice:
            auto sliceExp = cast(SliceExp)exp;

            //printf("SliceExp::checkModifiable %s\n", sliceExp.toChars());
            auto e1 = sliceExp.e1;
            if (e1.type.ty == Tsarray || (e1.op == EXP.index && e1.type.ty != Tarray) || e1.op == EXP.slice)
            {
                return e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.comma:
            return (cast(CommaExp)exp).e2.checkModifiable(sc, flag);

        case EXP.index:
            auto indexExp = cast(IndexExp)exp;
            auto e1 = indexExp.e1;
            if (e1.type.ty == Tsarray ||
                e1.type.ty == Taarray ||
                (e1.op == EXP.index && e1.type.ty != Tarray) ||
                e1.op == EXP.slice)
            {
                return e1.checkModifiable(sc, flag);
            }
            return Modifiable.yes;

        case EXP.question:
            auto condExp = cast(CondExp)exp;
            if (condExp.e1.checkModifiable(sc, flag) != Modifiable.no
                && condExp.e2.checkModifiable(sc, flag) != Modifiable.no)
                return Modifiable.yes;
            return Modifiable.no;

        default:
            return exp.type ? Modifiable.yes : Modifiable.no; // default modifiable
    }
}

/**
 * Verify if the given identifier is any of
 * _d_array{ctor,setctor,setassign,assign_l, assign_r}.
 *
 * Params:
 *  id = the identifier to verify
 *
 * Returns:
 *  `true` if the identifier corresponds to a construction of assignement
 *  runtime hook, `false` otherwise.
 */
bool isArrayConstructionOrAssign(const Identifier id)
{
    import dmd.id : Id;

    return id == Id._d_arrayctor || id == Id._d_arraysetctor ||
        id == Id._d_arrayassign_l || id == Id._d_arrayassign_r ||
        id == Id._d_arraysetassign;
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
