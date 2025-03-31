/**
 * Contains semantic routines specific to ImportC
 *
 * Specification: C11
 *
 * Copyright:   Copyright (C) 2021-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/importc.d, _importc.d)
 * Documentation:  https://dlang.org/phobos/dmd_importc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/importc.d
 */

module dmd.importc;

import core.stdc.stdio;

import dmd.astenums;
import dmd.dcast;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.tokens;
import dmd.typesem;

/**************************************
 * C11 does not allow array or function parameters.
 * Hence, adjust those types per C11 6.7.6.3 rules.
 * Params:
 *      t = parameter type to adjust
 *      sc = context
 * Returns:
 *      adjusted type
 */
Type cAdjustParamType(Type t, Scope* sc)
{
    if (!sc.inCfile)
        return t;

    Type tb = t.toBasetype();

    /* C11 6.7.6.3-7 array of T is converted to pointer to T
     */
    if (auto ta = tb.isTypeDArray())
    {
        t = ta.next.pointerTo();
    }
    else if (auto ts = tb.isTypeSArray())
    {
        t = ts.next.pointerTo();
    }
    /* C11 6.7.6.3-8 function is converted to pointer to function
     */
    else if (tb.isTypeFunction())
    {
        t = tb.pointerTo();
    }
    return t;
}

/***********************************************
 * C11 6.3.2.1-3 Convert expression that is an array of type to a pointer to type.
 * C11 6.3.2.1-4 Convert expression that is a function to a pointer to a function.
 * Params:
 *  e = ImportC expression to possibly convert
 *  sc = context
 * Returns:
 *  converted expression
 */
Expression arrayFuncConv(Expression e, Scope* sc)
{
    //printf("arrayFuncConv() %s\n", e.toChars());
    if (!sc.inCfile)
        return e;

    auto t = e.type.toBasetype();
    if (auto ta = t.isTypeDArray())
    {
        if (!checkAddressable(e, sc))
            return ErrorExp.get();
        e = e.castTo(sc, ta.next.pointerTo());
    }
    else if (auto ts = t.isTypeSArray())
    {
        if (!checkAddressable(e, sc))
            return ErrorExp.get();
        e = e.castTo(sc, ts.next.pointerTo());
    }
    else if (t.isTypeFunction())
    {
        e = new AddrExp(e.loc, e);
    }
    else
        return e;
    return e.expressionSemantic(sc);
}

/****************************************
 * Run semantic on `e`.
 * Expression `e` evaluates to an instance of a struct.
 * Look up `ident` as a field of that struct.
 * Params:
 *   e = evaluates to an instance of a struct
 *   sc = context
 *   id = identifier of a field in that struct
 *   arrow = -> was used
 * Returns:
 *   if successful `e.ident`
 *   if not then `ErrorExp` and message is printed
 */
Expression fieldLookup(Expression e, Scope* sc, Identifier id, bool arrow)
{
    e = e.expressionSemantic(sc);
    if (e.isErrorExp())
        return e;

    auto t = e.type;
    if (t.isTypePointer())
    {
        t = t.isTypePointer().next;
        auto pe = e.toChars();
        if (!arrow)
            error(e.loc, "since `%s` is a pointer, use `%s->%s` instead of `%s.%s`", pe, pe, id.toChars(), pe, id.toChars());
        e = new PtrExp(e.loc, e);
    }
    Dsymbol s;
    if (auto ts = t.isTypeStruct())
        s = ts.sym.search(e.loc, id, 0);
    if (!s)
    {
        error(e.loc, "`%s` is not a member of `%s`", id.toChars(), t.toChars());
        return ErrorExp.get();
    }
    Expression ef = new DotVarExp(e.loc, e, s.isDeclaration());
    return ef.expressionSemantic(sc);
}

/****************************************
 * C11 6.5.2.1-2
 * Apply C semantics to `E[I]` expression.
 * E1[E2] is lowered to *(E1 + E2)
 * Params:
 *      ae = ArrayExp to run semantics on
 *      sc = context
 * Returns:
 *      Expression if this was a C expression with completed semantic, null if not
 */
Expression carraySemantic(ArrayExp ae, Scope* sc)
{
    if (!sc.inCfile)
        return null;

    auto e1 = ae.e1.expressionSemantic(sc);

    assert(ae.arguments.length == 1);
    Expression e2 = (*ae.arguments)[0];

    /* CTFE cannot do pointer arithmetic, but it can index arrays.
     * So, rewrite as an IndexExp if we can.
     */
    auto t1 = e1.type.toBasetype();
    if (t1.isStaticOrDynamicArray())
    {
        e2 = e2.expressionSemantic(sc).arrayFuncConv(sc);
        // C doesn't do array bounds checking, so `true` turns it off
        return new IndexExp(ae.loc, e1, e2, true).expressionSemantic(sc);
    }

    e1 = e1.arrayFuncConv(sc);   // e1 might still be a function call
    e2 = e2.expressionSemantic(sc);
    auto t2 = e2.type.toBasetype();
    if (t2.isStaticOrDynamicArray())
    {
        return new IndexExp(ae.loc, e2, e1, true).expressionSemantic(sc); // swap operands
    }

    e2 = e2.arrayFuncConv(sc);
    auto ep = new PtrExp(ae.loc, new AddExp(ae.loc, e1, e2));
    return ep.expressionSemantic(sc);
}

/******************************************
 * Determine default initializer for const global symbol.
 */
void addDefaultCInitializer(VarDeclaration dsym)
{
    //printf("addDefaultCInitializer() %s\n", dsym.toChars());
    if (!(dsym.storage_class & (STC.static_ | STC.gshared)))
        return;
    if (dsym.storage_class & (STC.extern_ | STC.field | STC.in_ | STC.foreach_ | STC.parameter | STC.result))
        return;

    Type t = dsym.type;
    if (t.isTypeSArray() && t.isTypeSArray().isIncomplete())
    {
        dsym._init = new VoidInitializer(dsym.loc);
        return; // incomplete arrays will be diagnosed later
    }

    if (t.isMutable())
        return;

    auto e = dsym.type.defaultInit(dsym.loc, true);
    dsym._init = new ExpInitializer(dsym.loc, e);
}

/********************************************
 * Resolve cast/call grammar ambiguity.
 * Params:
 *      e = expression that might be a cast, might be a call
 *      sc = context
 * Returns:
 *      null means leave as is, !=null means rewritten AST
 */
Expression castCallAmbiguity(Expression e, Scope* sc)
{
    Expression* pe = &e;

    while (1)
    {
        // Walk down the postfix expressions till we find a CallExp or something else
        switch ((*pe).op)
        {
            case EXP.dotIdentifier:
                pe = &(*pe).isDotIdExp().e1;
                continue;

            case EXP.plusPlus:
            case EXP.minusMinus:
                pe = &(*pe).isPostExp().e1;
                continue;

            case EXP.array:
                pe = &(*pe).isArrayExp().e1;
                continue;

            case EXP.call:
                auto ce = (*pe).isCallExp();
                if (ce.e1.parens)
                {
                    ce.e1 = expressionSemantic(ce.e1, sc);
                    if (ce.e1.op == EXP.type)
                    {
                        const numArgs = ce.arguments ? ce.arguments.length : 0;
                        if (numArgs >= 1)
                        {
                            ce.e1.parens = false;
                            Expression arg;
                            foreach (a; (*ce.arguments)[])
                            {
                                arg = arg ? new CommaExp(a.loc, arg, a) : a;
                            }
                            auto t = ce.e1.isTypeExp().type;
                            *pe = arg;
                            return new CastExp(ce.loc, e, t);
                        }
                    }
                }
                return null;

            default:
                return null;
        }
    }
}

/********************************************
 * Implement the C11 notion of function equivalence,
 * which allows prototyped functions to match K+R functions,
 * even though they are different.
 * Params:
 *      tf1 = type of first function
 *      tf2 = type of second function
 * Returns:
 *      true if C11 considers them equivalent
 */

bool cFuncEquivalence(TypeFunction tf1, TypeFunction tf2)
{
    //printf("cFuncEquivalence()\n  %s\n  %s\n", tf1.toChars(), tf2.toChars());
    if (tf1.equals(tf2))
        return true;

    if (tf1.linkage != tf2.linkage)
        return false;

    // Allow func(void) to match func()
    if (tf1.parameterList.length == 0 && tf2.parameterList.length == 0)
        return true;

    if (!cTypeEquivalence(tf1.next, tf2.next))
        return false;   // function return types don't match

    if (tf1.parameterList.length != tf2.parameterList.length)
        return false;

    if (!tf1.parameterList.hasIdentifierList && !tf2.parameterList.hasIdentifierList) // if both are prototyped
    {
        if (tf1.parameterList.varargs != tf2.parameterList.varargs)
            return false;
    }

    foreach (i, fparam ; tf1.parameterList)
    {
        Type t1 = fparam.type;
        Type t2 = tf2.parameterList[i].type;

        /* Strip off head const.
         * Not sure if this is C11, but other compilers treat
         * `void fn(int)` and `fn(const int x)`
         * as equivalent.
         */
        t1 = t1.mutableOf();
        t2 = t2.mutableOf();

        if (!t1.equals(t2))
            return false;
    }

    //printf("t1: %s\n", tf1.toChars());
    //printf("t2: %s\n", tf2.toChars());
    return true;
}

/*******************************
 * Types haven't been merged yet, because we haven't done
 * semantic() yet.
 * But we still need to see if t1 and t2 are the same type.
 * Params:
 *      t1 = first type
 *      t2 = second type
 * Returns:
 *      true if they are equivalent types
 */
bool cTypeEquivalence(Type t1, Type t2)
{
    if (t1.equals(t2))
        return true;    // that was easy

    if (t1.ty != t2.ty || t1.mod != t2.mod)
        return false;

    if (auto tp = t1.isTypePointer())
        return cTypeEquivalence(tp.next, t2.nextOf());

    if (auto ta = t1.isTypeSArray())
        // Bug: should check array dimension
        return cTypeEquivalence(ta.next, t2.nextOf());

    if (auto ts = t1.isTypeStruct())
        return ts.sym is t2.isTypeStruct().sym;

    if (auto te = t1.isTypeEnum())
        return te.sym is t2.isTypeEnum().sym;

    if (auto tf = t1.isTypeFunction())
        return cFuncEquivalence(tf, tf.isTypeFunction());

    return false;
}

/**********************************************
 * ImportC tag symbols sit in a parallel symbol table,
 * so that this C code works:
 * ---
 * struct S { a; };
 * int S;
 * struct S s;
 * ---
 * But there are relatively few such tag symbols, so that would be
 * a waste of memory and complexity. An additional problem is we'd like the D side
 * to find the tag symbols with ordinary lookup, not lookup in both
 * tables, if the tag symbol is not conflicting with an ordinary symbol.
 * The solution is to put the tag symbols that conflict into an associative
 * array, indexed by the address of the ordinary symbol that conflicts with it.
 * C has no modules, so this associative array is tagSymTab[] in ModuleDeclaration.
 * A side effect of our approach is that D code cannot access a tag symbol that is
 * hidden by an ordinary symbol. This is more of a theoretical problem, as nobody
 * has mentioned it when importing C headers. If someone wants to do it,
 * too bad so sad. Change the C code.
 * This function fixes up the symbol table when faced with adding a new symbol
 * `s` when there is an existing symbol `s2` with the same name.
 * C also allows forward and prototype declarations of tag symbols,
 * this function merges those.
 * Params:
 *      sc = context
 *      s = symbol to add to symbol table
 *      s2 = existing declaration
 *      sds = symbol table
 * Returns:
 *      if s and s2 are successfully put in symbol table then return the merged symbol,
 *      null if they conflict
 */
Dsymbol handleTagSymbols(ref Scope sc, Dsymbol s, Dsymbol s2, ScopeDsymbol sds)
{
    enum log = false;
    if (log) printf("handleTagSymbols('%s') add %p existing %p\n", s.toChars(), s, s2);
    if (log) printf("  add %s %s, existing %s %s\n", s.kind(), s.toChars(), s2.kind(), s2.toChars());
    auto sd = s.isScopeDsymbol(); // new declaration
    auto sd2 = s2.isScopeDsymbol(); // existing declaration

    static if (log) void print(EnumDeclaration sd)
    {
        printf("members: %p\n", sd.members);
        printf("symtab: %p\n", sd.symtab);
        printf("endlinnum: %d\n", sd.endlinnum);
        printf("type: %s\n", sd.type.toChars());
        printf("memtype: %s\n", sd.memtype.toChars());
    }

    if (!sd2)
    {
        /* Look in tag table
         */
        if (log) printf(" look in tag table\n");
        if (auto p = cast(void*)s2 in sc._module.tagSymTab)
        {
            Dsymbol s2tag = *p;
            sd2 = s2tag.isScopeDsymbol();
            assert(sd2);        // only tags allowed in tag symbol table
        }
    }

    if (sd && sd2) // `s` is a tag, `sd2` is the same tag
    {
        if (log) printf(" tag is already defined\n");

        if (sd.kind() != sd2.kind())  // being enum/struct/union must match
            return null;              // conflict

        /* Not a redeclaration if one is a forward declaration.
         * Move members to the first declared type, which is sd2.
         */
        if (sd2.members)
        {
            if (!sd.members)
                return sd2;  // ignore the sd redeclaration
        }
        else if (sd.members)
        {
            sd2.members = sd.members; // transfer definition to sd2
            sd.members = null;
            if (auto ed2 = sd2.isEnumDeclaration())
            {
                auto ed = sd.isEnumDeclaration();
                if (ed.memtype != ed2.memtype)
                    return null;        // conflict

                // transfer ed's members to sd2
                ed2.members.foreachDsymbol( (s)
                {
                    if (auto em = s.isEnumMember())
                        em.ed = ed2;
                });

                ed2.type = ed.type;
                ed2.memtype = ed.memtype;
                ed2.added = false;
            }
            return sd2;
        }
        else
            return sd2; // ignore redeclaration
    }
    else if (sd) // `s` is a tag, `s2` is not
    {
        if (log) printf(" s is tag, s2 is not\n");
        /* add `s` as tag indexed by s2
         */
        sc._module.tagSymTab[cast(void*)s2] = s;
        return s;
    }
    else if (s2 is sd2) // `s2` is a tag, `s` is not
    {
        if (log) printf(" s2 is tag, s is not\n");
        /* replace `s2` in symbol table with `s`,
         * then add `s2` as tag indexed by `s`
         */
        sds.symtab.update(s);
        sc._module.tagSymTab[cast(void*)s] = s2;
        return s;
    }
    // neither s2 nor s is a tag
    if (log) printf(" collision\n");
    return null;
}


/**********************************************
 * ImportC allows redeclarations of C variables, functions and typedefs.
 *    extern int x;
 *    int x = 3;
 * and:
 *    extern void f();
 *    void f() { }
 * Attempt to merge them.
 * Params:
 *      sc = context
 *      s = symbol to add to symbol table
 *      s2 = existing declaration
 *      sds = symbol table
 * Returns:
 *      if s and s2 are successfully put in symbol table then return the merged symbol,
 *      null if they conflict
 */
Dsymbol handleSymbolRedeclarations(ref Scope sc, Dsymbol s, Dsymbol s2, ScopeDsymbol sds)
{
    enum log = false;
    if (log) printf("handleSymbolRedeclarations('%s')\n", s.toChars());
    if (log) printf("  add %s %s, existing %s %s\n", s.kind(), s.toChars(), s2.kind(), s2.toChars());

    static Dsymbol collision()
    {
        if (log) printf(" collision\n");
        return null;
    }
    /*
    Handle merging declarations with asm("foo") and their definitions
    */
    static void mangleWrangle(Declaration oldDecl, Declaration newDecl)
    {
        if (oldDecl && newDecl)
        {
            newDecl.mangleOverride = oldDecl.mangleOverride ? oldDecl.mangleOverride : null;
        }
    }

    auto vd = s.isVarDeclaration(); // new declaration
    auto vd2 = s2.isVarDeclaration(); // existing declaration

    if (vd && vd.isCmacro())
        return vd2;

    assert(!(vd2 && vd2.isCmacro()));

    if (vd && vd2)
    {
        /* if one is `static` and the other isn't, the result is undefined
         * behavior, C11 6.2.2.7
         */
        if ((vd.storage_class ^ vd2.storage_class) & STC.static_)
            return collision();

        const i1 =  vd._init && ! vd._init.isVoidInitializer();
        const i2 = vd2._init && !vd2._init.isVoidInitializer();

        if (i1 && i2)
            return collision();         // can't both have initializers

        mangleWrangle(vd2, vd);

        if (i1)                         // vd is the definition
        {
            vd2.storage_class |= STC.extern_;  // so toObjFile() won't emit it
            sds.symtab.update(vd);      // replace vd2 with the definition
            return vd;
        }

        /* BUG: the types should match, which needs semantic() to be run on it
         *    extern int x;
         *    int x;  // match
         *    typedef int INT;
         *    INT x;  // match
         *    long x; // collision
         * We incorrectly ignore these collisions
         */
        return vd2;
    }

    auto fd = s.isFuncDeclaration(); // new declaration
    auto fd2 = s2.isFuncDeclaration(); // existing declaration
    if (fd && fd2)
    {
        /* if one is `static` and the other isn't, the result is undefined
         * behavior, C11 6.2.2.7
         * However, match what gcc allows:
         *    static int sun1(); int sun1() { return 0; }
         * and:
         *    static int sun2() { return 0; } int sun2();
         * Both produce a static function.
         *
         * Both of these should fail:
         *    int sun3(); static int sun3() { return 0; }
         * and:
         *    int sun4() { return 0; } static int sun4();
         */
        // if adding `static`
        if (   fd.storage_class & STC.static_ &&
            !(fd2.storage_class & STC.static_))
        {
            return collision();
        }

        if (fd.fbody && fd2.fbody)
            return collision();         // can't both have bodies

        mangleWrangle(fd2, fd);

        if (fd.fbody)                   // fd is the definition
        {
            if (log) printf(" replace existing with new\n");
            sds.symtab.update(fd);      // replace fd2 in symbol table with fd
            fd.overnext = fd2;

            /* If fd2 is covering a tag symbol, then fd has to cover the same one
             */
            auto ps = cast(void*)fd2 in sc._module.tagSymTab;
            if (ps)
                sc._module.tagSymTab[cast(void*)fd] = *ps;

            return fd;
        }

        /* Just like with VarDeclaration, the types should match, which needs semantic() to be run on it.
         * FuncDeclaration::semantic() detects this, but it relies on .overnext being set.
         */
        fd2.overloadInsert(fd);

        return fd2;
    }

    auto td  = s.isAliasDeclaration();  // new declaration
    auto td2 = s2.isAliasDeclaration(); // existing declaration
    if (td && td2)
    {
        /* BUG: just like with variables and functions, the types should match, which needs semantic() to be run on it.
         * FuncDeclaration::semantic2() can detect this, but it relies overnext being set.
         */
        return td2;
    }

    return collision();
}
