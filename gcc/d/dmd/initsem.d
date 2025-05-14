/**
 * Semantic analysis of initializers.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/initsem.d, _initsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_initsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/initsem.d
 */

module dmd.initsem;

import core.stdc.stdio;
import core.checkedint;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astenums;
import dmd.dcast;
import dmd.declaration;
import dmd.dinterpret;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.importc;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.opover;
import dmd.optimize;
import dmd.safe : setUnsafe;
import dmd.statement;
import dmd.target;
import dmd.tokens;
import dmd.typesem;

/********************************
 * If possible, convert array initializer to associative array initializer.
 *
 *  Params:
 *     ai = array initializer to be converted
 *
 *  Returns:
 *     The converted associative array initializer or ErrorExp if `ai`
 *     is not an associative array initializer.
 */
Expression toAssocArrayLiteral(ArrayInitializer ai)
{
    //printf("ArrayInitializer::toAssocArrayInitializer(%s)\n", ai.toChars());
    //static int i; if (++i == 2) assert(0);

    auto no(const char* format, Initializer i)
    {
        error(i.loc, format, toChars(i));
        return ErrorExp.get();
    }

    const dim = ai.value.length;
    if (!dim)
        return no("invalid associative array initializer `%s`, use `null` instead", ai);

    auto keys   = new Expressions(dim);
    auto values = new Expressions(dim);
    foreach (i, iz; ai.value[])
    {
        assert(iz);
        auto ev = iz.initializerToExpression();
        if (!ev)
            return no("invalid value `%s` in initializer", iz);
        (*values)[i] = ev;

        auto ei = ai.index[i];
        if (!ei)
            return no("missing key for value `%s` in initializer", iz);
        (*keys)[i] = ei;
    }
    return new AssocArrayLiteralExp(ai.loc, keys, values);
}

/******************************************
 * Perform semantic analysis on init.
 * Params:
 *      init = Initializer AST node
 *      sc = context
 *      tx = type that the initializer needs to become. If tx is an incomplete
 *           type and the initializer completes it, it is updated to be the
 *           complete type. ImportC has incomplete types
 *      needInterpret = if CTFE needs to be run on this,
 *                      such as if it is the initializer for a const declaration
 * Returns:
 *      `Initializer` with completed semantic analysis, `ErrorInitializer` if errors
 *      were encountered
 */
Initializer initializerSemantic(Initializer init, Scope* sc, ref Type tx, NeedInterpret needInterpret)
{
    //printf("initializerSemantic() tx: %p %s\n", tx, tx.toChars());
    if (init.semanticDone)
        return init;

    Type t = tx;

    static Initializer err()
    {
        return new ErrorInitializer();
    }

    Initializer visitVoid(VoidInitializer i)
    {
        i.type = t;
        return i;
    }

    Initializer visitDefault(DefaultInitializer i)
    {
        i.type = t;
        return i;
    }

    Initializer visitError(ErrorInitializer i)
    {
        return i;
    }

    Initializer visitStruct(StructInitializer i)
    {
        //printf("StructInitializer::semantic(t = %s) %s\n", t.toChars(), i.toChars());
        /* This works by replacing the StructInitializer with an ExpInitializer.
          */
        t = t.toBasetype();
        if (auto tsa = t.isTypeSArray())
        {
            auto ts = tsa.nextOf().toBasetype().isTypeStruct();
            if (ts)
                t = ts;
        }
        if (auto ts = t.isTypeStruct())
        {
            StructDeclaration sd = ts.sym;
            // check if the sd has a regular ctor (user defined non-copy ctor)
            // that is not disabled.
            if (sd.hasRegularCtor(true))
            {
                error(i.loc, "%s `%s` has constructors, cannot use `{ initializers }`, use `%s( initializers )` instead", sd.kind(), sd.toChars(), sd.toChars());
                return err();
            }
            sd.size(i.loc);
            if (sd.sizeok != Sizeok.done)
                return err();

            Expression getExp(size_t j, Type fieldType)
            {
                // Convert initializer to Expression `ex`
                auto tm = fieldType.addMod(t.mod);
                auto iz = i.value[j].initializerSemantic(sc, tm, needInterpret);
                auto ex = iz.initializerToExpression(null, sc.inCfile);
                if (ex.op != EXP.error)
                    i.value[j] = iz;
                return ex;
            }

            auto elements = resolveStructLiteralNamedArgs(sd, t, sc, i.loc, i.field[], &getExp, (size_t j) => i.value[j].loc);
            if (!elements)
                return err();

            // Make a StructLiteralExp out of elements[]
            auto sle = new StructLiteralExp(i.loc, sd, elements, t);
            if (!sd.fill(i.loc, *elements, false))
                return err();
            sle.type = t;
            auto ie = new ExpInitializer(i.loc, sle);
            return ie.initializerSemantic(sc, t, needInterpret);
        }
        else if ((t.ty == Tdelegate || t.isPtrToFunction()) && i.value.length == 0)
        {
            const tok = (t.ty == Tdelegate) ? TOK.delegate_ : TOK.function_;
            /* Rewrite as empty delegate literal { }
             */
            Type tf = new TypeFunction(ParameterList(), null, LINK.d);
            auto fd = new FuncLiteralDeclaration(i.loc, Loc.initial, tf, tok, null);
            fd.fbody = new CompoundStatement(i.loc, new Statements());
            fd.endloc = i.loc;
            Expression e = new FuncExp(i.loc, fd);
            auto ie = new ExpInitializer(i.loc, e);
            return ie.initializerSemantic(sc, t, needInterpret);
        }
        if (t.ty != Terror)
            error(i.loc, "a struct is not a valid initializer for a `%s`", t.toChars());
        return err();
    }

    Initializer visitArray(ArrayInitializer i)
    {
        uint length;
        const(uint) amax = 0x80000000;
        bool errors = false;
        //printf("ArrayInitializer::semantic(%s), ai: %s\n", t.toChars(), toChars(i));
        t = t.toBasetype();
        switch (t.ty)
        {
        case Tsarray:
        case Tarray:
            break;
        case Tvector:
            t = t.isTypeVector().basetype;
            break;
        case Taarray:
        case Tstruct: // consider implicit constructor call
            {
                Expression e;
                // note: MyStruct foo = [1:2, 3:4] is correct code if MyStruct has a this(int[int])
                if (t.ty == Taarray || i.isAssociativeArray())
                    e = i.toAssocArrayLiteral();
                else
                    e = i.initializerToExpression();
                // Bugzilla 13987
                if (!e)
                {
                    error(i.loc, "cannot use array to initialize `%s`", t.toChars());
                    return err();
                }
                auto ei = new ExpInitializer(e.loc, e);
                return ei.initializerSemantic(sc, t, needInterpret);
            }

        case Tpointer:
            if (t.nextOf().isTypeFunction())
                goto default;
            break;

        default:
            error(i.loc, "cannot use array to initialize `%s`", t.toChars());
            return err();
        }
        i.type = t;
        length = 0;
        for (size_t j = 0; j < i.index.length; j++) // don't replace with foreach; j is modified
        {
            Expression idx = i.index[j];
            if (idx)
            {
                sc = sc.startCTFE();
                idx = idx.expressionSemantic(sc);
                sc = sc.endCTFE();
                idx = idx.ctfeInterpret();
                i.index[j] = idx;
                const uinteger_t idxvalue = idx.toInteger();
                if (idxvalue >= amax)
                {
                    error(i.loc, "array index %llu overflow", idxvalue);
                    errors = true;
                }
                length = cast(uint)idxvalue;
                if (idx.op == EXP.error)
                    errors = true;
            }
            Initializer val = i.value[j];
            ExpInitializer ei = val.isExpInitializer();
            if (ei && !idx)
                ei.expandTuples = true;
            auto tn = t.nextOf();
            val = val.initializerSemantic(sc, tn, needInterpret);
            if (val.isErrorInitializer())
                errors = true;
            ei = val.isExpInitializer();
            // found a tuple, expand it
            if (ei && ei.exp.op == EXP.tuple)
            {
                TupleExp te = ei.exp.isTupleExp();
                i.index.remove(j);
                i.value.remove(j);
                foreach (k, e; (*te.exps)[])
                {
                    i.index.insert(j + k, cast(Expression)null);
                    i.value.insert(j + k, new ExpInitializer(e.loc, e));
                }
                j--;
                continue;
            }
            else
            {
                i.value[j] = val;
            }
            ++length;
            if (length == 0)
            {
                error(i.loc, "array dimension overflow");
                return err();
            }
            if (length > i.dim)
                i.dim = length;
        }
        if (auto tsa = t.isTypeSArray())
        {
            if (sc.inCfile && tsa.isIncomplete())
            {
                // Change to array of known length
                auto tn = tsa.next.toBasetype();
                tsa = new TypeSArray(tn, new IntegerExp(Loc.initial, i.dim, Type.tsize_t));
                tx = tsa;      // rewrite caller's type
                i.type = tsa;  // remember for later passes
            }
            else
            {
                ulong edim = tsa.dim.toInteger();
                if (i.dim > edim)
                {
                    error(i.loc, "array initializer has %u elements, but array length is %llu", i.dim, edim);
                    return err();
                }
            }
        }
        if (errors)
            return err();

        const sz = t.nextOf().size();
        if (sz == SIZE_INVALID)
            return err();
        bool overflow;
        const max = mulu(i.dim, sz, overflow);
        if (overflow || max >= amax)
        {
            error(i.loc, "array dimension %llu exceeds max of %llu", ulong(i.dim), ulong(amax / sz));
            return err();
        }
        //printf("returns ai: %s\n", i.toChars());
        return i;
    }

    Initializer visitExp(ExpInitializer i)
    {
        //printf("ExpInitializer::semantic(%s), type = %s\n", i.exp.toChars(), t.toChars());
        if (needInterpret)
            sc = sc.startCTFE();
        i.exp = i.exp.expressionSemantic(sc);
        i.exp = resolveProperties(sc, i.exp);
        if (needInterpret)
            sc = sc.endCTFE();
        if (i.exp.op == EXP.error)
            return err();
        const olderrors = global.errors;

        /* ImportC: convert arrays to pointers, functions to pointers to functions
         */
        Type tb = t.toBasetype();
        if (tb.isTypePointer())
            i.exp = i.exp.arrayFuncConv(sc);

        /* Save the expression before ctfe
         * Otherwise the error message would contain for example "&[0][0]" instead of "new int"
         * Regression: https://issues.dlang.org/show_bug.cgi?id=21687
         */
        Expression currExp = i.exp;
        if (needInterpret)
        {
            // If the result will be implicitly cast, move the cast into CTFE
            // to avoid premature truncation of polysemous types.
            // eg real [] x = [1.1, 2.2]; should use real precision.
            if (i.exp.implicitConvTo(t) && !sc.inCfile)
            {
                i.exp = i.exp.implicitCastTo(sc, t);
            }
            if (!global.gag && olderrors != global.errors)
            {
                return i;
            }
            if (sc.inCfile)
            {
                /* the interpreter turns (char*)"string" into &"string"[0] which then
                 * it cannot interpret. Resolve that case by doing optimize() first
                 */
                i.exp = i.exp.optimize(WANTvalue);
                if (i.exp.isSymOffExp())
                {
                    /* `static variable cannot be read at compile time`
                     * https://issues.dlang.org/show_bug.cgi?id=22513
                     * Maybe this would be better addressed in ctfeInterpret()?
                     */
                    needInterpret = NeedInterpret.INITnointerpret;
                }
            }
            if (needInterpret)
                i.exp = i.exp.ctfeInterpret();
            if (i.exp.op == EXP.voidExpression)
                error(i.loc, "variables cannot be initialized with an expression of type `void`. Use `void` initialization instead.");
        }
        else
        {
            i.exp = i.exp.optimize(WANTvalue);
        }

        if (!global.gag && olderrors != global.errors)
        {
            return i; // Failed, suppress duplicate error messages
        }
        if (i.exp.type.isTypeTuple() && i.exp.type.isTypeTuple().arguments.length == 0)
        {
            Type et = i.exp.type;
            i.exp = new TupleExp(i.exp.loc, new Expressions());
            i.exp.type = et;
        }
        if (i.exp.op == EXP.type)
        {
            error(i.exp.loc, "initializer must be an expression, not `%s`", i.exp.toChars());
            return err();
        }
        // Make sure all pointers are constants
        if (needInterpret && hasNonConstPointers(i.exp))
        {
            error(i.exp.loc, "cannot use non-constant CTFE pointer in an initializer `%s`", currExp.toChars());
            return err();
        }
        Type ti = i.exp.type.toBasetype();
        if (i.exp.op == EXP.tuple && i.expandTuples && !i.exp.implicitConvTo(t))
        {
            return new ExpInitializer(i.loc, i.exp);
        }
        /* Look for case of initializing a static array with a too-short
         * string literal, such as:
         *  char[5] foo = "abc";
         * Allow this by doing an explicit cast, which will lengthen the string
         * literal.
         */
        if (i.exp.op == EXP.string_ && tb.ty == Tsarray)
        {
            StringExp se = i.exp.isStringExp();
            Type typeb = se.type.toBasetype();
            TY tynto = tb.nextOf().ty;
            if (!se.committed &&
                typeb.isStaticOrDynamicArray() && tynto.isSomeChar &&
                se.numberOfCodeUnits(tynto) < tb.isTypeSArray().dim.toInteger())
            {
                i.exp = se.castTo(sc, t);
                goto L1;
            }

            /* Lop off terminating 0 of initializer for:
             *  static char s[5] = "hello";
             */
            if (sc.inCfile &&
                typeb.ty == Tsarray &&
                tynto.isSomeChar &&
                tb.isTypeSArray().dim.toInteger() + 1 == typeb.isTypeSArray().dim.toInteger())
            {
                i.exp = se.castTo(sc, t);
                goto L1;
            }
        }
        /* C11 6.7.9-14..15
         * Initialize an array of unknown size with a string.
         * Change to static array of known size
         */
        if (sc.inCfile && i.exp.isStringExp() &&
            tb.isTypeSArray() && tb.isTypeSArray().isIncomplete())
        {
            StringExp se = i.exp.isStringExp();
            auto ts = new TypeSArray(tb.nextOf(), new IntegerExp(Loc.initial, se.len + 1, Type.tsize_t));
            t = typeSemantic(ts, Loc.initial, sc);
            i.exp.type = t;
            tx = t;
        }

        // Look for implicit constructor call
        if (tb.ty == Tstruct && !(ti.ty == Tstruct && tb.toDsymbol(sc) == ti.toDsymbol(sc)) && !i.exp.implicitConvTo(t))
        {
            StructDeclaration sd = tb.isTypeStruct().sym;
            if (sd.ctor)
            {
                // Rewrite as S().ctor(exp)
                Expression e;
                e = new StructLiteralExp(i.loc, sd, null);
                e = new DotIdExp(i.loc, e, Id.ctor);
                e = new CallExp(i.loc, e, i.exp);
                e = e.expressionSemantic(sc);
                if (needInterpret)
                    i.exp = e.ctfeInterpret();
                else
                    i.exp = e.optimize(WANTvalue);
            }
            else if (search_function(sd, Id.opCall))
            {
                /* https://issues.dlang.org/show_bug.cgi?id=1547
                 *
                 * Look for static opCall
                 *
                 * Rewrite as:
                 *  i.exp = typeof(sd).opCall(arguments)
                 */

                Expression e = typeDotIdExp(i.loc, sd.type, Id.opCall);
                e = new CallExp(i.loc, e, i.exp);
                e = e.expressionSemantic(sc);
                e = resolveProperties(sc, e);
                if (needInterpret)
                    i.exp = e.ctfeInterpret();
                else
                    i.exp = e.optimize(WANTvalue);
            }
        }

        // Look for the case of statically initializing an array with a single member.
        // Recursively strip static array / enum layers until a compatible element is found,
        // and return an `ArrayLiteralExp` repeating the initializer, or `null` if no match found
        // int[2][3] = 7       => [[7, 7], [7, 7], [7, 7]]
        // int[2] = new Object => null
        Expression sarrayRepeat(Type tb)
        {
            auto tsa = tb.isTypeSArray();
            if (!tsa)
                return null;

            // printf("i.exp = %s, tsa = %s\n", i.exp.toChars(), tsa.toChars());
            Expression elem = null;
            if (i.exp.implicitConvTo(tb.nextOf()))
                elem = i.exp.implicitCastTo(sc, tb.nextOf());
            else if (auto ae = sarrayRepeat(tb.nextOf().toBasetype()))
                elem = ae;
            else
                return null;

            auto arrayElements = new Expressions(cast(size_t) tsa.dim.toInteger());
            foreach (ref e; *arrayElements)
                e = elem;
            return new ArrayLiteralExp(i.exp.loc, tb, elem, arrayElements);
        }

        if (auto sa = sarrayRepeat(tb))
        {
            // printf("sa = %s\n", sa.toChars());
            i.exp = sa;
        }

        {
        auto tta = t.isTypeSArray();
        if (i.exp.implicitConvTo(t))
        {
            i.exp = i.exp.implicitCastTo(sc, t);
        }
        else if (sc.inCfile && i.exp.isStringExp() &&
            tta && (tta.next.ty == Tint8 || tta.next.ty == Tuns8) &&
            ti.ty == Tsarray && ti.nextOf().ty == Tchar)
        {
            /* unsigned char bbb[1] = "";
             *   signed char ccc[1] = "";
             */
            i.exp = i.exp.castTo(sc, t);
        }
        else
        {
            auto tba = tb.isTypeSArray();
            // Look for mismatch of compile-time known length to emit
            // better diagnostic message, as same as AssignExp::semantic.
            if (tba && i.exp.implicitConvTo(tba.next.arrayOf()) > MATCH.nomatch)
            {
                uinteger_t dim1 = tba.dim.toInteger();
                uinteger_t dim2 = dim1;
                if (auto ale = i.exp.isArrayLiteralExp())
                {
                    dim2 = ale.elements ? ale.elements.length : 0;
                }
                else if (auto se = i.exp.isSliceExp())
                {
                    if (Type tx = toStaticArrayType(se))
                        dim2 = tx.isTypeSArray().dim.toInteger();
                }
                if (dim1 != dim2)
                {
                    error(i.exp.loc, "mismatched array lengths, %d and %d", cast(int)dim1, cast(int)dim2);
                    i.exp = ErrorExp.get();
                }
            }
            Type et = i.exp.type;
            const errors = global.startGagging();
            i.exp = i.exp.implicitCastTo(sc, t);
            if (global.endGagging(errors))
                error(currExp.loc, "cannot implicitly convert expression `%s` of type `%s` to `%s`", currExp.toErrMsg(), et.toChars(), t.toChars());
        }
        }
    L1:
        if (i.exp.op == EXP.error)
        {
            return i;
        }
        if (needInterpret)
            i.exp = i.exp.ctfeInterpret();
        else
            i.exp = i.exp.optimize(WANTvalue);
        //printf("-ExpInitializer::semantic(): "); i.exp.print();
        return i;
    }

    Initializer visitC(CInitializer ci)
    {
        //printf("CInitializer::semantic() tx: %s t: %s ci: %s\n", (tx ? tx.toChars() : "".ptr), t.toChars(), toChars(ci));
        static if (0)
            if (auto ts = tx.isTypeStruct())
            {
                import dmd.common.outbuffer;
                OutBuffer buf;
                HdrGenState hgs;
                toCBuffer(ts.sym, buf, hgs);
                printf("%s\n", buf.peekChars());
            }

        /* Rewrite CInitializer into ExpInitializer, ArrayInitializer, or StructInitializer
         */
        t = t.toBasetype();

        if (auto tv = t.isTypeVector())
            t = tv.basetype;

        /* If `{ expression }` return the expression initializer
         */
        ExpInitializer isBraceExpression()
        {
            auto dil = ci.initializerList[];
            return (dil.length == 1 && !dil[0].designatorList)
                    ? dil[0].initializer.isExpInitializer()
                    : null;
        }

        /********************************
         */
        bool overlaps(VarDeclaration field, VarDeclaration[] fields, StructInitializer si)
        {
            foreach (fld; fields)
            {
                if (field.isOverlappedWith(fld))
                {
                    // look for initializer corresponding with fld
                    foreach (i, ident; si.field[])
                    {
                        if (ident == fld.ident && si.value[i])
                            return true;   // already an initializer for `field`
                    }
                }
            }
            return false;
        }

        /* Run semantic on ExpInitializer, see if it represents entire struct ts
         */
        bool representsStruct(ExpInitializer ei, TypeStruct ts)
        {
            if (needInterpret)
                sc = sc.startCTFE();
            ei.exp = ei.exp.expressionSemantic(sc);
            ei.exp = resolveProperties(sc, ei.exp);
            if (needInterpret)
                sc = sc.endCTFE();
            return ei.exp.implicitConvTo(ts) != MATCH.nomatch; // initializer represents the entire struct
        }

        /* If { } are omitted from substructs, use recursion to reconstruct where
         * brackets go
         * Params:
         *  ts = substruct to initialize
         *  index = index into ci.initializer, updated
         * Returns: struct initializer for this substruct
         */
        Initializer subStruct()(TypeStruct ts, ref size_t index)
        {
            //printf("subStruct(ts: %s, index %d)\n", ts.toChars(), cast(int)index);

            auto si = new StructInitializer(ci.loc);
            StructDeclaration sd = ts.sym;
            sd.size(ci.loc);
            if (sd.sizeok != Sizeok.done)
            {
                index = ci.initializerList.length;
                return err();
            }
            const nfields = sd.fields.length;

            foreach (fieldi; 0 .. nfields)
            {
                if (index >= ci.initializerList.length)
                    break;          // ran out of initializers
                auto di = ci.initializerList[index];
                if (di.designatorList && fieldi != 0)
                    break;          // back to top level

                VarDeclaration field;
                while (1)   // skip field if it overlaps with previously seen fields
                {
                    field = sd.fields[fieldi];
                    ++fieldi;
                    if (!overlaps(field, sd.fields[], si))
                        break;
                    if (fieldi == nfields)
                        break;
                }
                auto tn = field.type.toBasetype();
                auto tnsa = tn.isTypeSArray();
                auto tns = tn.isTypeStruct();
                auto ix = di.initializer;
                if (tnsa && ix.isExpInitializer())
                {
                    ExpInitializer ei = ix.isExpInitializer();
                    if (ei.exp.isStringExp() && tnsa.nextOf().isIntegral())
                    {
                        si.addInit(field.ident, ei);
                        ++index;
                    }
                    else
                        si.addInit(field.ident, subArray(tnsa, index)); // fwd ref of subArray is why subStruct is a template
                }
                else if (tns && ix.isExpInitializer())
                {
                    /* Disambiguate between an exp representing the entire
                     * struct, and an exp representing the first field of the struct
                     */
                    if (representsStruct(ix.isExpInitializer(), tns)) // initializer represents the entire struct
                    {
                        si.addInit(field.ident, initializerSemantic(ix, sc, tn, needInterpret));
                        ++index;
                    }
                    else                                // field initializers for struct
                        si.addInit(field.ident, subStruct(tns, index)); // the first field
                }
                else
                {
                    si.addInit(field.ident, ix);
                    ++index;
                }
            }
            //printf("subStruct() returns ai: %s, index: %d\n", si.toChars(), cast(int)index);
            return si;
        }

        /* If { } are omitted from subarrays, use recursion to reconstruct where
         * brackets go
         * Params:
         *  tsa = subarray to initialize
         *  index = index into ci.initializer, updated
         * Returns: array initializer for this subarray
         */
        Initializer subArray(TypeSArray tsa, ref size_t index)
        {
            //printf("array(tsa: %s, index %d)\n", tsa.toChars(), cast(int)index);
            if (tsa.isIncomplete())
            {
                // C11 6.2.5-20 "element type shall be complete whenever the array type is specified"
                assert(0); // should have been detected by parser
            }

            auto tnsa = tsa.nextOf().toBasetype().isTypeSArray();

            auto ai = new ArrayInitializer(ci.loc);
            ai.isCarray = true;

            foreach (n; 0 .. cast(size_t)tsa.dim.toInteger())
            {
                if (index >= ci.initializerList.length)
                    break;          // ran out of initializers
                auto di = ci.initializerList[index];
                if (di.designatorList)
                    break;          // back to top level
                else if (tnsa && di.initializer.isExpInitializer())
                {
                    ExpInitializer ei = di.initializer.isExpInitializer();
                    if (ei.exp.isStringExp() && tnsa.nextOf().isIntegral())
                    {
                        ai.addInit(null, ei);
                        ++index;
                    }
                    else
                        ai.addInit(null, subArray(tnsa, index));
                }
                else
                {
                    ai.addInit(null, di.initializer);
                    ++index;
                }
            }
            //printf("array() returns ai: %s, index: %d\n", ai.toChars(), cast(int)index);
            return ai;
        }

        if (auto ts = t.isTypeStruct())
        {
            auto si = new StructInitializer(ci.loc);
            StructDeclaration sd = ts.sym;
            sd.size(ci.loc);            // run semantic() on sd to get fields
            if (sd.sizeok != Sizeok.done)
            {
                return err();
            }
            const nfields = sd.fields.length;
            size_t fieldi = 0;

        Loop1:
            for (size_t index = 0; index < ci.initializerList.length; )
            {
                DesigInit di = ci.initializerList[index];
                Designators* dlist = di.designatorList;
                if (dlist)
                {
                    const length = (*dlist).length;
                    if (length == 0 || !(*dlist)[0].ident)
                    {
                        error(ci.loc, "`.identifier` expected for C struct field initializer `%s`", toChars(ci));
                        return err();
                    }
                    if (length > 1)
                    {
                        error(ci.loc, "only 1 designator currently allowed for C struct field initializer `%s`", toChars(ci));
                        return err();
                    }
                    auto id = (*dlist)[0].ident;
                    foreach (k, f; sd.fields[])         // linear search for now
                    {
                        if (f.ident == id)
                        {
                            fieldi = k;
                            si.addInit(id, di.initializer);
                            ++fieldi;
                            ++index;
                            continue Loop1;
                        }
                    }
                    error(ci.loc, "`.%s` is not a field of `%s`\n", id.toChars(), sd.toChars());
                    return err();
                }

                if (fieldi == nfields)
                    break;

                auto ix = di.initializer;

                /* If a C initializer is wrapped in a C initializer, with no designators,
                 * peel off the outer one
                 */
                if (ix.isCInitializer())
                {
                    CInitializer cix = ix.isCInitializer();
                    if (cix.initializerList.length == 1)
                    {
                        DesigInit dix = cix.initializerList[0];
                        if (!dix.designatorList)
                        {
                            Initializer inix = dix.initializer;
                            if (inix.isCInitializer())
                                ix = inix;
                        }
                    }
                }

                if (auto cix = ix.isCInitializer())
                {
                    /* ImportC loses the structure from anonymous structs, but this is retained
                     * by the initializer syntax. if a CInitializer has a Designator, it is probably
                     * a nested anonymous struct
                     */
                    int found;
                    foreach (dix; cix.initializerList)
                    {
                        Designators* dlistx = dix.designatorList;
                        if (!dlistx)
                            continue;
                        if ((*dlistx).length == 1 && (*dlistx)[0].ident)
                        {
                            auto id = (*dlistx)[0].ident;
                            foreach (k, f; sd.fields[])         // linear search for now
                            {
                                if (f.ident == id)
                                {
                                    fieldi = k;
                                    si.addInit(id, dix.initializer);
                                    ++fieldi;
                                    ++index;
                                    ++found;
                                    break;
                                }
                            }
                        }
                        else {
                            error(ci.loc, "only 1 designator currently allowed for C struct field initializer `%s`", toChars(ci));
                        }
                    }

                    if (found == cix.initializerList.length)
                        continue Loop1;
                }

                VarDeclaration field;
                while (1)   // skip field if it overlaps with previously seen fields
                {
                    field = sd.fields[fieldi];
                    ++fieldi;
                    if (!overlaps(field, sd.fields[], si))
                        break;
                    if (fieldi == nfields)
                        break;
                }

                auto tn = field.type.toBasetype();
                auto tnsa = tn.isTypeSArray();
                auto tns = tn.isTypeStruct();

                if (tnsa && ix.isExpInitializer())
                {
                    ExpInitializer ei = ix.isExpInitializer();
                    if (ei.exp.isStringExp() && tnsa.nextOf().isIntegral())
                    {
                        si.addInit(field.ident, ei);
                        ++index;
                    }
                    else
                        si.addInit(field.ident, subArray(tnsa, index));
                }
                else if (tns && ix.isExpInitializer())
                {
                    /* Disambiguate between an exp representing the entire
                     * struct, and an exp representing the first field of the struct
                     */
                    if (representsStruct(ix.isExpInitializer(), tns)) // initializer represents the entire struct
                    {
                        si.addInit(field.ident, initializerSemantic(ix, sc, tn, needInterpret));
                        ++index;
                    }
                    else                                // field initializers for struct
                        si.addInit(field.ident, subStruct(tns, index)); // the first field
                }
                else
                {
                    si.addInit(field.ident, di.initializer);
                    ++index;
                }
            }
            return initializerSemantic(si, sc, t, needInterpret);
        }
        else if (auto ta = t.isTypeSArray())
        {
            auto tn = t.nextOf().toBasetype();  // element type of array

            /* If it's an array of integral being initialized by `{ string }`
             * replace with `string`
             */
            if (tn.isIntegral())
            {
                if (ExpInitializer ei = isBraceExpression())
                {
                    if (ei.exp.isStringExp())
                        return ei.initializerSemantic(sc, t, needInterpret);
                }
            }

            auto tnsa = tn.isTypeSArray();      // array of array
            auto tns = tn.isTypeStruct();       // array of struct

            auto ai = new ArrayInitializer(ci.loc);
            ai.isCarray = true;
            for (size_t index = 0; index < ci.initializerList.length; )
            {
                auto di = ci.initializerList[index];
                if (auto dlist = di.designatorList)
                {
                    const length = (*dlist).length;
                    if (length == 0 || !(*dlist)[0].exp)
                    {
                        error(ci.loc, "`[ constant-expression ]` expected for C array element initializer `%s`", toChars(ci));
                        return err();
                    }
                    if (length > 1)
                    {
                        error(ci.loc, "only 1 designator currently allowed for C array element initializer `%s`", toChars(ci));
                        return err();
                    }
                    //printf("tn: %s, di.initializer: %s\n", tn.toChars(), di.initializer.toChars());
                    auto ix = di.initializer;
                    if (tnsa && ix.isExpInitializer())
                    {
                        // Wrap initializer in [ ]
                        auto ain = new ArrayInitializer(ci.loc);
                        ain.addInit(null, di.initializer);
                        ix = ain;
                        ai.addInit((*dlist)[0].exp, initializerSemantic(ix, sc, tn, needInterpret));
                        ++index;
                    }
                    else if (tns && ix.isExpInitializer())
                    {
                        /* Disambiguate between an exp representing the entire
                         * struct, and an exp representing the first field of the struct
                         */
                        if (representsStruct(ix.isExpInitializer(), tns)) // initializer represents the entire struct
                        {
                            ai.addInit((*dlist)[0].exp, initializerSemantic(ix, sc, tn, needInterpret));
                            ++index;
                        }
                        else                                // field initializers for struct
                            ai.addInit((*dlist)[0].exp, subStruct(tns, index)); // the first field
                    }
                    else
                    {
                        ai.addInit((*dlist)[0].exp, initializerSemantic(ix, sc, tn, needInterpret));
                        ++index;
                    }
                }
                else if (tnsa && di.initializer.isExpInitializer())
                {
                    ExpInitializer ei = di.initializer.isExpInitializer();
                    if (ei.exp.isStringExp() && tnsa.nextOf().isIntegral())
                    {
                        ai.addInit(null, ei);
                        ++index;
                    }
                    else
                        ai.addInit(null, subArray(tnsa, index));
                }
                else if (tns && di.initializer.isExpInitializer())
                {
                    /* Disambiguate between an exp representing the entire
                     * struct, and an exp representing the first field of the struct
                     */
                    if (representsStruct(di.initializer.isExpInitializer(), tns)) // initializer represents the entire struct
                    {
                        ai.addInit(null, initializerSemantic(di.initializer, sc, tn, needInterpret));
                        ++index;
                    }
                    else                                // field initializers for struct
                        ai.addInit(null, subStruct(tns, index)); // the first field
                }
                else
                {
                    ai.addInit(null, initializerSemantic(di.initializer, sc, tn, needInterpret));
                    ++index;
                }
            }
            return initializerSemantic(ai, sc, tx, needInterpret);
        }
        else if (ExpInitializer ei = isBraceExpression())
        {
            return visitExp(ei);
        }
        else
        {
            error(ci.loc, "unrecognized C initializer `%s` for type `%s`", toChars(ci), t.toChars());
            return err();
        }
    }

    mixin VisitInitializer!Initializer visit;
    auto result = visit.VisitInitializer(init);
    result.semanticDone = true;
    return (result !is null) ? result : new ErrorInitializer();
}

/***********************
 * Translate init to an `Expression` in order to infer the type.
 * Params:
 *      init = `Initializer` AST node
 *      sc = context
 * Returns:
 *      an equivalent `ExpInitializer` if successful, or `ErrorInitializer` if it cannot be translated
 */
Initializer inferType(Initializer init, Scope* sc)
{
    Initializer visitVoid(VoidInitializer i)
    {
        error(i.loc, "cannot infer type from void initializer");
        return new ErrorInitializer();
    }

    Initializer visitDefault(DefaultInitializer i)
    {
        error(i.loc, "cannot infer type from default initializer");
        return new ErrorInitializer();
    }

    Initializer visitError(ErrorInitializer i)
    {
        return i;
    }

    Initializer visitStruct(StructInitializer i)
    {
        error(i.loc, "cannot infer type from struct initializer");
        return new ErrorInitializer();
    }

    Initializer visitArray(ArrayInitializer init)
    {
        //printf("ArrayInitializer::inferType() %s\n", toChars());
        Expressions* keys = null;
        Expressions* values = new Expressions(init.value.length);
        Initializer no()
        {
            if (keys)
                error(init.loc, "not an associative array initializer");
            else
                error(init.loc, "cannot infer type from array initializer");
            return new ErrorInitializer();
        }
        const bool isAssoc = init.isAssociativeArray();
        if (isAssoc)
            keys = new Expressions(init.value.length);
        else
            values.zero();

        for (size_t i = 0; i < init.value.length; i++)
        {
            if (isAssoc)
            {
                Expression e = init.index[i];
                if (!e)
                    return no();
                (*keys)[i] = e;
            }
            else
                assert(!init.index[i]); // already asserted by isAssociativeArray()
            Initializer iz = init.value[i];
            if (!iz)
                return no();
            iz = iz.inferType(sc);
            if (iz.isErrorInitializer())
            {
                return iz;
            }
            (*values)[i] = iz.isExpInitializer().exp;
            assert(!(*values)[i].isErrorExp());
        }

        Expression e;
        e = isAssoc
            ? new AssocArrayLiteralExp(init.loc, keys, values)
            : new ArrayLiteralExp(init.loc, null, values);
        auto ei = new ExpInitializer(init.loc, e);
        return ei.inferType(sc);
    }

    Initializer visitExp(ExpInitializer init)
    {
        //printf("ExpInitializer::inferType() %s\n", init.toChars());
        init.exp = init.exp.expressionSemantic(sc);

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (init.exp.op == EXP.type)
            init.exp = resolveAliasThis(sc, init.exp);

        init.exp = resolveProperties(sc, init.exp);
        if (auto se = init.exp.isScopeExp())
        {
            TemplateInstance ti = se.sds.isTemplateInstance();
            if (ti && ti.semanticRun == PASS.semantic && !ti.aliasdecl)
                error(se.loc, "cannot infer type from %s `%s`, possible circular dependency", se.sds.kind(), se.toChars());
            else
                error(se.loc, "cannot infer type from %s `%s`", se.sds.kind(), se.toChars());
            return new ErrorInitializer();
        }

        // Give error for overloaded function addresses
        bool hasOverloads;
        if (auto f = isFuncAddress(init.exp, &hasOverloads))
        {
            if (checkForwardRef(f, init.loc))
            {
                return new ErrorInitializer();
            }
            if (hasOverloads && !f.isUnique())
            {
                error(init.exp.loc, "cannot infer type from overloaded function symbol `%s`", init.exp.toChars());
                return new ErrorInitializer();
            }
        }
        if (auto ae = init.exp.isAddrExp())
        {
            if (ae.e1.op == EXP.overloadSet)
            {
                error(init.exp.loc, "cannot infer type from overloaded function symbol `%s`", init.exp.toChars());
                return new ErrorInitializer();
            }
        }
        if (init.exp.isErrorExp())
        {
            return new ErrorInitializer();
        }
        if (!init.exp.type)
        {
            return new ErrorInitializer();
        }
        return init;
    }

    Initializer visitC(CInitializer i)
    {
        //printf("CInitializer.inferType()\n");
        error(i.loc, "TODO C inferType initializers not supported yet");
        return new ErrorInitializer();
    }

    mixin VisitInitializer!Initializer visit;
    auto result = visit.VisitInitializer(init);
    return (result !is null) ? result : new ErrorInitializer();
}

/***********************
 * Translate init to an `Expression`.
 * Params:
 *      init = `Initializer` AST node
 *      itype = if not `null`, type to coerce expression to
 *      isCfile = default initializers are different with C
 * Returns:
 *      `Expression` created, `null` if cannot, `ErrorExp` for other errors
 */
Expression initializerToExpression(Initializer init, Type itype = null, const bool isCfile = false)
{
    //printf("initializerToExpression() isCfile: %d\n", isCfile);

    Expression visitVoid(VoidInitializer)
    {
        return null;
    }

    Expression visitDefault(DefaultInitializer di)
    {
        return di.type ? di.type.defaultInit(Loc.initial, isCfile) : null;
    }

    Expression visitError(ErrorInitializer)
    {
        return ErrorExp.get();
    }

    /***************************************
     * This works by transforming a struct initializer into
     * a struct literal. In the future, the two should be the
     * same thing.
     */
    Expression visitStruct(StructInitializer)
    {
        // cannot convert to an expression without target 'ad'
        return null;
    }

    /********************************
     * If possible, convert array initializer to array literal.
     * Otherwise return NULL.
     */
    Expression visitArray(ArrayInitializer init)
    {
        //printf("ArrayInitializer::toExpression(), dim = %d\n", dim);
        //static int i; if (++i == 2) assert(0);
        uint edim;      // the length of the resulting array literal
        const(uint) amax = 0x80000000;
        Type t = null;  // type of the array literal being initialized
        if (init.type)
        {
            if (init.type == Type.terror)
            {
                return ErrorExp.get();
            }
            t = init.type.toBasetype();
            switch (t.ty)
            {
            case Tvector:
                t = t.isTypeVector().basetype;
                goto case Tsarray;

            case Tsarray:
                uinteger_t adim = t.isTypeSArray().dim.toInteger();
                if (adim >= amax)
                    return null;
                edim = cast(uint)adim;
                break;

            case Tpointer:
            case Tarray:
                edim = init.dim;
                break;

            default:
                assert(0);
            }
        }
        else
        {
            /* Calculate the length of the array literal
             */
            edim = cast(uint)init.value.length;
            size_t j = 0;
            foreach (i; 0 .. init.value.length)
            {
                if (auto e = init.index[i])
                {
                    if (e.op == EXP.int64)
                    {
                        const uinteger_t idxval = e.toInteger();
                        if (idxval >= amax)
                            return null;
                        j = cast(size_t)idxval;
                    }
                    else
                        return null;
                }
                ++j;
                if (j > edim)
                    edim = cast(uint)j;
            }
        }

        auto elements = new Expressions(edim);
        elements.zero();
        size_t j = 0;
        foreach (i; 0 .. init.value.length)
        {
            if (auto e = init.index[i])
                j = cast(size_t)e.toInteger();
            assert(j < edim);
            if (Initializer iz = init.value[i])
            {
                if (Expression ex = iz.initializerToExpression(null, isCfile))
                {
                    (*elements)[j] = ex;
                    ++j;
                }
                else
                    return null;
            }
            else
                return null;
        }

        /* Fill in any missing elements with the default initializer
         */
        Expression defaultInit = null;  // lazily create it
        foreach (ref element; (*elements)[0 .. edim])
        {
            if (!element)
            {
                if (!init.type) // don't know what type to use
                    return null;
                if (!defaultInit)
                    defaultInit = (cast(TypeNext)t).next.defaultInit(Loc.initial, isCfile);
                element = defaultInit;
            }
        }

        /* Expand any static array initializers that are a single expression
         * into an array of them
         *    e => [e, e, ..., e, e]
         */
        if (t)
        {
            Type tn = t.nextOf().toBasetype();
            if (tn.ty == Tsarray)
            {
                const dim = cast(size_t)(cast(TypeSArray)tn).dim.toInteger();
                Type te = tn.nextOf().toBasetype();
                foreach (ref e; *elements)
                {
                    if (te.equals(e.type))
                    {
                        auto elements2 = new Expressions(dim);
                        foreach (ref e2; *elements2)
                            e2 = e;
                        e = new ArrayLiteralExp(e.loc, tn, elements2);
                    }
                }
            }
        }

        /* If any elements are errors, then the whole thing is an error
         */
        foreach (e; (*elements)[0 .. edim])
        {
            if (e.op == EXP.error)
            {
                return e;
            }
        }

        Expression e = new ArrayLiteralExp(init.loc, init.type, elements);
        return e;
    }

    Expression visitExp(ExpInitializer i)
    {
        if (itype)
        {
            //printf("ExpInitializer::toExpression(t = %s) exp = %s\n", itype.toChars(), i.exp.toChars());
            Type tb = itype.toBasetype();
            Expression e = (i.exp.op == EXP.construct || i.exp.op == EXP.blit) ? (cast(AssignExp)i.exp).e2 : i.exp;
            if (tb.ty == Tsarray && e.implicitConvTo(tb.nextOf()))
            {
                TypeSArray tsa = cast(TypeSArray)tb;
                size_t d = cast(size_t)tsa.dim.toInteger();
                auto elements = new Expressions(d);
                for (size_t j = 0; j < d; j++)
                    (*elements)[j] = e;
                auto ae = new ArrayLiteralExp(e.loc, itype, elements);
                return ae;
            }
        }
        return i.exp;
    }

    Expression visitC(CInitializer i)
    {
        //printf("CInitializer.initializerToExpression(null, true)\n");
        return null;
    }

    mixin VisitInitializer!Expression visit;
    return visit.VisitInitializer(init);
}


/**************************************
 * Determine if expression has non-constant pointers, or more precisely,
 * a pointer that CTFE cannot handle.
 * Params:
 *    e = expression to check
 * Returns:
 *    true if it has non-constant pointers
 */
private bool hasNonConstPointers(Expression e)
{
    static bool checkArray(Expressions* elems)
    {
        foreach (e; *elems)
        {
            if (e && hasNonConstPointers(e))
                return true;
        }
        return false;
    }

    if (e.type.ty == Terror)
        return false;
    if (e.op == EXP.null_)
        return false;
    if (auto se = e.isStructLiteralExp())
    {
        return checkArray(se.elements);
    }
    if (auto ae = e.isArrayLiteralExp())
    {
        if (!ae.type.nextOf().hasPointers())
            return false;
        return checkArray(ae.elements);
    }
    if (auto ae = e.isAssocArrayLiteralExp())
    {
        if (ae.type.nextOf().hasPointers() && checkArray(ae.values))
            return true;
        if (ae.type.isTypeAArray().index.hasPointers())
            return checkArray(ae.keys);
        return false;
    }
    if (auto ae = e.isAddrExp())
    {
        if (ae.type.nextOf().isImmutable() || ae.type.nextOf().isConst())
        {
            return false;
        }
        if (auto se = ae.e1.isStructLiteralExp())
        {
            if (!(se.stageflags & StructLiteralExp.StageFlags.searchPointers))
            {
                const old = se.stageflags;
                se.stageflags |= StructLiteralExp.StageFlags.searchPointers;
                bool ret = checkArray(se.elements);
                se.stageflags = old;
                return ret;
            }
            else
            {
                return false;
            }
        }
        return true;
    }
    if (e.type.ty == Tpointer && !e.type.isPtrToFunction())
    {
        if (e.op == EXP.symbolOffset) // address of a global is OK
            return false;
        if (e.op == EXP.int64) // cast(void *)int is OK
            return false;
        if (e.op == EXP.string_) // "abc".ptr is OK
            return false;
        return true;
    }
    return false;
}

/**
Given the names and values of a `StructInitializer` or `CallExp`,
resolve it to a list of expressions to construct a `StructLiteralExp`.

Params:
    sd = struct
    t = type of struct (potentially including qualifiers such as `const` or `immutable`)
    sc = scope of the expression initializing the struct
    iloc = location of expression initializing the struct
    names = identifiers passed in argument list, `null` entries for positional arguments
    getExp = function that, given an index into `names` and destination type, returns the initializing expression
    getLoc = function that, given an index into `names`, returns a location for error messages

Returns: list of expressions ordered to the struct's fields, or `null` on error
*/
Expressions* resolveStructLiteralNamedArgs(StructDeclaration sd, Type t, Scope* sc,
    Loc iloc, Identifier[] names, scope Expression delegate(size_t i, Type fieldType) getExp,
    scope Loc delegate(size_t i) getLoc
)
{
    //expandTuples for non-identity arguments?
    const nfields = sd.nonHiddenFields();
    auto elements = new Expressions(nfields);
    auto elems = (*elements)[];
    foreach (ref elem; elems)
        elem = null;

    // Run semantic for explicitly given initializers
    // TODO: this part is slightly different from StructLiteralExp::semantic.
    bool errors = false;
    size_t fieldi = 0;
    foreach (j, id; names)
    {
        const argLoc = getLoc(j);
        if (id)
        {
            // Determine `fieldi` that `id` matches
            Dsymbol s = sd.search(iloc, id);
            if (!s)
            {
                s = sd.search_correct(id);
                if (s)
                    error(argLoc, "`%s` is not a member of `%s`, did you mean %s `%s`?", id.toChars(), sd.toChars(), s.kind(), s.toChars());
                else
                    error(argLoc, "`%s` is not a member of `%s`", id.toChars(), sd.toChars());
                return null;
            }
            s.checkDeprecated(iloc, sc);
            s = s.toAlias();

            // Find out which field index `s` is
            for (fieldi = 0; 1; fieldi++)
            {
                if (fieldi >= nfields)
                {
                    error(iloc, "`%s.%s` is not a per-instance initializable field", sd.toChars(), s.toChars());
                    return null;
                }
                if (s == sd.fields[fieldi])
                    break;
            }
        }
        if (nfields == 0)
        {
            error(argLoc, "initializer provided for struct `%s` with no fields", sd.toChars());
            return null;
        }
        if (j >= nfields)
        {
            error(argLoc, "too many initializers for `%s` with %d field%s", sd.toChars(),
                cast(int) nfields, nfields != 1 ? "s".ptr : "".ptr);
            return null;
        }
        if (fieldi >= nfields)
        {
            error(argLoc, "trying to initialize past the last field `%s` of `%s`", sd.fields[nfields - 1].toChars(), sd.toChars());
            return null;
        }

        VarDeclaration vd = sd.fields[fieldi];
        if (elems[fieldi])
        {
            error(argLoc, "duplicate initializer for field `%s`", vd.toChars());
            errors = true;
            elems[fieldi] = ErrorExp.get(); // for better diagnostics on multiple errors
            ++fieldi;
            continue;
        }

        // Check for @safe violations
        if (vd.type.hasPointers)
        {
            if ((!t.alignment.isDefault() && t.alignment.get() < target.ptrsize ||
                    (vd.offset & (target.ptrsize - 1))))
            {
                if (sc.setUnsafe(false, argLoc,
                    "field `%s.%s` assigning to misaligned pointers", sd, vd))
                {
                    errors = true;
                    elems[fieldi] = ErrorExp.get(); // for better diagnostics on multiple errors
                    ++fieldi;
                    continue;
                }
            }
        }

        // Check for overlapping initializations (can happen with unions)
        foreach (k, v2; sd.fields[0 .. nfields])
        {
            if (vd.isOverlappedWith(v2) && elems[k])
            {
                error(elems[k].loc, "overlapping initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                enum errorMsg = "`struct` initializers that contain anonymous unions" ~
                    " must initialize only the first member of a `union`. All subsequent" ~
                    " non-overlapping fields are default initialized";
                if (!sd.isUnionDeclaration())
                    .errorSupplemental(elems[k].loc, errorMsg);
                errors = true;
                continue;
            }
        }

        assert(sc);

        auto ex = getExp(j, vd.type);

        if (ex.op == EXP.error)
        {
            errors = true;
            elems[fieldi] = ErrorExp.get(); // for better diagnostics on multiple errors
            ++fieldi;
            continue;
        }

        elems[fieldi] = doCopyOrMove(sc, ex, null, false);
        ++fieldi;
    }
    if (errors)
        return null;

    return elements;
}
