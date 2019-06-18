/**
 * Semantic analysis of initializers.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/initsem.d, _initsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_initsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/initsem.d
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
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.opover;
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
    Expression e;
    //printf("ArrayInitializer::toAssocArrayInitializer()\n");
    //static int i; if (++i == 2) assert(0);
    const dim = ai.value.dim;
    auto keys = new Expressions(dim);
    auto values = new Expressions(dim);
    for (size_t i = 0; i < dim; i++)
    {
        e = ai.index[i];
        if (!e)
            goto Lno;
        (*keys)[i] = e;
        Initializer iz = ai.value[i];
        if (!iz)
            goto Lno;
        e = iz.initializerToExpression();
        if (!e)
            goto Lno;
        (*values)[i] = e;
    }
    e = new AssocArrayLiteralExp(ai.loc, keys, values);
    return e;
Lno:
    error(ai.loc, "not an associative array initializer");
    return ErrorExp.get();
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
extern(C++) Initializer initializerSemantic(Initializer init, Scope* sc, ref Type tx, NeedInterpret needInterpret)
{
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
        if (t.ty == Tsarray && t.nextOf().toBasetype().ty == Tstruct)
            t = t.nextOf().toBasetype();
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
            const nfields = sd.nonHiddenFields();
            //expandTuples for non-identity arguments?
            auto elements = new Expressions(nfields);
            auto elems = (*elements)[];
            foreach (ref elem; elems)
                elem = null;

            // Run semantic for explicitly given initializers
            // TODO: this part is slightly different from StructLiteralExp::semantic.
            bool errors = false;
            size_t fieldi = 0;
            foreach (j, id; i.field[])
            {
                if (id)
                {
                    /* Determine `fieldi` that `id` matches
                     */
                    Dsymbol s = sd.search(i.loc, id);
                    if (!s)
                    {
                        s = sd.search_correct(id);
                        const initLoc = i.value[j].loc;
                        if (s)
                            error(initLoc, "`%s` is not a member of `%s`, did you mean %s `%s`?", id.toChars(), sd.toChars(), s.kind(), s.toChars());
                        else
                            error(initLoc, "`%s` is not a member of `%s`", id.toChars(), sd.toChars());
                        return err();
                    }
                    s.checkDeprecated(i.loc, sc);
                    s = s.toAlias();

                    // Find out which field index `s` is
                    for (fieldi = 0; 1; fieldi++)
                    {
                        if (fieldi >= nfields)
                        {
                            error(i.loc, "`%s.%s` is not a per-instance initializable field", sd.toChars(), s.toChars());
                            return err();
                        }
                        if (s == sd.fields[fieldi])
                            break;
                    }
                }
                else if (fieldi >= nfields)
                {
                    error(i.loc, "too many initializers for `%s`", sd.toChars());
                    return err();
                }

                VarDeclaration vd = sd.fields[fieldi];
                if (elems[fieldi])
                {
                    error(i.loc, "duplicate initializer for field `%s`", vd.toChars());
                    errors = true;
                    continue;
                }

                // Check for @safe violations
                if (vd.type.hasPointers)
                {
                    if ((t.alignment() < target.ptrsize ||
                         (vd.offset & (target.ptrsize - 1))) &&
                        sc.func && sc.func.setUnsafe())
                    {
                        error(i.loc, "field `%s.%s` cannot assign to misaligned pointers in `@safe` code",
                            sd.toChars(), vd.toChars());
                        errors = true;
                    }
                }

                // Check for overlapping initializations (can happen with unions)
                foreach (k, v2; sd.fields[0 .. nfields])
                {
                    if (vd.isOverlappedWith(v2) && elems[k])
                    {
                        error(i.loc, "overlapping initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                        errors = true;
                        continue;
                    }
                }

                // Convert initializer to Expression `ex`
                assert(sc);
                auto tm = vd.type.addMod(t.mod);
                auto iz = i.value[j].initializerSemantic(sc, tm, needInterpret);
                auto ex = iz.initializerToExpression();
                if (ex.op == TOK.error)
                {
                    errors = true;
                    continue;
                }

                i.value[j] = iz;
                elems[fieldi] = doCopyOrMove(sc, ex);
                ++fieldi;
            }
            if (errors)
                return err();

            // Make a StructLiteralExp out of elements[]
            auto sle = new StructLiteralExp(i.loc, sd, elements, t);
            if (!sd.fill(i.loc, elements, false))
                return err();
            sle.type = t;
            auto ie = new ExpInitializer(i.loc, sle);
            return ie.initializerSemantic(sc, t, needInterpret);
        }
        else if ((t.ty == Tdelegate || t.isPtrToFunction()) && i.value.dim == 0)
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
        //printf("ArrayInitializer::semantic(%s)\n", t.toChars());
        if (i.sem) // if semantic() already run
        {
            return i;
        }
        i.sem = true;
        t = t.toBasetype();
        switch (t.ty)
        {
        case Tsarray:
        case Tarray:
            break;
        case Tvector:
            t = (cast(TypeVector)t).basetype;
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
            if (t.nextOf().ty != Tfunction)
                break;
            goto default;
        default:
            error(i.loc, "cannot use array to initialize `%s`", t.toChars());
            return err();
        }
        i.type = t;
        length = 0;
        for (size_t j = 0; j < i.index.dim; j++)
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
                if (idx.op == TOK.error)
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
            if (ei && ei.exp.op == TOK.tuple)
            {
                TupleExp te = cast(TupleExp)ei.exp;
                i.index.remove(j);
                i.value.remove(j);
                for (size_t k = 0; k < te.exps.dim; ++k)
                {
                    Expression e = (*te.exps)[k];
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
            length++;
            if (length == 0)
            {
                error(i.loc, "array dimension overflow");
                return err();
            }
            if (length > i.dim)
                i.dim = length;
        }
        if (t.ty == Tsarray)
        {
            uinteger_t edim = (cast(TypeSArray)t).dim.toInteger();
            if (i.dim > edim)
            {
                error(i.loc, "array initializer has %u elements, but array length is %llu", i.dim, edim);
                return err();
            }
        }
        if (errors)
            return err();

        const sz = t.nextOf().size();
        bool overflow;
        const max = mulu(i.dim, sz, overflow);
        if (overflow || max >= amax)
        {
            error(i.loc, "array dimension %llu exceeds max of %llu", ulong(i.dim), ulong(amax / sz));
            return err();
        }
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
        if (i.exp.op == TOK.error)
            return err();
        uint olderrors = global.errors;
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
            if (i.exp.implicitConvTo(t))
            {
                i.exp = i.exp.implicitCastTo(sc, t);
            }
            if (!global.gag && olderrors != global.errors)
            {
                return i;
            }
            i.exp = i.exp.ctfeInterpret();
            if (i.exp.op == TOK.voidExpression)
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
        if (i.exp.type.ty == Ttuple && (cast(TypeTuple)i.exp.type).arguments.dim == 0)
        {
            Type et = i.exp.type;
            i.exp = new TupleExp(i.exp.loc, new Expressions());
            i.exp.type = et;
        }
        if (i.exp.op == TOK.type)
        {
            i.exp.error("initializer must be an expression, not `%s`", i.exp.toChars());
            return err();
        }
        // Make sure all pointers are constants
        if (needInterpret && hasNonConstPointers(i.exp))
        {
            i.exp.error("cannot use non-constant CTFE pointer in an initializer `%s`", currExp.toChars());
            return err();
        }
        Type tb = t.toBasetype();
        Type ti = i.exp.type.toBasetype();
        if (i.exp.op == TOK.tuple && i.expandTuples && !i.exp.implicitConvTo(t))
        {
            return new ExpInitializer(i.loc, i.exp);
        }
        /* Look for case of initializing a static array with a too-short
         * string literal, such as:
         *  char[5] foo = "abc";
         * Allow this by doing an explicit cast, which will lengthen the string
         * literal.
         */
        if (i.exp.op == TOK.string_ && tb.ty == Tsarray)
        {
            StringExp se = cast(StringExp)i.exp;
            Type typeb = se.type.toBasetype();
            TY tynto = tb.nextOf().ty;
            if (!se.committed &&
                (typeb.ty == Tarray || typeb.ty == Tsarray) && tynto.isSomeChar &&
                se.numberOfCodeUnits(tynto) < (cast(TypeSArray)tb).dim.toInteger())
            {
                i.exp = se.castTo(sc, t);
                goto L1;
            }
        }

        /* C11 6.7.9-14..15
         * Initialize an array of unknown size with a string.
         * ImportC regards Tarray as an array of unknown size.
         * Change to static array of known size
         */
        if (sc.flags & SCOPE.Cfile && i.exp.op == TOK.string_ && tb.ty == Tarray)
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
            StructDeclaration sd = (cast(TypeStruct)tb).sym;
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
            else if (search_function(sd, Id.call))
            {
                /* https://issues.dlang.org/show_bug.cgi?id=1547
                 *
                 * Look for static opCall
                 *
                 * Rewrite as:
                 *  i.exp = typeof(sd).opCall(arguments)
                 */

                Expression e = typeDotIdExp(i.loc, sd.type, Id.call);
                e = new CallExp(i.loc, e, i.exp);
                e = e.expressionSemantic(sc);
                e = resolveProperties(sc, e);
                if (needInterpret)
                    i.exp = e.ctfeInterpret();
                else
                    i.exp = e.optimize(WANTvalue);
            }
        }
        // Look for the case of statically initializing an array
        // with a single member.
        if (tb.ty == Tsarray && !tb.nextOf().equals(ti.toBasetype().nextOf()) && i.exp.implicitConvTo(tb.nextOf()))
        {
            /* If the variable is not actually used in compile time, array creation is
             * redundant. So delay it until invocation of toExpression() or toDt().
             */
            t = tb.nextOf();
        }
        if (i.exp.implicitConvTo(t))
        {
            i.exp = i.exp.implicitCastTo(sc, t);
        }
        else
        {
            // Look for mismatch of compile-time known length to emit
            // better diagnostic message, as same as AssignExp::semantic.
            if (tb.ty == Tsarray && i.exp.implicitConvTo(tb.nextOf().arrayOf()) > MATCH.nomatch)
            {
                uinteger_t dim1 = (cast(TypeSArray)tb).dim.toInteger();
                uinteger_t dim2 = dim1;
                if (i.exp.op == TOK.arrayLiteral)
                {
                    ArrayLiteralExp ale = cast(ArrayLiteralExp)i.exp;
                    dim2 = ale.elements ? ale.elements.dim : 0;
                }
                else if (i.exp.op == TOK.slice)
                {
                    Type tx = toStaticArrayType(cast(SliceExp)i.exp);
                    if (tx)
                        dim2 = (cast(TypeSArray)tx).dim.toInteger();
                }
                if (dim1 != dim2)
                {
                    i.exp.error("mismatched array lengths, %d and %d", cast(int)dim1, cast(int)dim2);
                    i.exp = ErrorExp.get();
                }
            }
            i.exp = i.exp.implicitCastTo(sc, t);
        }
    L1:
        if (i.exp.op == TOK.error)
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
        if (ci.sem) // if semantic() already run
            return ci;
        //printf("CInitializer::semantic() (%s) %s\n", t.toChars(), ci.toChars());
        ci.sem = true;
        t = t.toBasetype();
        ci.type = t;    // later passes will need this

        auto dil = ci.initializerList[];
        size_t i = 0;   // index into dil[]
        const uint amax = 0x8000_0000;
        bool errors;

        /* If `{ expression }` return the expression initializer
         */
        ExpInitializer isBraceExpression()
        {
            return (dil.length == 1 && !dil[0].designatorList)
                    ? dil[0].initializer.isExpInitializer()
                    : null;
        }

        /* Convert struct initializer into ExpInitializer
         */
        Initializer structs(TypeStruct ts)
        {
            //printf("structs %s\n", ts.toChars());
            StructDeclaration sd = ts.sym;
            sd.size(ci.loc);
            if (sd.sizeok != Sizeok.done)
            {
                errors = true;
                return err();
            }
            const nfields = sd.nonHiddenFields();
            auto elements = new Expressions(nfields);
            auto elems = (*elements)[];
            foreach (ref elem; elems)
                elem = null;

          FieldLoop:
            for (size_t fieldi = 0; fieldi < nfields; ++fieldi)
            {
                if (i == dil.length)
                    break;

                auto di = dil[i];
                if (di.designatorList)
                {
                    error(ci.loc, "C designator-list not supported yet");
                    errors = true;
                    break;
                }

                VarDeclaration vd = sd.fields[fieldi];

                // Check for overlapping initializations (can happen with unions)
                foreach (k, v2; sd.fields[0 .. nfields])
                {
                    if (vd.isOverlappedWith(v2) && elems[k])
                    {
                        continue FieldLoop;     // skip it
                    }
                }

                ++i;

                // Convert initializer to Expression `ex`
                assert(sc);
                auto tm = vd.type.addMod(ts.mod);
                auto iz = di.initializer.initializerSemantic(sc, tm, needInterpret);
                auto ex = iz.initializerToExpression();
                if (ex.op == TOK.error)
                {
                    errors = true;
                    continue;
                }

                elems[fieldi] = ex;
            }
            if (errors)
                return err();

            // Make a StructLiteralExp out of elements[]
            Type tx = ts;
            auto sle = new StructLiteralExp(ci.loc, sd, elements, tx);
            if (!sd.fill(ci.loc, elements, false))
                return err();
            sle.type = tx;
            auto ie = new ExpInitializer(ci.loc, sle);
            return ie.initializerSemantic(sc, tx, needInterpret);
        }

        if (auto ts = t.isTypeStruct())
        {
            auto ei = structs(ts);
            if (errors)
                return err();
            if (i < dil.length)
            {
                error(ci.loc, "%d extra initializer(s) for `struct %s`", cast(int)(dil.length - i), ts.toChars());
                return err();
            }
            return ei;
        }

        auto tsa = t.isTypeSArray();
        auto ta = t.isTypeDArray();
        if (!(tsa || ta))
        {
            /* Not an array. See if it is `{ exp }` which can be
             * converted to an ExpInitializer
             */
            if (ExpInitializer ei = isBraceExpression())
            {
                return ei.initializerSemantic(sc, t, needInterpret);
            }

            error(ci.loc, "C non-array initializer (%s) %s not supported yet", t.toChars(), ci.toChars());
            return err();
        }

        /* If it's an array of integral being initialized by `{ string }`
         * replace with `string`
         */
        auto tn = t.nextOf();
        if (tn.isintegral())
        {
            if (ExpInitializer ei = isBraceExpression())
            {
                if (ei.exp.isStringExp())
                    return ei.initializerSemantic(sc, t, needInterpret);
            }
        }

        /* Support recursion to handle un-braced array initializers
         * Params:
         *    t = element type
         *    dim = max number of elements
         * Returns:
         *    # of elements in array
         */
        size_t array(Type t, size_t dim)
        {
            //printf(" type %s i %d dim %d dil.length = %d\n", t.toChars(), cast(int)i, cast(int)dim, cast(int)dil.length);
            auto tn = t.nextOf().toBasetype();
            if (auto tna = tn.isTypeDArray())
            {
                // C11 6.2.5-20 "element type shall be complete whenever the array type is specified"
                error(ci.loc, "incomplete element type `%s` not allowed", tna.toChars());
                errors = true;
                return 1;
            }
            if (i == dil.length)
                return 0;
            size_t n;
            auto tnsa = tn.isTypeSArray();
            const nelems = tnsa ? cast(size_t)tnsa.dim.toInteger() : 0;

            foreach (j; 0 .. dim)
            {
                auto di = dil[i];
                if (di.designatorList)
                {
                    error(ci.loc, "C designator-list not supported yet");
                    errors = true;
                    break;
                }
                if (tnsa && di.initializer.isExpInitializer())
                {
                    // no braces enclosing array initializer, so recurse
                    array(tnsa, nelems);
                }
                else if (auto tns = tn.isTypeStruct())
                {
                    dil[n].initializer = structs(tns);
                }
                else
                {
                    ++i;
                    auto tnx = tn; // in case initializerSemantic tries to change it
                    di.initializer = di.initializer.initializerSemantic(sc, tnx, needInterpret);
                    if (di.initializer.isErrorInitializer())
                        errors = true;
                    assert(tnx == tn); // sub-types should not be modified
                }
                ++n;
                if (i == dil.length)
                    break;
            }
            //printf(" n: %d i: %d\n", cast(int)n, cast(int)i);
            return n;
        }

        size_t dim = ta ? dil.length : cast(size_t)tsa.dim.toInteger();
        auto n = array(t, dim);

        if (errors)
            return err();

        if (ta) // array of unknown length
        {
            // Change to array of known length
            tsa = new TypeSArray(tn, new IntegerExp(Loc.initial, n, Type.tsize_t));
            tx = tsa;       // rewrite caller's type
            ci.type = tsa;  // remember for later passes
        }
        const uinteger_t edim = tsa.dim.toInteger();
        if (i < dil.length)
        {
            error(ci.loc, "%d extra initializer(s) for static array length of %d", cast(int)(dil.length - i), cast(int)edim);
            return err();
        }

        const sz = tn.size(); // element size
        bool overflow;
        const max = mulu(edim, sz, overflow);
        if (overflow || max >= amax)
        {
            error(ci.loc, "array dimension %llu exceeds max of %llu", ulong(edim), ulong(amax / sz));
            return err();
        }

        return ci;
    }

    final switch (init.kind)
    {
        case InitKind.void_:   return visitVoid  (cast(  VoidInitializer)init);
        case InitKind.error:   return visitError (cast( ErrorInitializer)init);
        case InitKind.struct_: return visitStruct(cast(StructInitializer)init);
        case InitKind.array:   return visitArray (cast( ArrayInitializer)init);
        case InitKind.exp:     return visitExp   (cast(   ExpInitializer)init);
        case InitKind.C_:      return visitC     (cast(     CInitializer)init);
    }
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
        Expressions* values;
        if (init.isAssociativeArray())
        {
            keys = new Expressions(init.value.dim);
            values = new Expressions(init.value.dim);
            for (size_t i = 0; i < init.value.dim; i++)
            {
                Expression e = init.index[i];
                if (!e)
                    goto Lno;
                (*keys)[i] = e;
                Initializer iz = init.value[i];
                if (!iz)
                    goto Lno;
                iz = iz.inferType(sc);
                if (iz.isErrorInitializer())
                {
                    return iz;
                }
                assert(iz.isExpInitializer());
                (*values)[i] = (cast(ExpInitializer)iz).exp;
                assert((*values)[i].op != TOK.error);
            }
            Expression e = new AssocArrayLiteralExp(init.loc, keys, values);
            auto ei = new ExpInitializer(init.loc, e);
            return ei.inferType(sc);
        }
        else
        {
            auto elements = new Expressions(init.value.dim);
            elements.zero();
            for (size_t i = 0; i < init.value.dim; i++)
            {
                assert(!init.index[i]); // already asserted by isAssociativeArray()
                Initializer iz = init.value[i];
                if (!iz)
                    goto Lno;
                iz = iz.inferType(sc);
                if (iz.isErrorInitializer())
                {
                    return iz;
                }
                assert(iz.isExpInitializer());
                (*elements)[i] = (cast(ExpInitializer)iz).exp;
                assert((*elements)[i].op != TOK.error);
            }
            Expression e = new ArrayLiteralExp(init.loc, null, elements);
            auto ei = new ExpInitializer(init.loc, e);
            return ei.inferType(sc);
        }
    Lno:
        if (keys)
        {
            error(init.loc, "not an associative array initializer");
        }
        else
        {
            error(init.loc, "cannot infer type from array initializer");
        }
        return new ErrorInitializer();
    }

    Initializer visitExp(ExpInitializer init)
    {
        //printf("ExpInitializer::inferType() %s\n", init.toChars());
        init.exp = init.exp.expressionSemantic(sc);

        // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
        if (init.exp.op == TOK.type)
            init.exp = resolveAliasThis(sc, init.exp);

        init.exp = resolveProperties(sc, init.exp);
        if (init.exp.op == TOK.scope_)
        {
            ScopeExp se = cast(ScopeExp)init.exp;
            TemplateInstance ti = se.sds.isTemplateInstance();
            if (ti && ti.semanticRun == PASS.semantic && !ti.aliasdecl)
                se.error("cannot infer type from %s `%s`, possible circular dependency", se.sds.kind(), se.toChars());
            else
                se.error("cannot infer type from %s `%s`", se.sds.kind(), se.toChars());
            return new ErrorInitializer();
        }

        // Give error for overloaded function addresses
        bool hasOverloads;
        if (auto f = isFuncAddress(init.exp, &hasOverloads))
        {
            if (f.checkForwardRef(init.loc))
            {
                return new ErrorInitializer();
            }
            if (hasOverloads && !f.isUnique())
            {
                init.exp.error("cannot infer type from overloaded function symbol `%s`", init.exp.toChars());
                return new ErrorInitializer();
            }
        }
        if (init.exp.op == TOK.address)
        {
            AddrExp ae = cast(AddrExp)init.exp;
            if (ae.e1.op == TOK.overloadSet)
            {
                init.exp.error("cannot infer type from overloaded function symbol `%s`", init.exp.toChars());
                return new ErrorInitializer();
            }
        }
        if (init.exp.op == TOK.error)
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
        //printf(CInitializer::inferType()\n");
        error(i.loc, "TODO C inferType initializers not supported yet");
        return new ErrorInitializer();
    }

    final switch (init.kind)
    {
        case InitKind.void_:   return visitVoid  (cast(  VoidInitializer)init);
        case InitKind.error:   return visitError (cast( ErrorInitializer)init);
        case InitKind.struct_: return visitStruct(cast(StructInitializer)init);
        case InitKind.array:   return visitArray (cast( ArrayInitializer)init);
        case InitKind.exp:     return visitExp   (cast(   ExpInitializer)init);
        case InitKind.C_:      return visitC     (cast(     CInitializer)init);
    }
}

/***********************
 * Translate init to an `Expression`.
 * Params:
 *      init = `Initializer` AST node
 *      itype = if not `null`, type to coerce expression to
 * Returns:
 *      `Expression` created, `null` if cannot, `ErrorExp` for other errors
 */
extern (C++) Expression initializerToExpression(Initializer init, Type itype = null)
{
    Expression visitVoid(VoidInitializer)
    {
        return null;
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
            edim = cast(uint)init.value.dim;
            size_t j = 0;
            foreach (i; 0 .. init.value.dim)
            {
                if (auto e = init.index[i])
                {
                    if (e.op == TOK.int64)
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
        foreach (i; 0 .. init.value.dim)
        {
            if (auto e = init.index[i])
                j = cast(size_t)e.toInteger();
            assert(j < edim);
            if (Initializer iz = init.value[i])
            {
                if (Expression ex = iz.initializerToExpression())
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
                    defaultInit = (cast(TypeNext)t).next.defaultInit(Loc.initial);
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
            if (e.op == TOK.error)
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
            Expression e = (i.exp.op == TOK.construct || i.exp.op == TOK.blit) ? (cast(AssignExp)i.exp).e2 : i.exp;
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
        //printf("CInitializer.initializerToExpression()\n");
        return null;
    }

    final switch (init.kind)
    {
        case InitKind.void_:   return visitVoid  (cast(  VoidInitializer)init);
        case InitKind.error:   return visitError (cast( ErrorInitializer)init);
        case InitKind.struct_: return visitStruct(cast(StructInitializer)init);
        case InitKind.array:   return visitArray (cast( ArrayInitializer)init);
        case InitKind.exp:     return visitExp   (cast(   ExpInitializer)init);
        case InitKind.C_:      return visitC     (cast(     CInitializer)init);
    }
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
    if (e.op == TOK.null_)
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
        if ((cast(TypeAArray)ae.type).index.hasPointers())
            return checkArray(ae.keys);
        return false;
    }
    if (auto ae = e.isAddrExp())
    {
        if (auto se = ae.e1.isStructLiteralExp())
        {
            if (!(se.stageflags & stageSearchPointers))
            {
                const old = se.stageflags;
                se.stageflags |= stageSearchPointers;
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
        if (e.op == TOK.symbolOffset) // address of a global is OK
            return false;
        if (e.op == TOK.int64) // cast(void *)int is OK
            return false;
        if (e.op == TOK.string_) // "abc".ptr is OK
            return false;
        return true;
    }
    return false;
}



