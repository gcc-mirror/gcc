// REQUIRED_ARGS: -o-


/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(13): Error: array operation `[1, 2, 3] - [1, 2, 3]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(16): Error: invalid array operation `"a" - "b"` (possible missing [])
---
*/
void test2603() // https://issues.dlang.org/show_bug.cgi?id=2603 - ICE(cgcs.c) on subtracting string literals
{
    auto c1 = [1,2,3] - [1,2,3];

    // this variation is wrong code on D2, ICE ..\ztc\cgcs.c 358 on D1.
    string c2 = "a" - "b";
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(38): Error: array operation `-a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(39): Error: array operation `~a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(41): Error: array operation `a[] + a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(42): Error: array operation `a[] - a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(43): Error: array operation `a[] * a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(44): Error: array operation `a[] / a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(45): Error: array operation `a[] % a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(46): Error: array operation `a[] ^ a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(47): Error: array operation `a[] & a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(48): Error: array operation `a[] | a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(49): Error: array operation `a[] ^^ a[]` without destination memory not allowed (possible missing [])
---
*/
void test9459()
{
    int[] a = [1, 2, 3];
    a = -a[];
    a = ~a[];

    a = a[] + a[];
    a = a[] - a[];
    a = a[] * a[];
    a = a[] / a[];
    a = a[] % a[];
    a = a[] ^ a[];
    a = a[] & a[];
    a = a[] | a[];
    a = a[] ^^ a[];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(75): Error: array operation `a[] + a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(76): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(77): Error: array operation `a[] * a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(78): Error: array operation `a[] / a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(79): Error: array operation `a[] % a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(80): Error: array operation `a[] ^ a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(81): Error: array operation `a[] & a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(82): Error: array operation `a[] | a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(83): Error: array operation `a[] ^^ 10` without destination memory not allowed
fail_compilation/fail_arrayop2.d(84): Error: array operation `-a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(85): Error: array operation `~a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(90): Error: array operation `[1] + a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(91): Error: array operation `[1] + a[]` without destination memory not allowed
---
*/
void test12179()
{
    void foo(int[]) {}
    int[1] a;

    foo(a[] + a[]);
    foo(a[] - a[]);
    foo(a[] * a[]);
    foo(a[] / a[]);
    foo(a[] % a[]);
    foo(a[] ^ a[]);
    foo(a[] & a[]);
    foo(a[] | a[]);
    foo(a[] ^^ 10);
    foo(-a[]);
    foo(~a[]);

    // from https://issues.dlang.org/show_bug.cgi?id=11992
    int[]   arr1;
    int[][] arr2;
    arr1 ~= [1] + a[];         // NG
    arr2 ~= [1] + a[];         // NG
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(105): Error: array operation `h * y[]` without destination memory not allowed
---
*/
void test12381()
{
    double[2] y;
    double h;

    double[2] temp1 = cast(double[2])(h * y[]);
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(118): Error: array operation `-a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(120): Error: array operation `(-a[])[0..4]` without destination memory not allowed
---
*/
float[] test12769(float[] a)
{
    if (a.length < 4)
        return -a[];
    else
        return (-a[])[0..4];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(137): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(139): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(140): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(143): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(145): Error: array operation `a[] - a[]` without destination memory not allowed
---
*/
void test13208()
{
    int[] a;

    auto arr = [a[] - a[]][0];

    auto aa1 = [1 : a[] - a[]];
    auto aa2 = [a[] - a[] : 1];

    struct S { int[] a; }
    auto s = S(a[] - a[]);

    auto n = int(a[] - a[]);
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(160): Error: array operation `a[] * a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(161): Error: array operation `(a[] * a[])[0..1]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(164): Error: array operation `a[] * a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(165): Error: array operation `(a[] * a[])[0..1]` without destination memory not allowed (possible missing [])
---
*/
void test13497()
{
    int[1] a;
    auto b1 = (a[] * a[])[];
    auto b2 = (a[] * a[])[0..1];

    int[] c;
    c = (a[] * a[])[];
    c = (a[] * a[])[0..1];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(181): Error: array operation `data[segmentId][28..29] & cast(ubyte)(1 << 0)` without destination memory not allowed
---
*/
void test13910()
{
    ubyte[][] data;
    size_t segmentId;

    bool isGroup()
    {
        return !!((data[segmentId][28..29]) & (1 << 0));
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(195): Error: array operation `a[] + 1` without destination memory not allowed
fail_compilation/fail_arrayop2.d(195): Error: array operation `a[] * 2` without destination memory not allowed
---
*/
void test14895()
{
    int[] a;
    int[] b = (a[] + 1) ~ a[] * 2;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(247): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(248): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(249): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(253): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(256): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(265): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(268): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(269): Error: array operation `"abc"[] + '\x01'` without destination memory not allowed
fail_compilation/fail_arrayop2.d(272): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(275): Error: cannot take address of expression `([1] * 6)[0..2]` because it is not an lvalue
fail_compilation/fail_arrayop2.d(278): Error: can only `*` a pointer, not a `int[]`
fail_compilation/fail_arrayop2.d(284): Error: array operation `da[] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(287): Error: array operation `da[] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(290): Error: cannot modify expression `[1] * 6` because it is not an lvalue
fail_compilation/fail_arrayop2.d(291): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(294): Error: cannot modify expression `[1] * 6` because it is not an lvalue
fail_compilation/fail_arrayop2.d(295): Error: cannot modify expression `([1] * 6)[]` because it is not an lvalue
fail_compilation/fail_arrayop2.d(298): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(299): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(300): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(303): Error: cannot modify expression `[1] * 6` because it is not an lvalue
fail_compilation/fail_arrayop2.d(304): Error: cannot modify expression `[1] * 6` because it is not an lvalue
fail_compilation/fail_arrayop2.d(307): Error: `[1] * 6` is not of integral type, it is a `int[]`
fail_compilation/fail_arrayop2.d(308): Error: `[1] * 6` is not of integral type, it is a `int[]`
fail_compilation/fail_arrayop2.d(309): Error: `[1] * 6` is not of integral type, it is a `int[]`
fail_compilation/fail_arrayop2.d(312): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(313): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(316): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(317): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(318): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(321): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(321): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(321): Error: array operation `[1] * 6` without destination memory not allowed
---
*/


// Test all expressions, which can take arrays as their operands but cannot be a part of array operation.
void test15407exp()
{
    struct S { int[] a; }
    void f(int[] a) {}

    int[] da;
    int[6] sa;

    { auto r = [[1] * 6]; }     // ArrayLiteralExp
    { auto r = [[1] * 6 :
                [1] * 6]; }     // AssocArrayLiteralExp

    //TupleExp
    // StructLiteralExp.elements <- preFunctionParameters in CallExp
    { auto r = S([1] * 6); }

    // NewExp.arguments <- preFunctionParameters
    { auto r = new S([1] * 6); }

    // TODO: TypeidExp
    //auto ti = typeid([1] * 6);
    //auto foo(T)(T t) {}
    //foo(typeid([1] * 6));
    //auto a = [typeid([1] * 6)];

    // CommaExp.e1
    { auto r = ([1] * 6, 1); }

    // AssertExp
    assert([1] * 6,
           cast(char)1 + "abc"[]);

    // CallExp.arguments <- preFunctionParameters
    f([1] * 6);

    // AddrExp, if a CT-known length slice can become an TypeSarray lvalue in the future.
    { auto r = &(([1] * 6)[0..2]); }

    // PtrExp, *([1] * 6).ptr is also invalid -> show better diagnostic
    { auto r = *([1] * 6); }




    // TypeDArray.dotExp, cannot check in ArrayLengthExp.semantic()
    { auto r = (6 * da[]).length; }

    // IndexExp - e1
    { auto x1 = (da[] * 6)[1]; }

    // Pre, PostExp - e1
    ([1] * 6)++;
    --([1] * 6);

    // AssignExp e1
    ([1] * 6) = 10;
    ([1] * 6)[] = 10;

    // BinAssignExp e1
    ([1] * 6) += 1;
    ([1] * 6)[] *= 2;
    ([1] * 6)[] ^^= 3;

    // CatExp e1
    ([1] * 6) ~= 1;
    ([1] * 6)[] ~= 2;

    // Shl, Shr, UshrExp - e1, e2 --> checkIntegralBin
    { auto r = ([1] * 6) << 1; }
    { auto r = ([1] * 6) >> 1; }
    { auto r = ([1] * 6) >>> 1; }

    // AndAnd, OrOrExp - e1, e2
    { auto r = sa[0..5] && [1] * 6; }
    { auto r = sa[0..5] || [1] * 6; }

    // Cmp, Equal, IdentityExp - e1, e2
    { auto r = sa[0..5] <= [1] * 6; }
    { auto r = sa[0..5] == [1] * 6; }
    { auto r = sa[0..5] is [1] * 6; }

    // CondExp - econd, e1, e2
    { auto r = [1] * 6 ? [1] * 6 : [1] * 6; }
}

/* TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(342): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(345): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(348): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(349): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(350): Deprecation: `[1] * 6` has no effect
fail_compilation/fail_arrayop2.d(350): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(353): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(356): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(359): Error: array operation `"str"[] + cast(immutable(char))1` without destination memory not allowed
fail_compilation/fail_arrayop2.d(367): Error: CTFE internal error: non-constant value `"uvt"`
fail_compilation/fail_arrayop2.d(367): Error: `"uvt"[] - '\x01'` cannot be interpreted at compile time
---
*/
// Test all statements, which can take arrays as their operands.
void test15407stmt() {
    // ExpStatement - exp
    [1] * 6;

    // DoStatement - condition
    do {} while ([1] * 6);

    // ForStatement - condition, increment
    for ([1] * 6;       // init == ExpStatement
         [1] * 6;
         [1] * 6) {}

    // ForeachStatement - aggr -> lowered to ForStatement
    foreach (e; [1] * 6) {}

    // IfStatement condition
    if ([1] * 6) {}

    // SwitchStatement - condition
    switch ("str"[] + 1)
    {
        case "tus":         break;
        default:            break;
    }
    // CaseStatement - exp
    switch ("tus")
    {
        case "uvt"[] - 1:   break;
        default:            break;
    }
}
