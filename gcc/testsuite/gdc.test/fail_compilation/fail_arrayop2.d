// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(12): Error: array operation `[1, 2, 3] - [1, 2, 3]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(15): Error: invalid array operation `"a" - "b"` (possible missing [])
---
*/
void test2603() // Issue 2603 - ICE(cgcs.c) on subtracting string literals
{
    auto c1 = [1,2,3] - [1,2,3];

    // this variation is wrong code on D2, ICE ..\ztc\cgcs.c 358 on D1.
    string c2 = "a" - "b";
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(37): Error: array operation `-a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(38): Error: array operation `~a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(40): Error: array operation `a[] + a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(41): Error: array operation `a[] - a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(42): Error: array operation `a[] * a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(43): Error: array operation `a[] / a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(44): Error: array operation `a[] % a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(45): Error: array operation `a[] ^ a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(46): Error: array operation `a[] & a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(47): Error: array operation `a[] | a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(48): Error: array operation `a[] ^^ a[]` without destination memory not allowed (possible missing [])
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
fail_compilation/fail_arrayop2.d(74): Error: array operation `a[] + a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(75): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(76): Error: array operation `a[] * a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(77): Error: array operation `a[] / a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(78): Error: array operation `a[] % a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(79): Error: array operation `a[] ^ a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(80): Error: array operation `a[] & a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(81): Error: array operation `a[] | a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(82): Error: array operation `a[] ^^ 10` without destination memory not allowed
fail_compilation/fail_arrayop2.d(83): Error: array operation `-a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(84): Error: array operation `~a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(89): Error: array operation `[1] + a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(90): Error: array operation `[1] + a[]` without destination memory not allowed
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

    // from issue 11992
    int[]   arr1;
    int[][] arr2;
    arr1 ~= [1] + a[];         // NG
    arr2 ~= [1] + a[];         // NG
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(104): Error: array operation `h * y[]` without destination memory not allowed
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
fail_compilation/fail_arrayop2.d(117): Error: array operation `-a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(119): Error: array operation `(-a[])[0..4]` without destination memory not allowed
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
fail_compilation/fail_arrayop2.d(136): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(138): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(139): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(142): Error: array operation `a[] - a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(144): Error: array operation `a[] - a[]` without destination memory not allowed
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
fail_compilation/fail_arrayop2.d(159): Error: array operation `a[] * a[]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(160): Error: array operation `(a[] * a[])[0..1]` without destination memory not allowed
fail_compilation/fail_arrayop2.d(163): Error: array operation `a[] * a[]` without destination memory not allowed (possible missing [])
fail_compilation/fail_arrayop2.d(164): Error: array operation `(a[] * a[])[0..1]` without destination memory not allowed (possible missing [])
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
fail_compilation/fail_arrayop2.d(180): Error: array operation `data[segmentId][28..29] & cast(ubyte)(1 << 0)` without destination memory not allowed
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
fail_compilation/fail_arrayop2.d(194): Error: array operation `a[] + 1` without destination memory not allowed
fail_compilation/fail_arrayop2.d(194): Error: array operation `a[] * 2` without destination memory not allowed
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
fail_compilation/fail_arrayop2.d(245): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(246): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(247): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(252): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(255): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(264): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(267): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(268): Error: array operation `"abc"[] + '\x01'` without destination memory not allowed
fail_compilation/fail_arrayop2.d(271): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(274): Error: ([1] * 6)[0..2] is not an lvalue
fail_compilation/fail_arrayop2.d(277): Error: can only * a pointer, not a 'int[]'
fail_compilation/fail_arrayop2.d(280): Error: [1] * 6 is not an lvalue
fail_compilation/fail_arrayop2.d(283): Error: array operation `da[] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(286): Error: array operation `da[] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(289): Error: [1] * 6 is not an lvalue
fail_compilation/fail_arrayop2.d(290): Error: invalid array operation `[1] * 6 -= 1` for element type `int`
fail_compilation/fail_arrayop2.d(293): Error: [1] * 6 is not an lvalue
fail_compilation/fail_arrayop2.d(294): Error: ([1] * 6)[] is not an lvalue
fail_compilation/fail_arrayop2.d(297): Error: invalid array operation `[1] * 6 += 1` for element type `int`
fail_compilation/fail_arrayop2.d(298): Error: invalid array operation `[1] * 6 *= 2` for element type `int`
fail_compilation/fail_arrayop2.d(299): Error: invalid array operation `[1] * 6 ^^= 3` for element type `int`
fail_compilation/fail_arrayop2.d(302): Error: [1] * 6 is not an lvalue
fail_compilation/fail_arrayop2.d(303): Error: [1] * 6 is not an lvalue
fail_compilation/fail_arrayop2.d(306): Error: '[1] * 6' is not of integral type, it is a int[]
fail_compilation/fail_arrayop2.d(307): Error: '[1] * 6' is not of integral type, it is a int[]
fail_compilation/fail_arrayop2.d(308): Error: '[1] * 6' is not of integral type, it is a int[]
fail_compilation/fail_arrayop2.d(311): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(312): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(315): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(316): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(317): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(320): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(320): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(320): Error: array operation `[1] * 6` without destination memory not allowed
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

    // NewExp.newargs/arguments <- preFunctionParameters
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

    // DeleteExp - e1
    delete ([1] * 6);

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

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop2.d(341): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(344): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(347): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(348): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(349): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(352): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(355): Error: array operation `[1] * 6` without destination memory not allowed
fail_compilation/fail_arrayop2.d(358): Error: array operation `"str"[] + cast(immutable(char))1` without destination memory not allowed
fail_compilation/fail_arrayop2.d(366): Error: CTFE internal error: non-constant value "uvt"[]
---
*/
// Test all statements, which can take arrays as their operands.
void test15407stmt()
{
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
