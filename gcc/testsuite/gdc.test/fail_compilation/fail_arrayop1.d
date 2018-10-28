// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(11): Error: invalid array operation `a + a` (possible missing [])
---
*/
void test2199(int[] a)  // Issue 2199 - Segfault using array operation in function call (from fail266.d)
{
    test2199(a + a);
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(29): Error: invalid array operation `-a` (possible missing [])
---
*/
void fail323()      // from fail323.d, maybe was a part of issue 3471 fix?
{
    void foo(double[]) {}

    auto a = new double[10],
         b = a.dup,
         c = a.dup,
         d = a.dup;

    foo(-a);
    // a[] = -(b[] * (c[] + 4)) + 5 * d[]; // / 3;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(54): Error: invalid array operation `-a` (possible missing [])
fail_compilation/fail_arrayop1.d(55): Error: invalid array operation `~a` (possible missing [])
fail_compilation/fail_arrayop1.d(56): Error: invalid array operation `a + a` (possible missing [])
fail_compilation/fail_arrayop1.d(57): Error: invalid array operation `a - a` (possible missing [])
fail_compilation/fail_arrayop1.d(58): Error: invalid array operation `a * a` (possible missing [])
fail_compilation/fail_arrayop1.d(59): Error: invalid array operation `a / a` (possible missing [])
fail_compilation/fail_arrayop1.d(60): Error: invalid array operation `a % a` (possible missing [])
fail_compilation/fail_arrayop1.d(61): Error: invalid array operation `a ^^ a` (possible missing [])
fail_compilation/fail_arrayop1.d(62): Error: invalid array operation `a & a` (possible missing [])
fail_compilation/fail_arrayop1.d(63): Error: invalid array operation `a | a` (possible missing [])
fail_compilation/fail_arrayop1.d(64): Error: invalid array operation `a ^ a` (possible missing [])
---
*/
void test3903()
{
    int[] a = [1, 2];
    int[] r;

    r = -a;
    r = ~a;
    r = a + a;
    r = a - a;
    r = a * a;
    r = a / a;
    r = a % a;
    r = a ^^ a;
    r = a & a;
    r = a | a;
    r = a ^ a;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(85): Error: invalid array operation `a += a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(86): Error: invalid array operation `a -= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(87): Error: invalid array operation `a *= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(88): Error: invalid array operation `a /= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(89): Error: invalid array operation `a %= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(90): Error: invalid array operation `a ^= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(91): Error: invalid array operation `a &= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(92): Error: invalid array operation `a |= a[]` (possible missing [])
fail_compilation/fail_arrayop1.d(93): Error: invalid array operation `a ^^= a[]` (possible missing [])
---
*/
void test9459()
{
    int[] a = [1, 2, 3];

    a += a[];
    a -= a[];
    a *= a[];
    a /= a[];
    a %= a[];
    a ^= a[];
    a &= a[];
    a |= a[];
    a ^^= a[];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(111): Error: invalid array operation `x1[] = x2[] * x3[]` because `X` doesn't support necessary arithmetic operations
fail_compilation/fail_arrayop1.d(115): Error: invalid array operation `s2[] += s1[]` because `string` is not a scalar type
fail_compilation/fail_arrayop1.d(119): Error: invalid array operation `pa1[] *= pa2[]` for element type `int*`
---
*/
void test11376()
{
    struct X { }

    auto x1 = [X()];
    auto x2 = [X()];
    auto x3 = [X()];
    x1[] = x2[] * x3[];

    string[] s1;
    string[] s2;
    s2[] += s1[];

    int*[] pa1;
    int*[] pa2;
    pa1[] *= pa2[];
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(131): Error: invalid array operation `a[] <<= 1` (possible missing [])
---
*/
void test11566()
{
    int[] a;
    a[] <<= 1;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayop1.d(147): Error: invalid array operation `a + b` (possible missing [])
fail_compilation/fail_arrayop1.d(148): Error: invalid array operation `x + y` (possible missing [])
fail_compilation/fail_arrayop1.d(149): Error: invalid array operation `"hel" + "lo."` (possible missing [])
---
*/
void test14649()
{
    char[] a, b, r;
    string x, y;

    r[] = a + b;
    r[] = x + y;
    r[] = "hel" + "lo.";
}
