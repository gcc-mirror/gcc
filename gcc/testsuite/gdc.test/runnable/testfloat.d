/* PERMUTE_ARGS: -O
 * Test floating point code generation
 */

import core.stdc.stdio;
import core.stdc.stdlib;

double value_1() {
    return 1;
}

double value_2() {
    return 2;
}

/***************************************/

void testcse1(T)()  // common subexpressions
{
    T a = value_1();
    T b = value_2();
    T x = a*a + a*a + a*a + a*a + a*a + a*a + a*a +
               a*b + a*b;
    printf("%g\n", cast(double)x);  // destroy scratch reg contents
    T y = a*a + a*a + a*a + a*a + a*a + a*a + a*a +
               a*b + a*b;
    assert(x == 11);
    assert(x == y);
}

void test240()
{
    testcse1!float();
    testcse1!double();
    testcse1!real();
}

/***************************************/

void testcse2(T)()  // common subexpressions
{
    T a = value_1();
    T b = value_2();
    T x = a*a + a*a + a*a + a*a + a*a + a*a + a*a +
               a*b + a*b + 1;
    printf("%g\n", cast(double)x);  // destroy scratch reg contents
    int i = (a*a + a*a + a*a + a*a + a*a + a*a + a*a + a*b + a*b) != 0;
    assert(i);
    assert(x == 12);
}

void test241()
{
    testcse2!float();
    testcse2!double();
    testcse2!real();
}

/***************************************/

void test1(float f)
{
    real r = f;
    double d = f;
}

void test2(long l)
{
    real r = l;
    double d = l;
}

void test3(float f)
{
    real r = f * f;
    double d = f * f;
}

void test3(long l)
{
    real r = l * l;
    double d = l * l;
}

/***************************************/

double foo4(int i, double d)
{
    return ((i << 1) - d) + ((i << 1) - d);
}

void test4()
{
    double d = foo4(3, 4);
    assert(d == 4);
}

/***************************************/

import core.math; // trigger use of sqrt intrinsic

void test5x(double p)
{
    bool b = p >= 0;
    double mean = (1 - p) / p;
    double skew = sqrt(1 - p);
}

void test5()
{
    test5x(2);
}

/***************************************/

import core.math; // trigger use of sqrt intrinsic

void dstatsEnforce(bool, string) { }

ulong invNegBinomCDF(double pVal, ulong n, double p)
{
    dstatsEnforce(p >= 0 && p <= 1,
        "p must be between 0, 1 for negative binomial distribution.");
    dstatsEnforce(pVal >= 0 && pVal <= 1,
        "P-values must be between 0, 1.");

    // Normal or gamma approx, then adjust.
    double mean = n * (1 - p) / p;
    double var = n * (1 - p) / (p * p);
    double skew = (2 - p) / sqrt(n * (1 - p));
    double kk = 4.0L / (skew * skew);
    double theta = sqrt(var / kk);
    double offset = (kk * theta) - mean + 0.5L;
    ulong guess;
    return 0;
}

void test6()
{
    invNegBinomCDF(2.0, 3, 4.0);
}

/***************************************/

float expDigamma(F)(in F x)
{
    return x;
}

float nextDown(float f) { return f; }

void test7()
{
    foreach (i; 1 .. 2)
    {
        assert(expDigamma(float(i)) >= expDigamma(float(i).nextDown));
    }
}

/***************************************/

void foo8_1(double x)
{
    printf("x = %g\n", x);
    assert(x == 0);
}

void foo8_2(double x)
{
    printf("x = %g\n", x);
    assert(x != 0);
}

void test8()
{
    foo8_1(0.0);
    foo8_2(1.0);
}

/***************************************/

void test9()
{
    double a = 9;
    double b = 3;
    double c = a * b + 1;
    double d = a + b + 1;
    printf("%g %g\n", c, d);    // clobber XMM registers
    assert(c == 28 && d == 13);
    double e = a * b - 1;
    double f = a + b - 1;       // reload 2 CSEs
    printf("%g %g\n", e, f);
    assert(e == 26 && f == 11);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=20349

double f20349(double a, int b)
{
    import core.math;
    return core.math.sqrt(-a / b) / b;
}

void test20349()
{
    assert(f20349(-9, 1) == 3);
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=20963

void test20963()
{
    ulong v = 0xE3251BACB112CB8B;
    double d = cast(double)v;
    printf("%a\n", d); //0x1.c64a37596225ap+63
    assert(d == 0x1.c64a375962259p+63);
}

/***************************************/


int main()
{
    test240();
    test241();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test20349();
    test20963();

    printf("Success\n");
    return EXIT_SUCCESS;
}
