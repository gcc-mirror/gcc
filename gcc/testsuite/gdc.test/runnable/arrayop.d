// RUNNABLE_PHOBOS_TEST
import std.math;

extern(C) int printf(const char*, ...);

string abc;

template Floating(T)
{
    T[3] a;
    T[3] b;
    T[3] c;

    T[] A()
    {
        printf("A\n");
        abc ~= "A";
        return a;
    }

    T[] B()
    {
        printf("B\n");
        abc ~= "B";
        return b;
    }

    T[] C()
    {
        printf("C\n");
        abc ~= "C";
        return c;
    }

    T D()
    {
        printf("D\n");
        abc ~= "D";
        return 4;
    }


    void testx()
    {
        a = [11, 22, 33];
        b = [1, 2, 3];
        c = [4, 5, 6];

        abc = null;
        A()[] = B()[] + C()[];
        assert(abc == "BCA");
        assert(a[0] == 5);
        assert(a[1] == 7);
        assert(a[2] == 9);

        abc = null;
        A()[] = B()[] + 4;
        assert(abc == "BA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        abc = null;
        A()[] = 4 + B()[];
        assert(abc == "BA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        abc = null;
        A()[] = D() + B()[];
        assert(abc == "DBA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        a = [11, 22, 33];
        abc = null;
        A()[] += B()[];
        assert(abc == "BA");
        assert(a[0] == 12);
        assert(a[1] == 24);
        assert(a[2] == 36);

        a = [11, 22, 33];
        A()[] += 4;
        assert(a[0] == 15);
        assert(a[1] == 26);
        assert(a[2] == 37);

        a = [11, 22, 33];
        A()[] -= 4;
        assert(a[0] == 7);
        assert(a[1] == 18);
        assert(a[2] == 29);

        a = [11, 22, 33];
        A()[] *= 4;
        assert(a[0] == 44);
        assert(a[1] == 88);
        assert(a[2] == 132);

        a = [4, 8, 32];
        A()[] /= 4;
        assert(a[0] == 1);
        assert(a[1] == 2);
        assert(a[2] == 8);

        a = [4, 8, 33];
        A()[] %= 4;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 1);

        a = [11, 22, 33];
        abc = null;
        A()[] += 4 + B()[];
        assert(abc == "BA");
        assert(a[0] == 16);
        assert(a[1] == 28);
        assert(a[2] == 40);

        abc = null;
        A()[] = B()[] - C()[];
        assert(abc == "BCA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == -3);
        assert(a[1] == -3);
        assert(a[2] == -3);

        abc = null;
        A()[] = -B()[] - C()[];
        assert(abc == "BCA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == -5);
        assert(a[1] == -7);
        assert(a[2] == -9);

        abc = null;
        A()[] = B()[] + C()[] * 4;
        assert(abc == "BCA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == 17);
        assert(a[1] == 22);
        assert(a[2] == 27);

        abc = null;
        A()[] = B()[] + C()[] * B()[];
        assert(abc == "BCBA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == 5);
        assert(a[1] == 12);
        assert(a[2] == 21);

        abc = null;
        A()[] = B()[] + C()[] / 2;
        assert(abc == "BCA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == 3);
        assert(a[1] == 4.5);
        assert(a[2] == 6);

        abc = null;
        A()[] = B()[] + C()[] % 2;
        assert(abc == "BCA");
        printf("%Lg, %Lg, %Lg\n", cast(real)a[0], cast(real)a[1], cast(real)a[2]);
        assert(a[0] == 1);
        assert(a[1] == 3);
        assert(a[2] == 3);
    }
}

mixin Floating!(float) Ffloat;
mixin Floating!(double) Fdouble;
mixin Floating!(real) Freal;

void test1()
{
    Ffloat.testx();
    Fdouble.testx();
    Freal.testx();
}

/************************************************************************/

template Integral(T)
{
    T[3] a;
    T[3] b;
    T[3] c;

    T[] A()
    {
        printf("A\n");
        abc ~= "A";
        return a;
    }

    T[] B()
    {
        printf("B\n");
        abc ~= "B";
        return b;
    }

    T[] C()
    {
        printf("C\n");
        abc ~= "C";
        return c;
    }

    T D()
    {
        printf("D\n");
        abc ~= "D";
        return 4;
    }


    void testx()
    {
        a = [11, 22, 33];
        b = [1, 2, 3];
        c = [4, 5, 6];

        abc = null;
        A()[] = B()[] + C()[];
        assert(abc == "BCA");
        assert(a[0] == 5);
        assert(a[1] == 7);
        assert(a[2] == 9);

        abc = null;
        A()[] = B()[] + 4;
        assert(abc == "BA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        abc = null;
        A()[] = 4 + B()[];
        assert(abc == "BA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        abc = null;
        A()[] = D() + B()[];
        assert(abc == "DBA");
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        a = [11, 22, 33];
        abc = null;
        A()[] += B()[];
        assert(abc == "BA");
        assert(a[0] == 12);
        assert(a[1] == 24);
        assert(a[2] == 36);

        a = [11, 22, 33];
        A()[] += 4;
        assert(a[0] == 15);
        assert(a[1] == 26);
        assert(a[2] == 37);

        a = [11, 22, 33];
        A()[] -= 4;
        assert(a[0] == 7);
        assert(a[1] == 18);
        assert(a[2] == 29);

        a = [11, 22, 27];
        A()[] *= 4;
        assert(a[0] == 44);
        assert(a[1] == 88);
        assert(a[2] == 108);

        a = [11, 22, 33];
        A()[] /= 4;
        assert(a[0] == 2);
        assert(a[1] == 5);
        assert(a[2] == 8);

        a = [11, 22, 33];
        A()[] %= 4;
        assert(a[0] == 3);
        assert(a[1] == 2);
        assert(a[2] == 1);

        a = [1, 2, 7];
        A()[] &= 4;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 4);

        a = [1, 2, 7];
        A()[] |= 4;
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 7);

        a = [1, 2, 7];
        A()[] ^= 4;
        assert(a[0] == 5);
        assert(a[1] == 6);
        assert(a[2] == 3);

        a = [11, 22, 33];
        abc = null;
        A()[] += 4 + B()[];
        assert(abc == "BA");
        assert(a[0] == 16);
        assert(a[1] == 28);
        assert(a[2] == 40);

        abc = null;
        A()[] = B()[] - C()[];
        assert(abc == "BCA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == -3);
        assert(a[1] == -3);
        assert(a[2] == -3);

        abc = null;
        A()[] = -B()[] - C()[];
        assert(abc == "BCA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == -5);
        assert(a[1] == -7);
        assert(a[2] == -9);

        abc = null;
        A()[] = B()[] + C()[] * 4;
        assert(abc == "BCA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == 17);
        assert(a[1] == 22);
        assert(a[2] == 27);

        abc = null;
        A()[] = B()[] + C()[] * B()[];
        assert(abc == "BCBA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == 5);
        assert(a[1] == 12);
        assert(a[2] == 21);

        abc = null;
        A()[] = B()[] + C()[] / 2;
        assert(abc == "BCA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == 3);
        assert(a[1] == 4);
        assert(a[2] == 6);

        abc = null;
        A()[] = B()[] + C()[] % 2;
        assert(abc == "BCA");
        printf("%lld, %lld, %lld\n", cast(long)a[0], cast(long)a[1], cast(long)a[2]);
        assert(a[0] == 1);
        assert(a[1] == 3);
        assert(a[2] == 3);

        abc = null;
        A()[] = ~B()[];
        assert(abc == "BA");
        assert(a[0] == ~cast(T)1);
        assert(a[1] == ~cast(T)2);
        assert(a[2] == ~cast(T)3);

        abc = null;
        A()[] = B()[] & 2;
        assert(abc == "BA");
        assert(a[0] == 0);
        assert(a[1] == 2);
        assert(a[2] == 2);

        abc = null;
        A()[] = B()[] | 2;
        assert(abc == "BA");
        assert(a[0] == 3);
        assert(a[1] == 2);
        assert(a[2] == 3);

        abc = null;
        A()[] = B()[] ^ 2;
        assert(abc == "BA");
        assert(a[0] == 3);
        assert(a[1] == 0);
        assert(a[2] == 1);
    }
}

/************************************************************************/

mixin Integral!(byte) Fbyte;
mixin Integral!(short) Fshort;
mixin Integral!(int) Fint;
mixin Integral!(long) Flong;

void test2()
{
    Fbyte.testx();
    Fshort.testx();
    Fint.testx();
    Flong.testx();
}

/************************************************************************/

void test3()
{
    auto a = new double[10], b = a.dup, c = a.dup, d = a.dup;
    a[] = -(b[] * (c[] + 4)) + 5 * d[] / 3.0;
}

/************************************************************************/

void test4()
{
    int[] a, b;
    if (a && b) {}
}

/***************************************************/

void test4662()
{
    immutable double[] nums = [1.0, 2.0];

    static assert(!is(typeof({ nums[] += nums[]; })));
    static assert(!is(typeof({ nums[] -= nums[]; })));
    static assert(!is(typeof({ nums[] /= nums[]; })));
    static assert(!is(typeof({ nums[] += 4; })));
    static assert(!is(typeof({ nums[] /= 7; })));
}

/***************************************************/
// 5284

void bug5284_1()
{
    class C { int v; }

              C [] mda;
    immutable(C)[] ida;
    static assert(!__traits(compiles, (mda[] = ida[])));

              C [1] msa;
    immutable(C)[1] isa;
    static assert(!__traits(compiles, (msa[] = isa[])));

              C  m;
    immutable(C) i;
    static assert(!__traits(compiles, m = i));
}
void bug5284_2a()
{
    struct S { int v; }

              S [] mda;
    immutable(S)[] ida;
    mda[] = ida[];

              S [1] msa;
    immutable(S)[1] isa;
    msa[] = isa[];

              S  m = S();
    immutable(S) i = immutable(S)();
    m = i;
}
void bug5284_2b()
{
    struct S { int v; int[] arr; }

              S [] mda;
    immutable(S)[] ida;
    static assert(!__traits(compiles, (mda[] = ida[])));

              S [1] msa;
    immutable(S)[1] isa;
    static assert(!__traits(compiles, (msa[] = isa[])));

              S  m;
    immutable(S) i;
    static assert(!__traits(compiles, m = i));
}
void bug5284_3()
{
              int [] ma;
    immutable(int)[] ia;
    ma[] = ia[];

    int m;
    immutable(int) i;
    m = i;
}

void test5()
{
    bug5284_1();
    bug5284_2a();
    bug5284_2b();
    bug5284_3();
}

/************************************************************************/

void test6()
{
    int[10] a = [1,2,3,4,5,6,7,8,9,10];
    int[10] b;

    b = a[] ^^ 2;
    assert(b[0] == 1);
    assert(b[1] == 4);
    assert(b[2] == 9);
    assert(b[3] == 16);
    assert(b[4] == 25);
    assert(b[5] == 36);
    assert(b[6] == 49);
    assert(b[7] == 64);
    assert(b[8] == 81);
    assert(b[9] == 100);

    int[10] c = 3;
    b = a[] ^^ c[];
    assert(b[0] == 1);
    assert(b[1] == 8);
    assert(b[2] == 27);
    assert(b[3] == 64);
    assert(b[4] == 125);
    assert(b[5] == 216);
    assert(b[6] == 343);
    assert(b[7] == 512);
    assert(b[8] == 729);
    assert(b[9] == 1000);
}

/************************************************************************/

void test8390() {
    const int[] a = new int[5];
    int[] b = new int[5];
    b[] += a[];
}

/************************************************************************/
// 8651

void test8651()
{
    void test(T)() @safe pure nothrow
    {
        T[3] a = [11, 22, 33];
        T[3] b = [1, 2, 3];
        T[3] c = [4, 5, 6];
        T    d = 4;

        // Arithmetic array ops
        {
            a[] = b[] + c[];
            a[] = b[] + 4;
            a[] = 4 + b[];
            a[] = d + b[];
            a[] += b[];
            a[] += 4;
            a[] -= 4;
            a[] *= 4;
            a[] /= 4;
            a[] %= 4;
            a[] += 4 + b[];
            a[] = b[] - c[];
            a[] = -b[] - c[];
            a[] = b[] + c[] * 4;
            a[] = b[] + c[] * b[];
            a[] = b[] + c[] / 2;
            a[] = b[] + c[] % 2;
        }
        // Bitwise array ops
        static if (is(typeof(T.init & T.init)))
        {
            a[] &= 4;
            a[] |= 4;
            a[] ^= 4;
            a[] = ~b[];
            a[] = b[] & 2;
            a[] = b[] | 2;
            a[] = b[] ^ 2;
        }
    }

    test!float();
    test!double();
    test!real();

    test!byte();
    test!short();
    test!int();
    test!long();
}

/************************************************************************/
// 9656

void test9656()
{
    static class C {}
    static struct S
    {
        immutable int[] narr1;
        immutable int[] narr2;
        immutable C[] carr1;
        immutable C[] carr2;
        this(int n) {
            narr1 = new int[](3); // OK, expected
            narr2 = [1,2,3].dup;  // NG -> OK
            carr1 = [new C].dup;  // NG -> OK

            C c = new C;
            static assert(!__traits(compiles, carr2 = [c]));
        }
    }

    {
        int[] ma = [1,2,3];
        immutable ia = ma.dup;
    }


    {
        static struct V { int val; }
        V[] ma = [V(1), V(2)];
        immutable ia = ma.dup;
    }

    {
        static struct R { int* ptr; }
        R[] ma = [R(new int), R(null)];
        static assert(!__traits(compiles, { immutable ia = rarr.dup; }));
    }

    {
        C[] ma = [new C(), new C()];
        static assert(!__traits(compiles, { immutable ia = carr.dup; }));
    }
}

/************************************************************************/
// 10282

void test10282()
{
    int[3]           a1 = [1, 3, 6];
    int[3]           a2 = [1, 3, 6] * 3;    // OK
    const     int[3] a3 = a1[] * 3;         // OK <- Error
    const     int[3] a4 = [1, 3, 6] * 3;    // OK <- Error
    immutable int[3] a5 = [1, 3, 6] * 3;    // OK <- Error

    assert(a1[0] == 1 && a1[1] == 3 && a1[2] == 6);
    assert(a2[0] == 3 && a2[1] == 9 && a2[2] == 18);
    assert(a3[0] == 3 && a3[1] == 9 && a3[2] == 18);
    assert(a4[0] == 3 && a4[1] == 9 && a4[2] == 18);
    assert(a5[0] == 3 && a5[1] == 9 && a5[2] == 18);
}

/************************************************************************/
// 10433

void test10433()
{
    void foo(T)(in int[] v1, in T v2)
    {
        int[2] r;
        r[] = v1[] + v2[];
    }

    immutable int[] v = [10, 20];
    foo(v, v);
}

/************************************************************************/
// 10684

void test10684a()
{
    int[] a = [0, 0];
    a[] += [10, 20][];
}

void test10684b()
{
    int[] a = [1, 2, 3];
    int[] b = [4, 5, 6];

    // Allow array literal as the operand of array oeration
    a[] += [1, 2, 3];
    assert(a == [2, 4, 6]);

    a[] *= b[] + [1, 1, 1];
    assert(a == [2*(4+1), 4*(5+1), 6*(6+1)]);

    a[] = [9, 8, 7] - [1, 2, 3];
    assert(a == [8, 6, 4]);

    a[] = [2, 4, 6] / 2;
    assert(a == [1,2,3]);

    // Disallow: [1,2,3] is not an lvalue
    static assert(!__traits(compiles, { [1,2,3] = a[] * 2; }));
    static assert(!__traits(compiles, { [1,2,3] += a[] * b[]; }));
}

/************************************************************************/
// 11376

template TL11376(T...)
{
    alias TL11376 = T;
}

auto sumArrs11376(T0, T1)(T0[] a, T1[] b)
{
    a[] += b[]; //no ICE without this line
    return a;
}

static assert(!__traits(compiles, sumArrs11376(TL11376!(string[], string).init)));

/************************************************************************/
// 11525

void test11525()
{
    static struct Complex(T)
    {
        T re, im;

        ref opOpAssign(string op : "*")(Complex z)
        {
            auto temp = re*z.re - im*z.im;
            im = im*z.re + re*z.im;
            re = temp;
            return this;
        }
    }

    auto a = [Complex!double(2, 2)];
    assert(a.length == 1 && a[0].re == 2 && a[0].im == 2);
    a[] *= a[];
    assert(a.length == 1 && a[0].re == 0 && a[0].im == 8);
}

/************************************************************************/
// 12250

void f12250(inout int[] p, inout int[] q, int[] r)
{
    r[] = p[] + q[];
    assert(r == [5,7,9]);
    r[] -= p[] - q[];
    assert(r == [8,10,12]);
}

void test12250()
{
    immutable int[3] x = [1,2,3], y = [4,5,6];
    int[3] z;
    f12250(x[], y[], z[]);
}

/************************************************************************/
// 12179

void test12179()
{
    void foo(int[]) {}
    int[1] a;

    foo(a[] = a[]);
    foo(a[] += a[]);
    foo(a[] -= a[]);
    foo(a[] *= a[]);
    foo(a[] /= a[]);
    foo(a[] %= a[]);
    foo(a[] ^= a[]);
    foo(a[] &= a[]);
    foo(a[] |= a[]);
    foo(a[] ^^= a[]);

    // from issue 11992
    int[]   arr1;
    int[][] arr2;
    arr1 ~= (a[] = [1] + a[]); // OK
    arr2 ~= (a[] = [1] + a[]); // OK
}

/************************************************************************/
// 12780

void test12780()
{
    int ival = 2;
    int[] iarr = [1, 2, 3];
    double dval = 2.0;
    double[] darr = [4, 5, 6];

    double[] oarr = [0, 0, 0];

    // multiply array operations
    oarr[] = dval * iarr[];
    assert(oarr == [dval * iarr[0],
                    dval * iarr[1],
                    dval * iarr[2]]);

    oarr[] = iarr[] / dval;
    assert(oarr == [iarr[0] / dval,
                    iarr[1] / dval,
                    iarr[2] / dval]);

    oarr[] = dval * (ival + iarr[]);
    assert(oarr == [dval * (ival + iarr[0]),
                    dval * (ival + iarr[1]),
                    dval * (ival + iarr[2])]);

    oarr[] = (iarr[] & ival) / dval;
    assert(oarr == [(iarr[0] & ival) / dval,
                    (iarr[1] & ival) / dval,
                    (iarr[2] & ival) / dval]);

    oarr[] = darr[] + iarr[];
    assert(oarr == [darr[0] + iarr[0],
                    darr[1] + iarr[1],
                    darr[2] + iarr[2]]);

    oarr[] = iarr[] - darr[];
    assert(oarr == [iarr[0] - darr[0],
                    iarr[1] - darr[1],
                    iarr[2] - darr[2]]);

    oarr[] = darr[] * (ival & iarr[]);
    assert(oarr == [darr[0] * (ival & iarr[0]),
                    darr[1] * (ival & iarr[1]),
                    darr[2] * (ival & iarr[2])]);

    oarr[] = (iarr[] ^ ival) / darr[];
    assert(oarr == [(iarr[0] ^ ival) / darr[0],
                    (iarr[1] ^ ival) / darr[1],
                    (iarr[2] ^ ival) / darr[2]]);
}

/************************************************************************/
// 13497

void test13497()
{
    int[1] a = [2], b = [3];
    int[1] c1 =  a[] * b[];
    int[1] c2 = (a[] * b[])[];
    assert(c1 == [6]);
    assert(c2 == [6]);
}

/************************************************************************/
// 14649

void test14649()
{
    char[] a = "abc".dup;
    char[] b = [char(1), char(2), char(3)];
    string x = "abc";
    string y = [char(1), char(2), char(3)];
    char[] r = new char[](3);

    r[] = a[] + b[];
    assert(r == "bdf");

    r[] = x[] + y[];
    assert(r == "bdf");

    r[] = "hel"[] + "lo."[];
    assert(r == [('h'+'l'), ('e'+'o'), ('l'+'.')]);

    enum s = "abc";
    r[] = s[0..3] + "def"[0..3];
    assert(r == [('a'+'d'), ('b'+'e'), ('c'+'f')]);
}

/************************************************************************/
// 14851

void test14851()
{
    int[8] a, b, c;

    c   = a[] | b[];    // OK <- NG from 2.068.0-b2
    c   = a[] ^ b[];    // OK <- NG from 2.068.0-b2

    c[] = a[] | b[];    // OK
    c[] = a[] ^ b[];    // OK
}

/************************************************************************/

int main()
{
    version(X86)
    {
        test1();
        test2();
    }
    else version(X86_64)
    {
        test1();
        test2();
    }
    else
    {
        //pragma(msg, "Test skipped because arrayop evaluation order is ill-defined.");
    }
    test3();
    test4();
    test5();
    test6();
    test8390();
    test8651();
    test9656();
    test10282();
    test10433();
    test10684a();
    test10684b();
    test11525();
    test12250();
    test12780();
    test13497();
    test14649();
    test14851();

    printf("Success\n");
    return 0;
}


version (none)
{
extern (C) T[] _arraySliceSliceAddSliceAssignd(T[] a, T[] c, T[] b)
{
    foreach (i; 0 .. a.length)
        a[i] = b[i] + c[i];
    return a;
}
}
