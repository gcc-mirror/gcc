/*
REQUIRED_ARGS: -d
TEST_OUTPUT:
---
---
*/

import core.stdc.math : isnan, signbit;
import core.stdc.stdio;

template AliasSeq(T...) { alias T AliasSeq; }

/************************************/

static assert(-(6i) == -6i);
static assert(-(1 + 6i) == -1 - 6i);

static assert(!3.7i == 0);
static assert(!0.0i == 1);
static assert(!(2+3.7i) == 0);
static assert(!(0+3.7i) == 0);
static assert(!(2+0.0i) == 0);
static assert(!(0+0.0i) == 1);

static assert(-6i + 2i == -4i);
static assert(6i - 1i == 5i);

static assert((3.6 + 7.2i) / (1 + 0i) == 3.6 + 7.2i);
static assert((3.6 + 7.2i) / (0.0 + 1i) == 7.2 - 3.6i);

static assert((7.2i < 6.2i) == 0);

static assert((7.2i == 6.2i) == 0);
static assert((7.2i != 6.2i) == 1);

static assert((7.2i == 7.2i) == 1);
static assert((7.2i != 7.2i) == 0);

static assert((5.1 is 5.1i) == 0);
static assert((5.1 !is 5.1i) == 1);

/***************************************/

ireal f2() { return 1i; }

void test2()
{
    creal v = 0+0i;

    v += f2();
    assert(v == 0 + 1i);

    v = v + f2();
    assert(v == 0 + 2i);
}

/***************************************/

cdouble[1] a3;
cdouble[1] b3;

cdouble[] concat3() {
        return a3~b3;
}

void test3()
{
        a3[]=0.5+1.0i;
        b3[]=0.5+3.0i;

        cdouble[] arr=concat3();

        assert(arr.length==2);
        assert(arr[0]==0.5+1.0i);
        assert(arr[1]==0.5+3.0i);
}

/***************************************/

creal[1] a4;
creal[1] b4;

creal[] concat4() {
        return a4~b4;
}

void test4()
{
        a4[]=0.5+1.0i;
        b4[]=0.5+3.0i;

        creal[] arr=concat4();

        assert(arr.length==2);
        assert(arr[0]==0.5+1.0i);
        assert(arr[1]==0.5+3.0i);
}

/***************************************/

void test5()
{
        ifloat i=1.0fi;
//      i += 2.2;
//      assert(i == 1i);
}

/***************************************/

void test6()
{
        float i=1.0f;
//      i /= 2.2fi;
//      assert(i == 0);
}

/***************************************/

void test7()
{
        creal x=1.0i+2.0;
        creal[] arr;

        arr = arr ~ x;
        assert(arr.length==1);
        assert(arr[0]==1.0i+2.0);

        x=0.0i+5.0;
        assert(arr[0]==1.0i+2.0);
}

/****************************************/

creal[1] a8;
creal[1] b8;

creal[] concat8() {
    return a8 ~ b8;
}

void test8()
{
    a8[]=0.5L+1.0Li;
    b8[]=0.5L+3.0Li;

    creal[] arr=concat8();

    assert(arr.length==2);
    assert(arr[0]==0.5L+1.0Li);
    assert(arr[1]==0.5L+3.0Li);
}

/***************************************/

creal[1] a9;
creal[1] b9;

creal[] concat9() {
    return a9~b9;
}

void test9()
{
    a9[]=0.5L+1.0Li;
    b9[]=0.5L+3.0Li;

    creal[] arr=concat9();

    assert(arr.length==2);
    assert(arr[0]==0.5L+1.0Li);
    assert(arr[1]==0.5L+3.0Li);
}


/***************************************/

void test10()
{
    ifloat a = 1.0i;
    assert(a.im == 1.0);

    const ifloat b = 2.0i;
    static assert(b.im == 2.0); // FAIL

}

/***************************************/

void test11()
{
    real r = real.nan;
    assert( r!=0 );
    if (r==0) assert(0);

    ireal ir = ireal.nan;
    assert( ir!=0 );
    assert( ir!=0i );
    if (ir==0) assert(0);
    if (ir==0i) assert(0);

    creal cr = creal.nan;
    assert( cr!=0 );
    assert( cr!=0i );
    if (cr==0) assert(0);
    if (cr==0i) assert(0);

    double d = double.nan;
    assert( d!=0 );
    if (d==0) assert(0);

    idouble id = idouble.nan;
    assert( id!=0 );
    assert( id!=0i );
    if (id==0) assert(0);
    if (id==0i) assert(0);

    cdouble cd = cdouble.nan;
    assert( cd!=0 );
    assert( cd!=0i );
    if (cd==0) assert(0);
    if (cd==0i) assert(0);

    float f = float.nan;
    assert( f!=0 );
    if (f==0) assert(0);

    ifloat ifx = ifloat.nan;
    assert( ifx!=0 );
    assert( ifx!=0i );
    if (ifx==0) assert(0);
    if (ifx==0i) assert(0);

    cfloat cf = cfloat.nan;
    assert( cf!=0 );
    assert( cf!=0i );
    if (cf==0) assert(0);
    if (cf==0i) assert(0);
}

/***************************************/

void test12()
{
    real x = 3;
    creal a = (2 + 4i) % 3;
    printf("%Lg %Lgi\n", a.re, a.im);
    assert(a == 2 + 1i);

    creal b = (2 + 4i) % x;
    printf("%Lg %Lgi\n", b.re, b.im);
    assert(b == a);
}

/***************************************/

void test13()
{
        ireal a = 5i;
        ireal b = a % 2;
        printf("%Lg %Lgi\n", b.re, b.im);
        assert(b == 1i);
}

/***************************************/

cdouble inv( cdouble expr )
{
    return (1.0 + 0.0i) / expr;
}

/***************************************/

void test14()
{
    cfloat c;
    cfloat d;
    assert(c != d);

    cdouble e;
    cdouble f;
    assert(e != f);

    creal g;
    creal h;
    assert(g != h);
}

/***************************************/

void test7581()
{
    cfloat a() { return cfloat.nan; }
    assert(a() != 0);
}

/***************************************/

float f() { return 1.0f; }
ifloat i() { return 1.0fi; }

void test7594()
{
    assert(f() + i() == 1.0f + 1.0fi);
}

/***************************************/

cdouble conv(cfloat a)
{
    return a;
}

void test7593()
{
    assert(conv(1.0f+1.0fi) == 1.0+1.0i);
}

/***************************************/

cfloat get() { return cfloat.nan; }

void test7591()
{
    assert(!(get() == 0));
}

/***************************************/

void foo8966(cfloat x)
{
    assert(x.re == 3.0f);
}

__gshared cfloat[] a8966;

void test8966()
{
    a8966 = new cfloat[2];
    a8966[0] = 3.0f + 1.0fi;
    foo8966(a8966[0]);
}

/***************************************/

void formatTest2(cfloat s, double re, double im)
{
    assert(s.re == re);
    assert(s.im == im);
}

cfloat getcf()
{
    return 2 + 1i;
}

void test10677()
{
    formatTest2( getcf(), 2, 1 );
}

/***************************************/

void test7806()
{
    for (idouble i = -2i; i <= 2i; i += .125i)
        for (double r = -2; r <= 2; r += .0625)
        {
            cdouble c = r + i;
            printf("%g %gi\n", c.re, c.im);
        }
}

/***************************************/

void test7976() {
    creal[] a = new creal[2];
    auto b = a[0] = a[1];
}

/***************************************/

cfloat foo15f(ifloat re, float im)
{
    return re + im;
}

cfloat bar15f(float re, ifloat im)
{
    return re + im;
}

cdouble foo15(idouble re, double im)
{
    return re + im;
}

cdouble bar15(double re, idouble im)
{
    return re + im;
}

creal foo15r(ireal re, real im)
{
    return re + im;
}

creal bar15r(real re, ireal im)
{
    return re + im;
}

void test15()
{
    assert(foo15f(1.0fi, 2.0f) == 2.0f + 1.0fi);
    assert(bar15f(1.0f, 2.0fi) == 1.0f + 2.0fi);

    assert(foo15(1.0i, 2.0) == 2.0 + 1.0i);
    assert(bar15(1.0, 2.0i) == 1.0 + 2.0i);

    assert(foo15r(1.0Li, 2.0L) == 2.0L + 1.0Li);
    assert(bar15r(1.0L, 2.0Li) == 1.0L + 2.0Li);
}

/************************************/

void test16()
{
     real n = -0.0;
     const real m = -0.0;

     creal c = -0.0 + 3i;
     creal d = n + 3i;
     creal e = m + 3i;

     assert(signbit(c.re) != 0);
     assert(signbit(d.re) != 0);
     assert(signbit(e.re) != 0);
}

/************************************/

void test17()
{
    void test(cdouble v)
    {
            auto x2 = cdouble(v);
            assert(x2 == v);
    }
    test(1.2+3.4i);
}

/************************************/

template factorial18(float n, cdouble c, string sss, string ttt)
{
    static if (n == 1)
        const float factorial18 = 1;
    else
        const float factorial18 = n * 2;
}

template bar18(wstring abc, dstring def)
{
    const int x = 3;
}

void test18()
{
    float f = factorial18!(4.25, 6.8+3i, "hello", null);
    printf("%g\n", f);
    assert(f == 8.5);
    int i = bar18!("abc"w, "def"d).x;
    printf("%d\n", i);
    assert(i == 3);
}

/*****************************************/

void test19()
{
    float f;
    double d;
    real r;

    if (f > ifloat.max)
        goto Loverflow;
    if (d > ifloat.max)
        goto Loverflow;
    if (r > ifloat.max)
        goto Loverflow;

    if (ifloat.max < f)
        goto Loverflow;
    if (ifloat.max < d)
        goto Loverflow;
    if (ifloat.max < r)
        goto Loverflow;

    return;

  Loverflow:
    return;
}

/*****************************************/

void test20()
{
  double d = 1;
  cdouble cd = 1+0i;
  assert(cd == 1.0 + 0i);
}

/*****************************************/

void test21()
{
   cdouble[] a;
   cdouble[] b;
   foreach(ref cdouble d; b)
     {
       d = -a[0];
       for(;;){}
     }
}

/*************************************/

void test22()
{
    static creal[] params = [1+0i, 3+0i, 5+0i];

    printf("params[0] = %Lf + %Lfi\n", params[0].re, params[0].im);
    printf("params[1] = %Lf + %Lfi\n", params[1].re, params[1].im);
    printf("params[2] = %Lf + %Lfi\n", params[2].re, params[2].im);

    creal[] sums = new creal[3];
    sums[] = 0+0i;

    foreach(creal d; params)
    {
        creal prod = d;

        printf("prod = %Lf + %Lfi\n", prod.re, prod.im);
        for(int i; i<2; i++)
        {
            sums[i] += prod;
            prod *= d;
        }
        sums[2] += prod;
    }

    printf("sums[0] = %Lf + %Lfi", sums[0].re, sums[0].im);
    assert(sums[0].re==9);
    assert(sums[0].im==0);
    assert(sums[1].re==35);
    assert(sums[1].im==0);
    assert(sums[2].re==153);
    assert(sums[2].im==0);
}

/*******************************************/

cdouble y23;

cdouble f23(cdouble x)
{
    return (y23 = x);
}

void test23()
{
    f23(1.0+2.0i);
    assert(y23 == 1.0+2.0i);
}

/*************************************/

ifloat func_24_1(ifloat f, double d)
{
//    f /= cast(cdouble)d;
    return f;
}

ifloat func_24_2(ifloat f, double d)
{
    f = cast(ifloat)(f / cast(cdouble)d);
    return f;
}

float func_24_3(float f, double d)
{
//    f /= cast(cdouble)d;
    return f;
}

float func_24_4(float f, double d)
{
    f = cast(float)(f / cast(cdouble)d);
    return f;
}

void test24()
{
    ifloat f = func_24_1(10i, 8);
    printf("%fi\n", f);
//    assert(f == 1.25i);

    f = func_24_2(10i, 8);
    printf("%fi\n", f);
    assert(f == 1.25i);

    float g = func_24_3(10, 8);
    printf("%f\n", g);
//    assert(g == 1.25);

    g = func_24_4(10, 8);
    printf("%f\n", g);
    assert(g == 1.25);
}

/*******************************************/

void test25()
{
    ireal x = 4.0Li;
    ireal y = 4.0Li;
    ireal z = 4Li;
    creal c = 4L + 0Li;
}

/*************************************/

string toString26(cdouble z)
{
    char[ulong.sizeof*8] buf;

    auto len = snprintf(buf.ptr, buf.sizeof, "%f+%fi", z.re, z.im);
    return buf[0 .. len].idup;
}

void test26()
{
  static cdouble[] A = [1+0i, 0+1i, 1+1i];
  string s;

  foreach( cdouble z; A )
  {
    s = toString26(z);
    printf("%.*s  ", cast(int)s.length, s.ptr);
  }
  printf("\n");

  for(int ii=0; ii<A.length; ii++ )
    A[ii] += -1i*A[ii];

  assert(A[0] == 1 - 1i);
  assert(A[1] == 1 + 1i);
  assert(A[2] == 2);

  foreach( cdouble z; A )
  {
    s = toString26(z);
    printf("%.*s  ", cast(int)s.length, s.ptr);
  }
  printf("\n");
}

/*************************************/

void test27()
{
    creal z = 1. + 2.0i;

    real r = z.re;
    assert(r == 1.0);

    real i = z.im;
    assert(i == 2.0);

    assert(r.im == 0.0);
    assert(r.re == 1.0);

    assert(i.re == 2.0);
    assert(i.im == 0.0i);
}

/*************************************/

void test28()
{
    alias cdouble X;
    X four = cast(X) (4.0i + 0.4);
}

/*************************************/

void test29()
{
    ireal a = 6.5i % 3i;
    printf("%Lfi %Lfi\n", a, a - .5i);
    assert(a == .5i);

    a = 6.5i % 3;
    printf("%Lfi %Lfi\n", a, a - .5i);
    assert(a == .5i);

    real b = 6.5 % 3i;
    printf("%Lf %Lf\n", b, b - .5);
    assert(b == .5);

    b = 6.5 % 3;
    printf("%Lf %Lf\n", b, b - .5);
    assert(b == .5);
}

/*************************************/

void test30()
{
    cfloat f = 1+0i;
    f %= 2fi;
    printf("%f + %fi\n", f.re, f.im);
    assert(f == 1 + 0i);

    cdouble d = 1+0i;
    d %= 2i;
    printf("%f + %fi\n", d.re, d.im);
    assert(d == 1 + 0i);

    creal r = 1+0i;
    r %= 2i;
    printf("%Lf + %Lfi\n", r.re, r.im);
    assert(r == 1 + 0i);
}

/*************************************/

void test31()
{
    cfloat f = 1+0i;
    f %= 2i;
    printf("%f + %fi\n", f.re, f.im);
    assert(f == 1);

    cdouble d = 1+0i;
    d = d % 2i;
    printf("%f + %fi\n", d.re, d.im);
    assert(d == 1);

    creal r = 1+0i;
    r = r % 2i;
    printf("%Lf + %Lfi\n", r.re, r.im);
    assert(r == 1);
}

/*************************************/

void assertEqual(real* a, real* b, string file = __FILE__, size_t line = __LINE__)
{
    auto x = cast(ubyte*)a;
    auto y = cast(ubyte*)b;

    // Only compare the 10 value bytes, the padding bytes are of undefined
    // value.
    version (X86) enum count = 10;
    else version (X86_64) enum count = 10;
    else enum count = real.sizeof;
    for (size_t i = 0; i < count; i++)
    {
        if (x[i] != y[i])
        {
            printf("%02zd: %02x %02x\n", i, x[i], y[i]);
            import core.exception;
            throw new AssertError(file, line);
        }
    }
}

void assertEqual(creal* a, creal* b, string file = __FILE__, size_t line = __LINE__)
{
    assertEqual(cast(real*)a, cast(real*)b, file, line);
    assertEqual(cast(real*)a + 1, cast(real*)b + 1, file, line);
}

void test32()
{
    creal a = creal.nan;
    creal b = real.nan + ireal.nan;
    assertEqual(&a, &b);

    real c= real.nan;
    real d=a.re;
    assertEqual(&c, &d);

    d=a.im;
    assertEqual(&c, &d);
}

/*************************************/

void test33()
{
    creal a = creal.infinity;
    creal b = real.infinity + ireal.infinity;
    assertEqual(&a, &b);

    real c = real.infinity;
    real d=a.re;
    assertEqual(&c, &d);

    d=a.im;
    assertEqual(&c, &d);
}

/*************************************/

void test34()
{
    creal a = creal.nan;
    creal b = creal.nan;
    b = real.nan + ireal.nan;
    assertEqual(&a, &b);

    real c = real.nan;
    real d=a.re;
    assertEqual(&c, &d);

    d=a.im;
    assertEqual(&c, &d);
}

/*************************************/

ireal x35;

void foo35()
{
    x35 = -x35;
}

void bar35()
{
    return foo35();
}

void test35()
{
    x35=2i;
    bar35();
    assert(x35==-2i);
}

/*************************************/

void test36()
{
    ireal imag = 2.5i;
    printf ("test of imag*imag = %Lf\n",imag*imag);
    assert(imag * imag == -6.25);
}

/*************************************/

void test37()
{
    creal z1 = 1. - 2.0i;
    ireal imag_part = z1.im/1i;
}

/***********************************/

void test38()
{
    ireal imag = 2.5i;
    //printf ("test of imag*imag = %Lf\n",imag*imag);
    real f = imag * imag;
    assert(f == -6.25);
}

/***********************************/

void test39()
{
    creal z = 1 + 2.5i;
    real e = z.im;

    printf ("e = %Lf\n", e);
    assert(e == 2.5);
}

/***********************************/

void test40()
{
    ifloat b = cast(ifloat)1i;
    assert(b == 1.0i);

    ifloat c = 2fi;
    assert(c == 2.0i);

    c = 0fi;
    assert(c == 0i);
}

/***************************************************/

void test41()
{
        creal a=1.3L+9.7Li;
        assert(a.re == 1.3L);
        assert(a.im == 9.7L);
}

/***************************************************/

void test42()
{
        creal c = 2.7L + 0i;
        assert(c.re==2.7L);
        assert(c.im==0.0L);
}

/***********************************/

void test43()
{
    creal C,Cj;
    real y1,x1;

    C = x1 + y1*1i + Cj;
    C = 1i*y1 + x1 + Cj;
    C = Cj + 1i*y1 + x1;
    C = y1*1i + Cj + x1;
    C = 1i*y1 + Cj;
    C = Cj + 1i*y1;
}

/***************************************************/

void test44()
{
    ifloat f = 1.0fi;
//    f *= 2.0fi; // illegal but compiles
    printf("%g\n", f);
//    assert(f == 0i);
}

/******************************************************/

void test45()
{
    TypeInfo ti;

    ti = typeid(ifloat[]);
    assert(!(ti is null));
    ti = typeid(idouble[]);
    assert(!(ti is null));
    ti = typeid(ireal[]);
    assert(!(ti is null));

    ti = typeid(cfloat[]);
    assert(!(ti is null));
    ti = typeid(cdouble[]);
    assert(!(ti is null));
    ti = typeid(creal[]);
    assert(!(ti is null));
}

/******************************************************/

void test46()
{
    TypeInfo ti = typeid(ifloat*);
    assert(!(ti is null));
    assert(ti.tsize==(ifloat*).sizeof);
    assert(ti.toString()=="ifloat*");
}

/******************************************************/

void test47()
{
    TypeInfo ti = typeid(cfloat*);
    assert(!(ti is null));
    assert(ti.tsize==(cfloat*).sizeof);
    assert(ti.toString()=="cfloat*");
}

/******************************************************/

void test48()
{
    TypeInfo ti = typeid(idouble*);
    assert(!(ti is null));
    assert(ti.tsize==(idouble*).sizeof);
    assert(ti.toString()=="idouble*");
}

/******************************************************/

void test49()
{
    TypeInfo ti = typeid(cdouble*);
    assert(!(ti is null));
    assert(ti.tsize==(cdouble*).sizeof);
    assert(ti.toString()=="cdouble*");
}

/***********************************/

void foo51(creal a)
{
    assert(a == -8i);
}

void test51()
{
    assert((2-2i)*(2-2i) == -8i);

    cdouble a = (2-2i)*(2-2i);
    assert(a == -8i);

    foo51((2-2i)*(2-2i));
}

/******************************************************/

void test52()
{
    TypeInfo ti = typeid(ireal*);
    assert(!(ti is null));
    assert(ti.tsize==(ireal*).sizeof);
    assert(ti.toString()=="ireal*");
}

/******************************************************/

void test53()
{
    TypeInfo ti = typeid(creal*);
    assert(!(ti is null));
    assert(ti.tsize==(creal*).sizeof);
    assert(ti.toString()=="creal*");
}

/*******************************************/

auto init(T)(T val) { return 1; }

void test54()
{
    // See built-in 'init' property
    assert(10i  .init is idouble.nan);

    // x.init() has parens, so it runs UFCS call
    assert(10i  .init() == 1);

    // x.init!YYY matches templatized UFCS call.
    assert(10i  .init!idouble()    == 1);
}

/*******************************************/

creal x55;

void foo55()
{
        x55 = -x55;
}

void bar55()
{
        return foo55();
}

void test55()
{
        x55 = 2.0L + 0.0Li;
        bar55();
        assert(x55 == -2.0L + 0.0Li);
}

/***************************************************/

template Q(s...) { alias s q; }

void test56()
{
    enum complex80 = Q!( 1+1.0i ).q.stringof;
}

/********************************************************/

void test57()
{
    assert(__traits(isArithmetic, ifloat) == true);
    assert(__traits(isArithmetic, idouble) == true);
    assert(__traits(isArithmetic, ireal) == true);
    assert(__traits(isArithmetic, cfloat) == true);
    assert(__traits(isArithmetic, cdouble) == true);
    assert(__traits(isArithmetic, creal) == true);

    assert(__traits(isScalar, ifloat) == true);
    assert(__traits(isScalar, idouble) == true);
    assert(__traits(isScalar, ireal) == true);
    assert(__traits(isScalar, cfloat) == true);
    assert(__traits(isScalar, cdouble) == true);
    assert(__traits(isScalar, creal) == true);

    assert(__traits(isFloating, ifloat) == true);
    assert(__traits(isFloating, idouble) == true);
    assert(__traits(isFloating, ireal) == true);
    assert(__traits(isFloating, cfloat) == true);
    assert(__traits(isFloating, cdouble) == true);
    assert(__traits(isFloating, creal) == true);

    assert(__traits(isIntegral, ifloat) == false);
    assert(__traits(isIntegral, idouble) == false);
    assert(__traits(isIntegral, ireal) == false);
    assert(__traits(isIntegral, cfloat) == false);
    assert(__traits(isIntegral, cdouble) == false);
    assert(__traits(isIntegral, creal) == false);

    assert(__traits(isUnsigned, ifloat) == false);
    assert(__traits(isUnsigned, idouble) == false);
    assert(__traits(isUnsigned, ireal) == false);
    assert(__traits(isUnsigned, cfloat) == false);
    assert(__traits(isUnsigned, cdouble) == false);
    assert(__traits(isUnsigned, creal) == false);

    assert(__traits(isAssociativeArray, ifloat) == false);
    assert(__traits(isAssociativeArray, idouble) == false);
    assert(__traits(isAssociativeArray, ireal) == false);
    assert(__traits(isAssociativeArray, cfloat) == false);
    assert(__traits(isAssociativeArray, cdouble) == false);
    assert(__traits(isAssociativeArray, creal) == false);

    assert(__traits(isStaticArray, ifloat) == false);
    assert(__traits(isStaticArray, idouble) == false);
    assert(__traits(isStaticArray, ireal) == false);
    assert(__traits(isStaticArray, cfloat) == false);
    assert(__traits(isStaticArray, cdouble) == false);
    assert(__traits(isStaticArray, creal) == false);

    assert(__traits(isAbstractClass, ifloat) == false);
    assert(__traits(isAbstractClass, idouble) == false);
    assert(__traits(isAbstractClass, ireal) == false);
    assert(__traits(isAbstractClass, cfloat) == false);
    assert(__traits(isAbstractClass, cdouble) == false);
    assert(__traits(isAbstractClass, creal) == false);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=3382

real toreal(ireal x){ return x.im; }

void test3382()
{
    assert(1.4i.toreal() == 1.4);
}

/***************************************************/

alias ireal BUG3919;
alias typeof(BUG3919.init*BUG3919.init) ICE3919;
alias typeof(BUG3919.init/BUG3919.init) ICE3920;

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8454

double sqrt8454(double d) { return d/2; }
void foo8454(cdouble m) {}

void test8454()
{
    foo8454(0 - sqrt8454(1.0) * 1i);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9046

void test9046()
{
    foreach (T; AliasSeq!(ifloat, idouble, ireal, cfloat, cdouble, creal))
    foreach (U; AliasSeq!(T, const T, immutable T, shared T, shared const T, inout T, shared inout T))
    {
        static assert(is(typeof(U.init) == U));
    }
}

/********************************************/
// https://issues.dlang.org/show_bug.cgi?id=9112

void test9112a()    //  T() and T(v)
{
    void test(T)(T v)
    {
        foreach (string qual; AliasSeq!("", "const ", "immutable "))
        {
            mixin("alias U = "~qual~T.stringof~";");
            //pragma(msg, U);

            mixin("auto x1 = "~qual~T.stringof~"();");      // U()      default construction syntax
            mixin("auto x2 = "~qual~T.stringof~"(v);");     // U(v)
            static assert(!__traits(compiles, mixin(qual~T.stringof~"(v, v)")));    // U(v, v)
            static assert(is(typeof(x1) == U));
            static assert(is(typeof(x2) == U));
            static if ( is(typeof(U.nan) :  real)) assert( isnan(x1.re) && !isnan(x1.im), U.stringof);
            static if ( is(typeof(U.nan) : ireal)) assert(!isnan(x1.re) &&  isnan(x1.im), U.stringof);
            static if ( is(typeof(U.nan) : creal)) assert( isnan(x1.re) &&  isnan(x1.im), U.stringof);
            static if (!is(typeof(U.nan)))         assert( x1 == U.init,                  U.stringof);
            assert(x2 == v, U.stringof);
        }
    }
    test!(ifloat )(1.4142i);
    test!(idouble)(1.4142i);
    test!(ireal  )(1.4142i);
    test!(cfloat )(1.2+3.4i);
    test!(cdouble)(1.2+3.4i);
    test!(creal  )(1.2+3.4i);

    static assert(!__traits(compiles, double(3.14i)));
}

void test9112b()    // new T(v)
{
    void test(T)(T v)
    {
        foreach (string qual; AliasSeq!("", "const ", "immutable "))
        {
            mixin("alias U = "~qual~T.stringof~";");
            //pragma(msg, U);

            mixin("auto p1 = new "~qual~T.stringof~"();");      // U()      default construction syntax
            mixin("auto p2 = new "~qual~T.stringof~"(v);");     // U(v)
            static assert(!__traits(compiles, mixin("new "~qual~T.stringof~"(v, v)")));    // U(v, v)
            static assert(is(typeof(p1) == U*));
            static assert(is(typeof(p2) == U*));
            assert( p1 !is null);
            assert( p2 !is null);
            auto x1 = *p1;
            auto x2 = *p2;
            static if ( is(typeof(U.nan) :  real)) assert( isnan(x1.re) && !isnan(x1.im), U.stringof);
            static if ( is(typeof(U.nan) : ireal)) assert(!isnan(x1.re) &&  isnan(x1.im), U.stringof);
            static if ( is(typeof(U.nan) : creal)) assert( isnan(x1.re) &&  isnan(x1.im), U.stringof);
            static if (!is(typeof(U.nan)))         assert( x1 == U.init,                  U.stringof);
            assert(x2 == v, U.stringof);
        }
    }

    test!(ifloat )(1.4142i);
    test!(idouble)(1.4142i);
    test!(ireal  )(1.4142i);
    test!(cfloat )(1.2+3.4i);
    test!(cdouble)(1.2+3.4i);
    test!(creal  )(1.2+3.4i);

    static assert(!__traits(compiles, new double(3.14i)));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10639

struct S1
{
    cdouble val;
}

void formatTest(S1 s, double re, double im)
{
    assert(s.val.re == re);
    assert(s.val.im == im);
}

void test10639()
{
    S1 s = S1(3+2.25i);
    formatTest(s, 3, 2.25);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10842

template Test10842(F, T)
{
    bool res;
    F from()
    {
        res = true;
        return F.init;
    }
    T to()
    {
        // The cast operand had incorrectly been eliminated
        return cast(T)from();
    }
    bool test()
    {
        res = false;
        to();
        return res;
    }
}

void test10842()
{
    foreach (From; AliasSeq!(bool, byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real))
    {
        foreach (To; AliasSeq!(ifloat, idouble, ireal))
        {
            if (!Test10842!(From, To).test())
                assert(0);
        }
    }

    foreach (From; AliasSeq!(ifloat, idouble, ireal))
    {
        foreach (To; AliasSeq!(/*bool*, */byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real))
        {
            if (!Test10842!(From, To).test())
                assert(0);
        }
    }
}

/***************************************************/

void test10927()
{
    static assert( (1+2i) ^^ 3 == -11 - 2i );
    auto a = (1+2i) ^^ 3;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13252

alias TypeTuple13252(T...) = T;

static assert(is(typeof(TypeTuple13252!(cast(cfloat )(1 + 2i))[0]) == cfloat ));
static assert(is(typeof(TypeTuple13252!(cast(cdouble)(1 + 2i))[0]) == cdouble));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14218

void test14218()
{
    version (DigitalMars)
    {
        // Questionable but currently accepted by DMD (but not GDC).
        foreach (To; AliasSeq!(ifloat, idouble, ireal))
        {
            auto x = cast(To)null;
            assert(x == 0);     // 0i
        }

        // Internal error: backend/el.c in el_long()
        //foreach (To; AliasSeq!(cfloat, cdouble, creal))
        //{
        //    static assert(!__traits(compiles, { auto x = cast(To)null; }));
        //}
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15653

alias TypeTuple15653(T...) = T;

void test15653()
{
    void foo(U, T)(const T x)     { static assert(is(T == U)); }
    void bar(U, T)(immutable T x) { static assert(is(T == U)); }

    struct X { int a; long[2] b; }
    struct Y { int* a; long[] b; }

    foreach (U; TypeTuple15653!(
                                ifloat, idouble, ireal,
                                cfloat, cdouble, creal))
    {
        foo!U(U.init);      // OK
        bar!U(U.init);      // Was error, now OK

        U u;
        foo!U(u);           // OK
        bar!U(u);           // Was error, now OK
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=17087

cfloat toComplex(int x) { return cast(cfloat)x; }

void test17087()
{
    assert (toComplex(1) == 1.0);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=17677

void test17677()
{
    cfloat v2 = 0.0f + 0.0fi;
    ulong v1 = 1;
    auto z = v2 + v1;
    assert(z == 1.0f);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=17677

float getreal_rcx(cfloat z)
{
    return z.re;
}
float getimag_rcx(cfloat z)
{
    return z.im;
}

float getreal_rdx(cfloat z, int)
{
    return z.re;
}
float getimag_rdx(cfloat z, int)
{
    return z.im;
}

float getreal_r8(cfloat z, int, int)
{
    return z.re;
}
float getimag_r8(cfloat z, int, int)
{
    return z.im;
}

float getreal_r9(cfloat z, int, int, int)
{
    return z.re;
}
float getimag_r9(cfloat z, int, int, int)
{
    return z.im;
}

float getreal_stack(cfloat z, int, int, int, int)
{
    return z.re;
}
float getimag_stack(cfloat z, int, int, int, int)
{
    return z.im;
}

void test18772a()
{
    cfloat[1] A;
    float[1] B;
    int i = 0;
    A[0] = 2.0f + 4i;
    B[0] = 3.0f;
    assert(6.0 == getreal_rcx(A[i] * B[i]));
    assert(12.0 == getimag_rcx(A[i] * B[i]));

    assert(6.0 == getreal_rdx(A[i] * B[i], 1));
    assert(12.0 == getimag_rdx(A[i] * B[i], 1));

    assert(6.0 == getreal_r8(A[i] * B[i], 1, 2));
    assert(12.0 == getimag_r8(A[i] * B[i], 1, 2));

    assert(6.0 == getreal_r9(A[i] * B[i], 1, 2, 3));
    assert(12.0 == getimag_r9(A[i] * B[i], 1, 2, 3));

    assert(6.0 == getreal_stack(A[i] * B[i], 1, 2, 3, 4));
    assert(12.0 == getimag_stack(A[i] * B[i], 1, 2, 3, 4));
}

void test18772b(T)()
{
    static auto getre0(T z)
    {
        return z.re;
    }
    static auto getim0(T z)
    {
        return z.im;
    }

    T z = 3 + 4i;
    auto d = z.re;

    assert(getre0(d * z) == d * 3);
    assert(getim0(d * z) == d * 4);
}

void test18772()
{
    test18772a();

    test18772b!cfloat();
    test18772b!cdouble();
    test18772b!creal();
}

/***************************************/

int main(char[][] args)
{

    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test7581();
    test7594();
    test7593();
    test7591();
    test8966();
    test10677();
    test7806();
    test7976();
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test23();
    test24();
    test25();
    test26();
    test27();
    test28();
    test29();
    test30();
    test31();
    test32();
    test33();
    test34();
    test35();
    test36();
    test37();
    test38();
    test39();
    test40();
    test41();
    test42();
    test43();
    test44();
    test45();
    test46();
    test47();
    test48();
    test49();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test8454();
    test9046();
    test9112a();
    test9112b();
    test10639();
    test10842();
    test14218();
    test15653();
    test17087();
    test17677();
    test18772();

    printf("Success!\n");
    return 0;
}
