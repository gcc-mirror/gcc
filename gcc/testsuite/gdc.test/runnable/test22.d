// RUNNABLE_PHOBOS_TEST
// REQUIRED_ARGS:

import std.math: poly;
import core.stdc.stdarg;

extern(C)
{
    int printf(const char*, ...);
    version(Windows)
    {
        int _snprintf(char*, size_t, const char*, ...);
        alias _snprintf snprintf;
    }
    else
        int snprintf(char*, size_t, const char*, ...);
}

/*************************************/

// http://www.digitalmars.com/d/archives/digitalmars/D/bugs/4766.html
// Only with -O

real randx()
{
    return 1.2;
}

void test1()
{
    float x10=randx();
    float x11=randx();
    float x20=randx();
    float x21=randx();
    float y10=randx();
    float y11=randx();
    float y20=randx();
    float y21=randx();

    float tmp=(
    x20*x21 + y10*y10 + y10*y11 + y11*y11 +
    y11*y20 + y20*y20 + y10*y21 + y11*y21 +
    y21*y21);
    assert(tmp > 0);
}

/*************************************/

void test2()
{
        double x10=randx();
        double x11=randx();
        double x20=randx();
        double x21=randx();
        double y10=randx();
        double y11=randx();
        double y20=randx();
        double y21=randx();

    double tmp=(
    x20*x21 + y10*y10 + y10*y11 + y11*y11 +
    y11*y20 + y20*y20 + y10*y21 + y11*y21 +
    y21*y21);
    assert(tmp > 0);
}

/*************************************/

void test3()
{
    real x10=randx();
    real x11=randx();
    real x20=randx();
    real x21=randx();
    real y10=randx();
    real y11=randx();
    real y20=randx();
    real y21=randx();

    real tmp=(
    x20*x21 + y10*y10 + y10*y11 + y11*y11 +
    y11*y20 + y20*y20 + y10*y21 + y11*y21 +
    y21*y21);
    assert(tmp > 0);
}

/*************************************/

void test4()
{
     printf("main() : (-128 >= 0)=%s, (-128 <= 0)=%s\n",
              cast(char*)(-128 >= 0 ? "true" : "false"),
              cast(char*)(-128 <= 0 ?  "true" : "false"));

     printf("main() : (128 >= 0)=%s, (128 <= 0)=%s\n",
              cast(char*)(128 >= 0 ? "true" : "false"),
              cast(char*)(128 <= 0 ?  "true" : "false"));

     assert((-128 >= 0 ? "true" : "false") == "false"),
     assert((-128 <= 0 ? "true" : "false") == "true");
     assert((+128 >= 0 ? "true" : "false") == "true"),
     assert((+128 <= 0 ? "true" : "false") == "false");
}


/*************************************/

int foo5() { assert(0); } // No return.

int abc5()
{
    printf("foo = %d\n", foo5());
    return 0;
}

void test5()
{
}

/*************************************/

void test6()
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

void test7()
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

void test8()
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

class A9
{
    this(int[] params ...)
    {
        for (int i = 0; i < params.length; i++)
        {
            assert(params[i] == i + 1);
        }
    }
}

class B9
{
    this()
    {
        init();
    }

    private void init()
    {
        A9 test1 = new A9(1, 2, 3);
        A9 test2 = new A9(1, 2, 3, 4);
        int[3] arg; A9 test3 = new A9((arg[0]=1, arg[1]=2, arg[2]=3, arg));
    }
}

void test9()
{
    B9 test2 = new B9();
}

/*************************************/

void test10()
{
    auto i = 5u;
    auto s = typeid(typeof(i)).toString;
    printf("%.*s\n", s.length, s.ptr);
    assert(typeid(typeof(i)) == typeid(uint));
}

/*************************************/

void test11()
{
    printf("%d\n", 3);
    printf("xhello world!\n");
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
            printf("%02d: %02x %02x\n", i, x[i], y[i]);
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

void test12()
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

void test13()
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

void test14()
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

ireal x15;

void foo15()
{
    x15 = -x15;
}

void bar15()
{
    return foo15();
}

void test15()
{
    x15=2i;
    bar15();
    assert(x15==-2i);
}

/*************************************/

real x16;

void foo16()
{
    x16 = -x16;
}

void bar16()
{
    return foo16();
}

void test16()
{
    x16=2;
    bar16();
    assert(x16==-2);
}

/*************************************/

class Bar17
{
        this(...) {}
}

class Foo17
{
        void opAdd (Bar17 b) {}
}

void test17()
{
        auto f = new Foo17;
        f + new Bar17;
}


/*************************************/

template u18(int n)
{
    static if (n==1) {
       int a = 1;
    } else
       int b = 4;
}

void test18()
{
   mixin u18!(2);
   assert(b == 4);
}

/*************************************/

class DP
{
  private:
    void I(char[] p)
    {
        I(p[1..p.length]);
    }
}

void test19()
{
}

/*************************************/

struct Struct20
{
    int i;
}

void test20()
{
    auto s = new Struct20;
    s.i = 7;
}

/*************************************/

class C21(float f)
{
    float ff = f;
}

void test21()
{
    auto a = new C21!(1.2);
    C21!(1.2) b = new C21!(1.2);
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

/*************************************/

const int c23 = b23 * b23;
const int a23 = 1;
const int b23 = a23 * 3;

template T23(int n)
{
    int[n] x23;
}

mixin T23!(c23);

void test23()
{
    assert(x23.length==9);
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

/*************************************/

template cat(int n)
{
   const int dog = n;
}

const char [] bird = "canary";

const int sheep = cat!(bird.length).dog;

void test25()
{
    assert(sheep == 6);
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
    printf("%.*s  ", s.length, s.ptr);
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
    printf("%.*s  ", s.length, s.ptr);
  }
  printf("\n");
}

/*************************************/

void test27()
{   int x;

    string s = (int*function(int ...)[]).mangleof;
    printf("%.*s\n", s.length, s.ptr);
    assert((int*function(int ...)[]).mangleof == "APFiXPi");
    assert(typeof(x).mangleof == "i");
    assert(x.mangleof == "_D6test226test27FZ1xi");
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
    ulong a = 10_000_000_000_000_000,
          b =  1_000_000_000_000_000;

    printf("test29\n%lx\n%lx\n%lx\n", a, b, a / b);
    assert((a / b) == 10);
}

static assert((10_000_000_000_000_000 / 1_000_000_000_000_000) == 10);

/*************************************/

template chook(int n)
{
   const int chook = 3;
}

template dog(alias f) {
    const int dog =  chook!(f.mangleof.length);
}

class pig {}

const int goose = dog!(pig);

void test30()
{
    printf("%d\n", goose);
    assert(goose == 3);
}

/*************************************/

template dog31(string sheep)
{
  immutable string dog31 = "daschund";
}

void test31()
{
    string duck = dog31!("bird"[1..3]);

    assert(duck == "daschund");
}

/*************************************/

struct particle
{
    int   active; /* Active (Yes/No) */
    float life;   /* Particle Life   */
    float fade;   /* Fade Speed      */

    float r;      /* Red Value       */
    float g;      /* Green Value     */
    float b;      /* Blue Value      */

    float x;      /* X Position      */
    float y;      /* Y Position      */

    float xi;     /* X Direction     */
    float yi;     /* Y Direction     */

    float xg;     /* X Gravity       */
    float yg;     /* Y Gravity       */
}

particle particles[10000];

void test32()
{
}

/*************************************/

class Foo33
{
    template foo()
    {
        int foo() { return 6; }
    }
}


void test33()
{
    Foo33 f = new Foo33;

    assert(f.foo!()() == 6);
    with (f)
        assert(foo!()() == 6);
}

/*************************************/

template dog34(string duck)
{
    const int dog34 = 2;
}

void test34()
{
    int aardvark = dog34!("cat" ~ "pig");

    assert(aardvark == 2);
}

/*************************************/

class A35
{
    private bool quit;
    void halt() {quit = true;}
    bool isHalted() {return quit;}
}

void test35()
{
    auto a = new A35;

    a.halt;         // error here
    a.halt();

    a.isHalted;     // error here
    bool done = a.isHalted;
    if (a.isHalted)
    {
    }
}


/*************************************/

void test36()
{
    bool q = (0.9 + 3.5L == 0.9L + 3.5L);
    assert(q);
    static assert(0.9 + 3.5L == 0.9L + 3.5L);
    assert(0.9 + 3.5L == 0.9L + 3.5L);
}

/*************************************/

abstract class Foo37(T)
{
    void bar () { }
}

class Bar37 : Foo37!(int)
{
}

void test37()
{
    auto f = new Bar37;
}

/*************************************/

void test38()
{
        auto s=`hello`;
        assert(s.length==5);
        assert(s[0]=='h');
        assert(s[1]=='e');
        assert(s[2]=='l');
        assert(s[3]=='l');
        assert(s[4]=='o');
}

/*************************************/

void test39()
{
        int value=1;
        string key = "eins";
        int[char[]] array;

        array[key]=value;
        int* ptr = key in array;

        assert(value == *ptr);
}

/*************************************/

void test40()
{
        auto s=r"hello";
        assert(s.length==5);
        assert(s[0]=='h');
        assert(s[1]=='e');
        assert(s[2]=='l');
        assert(s[3]=='l');
        assert(s[4]=='o');
}

/*************************************/

void test41()
{
    version (Windows)
    {
        version(D_InlineAsm){
                double a = 1.2;
                double b = 0.2;
                double c = 1.4;

                asm{
                        movq XMM0, a;
                        movq XMM1, b;
                        addsd XMM1, XMM0;
                        movq c, XMM1;
                }

                a += b;

                b = a-c;
                b = (b>0) ? b : (-1 * b);

                assert(b < b.epsilon*4);
        }
    }
}

/*************************************/

const char[] tapir = "some horned animal";

const byte[] antelope = cast(byte []) tapir;

void test42()
{
}

/*************************************/

void test43()
{
     string armadillo = "abc" ~ 'a';
     assert(armadillo == "abca");
     string armadillo2 = 'b' ~ "abc";
     assert(armadillo2 == "babc");
}

/*************************************/

const uint baboon44 = 3;

const int monkey44 = 4;

const ape44 = monkey44 * baboon44;

void test44()
{
    assert(ape44 == 12);
}

/*************************************/

class A45
{
 this()
 {
  b = new B();
  b.x = 5; // illegal
 }

 class B
 {
  protected int x;
 }

 B b;
}

void test45()
{
}


/*************************************/

class C46(T)
{
    private T i; // or protected or package
}

void test46()
{
    C46!(int) c = new C46!(int); //  class t4.C46!(int).C46 member i is not accessible
    c.i = 10;
}

/*************************************/

void bug5809()
{
    ushort[2] x = void;
    x[0] = 0;
    x[1] = 0x1234;
    ushort *px =  &x[0];

    uint b = px[0];

    assert(px[0] == 0);
}

/*************************************/

void bug7546()
{
    double p = -0.0;
    assert(p == 0);
}


/*************************************/

real poly_asm(real x, real[] A)
in
{
    assert(A.length > 0);
}
body
{
    version (D_InlineAsm_X86)
    {
        version (linux)
        {
        asm     // assembler by W. Bright
        {
            // EDX = (A.length - 1) * real.sizeof
            mov     ECX,A[EBP]          ; // ECX = A.length
            dec     ECX                 ;
            lea     EDX,[ECX][ECX*8]    ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,A+4[EBP]        ;
            fld     real ptr [EDX]      ; // ST0 = coeff[ECX]
            jecxz   return_ST           ;
            fld     x[EBP]              ; // ST0 = x
            fxch    ST(1)               ; // ST1 = x, ST0 = r
            align   4                   ;
    L2:     fmul    ST,ST(1)            ; // r *= x
            fld     real ptr -12[EDX]   ;
            sub     EDX,12              ; // deg--
            faddp   ST(1),ST            ;
            dec     ECX                 ;
            jne     L2                  ;
            fxch    ST(1)               ; // ST1 = r, ST0 = x
            fstp    ST(0)               ; // dump x
            align   4                   ;
    return_ST:                          ;
            ;
        }
        }
        else version (OSX)
        {
            asm // assembler by W. Bright
            {
                // EDX = (A.length - 1) * real.sizeof
                mov     ECX,A[EBP]              ; // ECX = A.length
                dec     ECX                     ;
                lea     EDX,[ECX*8]             ;
                add     EDX,EDX                 ;
                add     EDX,A+4[EBP]            ;
                fld     real ptr [EDX]          ; // ST0 = coeff[ECX]
                jecxz   return_ST               ;
                fld     x[EBP]                  ; // ST0 = x
                fxch    ST(1)                   ; // ST1 = x, ST0 = r
                align   4                       ;
        L2:     fmul    ST,ST(1)                ; // r *= x
                fld     real ptr -16[EDX]       ;
                sub     EDX,16                  ; // deg--
                faddp   ST(1),ST                ;
                dec     ECX                     ;
                jne     L2                      ;
                fxch    ST(1)                   ; // ST1 = r, ST0 = x
                fstp    ST(0)                   ; // dump x
                align   4                       ;
        return_ST:                              ;
                ;
            }
        }
        else version (FreeBSD)
        {
        asm     // assembler by W. Bright
        {
            // EDX = (A.length - 1) * real.sizeof
            mov     ECX,A[EBP]          ; // ECX = A.length
            dec     ECX                 ;
            lea     EDX,[ECX][ECX*8]    ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,A+4[EBP]        ;
            fld     real ptr [EDX]      ; // ST0 = coeff[ECX]
            jecxz   return_ST           ;
            fld     x[EBP]              ; // ST0 = x
            fxch    ST(1)               ; // ST1 = x, ST0 = r
            align   4                   ;
    L2:     fmul    ST,ST(1)            ; // r *= x
            fld     real ptr -12[EDX]   ;
            sub     EDX,12              ; // deg--
            faddp   ST(1),ST            ;
            dec     ECX                 ;
            jne     L2                  ;
            fxch    ST(1)               ; // ST1 = r, ST0 = x
            fstp    ST(0)               ; // dump x
            align   4                   ;
    return_ST:                          ;
            ;
        }
        }
        else version (Solaris)
        {
        asm     // assembler by W. Bright
        {
            // EDX = (A.length - 1) * real.sizeof
            mov     ECX,A[EBP]          ; // ECX = A.length
            dec     ECX                 ;
            lea     EDX,[ECX][ECX*8]    ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,ECX             ;
            add     EDX,A+4[EBP]        ;
            fld     real ptr [EDX]      ; // ST0 = coeff[ECX]
            jecxz   return_ST           ;
            fld     x[EBP]              ; // ST0 = x
            fxch    ST(1)               ; // ST1 = x, ST0 = r
            align   4                   ;
    L2:     fmul    ST,ST(1)            ; // r *= x
            fld     real ptr -12[EDX]   ;
            sub     EDX,12              ; // deg--
            faddp   ST(1),ST            ;
            dec     ECX                 ;
            jne     L2                  ;
            fxch    ST(1)               ; // ST1 = r, ST0 = x
            fstp    ST(0)               ; // dump x
            align   4                   ;
    return_ST:                          ;
            ;
        }
        }
        else
        {
        asm     // assembler by W. Bright
        {
            // EDX = (A.length - 1) * real.sizeof
            mov     ECX,A[EBP]          ; // ECX = A.length
            dec     ECX                 ;
            lea     EDX,[ECX][ECX*8]    ;
            add     EDX,ECX             ;
            add     EDX,A+4[EBP]        ;
            fld     real ptr [EDX]      ; // ST0 = coeff[ECX]
            jecxz   return_ST           ;
            fld     x[EBP]              ; // ST0 = x
            fxch    ST(1)               ; // ST1 = x, ST0 = r
            align   4                   ;
    L2:     fmul    ST,ST(1)            ; // r *= x
            fld     real ptr -10[EDX]   ;
            sub     EDX,10              ; // deg--
            faddp   ST(1),ST            ;
            dec     ECX                 ;
            jne     L2                  ;
            fxch    ST(1)               ; // ST1 = r, ST0 = x
            fstp    ST(0)               ; // dump x
            align   4                   ;
    return_ST:                          ;
            ;
        }
        }
    }
    else
    {
        printf("Sorry, you don't seem to have InlineAsm_X86\n");
        return 0;
    }
}

real poly_c(real x, real[] A)
in
{
    assert(A.length > 0);
}
body
{
    ptrdiff_t i = A.length - 1;
    real r = A[i];
    while (--i >= 0)
    {
        r *= x;
        r += A[i];
    }
    return r;
}

void test47()
{
    real x = 3.1;
    static real pp[] = [56.1, 32.7, 6];
    real r;

    printf("The result should be %Lf\n",(56.1L + (32.7L + 6L * x) * x));
    printf("The C version outputs %Lf\n", poly_c(x, pp));
    printf("The asm version outputs %Lf\n", poly_asm(x, pp));
    printf("The std.math version outputs %Lf\n", poly(x, pp));

    r = (56.1L + (32.7L + 6L * x) * x);
    assert(r == poly_c(x, pp));
    version (D_InlineAsm_X86)
        assert(r == poly_asm(x, pp));
    assert(r == poly(x, pp));
}

/*************************************/

const c48 = 1uL-1;

void test48()
{
    assert(c48 == 0);
}

/*************************************/

template cat49()
{
     static assert(1);  // OK
     static if (1)
     {
        static assert(1); // doesn't work
        static if (1)
        {
             static assert(1);  // OK
             const int cat49 = 3;
        }
     }
}

void test49()
{
    const int a = cat49!();
    assert(a == 3);
}

/*************************************/

void test50()
{
    if (auto x = 1)
    {
        assert(typeid(typeof(x)) == typeid(int));
        assert(x == 1);
    }
    else
        assert(0);

    if (int x = 1)
    {
        assert(typeid(typeof(x)) == typeid(int));
        assert(x == 1);
    }
    else
        assert(0);

    if (1)
    {
    }
    else
        assert(0);
}

/*************************************/

void test51()
{
    bool b;
    assert(b == false);
    b &= 1;
    assert(b == false);
    b |= 1;
    assert(b == true);
    b ^= 1;
    assert(b == false);
    b = b | true;
    assert(b == true);
    b = b & false;
    assert(b == false);
    b = b ^ true;
    assert(b == true);
    b = !b;
    assert(b == false);
}

/*************************************/

alias int function (int) x52;

template T52(string str){
        const int T52 = 1;
}

static assert(T52!(x52.mangleof));

void test52()
{
}

/*************************************/
import std.stdio;
import core.stdc.stdarg;

void myfunc(int a1, ...) {
        va_list argument_list;
        TypeInfo argument_type;
        string sa; int ia; double da;
        writefln("%d variable arguments", _arguments.length);
        writefln("argument types %s", _arguments);
        va_start(argument_list, a1);
        for (int i = 0; i < _arguments.length; ) {
                if ((argument_type=_arguments[i++]) == typeid(string)) {
                        va_arg(argument_list, sa);
                        writefln("%d) string arg = '%s', length %d", i+1, sa.length<=20? sa : "?", sa.length);
                } else if (argument_type == typeid(int)) {
                        va_arg(argument_list, ia);
                        writefln("%d) int arg = %d", i+1, ia);
                } else if (argument_type == typeid(double)) {
                        va_arg(argument_list, da);
                        writefln("%d) double arg = %f", i+1, da);
                } else {
                        throw new Exception("invalid argument type");
                }
        }
        va_end(argument_list);
}

void test6758() {
        myfunc(1, 2, 3, 4, 5, 6, 7, 8, "9", "10");                              // Fails.
        myfunc(1, 2.0, 3, 4, 5, 6, 7, 8, "9", "10");                    // Works OK.
        myfunc(1, 2, 3, 4, 5, 6, 7, "8", "9", "10");                    // Works OK.
        myfunc(1, "2", 3, 4, 5, 6, 7, 8, "9", "10");                    // Works OK.
}


/*************************************/

int main()
{
    test1();
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
    bug5809();
    bug7546();
    test47();
    test48();
    test49();
    test50();
    test51();
    test52();
    test6758();

    printf("Success\n");
    return 0;
}
