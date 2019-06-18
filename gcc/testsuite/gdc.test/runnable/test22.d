// REQUIRED_ARGS:

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
        int[3] arg;
        arg[0]=1, arg[1]=2, arg[2]=3;
        A9 test3 = new A9(arg);
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
    printf("%.*s\n", cast(int)s.length, s.ptr);
    assert(typeid(typeof(i)) == typeid(uint));
}

/*************************************/

void test11()
{
    printf("%d\n", 3);
    printf("xhello world!\n");
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
        void opBinary(string op : "+") (Bar17 b) {}
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

void test27()
{   int x;

    string s = (int*function(int ...)[]).mangleof;
    printf("%.*s\n", cast(int)s.length, s.ptr);
    assert((int*function(int ...)[]).mangleof == "APFiXPi");
    assert(typeof(x).mangleof == "i");
    assert(x.mangleof == "_D6test226test27FZ1xi");
}

/*************************************/

void test29()
{
    ulong a = 10_000_000_000_000_000,
          b =  1_000_000_000_000_000;

    printf("test29\n%llx\n%llx\n%llx\n", a, b, a / b);
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

particle[10000] particles;

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
do
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
do
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
    static real[] pp = [56.1, 32.7, 6];
    real r;

    printf("The result should be %Lf\n",(56.1L + (32.7L + 6L * x) * x));
    printf("The C version outputs %Lf\n", poly_c(x, pp));
    printf("The asm version outputs %Lf\n", poly_asm(x, pp));

    r = (56.1L + (32.7L + 6L * x) * x);
    assert(r == poly_c(x, pp));
    version (D_InlineAsm_X86)
        assert(r == poly_asm(x, pp));
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

void myfunc(int a1, ...) {
        va_list argument_list;
        TypeInfo argument_type;
        string sa; int ia; double da;
        assert(_arguments.length == 9);

        va_start(argument_list, a1);
        for (int i = 0; i < _arguments.length; ) {
                if ((argument_type=_arguments[i++]) == typeid(string)) {
                        va_arg(argument_list, sa);
                        switch (i)
                        {
                            case 1: assert(sa == "2"); break;
                            case 7: assert(sa == "8"); break;
                            case 8: assert(sa == "9"); break;
                            case 9: assert(sa == "10"); break;
                            default:
                                printf("i = %d\n", i);
                                assert(false);
                        }
                } else if (argument_type == typeid(int)) {
                        va_arg(argument_list, ia);
                        assert(ia == i+1);
                } else if (argument_type == typeid(double)) {
                        va_arg(argument_list, da);
                        const e = i+1;
                        assert((e - 0.0001) < da && da < (e + 0.0001));
                } else {
                        assert(false, argument_type.toString());
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

real f18573() { return 1; }

void test18573()
{
    cast(void) f18573();
    cast(void) f18573();
    cast(void) f18573();
    cast(void) f18573();
    cast(void) f18573();
    cast(void) f18573();
    cast(void) f18573();

    real b = 2;
    assert(b == 2); /* fails; should pass */
}

/*************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test9();
    test10();
    test11();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test23();
    test25();
    test27();
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
    test18573();

    printf("Success\n");
    return 0;
}
