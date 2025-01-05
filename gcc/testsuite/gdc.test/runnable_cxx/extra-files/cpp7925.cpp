#include <stdarg.h>
#include <assert.h>

class C1
{
public:
    virtual ~C1();

    int i;

    int f0();
    int f1(int a);
    int f2(int a, int b);
    virtual int f3(int a, int b);
    int f4(int a, ...);
};

C1::~C1()
{
}

int C1::f0()
{
    return i;
}

int C1::f1(int a)
{
    return i + a;
}

int C1::f2(int a, int b)
{
    return i + a + b;
}

int C1::f3(int a, int b)
{
    return i + a + b;
}

int C1::f4(int a, ...)
{
    int r = i + a;
    int last = a;

    va_list argp;
    va_start(argp, a);
    while (last)
    {
        last = va_arg(argp, int);
        r += last;
    }
    va_end(argp);
    return r;
}

C1 *createC1()
{
    return new C1();
}

class C2
{
public:
    virtual ~C2();

    int i;

    int f0();
    int f1(int a);
    int f2(int a, int b);
    virtual int f3(int a, int b);
    int f4(int a, ...);
};

C2 *createC2();

void runCPPTests()
{
    C2 *c2 = createC2();
    c2->i = 100;
    assert(c2->f0() == 100);
    assert(c2->f1(1) == 101);
    assert(c2->f2(20, 3) == 123);
    assert(c2->f3(20, 3) == 123);
    assert(c2->f4(20, 3, 0) == 123);

    int (C2::*fp0)() = &C2::f0;
    int (C2::*fp1)(int) = &C2::f1;
    int (C2::*fp2)(int, int) = &C2::f2;
    int (C2::*fp3)(int, int) = &C2::f3;
    int (C2::*fp4)(int, ...) = &C2::f4;
    assert((c2->*(fp0))() == 100);
    assert((c2->*(fp1))(1) == 101);
    assert((c2->*(fp2))(20, 3) == 123);
    assert((c2->*(fp3))(20, 3) == 123);
    assert((c2->*(fp4))(20, 3, 0) == 123);
}
