// EXTRA_CPP_SOURCES: cpp7925.cpp

/*
Exclude -O due to a codegen bug on OSX:
https://issues.dlang.org/show_bug.cgi?id=22556

PERMUTE_ARGS(osx): -inline -release -g
*/

import core.vararg;

extern(C++) class C1
{
public:
    ~this();

    int i;

    final int f0();
    final int f1(int a);
    final int f2(int a, int b);
    int f3(int a, int b);
    final int f4(int a, ...);
};

extern(C++) C1 createC1();

extern(C++) class C2
{
public:
    ~this()
    {
    }

    int i;

    final int f0()
    {
        return i;
    }

    final int f1(int a)
    {
        return i + a;
    }

    final int f2(int a, int b)
    {
        return i + a + b;
    }

    int f3(int a, int b)
    {
        return i + a + b;
    }

    final int f4(int a, ...)
    {
        int r = i + a;
        int last = a;

        va_list argp;
        va_start(argp, a);
        while (last)
        {
            last = va_arg!int(argp);
            r += last;
        }
        va_end(argp);
        return r;
    }
};

extern(C++) C2 createC2()
{
    return new C2;
}

auto callMember(alias F, Params...)(__traits(parent, F) obj, Params params)
{
    static if(__traits(getFunctionVariadicStyle, F) == "stdarg")
        enum varargSuffix = ", ...";
    else
        enum varargSuffix = "";

    static if(is(typeof(&F) R == return) && is(typeof(F) P == __parameters))
        mixin("extern(" ~ __traits(getLinkage, F) ~ ") R delegate(P" ~ varargSuffix ~ ") dg;");
    dg.funcptr = &F;
    dg.ptr = cast(void*)obj;
    return dg(params);
}

extern(C++) void runCPPTests();

void main()
{
    C1 c1 = createC1();
    c1.i = 100;
    assert(c1.f0() == 100);
    assert(c1.f1(1) == 101);
    assert(c1.f2(20, 3) == 123);
    assert(c1.f3(20, 3) == 123);
    assert(c1.f4(20, 3, 0) == 123);

    auto dg0 = &c1.f0;
    auto dg1 = &c1.f1;
    auto dg2 = &c1.f2;
    auto dg3 = &c1.f3;
    auto dg4 = &c1.f4;
    assert(dg0() == 100);
    assert(dg1(1) == 101);
    assert(dg2(20, 3) == 123);
    assert(dg3(20, 3) == 123);
    assert(dg4(20, 3, 0) == 123);

    assert(callMember!(C1.f0)(c1) == 100);
    assert(callMember!(C1.f1)(c1, 1) == 101);
    assert(callMember!(C1.f2)(c1, 20, 3) == 123);
    assert(callMember!(C1.f3)(c1, 20, 3) == 123);
    assert(callMember!(C1.f4)(c1, 20, 3, 0) == 123);

    int i;
    extern(C++) void delegate() lamdba1 = () {
        i = 5;
    };
    lamdba1();
    assert(i == 5);

    extern(C++) int function(int, int) lamdba2 = (int a, int b) {
        return a + b;
    };
    assert(lamdba2(3, 4) == 7);

    extern(C++) void delegate(int, ...) lamdba3 = (int a, ...) {
        i = a;
        int last = a;

        va_list argp;
        va_start(argp, a);
        while (last)
        {
            last = va_arg!int(argp);
            i += last;
        }
        va_end(argp);
    };
    lamdba3(1000, 200, 30, 4, 0);
    assert(i == 1234);

    runCPPTests();
}
