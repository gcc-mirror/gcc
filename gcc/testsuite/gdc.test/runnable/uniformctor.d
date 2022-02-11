/*
RUN_OUTPUT:
---
Success
---
*/
extern(C) int printf(const char*, ...);
template TypeTuple(TL...) { alias TypeTuple = TL; }

import core.stdc.math : isnan;

/********************************************/
// https://issues.dlang.org/show_bug.cgi?id=9112

void test9112a()    //  T() and T(v)
{
    void test(T)(T v)
    {
        foreach (string qual; TypeTuple!("", "const ", "immutable "))
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
    static assert(!__traits(compiles, { auto x1 = void(); }));
    static assert(!__traits(compiles, { auto x2 = void(1); }));
    test!( byte  )(10);
    test!(ubyte  )(10);
    test!( short )(10);
    test!(ushort )(10);
    test!( int   )(10);
    test!(uint   )(10);
    test!( long  )(10);
    test!(ulong  )(10);
    test!( float )(3.14);
    test!( double)(3.14);
    test!( real  )(3.14);
    test!( char  )('A');
    test!(wchar  )('A');
    test!(dchar  )('A');
    test!(bool   )(true);

    static assert(!__traits(compiles, int(1.42)));  // in curre,t this is disallowed

    {
        int x;
        alias T = int*;
      //auto p = int*(&x);      // Error: found '*' when expecting '.' following int
      //auto p = (int*)(&x);    // Error: C style cast illegal, use cast(int*)&x
        auto p = T(&x);
        assert( p == &x);
        assert(*p ==  x);
    }
}

enum Enum : long { a = 10, b = 20 }

void test9112b()    // new T(v)
{
    void test(T)(T v)
    {
        foreach (string qual; TypeTuple!("", "const ", "immutable "))
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

    static assert(!__traits(compiles, { auto x1 = new void(); }));
    static assert(!__traits(compiles, { auto x2 = new void(1); }));
    static assert(!__traits(compiles, { auto x2 = new void(1, 2); }));
    test!( byte  )(10);
    test!(ubyte  )(10);
    test!( short )(10);
    test!(ushort )(10);
    test!( int   )(10);
    test!(uint   )(10);
    test!( long  )(10);
    test!(ulong  )(10);
    test!( float )(3.14);
    test!( double)(3.14);
    test!( real  )(3.14);
    test!( char  )('A');
    test!(wchar  )('A');
    test!(dchar  )('A');
    test!(bool   )(true);
    test!(Enum   )(Enum.a);

    void testPtr(T)(T v)
    {
        T* pv = &v;
        T** ppv = new T*(pv);
        assert( *ppv == pv);
        assert(**ppv ==  v);
    }
    foreach (T; TypeTuple!(int, const long, immutable double))
    {
        testPtr!T(10);
    }
    foreach (T; TypeTuple!(Enum, const Enum, immutable Enum))
    {
        testPtr!T(Enum.a);
    }

    static assert(!__traits(compiles, new const int(1, 2)));

    static assert(!__traits(compiles, new int(1.42)));  // in curre,t this is disallowed

    // int(1) in directly on statement scope should be parsed as an expression, but
    // would fail to compile because of "has no effect" error.
    static assert(!__traits(compiles, { int(1); }));
}

/********************************************/

int main()
{
    test9112a();
    test9112b();

    printf("Success\n");
    return 0;
}
