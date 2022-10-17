/*
TEST_OUTPUT:
---
decl: test
stmt: test
---

RUN_OUTPUT:
---
0: getter
1: setter
2: getter
3: setter
4: setter
5: compile error
6: compile error
7: setter
Success
---
*/
extern (C) int printf(const char* fmt, ...);

/*******************************************/

template select(alias v1, alias v2)
{
    enum select = v2;
}

struct Test(int N)
{
    int value;
    int getset;

    static if (N == 0)
    {
        ref foo(){ getset = 1; return value; }

        enum result = select!(0, 1);
        // prints "getter"
    }
    static if (N == 1)
    {
        ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(0, 2);
        // prints "setter"
    }
    static if (N == 2)
    {
        @property ref foo(){ getset = 1; return value; }

        enum result = select!(1, 1);
        // prints "getter"
    }
    static if (N == 3)
    {
        @property ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(2, 2);
        // prints "setter"
    }


    static if (N == 4)
    {
        ref foo()     { getset = 1; return value; }
        ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(0, 2);
        // prints "setter"
    }
    static if (N == 5)
    {
        @property ref foo()     { getset = 1; return value; }
                  ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(0, 0);
        // test.d(xx): Error: cannot overload both property and non-property functions
    }
    static if (N == 6)
    {
                  ref foo()     { getset = 1; return value; }
        @property ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(0, 0);
        // test.d(xx): Error: cannot overload both property and non-property functions
    }
    static if (N == 7)
    {
        @property ref foo()     { getset = 1; return value; }
        @property ref foo(int x){ getset = 2; value = x; return value; }

        enum result = select!(2, 2);
        // prints "setter"
    }
}

template seq(T...)
{
    alias T seq;
}
template iota(int begin, int end)
    if (begin <= end)
{
    static if (begin == end)
        alias seq!() iota;
    else
        alias seq!(begin, iota!(begin+1, end)) iota;
}

void test1()
{
    foreach (N; iota!(0, 8))
    {
        printf("%d: ", N);

        Test!N s;
        static if (Test!N.result == 0)
        {
            static assert(!is(typeof({ s.foo = 1; })));
            printf("compile error\n");
        }
        else
        {
            s.foo = 1;
            if (s.getset == 1)
                printf("getter\n");
            else
                printf("setter\n");
            assert(s.getset == Test!N.result);
        }
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7722

class Foo7722 {}
void spam7722(Foo7722 f) {}

void test7722()
{
    auto f = new Foo7722;
    f.spam7722;
}

/*******************************************/

@property void check(alias v1, alias v2, alias dg)()
{
    void checkImpl(alias v)()
    {
        static if (v == 0)
            static assert(!__traits(compiles, dg(0)));
        else
            assert(dg(0) == v);
    }

    checkImpl!(v2)();
}

struct S {}

int foo(int n)          { return 1; }
int foo(int n, int m)   { return 2; }
int goo(int[] a)        { return 1; }
int goo(int[] a, int m) { return 2; }
int bar(S s)            { return 1; }
int bar(S s, int n)     { return 2; }

int baz(X)(X x)         { return 1; }
int baz(X)(X x, int n)  { return 2; }

int temp;
ref int boo(int n)      { return temp; }
ref int coo(int[] a)    { return temp; }
ref int mar(S s)        { return temp; }

ref int maz(X)(X x)     { return temp; }

void test7722a()
{
    int n;
    int[] a;
    S s;

    check!(1, 1, x =>    foo(4)     );      check!(1, 1, x =>    baz(4)     );
    check!(1, 1, x =>  4.foo()      );      check!(1, 1, x =>  4.baz()      );
    check!(0, 1, x =>  4.foo        );      check!(0, 1, x =>  4.baz        );
    check!(2, 2, x =>    foo(4, 2)  );      check!(2, 2, x =>    baz(4, 2)  );
    check!(2, 2, x =>  4.foo(2)     );      check!(2, 2, x =>  4.baz(2)     );
    check!(0, 2, x => (4.foo = 2)   );      check!(0, 2, x => (4.baz = 2)   );

    check!(1, 1, x =>    goo(a)     );      check!(1, 1, x =>    baz(a)     );
    check!(1, 1, x =>  a.goo()      );      check!(1, 1, x =>  a.baz()      );
    check!(0, 1, x =>  a.goo        );      check!(0, 1, x =>  a.baz        );
    check!(2, 2, x =>    goo(a, 2)  );      check!(2, 2, x =>    baz(a, 2)  );
    check!(2, 2, x =>  a.goo(2)     );      check!(2, 2, x =>  a.baz(2)     );
    check!(0, 2, x => (a.goo = 2)   );      check!(0, 2, x => (a.baz = 2)   );

    check!(1, 1, x =>    bar(s)     );      check!(1, 1, x =>    baz(s)     );
    check!(1, 1, x =>  s.bar()      );      check!(1, 1, x =>  s.baz()      );
    check!(0, 1, x =>  s.bar        );      check!(0, 1, x =>  s.baz        );
    check!(2, 2, x =>    bar(s, 2)  );      check!(2, 2, x =>    baz(s, 2)  );
    check!(2, 2, x =>  s.bar(2)     );      check!(2, 2, x =>  s.baz(2)     );
    check!(0, 2, x => (s.bar = 2)   );      check!(0, 2, x => (s.baz = 2)   );

    check!(2, 2, x => (  boo(4) = 2));      check!(2, 2, x => (  maz(4) = 2));
    check!(0, 2, x => (4.boo    = 2));      check!(0, 2, x => (4.maz    = 2));
    check!(2, 2, x => (  coo(a) = 2));      check!(2, 2, x => (  maz(a) = 2));
    check!(0, 2, x => (a.coo    = 2));      check!(0, 2, x => (a.maz    = 2));
    check!(2, 2, x => (  mar(s) = 2));      check!(2, 2, x => (  maz(s) = 2));
    check!(0, 2, x => (s.mar    = 2));      check!(0, 2, x => (s.maz    = 2));
}

int hoo(T)(int n)          { return 1; }
int hoo(T)(int n, int m)   { return 2; }
int koo(T)(int[] a)        { return 1; }
int koo(T)(int[] a, int m) { return 2; }
int var(T)(S s)            { return 1; }
int var(T)(S s, int n)     { return 2; }

int vaz(T, X)(X x)         { return 1; }
int vaz(T, X)(X x, int n)  { return 2; }

//int temp;
ref int voo(T)(int n)      { return temp; }
ref int woo(T)(int[] a)    { return temp; }
ref int nar(T)(S s)        { return temp; }

ref int naz(T, X)(X x)     { return temp; }

void test7722b()
{
    int n;
    int[] a;
    S s;

    check!(1, 1, x =>    hoo!int(4)     );  check!(1, 1, x =>    vaz!int(4)     );
    check!(1, 1, x =>  4.hoo!int()      );  check!(1, 1, x =>  4.vaz!int()      );
    check!(0, 1, x =>  4.hoo!int        );  check!(0, 1, x =>  4.vaz!int        );
    check!(2, 2, x =>    hoo!int(4, 2)  );  check!(2, 2, x =>    vaz!int(4, 2)  );
    check!(2, 2, x =>  4.hoo!int(2)     );  check!(2, 2, x =>  4.vaz!int(2)     );
    check!(0, 2, x => (4.hoo!int = 2)   );  check!(0, 2, x => (4.vaz!int = 2)   );

    check!(1, 1, x =>    koo!int(a)     );  check!(1, 1, x =>    vaz!int(a)     );
    check!(1, 1, x =>  a.koo!int()      );  check!(1, 1, x =>  a.vaz!int()      );
    check!(0, 1, x =>  a.koo!int        );  check!(0, 1, x =>  a.vaz!int        );
    check!(2, 2, x =>    koo!int(a, 2)  );  check!(2, 2, x =>    vaz!int(a, 2)  );
    check!(2, 2, x =>  a.koo!int(2)     );  check!(2, 2, x =>  a.vaz!int(2)     );
    check!(0, 2, x => (a.koo!int = 2)   );  check!(0, 2, x => (a.vaz!int = 2)   );

    check!(1, 1, x =>    var!int(s)     );  check!(1, 1, x =>    vaz!int(s)     );
    check!(1, 1, x =>  s.var!int()      );  check!(1, 1, x =>  s.vaz!int()      );
    check!(0, 1, x =>  s.var!int        );  check!(0, 1, x =>  s.vaz!int        );
    check!(2, 2, x =>    var!int(s, 2)  );  check!(2, 2, x =>    vaz!int(s, 2)  );
    check!(2, 2, x =>  s.var!int(2)     );  check!(2, 2, x =>  s.vaz!int(2)     );
    check!(0, 2, x => (s.var!int = 2)   );  check!(0, 2, x => (s.vaz!int = 2)   );

    check!(2, 2, x => (  voo!int(4) = 2));  check!(2, 2, x => (  naz!int(4) = 2));
    check!(0, 2, x => (4.voo!int    = 2));  check!(0, 2, x => (4.naz!int    = 2));
    check!(2, 2, x => (  woo!int(a) = 2));  check!(2, 2, x => (  naz!int(a) = 2));
    check!(0, 2, x => (a.woo!int    = 2));  check!(0, 2, x => (a.naz!int    = 2));
    check!(2, 2, x => (  nar!int(s) = 2));  check!(2, 2, x => (  naz!int(s) = 2));
    check!(0, 2, x => (s.nar!int    = 2));  check!(0, 2, x => (s.naz!int    = 2));
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7174

void test7174()
{
    @property bool foo7174() { return true; }
    static if (foo7174) {}
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7274

@property foo7274(){ return "test"; }
@property bar7274(){ return "kernel32.lib"; }

pragma(msg, "decl: ", foo7274);   // print "decl: foo", not "decl: test"
version(Windows) pragma(lib, bar7274); // Error: pragma lib string expected for library name, not 'bar'

void test7274()
{
    pragma(msg, "stmt: ", foo7274);  // print "stmt: foo", not "stmt: test"
    //pragma(lib, bar);   // Error: pragma(lib) not allowed as statement
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7275

void test7275()
{
    @property int bar1() { return 0; }
    @property int bar2() { return 1; }
    @property int bar3() { return 2; }

    switch (0){
        case bar1:  break;
        case bar2: ..
        case bar3:  break;
        default:    break;
    }
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=7538

void test7538()
{
    struct P
    {
        @property long pr() { return 1; }
        @property void pr(int) {}

        @property long a1()() { return 1; }
        @property void a1()(int) {}
        template a2() { @property long a2() { return 1; } }
        template a2() { @property void a2(int) {} }
        template a3() { long a3() @property { return 1; } }
        template a3() { void a3(int) @property {} }

      static
      {
        @property long b1()() { return 1; }
        @property void b1()(int) {}
        template b2() { @property long b2() { return 1; } }
        template b2() { @property void b2(int) {} }
        template b3() { long b3() @property { return 1; } }
        template b3() { void b3(int) @property {} }
      }

        @property long c1(T)()    { return 1; }
        @property long c1(T)(int) { return 1; }
        template c2(T) { @property long c2() { return 1; } }
        template c2(T) { @property void c2(int) {} }
        template c3(T) { long c3() @property { return 1; } }
        template c3(T) { void c3(int) @property {} }

      static
      {
        @property long d1(T)() { return 1; }
        @property void d1(T)(int) {}
        template d2(T) { @property long d2() { return 1; } }
        template d2(T) { @property void d2(int) {} }
        template d3(T) { long d3() @property { return 1; } }
        template d3(T) { void d3(int) @property {} }
      }

        void test()
        {
            // TOKvar
            static assert(is(typeof(pr) == long));

            // TOKtemplate
            static assert(is(typeof(b1) == long));
            static assert(is(typeof(b2) == long));
            static assert(is(typeof(b3) == long));

            // TOKimport
            static assert(is(typeof(d1!int) == long));
            static assert(is(typeof(d2!int) == long));
            static assert(is(typeof(d3!int) == long));
        }
    }
    P p;
    {
        // TOKdotvar
        static assert(is(typeof(p.pr) == long));

        // TOKdottd
        static assert(is(typeof(p.a1) == long));
        static assert(is(typeof(p.a2) == long));
        static assert(is(typeof(p.a3) == long));

        // TOKimport
        static assert(is(typeof(P.b1) == long));
        static assert(is(typeof(P.b2) == long));
        static assert(is(typeof(P.b3) == long));

        // TOKdotti;
        static assert(is(typeof(p.c1!int) == long));
        static assert(is(typeof(p.c2!int) == long));
        static assert(is(typeof(p.c3!int) == long));
    }

    struct F
    {
        long fn() { return 1; }
        void fn(int) {}

        long a1()() { return 1; }
        void a1()(int) {}
        template a2() { long a2() { return 1; } }
        template a2() { void a2(int) {} }

      static
      {
        long b1()() { return 1; }
        void b1()(int) {}
        template b2() { long b2() { return 1; } }
        template b2() { void b2(int) {} }
      }

        long c1(T)()    { return 1; }
        long c1(T)(int) { return 1; }
        template c2(T) { long c2() { return 1; } }
        template c2(T) { void c2(int) {} }

      static
      {
        long d1(T)() { return 1; }
        void d1(T)(int) {}
        template d2(T) { long d2() { return 1; } }
        template d2(T) { void d2(int) {} }
      }

        void test()
        {
            // TOKvar
            static assert( is(typeof(fn) == function));

            // TOKtemplate
            static assert(!is(typeof(b1) == long));
            static assert(!is(typeof(b2) == long));

            // TOKimport
            static assert(!is(typeof(d1!int) == long));
            static assert(!is(typeof(d2!int) == long));
        }
    }
    F f;
    {
        // TOKdotvar
        static assert(is( typeof(f.fn) == function));

        // TOKdottd
        static assert(!is(typeof(f.a1) == long));
        static assert(!is(typeof(f.a2) == long));

        // TOKimport
        static assert(!is(typeof(F.b1) == long));
        static assert(!is(typeof(F.b2) == long));

        // TOKdotti;
        static assert(!is(typeof(f.c1!int) == long));
        static assert(!is(typeof(f.c2!int) == long));
    }
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=8251

struct S8251
{
    static @property int min() { return 123; }
}
@property int T8251_Min() { return 456; }

template T8251a (int v)             { int T8251a  = v; }

template T8251b1(int v = S8251.min) { int T8251b1 = v; }
template T8251b2(int v = T8251_Min) { int T8251b2 = v; }

template T8251c1(int v : S8251.min) { int T8251c1 = v; }
template T8251c2(int v : T8251_Min) { int T8251c2 = v; }

void test8251()
{
    static assert(S8251.min == 123);    // OK
    static assert(T8251_Min == 456);    // OK
    int a0 = T8251a!(S8251.min());      // OK
    int b0 = T8251a!(T8251_Min());      // OK

    // TemplateValueParameter
    int a1 = T8251a!(S8251.min);        // NG
    int b1 = T8251a!(T8251_Min);        // NG

    // TemplateValueParameterDefault
    int a2 = T8251b1!();                // NG
    int b2 = T8251b2!();                // NG

    // TemplateValueParameterSpecialization
    int a3 = T8251c1!(123);             // NG
    int b3 = T8251c2!(456);             // NG
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=9063

@property bool foo9063(){ return true; }
static assert(foo9063);

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=9234

class Fizz9234
{
    void bar() {}
    Foo9234!bar foobar;
}

struct Foo9234(alias F) {}
struct Foo9234(string thunk) {}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=10103

mixin template Getter10103()
{
    @property auto foo() { return v; }
    @property auto bar()() { return v; }
    @property auto baz(T)() { return v; }

    static @property auto goo() { return 1; }
}

mixin template Setter10103()
{
    @property void foo(int x) { v = x; }
    @property void bar()(int x) { v = x; }
    @property void baz(T)(int x) { v = x; }

    static @property void goo(int x) {}
}

struct Foo10103
{
    int v;
    mixin Getter10103!();
    mixin Setter10103!();
}

void test10103()
{
    auto f = Foo10103(4);

    f.foo;
    f.foo = 3;

    f.bar;
    f.bar = 3;

    f.baz!int;
    f.baz!int = 3;

    Foo10103.goo = 3;
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=10197

template OriginalType10197(T)
{
    static if (is(T U == enum))
        alias OriginalType10197 = U;
    else
        static assert(0);
}

void test10197()
{
    enum E : int { F = -20 }
    struct S
    {
        int val;
        @trusted @property T as(T)()
        if (is(T == int) && !is(T == enum))
        {
            return cast(T)(val);
        }
        @trusted @property T as(T)()
        if (is(T == enum))
        {
            return cast(T)as!(OriginalType10197!T);
        }
    }

    S val = S(-20);
    assert(val.as!int == -20);
    assert(val.as!E == E.F);
}

/*****************************************/

int main()
{
    test1();
    test7722();
    test7722a();
    test7722b();
    test7174();
    test7274();
    test7275();
    test7538();
    test8251();
    test10103();
    test10197();

    printf("Success\n");
    return 0;
}
