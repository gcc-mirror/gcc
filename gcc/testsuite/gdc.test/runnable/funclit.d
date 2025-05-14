/*
TEST_OUTPUT:
---
int delegate() pure nothrow @nogc @safe delegate() pure nothrow @nogc @safe delegate() pure nothrow @safe
int delegate() pure nothrow @nogc @safe delegate() pure nothrow @nogc @safe delegate() pure nothrow @safe
int
int
int[]
int delegate() pure nothrow @nogc @safe function() pure nothrow @safe
---

RUN_OUTPUT:
---
Success
---
*/
import core.vararg;

extern (C) int printf(const char*, ...);

/***************************************************/
// lambda syntax check

auto una(alias dg)(int n)
{
    return dg(n);
}
auto bin(alias dg)(int n, int m)
{
    return dg(n, m);
}
void test1()
{
    assert(una!(      a  => a*2 )(2) == 4);
    assert(una!( (    a) => a*2 )(2) == 4);
    assert(una!( (int a) => a*2 )(2) == 4);
    assert(una!(             (    a){ return a*2; } )(2) == 4);
    assert(una!( function    (    a){ return a*2; } )(2) == 4);
    assert(una!( function int(    a){ return a*2; } )(2) == 4);
    assert(una!( function    (int a){ return a*2; } )(2) == 4);
    assert(una!( function int(int a){ return a*2; } )(2) == 4);
    assert(una!( delegate    (    a){ return a*2; } )(2) == 4);
    assert(una!( delegate int(    a){ return a*2; } )(2) == 4);
    assert(una!( delegate    (int a){ return a*2; } )(2) == 4);
    assert(una!( delegate int(int a){ return a*2; } )(2) == 4);

    // partial parameter specialization syntax
    assert(bin!( (    a,     b) => a*2+b )(2,1) == 5);
    assert(bin!( (int a,     b) => a*2+b )(2,1) == 5);
    assert(bin!( (    a, int b) => a*2+b )(2,1) == 5);
    assert(bin!( (int a, int b) => a*2+b )(2,1) == 5);
    assert(bin!(             (    a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!(             (int a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!(             (    a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!(             (int a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function    (    a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function    (int a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function    (    a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function    (int a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function int(    a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function int(int a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function int(    a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( function int(int a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate    (    a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate    (int a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate    (    a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate    (int a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate int(    a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate int(int a,     b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate int(    a, int b){ return a*2+b; } )(2,1) == 5);
    assert(bin!( delegate int(int a, int b){ return a*2+b; } )(2,1) == 5);
}

/***************************************************/
// on initializer

void test2()
{
    // explicit typed binding ignite parameter types inference
    int function(int) fn1 = a => a*2;                               assert(fn1(2) == 4);
    int function(int) fn2 =             (    a){ return a*2; };     assert(fn2(2) == 4);
    int function(int) fn3 = function    (    a){ return a*2; };     assert(fn3(2) == 4);
    int function(int) fn4 = function int(    a){ return a*2; };     assert(fn4(2) == 4);
    int function(int) fn5 = function    (int a){ return a*2; };     assert(fn5(2) == 4);
    int function(int) fn6 = function int(int a){ return a*2; };     assert(fn6(2) == 4);
    int delegate(int) dg1 = a => a*2;                               assert(dg1(2) == 4);
    int delegate(int) dg2 =             (    a){ return a*2; };     assert(dg2(2) == 4);
    int delegate(int) dg3 = delegate    (    a){ return a*2; };     assert(dg3(2) == 4);
    int delegate(int) dg4 = delegate int(    a){ return a*2; };     assert(dg4(2) == 4);
    int delegate(int) dg5 = delegate    (int a){ return a*2; };     assert(dg5(2) == 4);
    int delegate(int) dg6 = delegate int(int a){ return a*2; };     assert(dg6(2) == 4);

    // function/delegate mismatching always raises an error
    static assert(!__traits(compiles, { int function(int) xfg3 = delegate    (    a){ return a*2; }; }));
    static assert(!__traits(compiles, { int function(int) xfg4 = delegate int(    a){ return a*2; }; }));
    static assert(!__traits(compiles, { int function(int) xfg5 = delegate    (int a){ return a*2; }; }));
    static assert(!__traits(compiles, { int function(int) xfg6 = delegate int(int a){ return a*2; }; }));
    static assert(!__traits(compiles, { int delegate(int) xdn3 = function    (    a){ return a*2; }; }));
    static assert(!__traits(compiles, { int delegate(int) xdn4 = function int(    a){ return a*2; }; }));
    static assert(!__traits(compiles, { int delegate(int) xdn5 = function    (int a){ return a*2; }; }));
    static assert(!__traits(compiles, { int delegate(int) xdn6 = function int(int a){ return a*2; }; }));

    // auto binding requires explicit parameter types at least
    static assert(!__traits(compiles, { auto afn1 = a => a*2;                           }));
    static assert(!__traits(compiles, { auto afn2 =             (    a){ return a*2; }; }));
    static assert(!__traits(compiles, { auto afn3 = function    (    a){ return a*2; }; }));
    static assert(!__traits(compiles, { auto afn4 = function int(    a){ return a*2; }; }));
    static assert(!__traits(compiles, { auto adg3 = delegate    (    a){ return a*2; }; }));
    static assert(!__traits(compiles, { auto adg4 = delegate int(    a){ return a*2; }; }));
    auto afn5 = function    (int a){ return a*2; };     assert(afn5(2) == 4);
    auto afn6 = function int(int a){ return a*2; };     assert(afn6(2) == 4);
    auto adg5 = delegate    (int a){ return a*2; };     assert(adg5(2) == 4);
    auto adg6 = delegate int(int a){ return a*2; };     assert(adg6(2) == 4);

    // partial specialized lambda
    string delegate(int, string) dg =
        (n, string s){
            string r = "";
            foreach (_; 0..n) r~=s;
            return r;
        };
    assert(dg(2, "str") == "strstr");
}

/***************************************************/
// on return statement

void test3()
{
    // inference matching system is same as on initializer
    int delegate(int) mul(int x)
    {
        return a => a * x;
    }
    assert(mul(5)(2) == 10);
}

/***************************************************/
// on function arguments

auto foo4(int delegate(int) dg) { return dg(10); }
auto foo4(int delegate(int, int) dg) { return dg(10, 20); }

void nbar4fp(void function(int) fp) { }
void nbar4dg(void delegate(int) dg) { }
void tbar4fp(T,R)(R function(T) dg) { static assert(is(typeof(dg) == void function(int))); }
void tbar4dg(T,R)(R delegate(T) dg) { static assert(is(typeof(dg) == void delegate(int))); }

auto nbaz4(void function() fp) { return 1; }
auto nbaz4(void delegate() dg) { return 2; }
auto tbaz4(R)(R function() dg) { static assert(is(R == void)); return 1; }
auto tbaz4(R)(R delegate() dg) { static assert(is(R == void)); return 2; }

auto thoo4(T)(T lambda){ return lambda; }

void tfun4a()(int function(int) a){}
void tfun4b(T)(T function(T) a){}
void tfun4c(T)(T f){}

void test4()
{
    int v;
    static void sfoo() {}
           void nfoo() {}

    // parameter type inference + overload resolution
    assert(foo4((a)   => a * 2) == 20);
    assert(foo4((a,b) => a * 2 + b) == 40);

    // function/delegate inference
    nbar4fp((int x){ });
    nbar4dg((int x){ });
    tbar4fp((int x){ });
    tbar4dg((int x){ });

    // function/delegate inference + overload resolution
    assert(nbaz4({ }) == 1);
    assert(nbaz4({ v = 1; }) == 2);
    assert(nbaz4({ sfoo(); }) == 1);    // https://issues.dlang.org/show_bug.cgi?id=8836
    assert(nbaz4({ nfoo(); }) == 2);

    assert(tbaz4({ }) == 1);
    assert(tbaz4({ v = 1; }) == 2);
    assert(tbaz4({ sfoo(); }) == 1);
    assert(tbaz4({ nfoo(); }) == 2);

    // template function deduction
    static assert(is(typeof(thoo4({ })) : void function()));
    static assert(is(typeof(thoo4({ v = 1;  })) : void delegate()));

    tfun4a(a => a);
    static assert(!__traits(compiles, { tfun4b(a => a); }));
    static assert(!__traits(compiles, { tfun4c(a => a); }));
}

void fsvarg4(int function(int)[] a...){}
void fcvarg4(int dummy, ...){}

void tsvarg4a()(int function(int)[] a...){}
void tsvarg4b(T)(T function(T)[] a...){}
void tsvarg4c(T)(T [] a...){}
void tcvarg4()(int dummy, ...){}

void test4v()
{
    fsvarg4(function(int a){ return a; });      // OK
    fsvarg4(a => a);                            // OK

    fcvarg4(0, function(int a){ return a; });   // OK
    static assert(!__traits(compiles, { fcvarg4(0, a => a); }));

    tsvarg4a(function(int a){ return a; });     // OK
    tsvarg4b(function(int a){ return a; });     // OK
    tsvarg4c(function(int a){ return a; });     // OK
    tsvarg4a(a => a);
    static assert(!__traits(compiles, { tsvarg4b(a => a); }));
    static assert(!__traits(compiles, { tsvarg4c(a => a); }));

    tcvarg4(0, function(int a){ return a; });   // OK
    static assert(!__traits(compiles, { tcvarg4(0, a => a); }));
}

// A lambda in function default argument should be deduced to delegate, by the
// preparation inferType call in TypeFunction.semantic.
void test4_findRoot(scope bool delegate(real lo, real hi) tolerance = (real a, real b) => false)
{}

/***************************************************/
// on CallExp::e1

void test5()
{
    assert((a => a*2)(10) == 20);
    assert((    a,        s){ return s~s; }(10, "str") == "strstr");
    assert((int a,        s){ return s~s; }(10, "str") == "strstr");
    assert((    a, string s){ return s~s; }(10, "str") == "strstr");
    assert((int a, string s){ return s~s; }(10, "str") == "strstr");
}

/***************************************************/
// escape check to nested function symbols

void checkNestedRef(alias dg)(bool isnested)
{
    static if (is(typeof(dg) == delegate))
        enum isNested = true;
    else static if ((is(typeof(dg) PF == F*, F) && is(F == function)))
        enum isNested = false;
    else
        static assert(0);

    assert(isnested == isNested);
    dg();
}

void freeFunc(){}

void test6()
{
    static void localFunc(){}
    void nestedLocalFunc(){}

    checkNestedRef!({  })(false);

    checkNestedRef!({ freeFunc(); })(false);
    checkNestedRef!({ localFunc(); })(false);
    checkNestedRef!({ nestedLocalFunc(); })(true);
    checkNestedRef!({ void inner(){} inner(); })(false);

    checkNestedRef!({ auto f = &freeFunc; })(false);
    checkNestedRef!({ auto f = &localFunc; })(false);
    checkNestedRef!({ auto f = &nestedLocalFunc; })(true);
    checkNestedRef!({ void inner(){} auto f = &inner; })(false);
}

/***************************************************/
// on AssignExp::e2

void test7()
{
    int function(int) fp;
    fp = a => a;
    fp = (int a) => a;
    fp = function(int a) => a;
    fp = function int(int a) => a;
    static assert(!__traits(compiles, { fp = delegate(int a) => a; }));
    static assert(!__traits(compiles, { fp = delegate int(int a) => a; }));

    int delegate(int) dg;
    dg = a => a;
    dg = (int a) => a;
    dg = delegate(int a) => a;
    dg = delegate int(int a) => a;
    static assert(!__traits(compiles, { dg = function(int a) => a; }));
    static assert(!__traits(compiles, { dg = function int(int a) => a; }));
}

/***************************************************/
// on StructLiteralExp::elements

void test8()
{
    struct S
    {
        int function(int) fp;
    }
    auto s1 = S(a => a);
    static assert(!__traits(compiles, { auto s2 = S((a, b) => a); }));
}

/***************************************************/
// on concat operation

void test9()
{
    int function(int)[] a2;
    a2 ~= x => x;
}

/***************************************************/
// on associative array key

void test10()
{
    int[int function()] aa;
    assert(!aa.remove(() => 1));

    int[int function(int)] aa2;
    assert(!aa2.remove(x => 1));
}

/***************************************************/
// on common type deduction

void test11()
{
    auto a1 = [x => x, (int x) => x * 2];
    static assert(is(typeof(a1[0]) == int function(int) pure @safe nothrow @nogc));
    assert(a1[0](10) == 10);
    assert(a1[1](10) == 20);

    //int n = 10;
    //auto a2 = [x => n, (int x) => x * 2];
    //static assert(is(typeof(a2[0]) == int delegate(int) @safe nothrow));
    //assert(a2[0](99) == 10);
    //assert(a2[1](10) == 20);

    int function(int) fp = true ? (x => x) : (x => x*2);
    assert(fp(10) == 10);

    int m = 10;
    int delegate(int) dg = true ? (x => x) : (x => m*2);
    assert(dg(10) == 10);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=3235

void test3235()
{
    // from TDPL
    auto f = (int i) {};
    static if (is(typeof(f) _ == F*, F) && is(F == function))
    {} else static assert(0);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6714

void foo6714x(int function (int, int) a){}
void bar6714x(int delegate (int, int) a){}

int bar6714y(double delegate(int, int) a){ return 1; }
int bar6714y(   int delegate(int, int) a){ return 2; }

void test6714()
{
    foo6714x((a, b) { return a + b; });
    bar6714x((a, b) { return a + b; });

    assert(bar6714y((a, b){ return 1.0;  }) == 1);
    assert(bar6714y((a, b){ return 1.0f; }) == 1);
    assert(bar6714y((a, b){ return a;    }) == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7207
// on CastExp

void test7202()
{
    auto dg = cast(int function(int))(a => a);
    assert(dg(10) == 10);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7288

void test7288()
{
    // 7288 -> OK
    auto foo()
    {
        int x;
        return () { return () => x; };
    }
    pragma(msg, typeof(&foo));
    alias int delegate() pure nothrow @nogc @safe delegate() pure nothrow @nogc @safe delegate() pure nothrow @safe Dg;
    pragma(msg, Dg);
    static assert(is(typeof(&foo) == Dg));  // should pass
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7499

void test7499()
{
    int function(int)[]   a1 = [ x => x ];  // 7499
    int function(int)[][] a2 = [[x => x]];  // +a
    assert(a1[0]   (10) == 10);
    assert(a2[0][0](10) == 10);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7500

void test7500()
{
    alias immutable bool function(int[]) Foo;
    Foo f = a => true;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7525

void test7525()
{
    {
        char[] delegate() a = { return null; };
           int delegate() b = { return 1U; };
          uint delegate() c = { return 1; };
         float delegate() d = { return 1.0; };
        double delegate() e = { return 1.0f; };
    }

    {
        char[] delegate(int) a = (x){ return null; };
           int delegate(int) b = (x){ return 1U; };
          uint delegate(int) c = (x){ return 1; };
         float delegate(int) d = (x){ return 1.0; };
        double delegate(int) e = (x){ return 1.0f; };
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7582

void test7582()
{
    void delegate(int) foo;
    void delegate(int) foo2;
    foo = (a) {
        foo2 = (b) { };
    };
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7649

void test7649()
{
    void foo(int function(int) fp = x => 1)
    {
        assert(fp(1) == 1);
    }
    foo();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7650

void test7650()
{
    int[int function(int)] aa1 = [x=>x:1, x=>x*2:2];
    foreach (k, v; aa1) {
        if (v == 1) assert(k(10) == 10);
        if (v == 2) assert(k(10) == 20);
    }

    int function(int)[int] aa2 = [1:x=>x, 2:x=>x*2];
    assert(aa2[1](10) == 10);
    assert(aa2[2](10) == 20);

    int n = 10;
    int[int delegate(int)] aa3 = [x=>n+x:1, x=>n+x*2:2];
    foreach (k, v; aa3) {
        if (v == 1) assert(k(10) == 20);
        if (v == 2) assert(k(10) == 30);
    }

    int delegate(int)[int] aa4 = [1:x=>n+x, 2:x=>n+x*2];
    assert(aa4[1](10) == 20);
    assert(aa4[2](10) == 30);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7705

void test7705()
{
    void foo1(void delegate(ref int ) dg){ int x=10; dg(x); }
    foo1((ref x){ pragma(msg, typeof(x)); assert(x == 10); });

    void foo2(void delegate(int, ...) dg){ dg(20, 3.14); }
    foo2((x,...){ pragma(msg, typeof(x)); assert(x == 20); });

    void foo3(void delegate(int[]...) dg){ dg(1, 2, 3); }
    foo3((x ...){ pragma(msg, typeof(x)); assert(x == [1,2,3]); });
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7713

void foo7713(T)(T delegate(in Object) dlg)
{}
void test7713()
{
   foo7713( (in obj) { return 15; } );   // line 6
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7743

auto foo7743a()
{
    int x = 10;
    return () nothrow {
        return x;
    };
}
auto foo7743b()
{
    int x = 10;
    return () nothrow => x;
}
void test7743()
{
    pragma(msg, typeof(&foo7743a));
    static assert(is(typeof(&foo7743a) == int delegate() pure nothrow @nogc @safe function() pure nothrow @safe));
    assert(foo7743a()() == 10);

    static assert(is(typeof(&foo7743b) == int delegate() pure nothrow @nogc @safe function() pure nothrow @safe));
    assert(foo7743b()() == 10);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7761

enum dg7761 = (int a) pure => 2 * a;

void test7761()
{
    static assert(is(typeof(dg7761) == int function(int) pure @safe nothrow @nogc));
    assert(dg7761(10) == 20);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7941

void test7941()
{
    static assert(!__traits(compiles, { enum int c = function(){}; }));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8005

void test8005()
{
    auto n = (a, int n = 2){ return n; }(1);
    assert(n == 2);
}

/***************************************************/
// test8198

void test8198()
{
    T delegate(T) zero(T)(T delegate(T) f)
    {
        return x => x;
    }

    T delegate(T) delegate(T delegate(T)) succ(T)(T delegate(T) delegate(T delegate(T)) n)
    {
        return f => x => f(n(f)(x));
    }

    uint delegate(uint) delegate(uint delegate(uint)) n = &zero!uint;
    foreach (i; 0..10)
    {
        assert(n(x => x + 1)(0) == i);
        n = succ(n);
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8226

immutable f8226 = (int x) => x * 2;

void test8226()
{
    assert(f8226(10) == 20);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8241

auto exec8241a(alias a = function(x) => x, T...)(T as)
{
    return a(as);
}

auto exec8241b(alias a = (x) => x, T...)(T as)
{
    return a(as);
}

void test8241()
{
    exec8241a(2);
    exec8241b(2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8242

template exec8242(alias a, T...)
{
    auto func8242(T as)
    {
        return a(as);
    }
}

mixin exec8242!(x => x, int);
mixin exec8242!((string x) => x, string);

void test8242()
{
    func8242(1);
    func8242("");
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8315

void test8315()
{
    bool b;
    foo8315!(a => b)();
}

void foo8315(alias pred)()
if (is(typeof(pred(1)) == bool))
{}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8397

void test8397()
{
    void function(int) f;
  static assert(!is(typeof({
    f = function(string x) {};
  })));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8496

void test8496()
{
    alias extern (C) void function() Func;

    Func fp = (){};

    fp = (){};
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8575

template tfunc8575(func...)
{
    auto tfunc8575(U)(U u) { return func[0](u); }
}
auto bar8575(T)(T t)
{
    return tfunc8575!(a => a)(t);
}
void foo8575a() { assert(bar8575(uint.init + 1) == +1); }
void foo8575b() { assert(bar8575( int.init - 1) == -1); }

void test8575()
{
    foo8575a();
    foo8575b();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9153

void writeln9153(string s){}

void test9153()
{
    auto tbl1 = [
        (string x) { writeln9153(x); },
        (string x) { x ~= 'a'; },
    ];
    auto tbl2 = [
        (string x) { x ~= 'a'; },
        (string x) { writeln9153(x); },
    ];
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9393

template ifThrown9393a(E)
{
    void ifThrown9393a(T)(scope T delegate(E) errHandler)
    {
    }
}
void ifThrown9393b(E, T)(scope T delegate(E) errHandler)
{
}

void foo9393(T)(void delegate(T) dg){ dg(T.init); }
void foo9393()(void delegate(int) dg){ foo9393!int(dg); }

void test9393()
{
    ifThrown9393a!Exception(e => 10);
    ifThrown9393b!Exception(e => 10);

    foo9393((x){ assert(x == int.init); });
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9415

void test9415()
{
    int z;
    typeof((int a){return z;}) dg;
    dg = (int a){return z;};
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9628

template TypeTuple9628(TL...) { alias TypeTuple9628 = TL; }
void map9628(alias func)() { func(0); }

void test9628()
{
    auto items = [[10, 20], [30]];
    size_t[] res;

    res = null;
    foreach (_; 0 .. 2)
    {
        foreach (sub; items)
        {
            map9628!((       i){ res ~= sub.length; });
            map9628!((size_t i){ res ~= sub.length; });
        }
    }
    assert(res == [2,2,1,1, 2,2,1,1]);

    res = null;
    foreach (_; TypeTuple9628!(0, 1))
    {
        foreach (sub; items)
        {
            map9628!((       i){ res ~= sub.length; });
            map9628!((size_t i){ res ~= sub.length; });
        }
    }
    assert(res == [2,2,1,1, 2,2,1,1]);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9928

void test9928()
{
    void* smth = (int x) { return x; };
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10133

ptrdiff_t countUntil10133(alias pred, R)(R haystack)
{
    typeof(return) i;

    alias T = dchar;

    foreach (T elem; haystack)
    {
        if (pred(elem)) return i;
        ++i;
    }

    return -1;
}

bool func10133(string s)() if (countUntil10133!(x => x == 'x')(s) == 1)
{
    return true;
}

bool func10133a(string s)() if (countUntil10133!(x => s == "x")(s) != -1)
{
    return true;
}
bool func10133b(string s)() if (countUntil10133!(x => s == "x")(s) != -1)
{
    return true;
}

void test10133()
{
    func10133!("ax")();

    func10133a!("x")();
    static assert(!is(typeof(func10133a!("ax")())));
    static assert(!is(typeof(func10133b!("ax")())));
    func10133b!("x")();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10219

void test10219()
{
    interface I { }
    class C : I { }

    void test_dg(I delegate(C) dg)
    {
        C c = new C;
        void* cptr = cast(void*) c;
        void* iptr = cast(void*) cast(I) c;
        void* xptr = cast(void*) dg(c);
        assert(cptr != iptr);
        assert(cptr != xptr); // should pass
        assert(iptr == xptr); // should pass
    }

    C delegate(C c) dg = delegate C(C c) { return c; };
    static assert(!__traits(compiles, { test_dg(dg); }));
    static assert(!__traits(compiles, { test_dg(delegate C(C c) { return c; }); }));
    static assert(!__traits(compiles, { I delegate(C) dg2 = dg; }));

    // creates I delegate(C)
    test_dg(c => c);
    test_dg(delegate(C c) => c);

    void test_fp(I function(C) fp)
    {
        C c = new C;
        void* cptr = cast(void*) c;
        void* iptr = cast(void*) cast(I) c;
        void* xptr = cast(void*) fp(c);
        assert(cptr != iptr);
        assert(cptr != xptr); // should pass
        assert(iptr == xptr); // should pass
    }

    C function(C c) fp = function C(C c) { return c; };
    static assert(!__traits(compiles, { test_fp(fp); }));
    static assert(!__traits(compiles, { test_fp(function C(C c) { return c; }); }));
    static assert(!__traits(compiles, { I function(C) fp2 = fp; }));

    // creates I function(C)
    test_fp(c => c);
    test_fp(function(C c) => c);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10288

T foo10288(T)(T x)
{
    void lambda() @trusted nothrow { x += 10; }
    lambda();
    return x;
}

T bar10288(T)(T x)
{
    () @trusted { x += 10; } ();
    return x;
}

T baz10288(T)(T arg)
{
    static int g = 10;
    () @trusted { x += g; } ();
    return x;
}

void test10288() @safe pure nothrow
{
    assert(foo10288(10) == 20); // OK
    assert(bar10288(10) == 20); // OK <- NG
    static assert(!__traits(compiles, baz10288(10)));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10666

struct S10666
{
    int val;
    ~this() {}
}

void foo10666(S10666 s1)
{
    S10666 s2;

    /* Even if closureVars(s1 and s2) are accessed by directly called lambdas,
     * they won't escape the scope of this function.
     */
    auto x1 = (){ return s1.val; }();   // OK
    auto x2 = (){ return s2.val; }();   // OK
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11081

T ifThrown11081(E : Throwable, T)(T delegate(E) errorHandler)
{
    return errorHandler();
}

void test11081()
{
    static if (__traits(compiles, ifThrown11081!Exception(e => 0)))
    {
    }
    static if (__traits(compiles, ifThrown11081!Exception(e => 0)))
    {
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11220

int parsePrimaryExp11220(int x)
{
    parseAmbig11220!( (parsed){ x += 1; } )();
    return 1;
}

typeof(handler(1)) parseAmbig11220(alias handler)()
{
    return handler(parsePrimaryExp11220(1));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11230

template map11230(fun...)
{
    auto map11230(Range)(Range r)
    {
        return MapResult11230!(fun, Range)(r);
    }
}

struct MapResult11230(alias fun, R)
{
    R _input;
    this(R input) { _input = input; }
}

class A11230 { A11230[] as; }
class B11230 { A11230[] as; }
class C11230 : A11230 {}

C11230 visit11230(A11230 a)
{
    a.as.map11230!(a => visit11230);
    return null;
}
C11230 visit11230(B11230 b)
{
    b.as.map11230!(a => visit11230);
    return null;
}
C11230 visit11230()
{
    return null;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10336

struct S10336
{
    template opDispatch(string name)
    {
        enum opDispatch = function(int x) {
            return x;
        };
    }
}

void test10336()
{
    S10336 s;
    assert(s.hello(12) == 12);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10928

struct D10928
{
    int x;
    ~this() @nogc {}
}

void f10928a(D10928 bar)
{
    (){ bar.x++; }();
}
void f10928b(D10928 bar) @nogc
{
    (){ bar.x++; }();
}

void test10928()
{
    f10928a(D10928.init);
    f10928b(D10928.init);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11661

void test11661()
{
    void delegate() dg = {};  // OK
    void function() fp = {};  // OK <- NG
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11774

void f11774(T, R)(R delegate(T[]) dg)
{
    T[] src;
    dg(src);
}

void test11774()
{
    int[] delegate(int[]) dg = (int[] a) => a;
    f11774!int(dg);
    f11774!Object(a => a);
    f11774!int(dg);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12421

void test12421()
{
    void test(string decl, bool polymorphic = true)()
    {
        // parse AliasDeclaration in Statement
        mixin("alias f = " ~ decl ~ ";");
        assert(f(1) == 1);
      static if (polymorphic)
        assert(f("s") == "s");

        // parse AliasDeclaration in DeclDefs
        mixin("template X() { alias g = " ~ decl ~ "; }");
        alias g = X!().g;
        assert(g(1) == 1);
      static if (polymorphic)
        assert(g("s") == "s");
    }

    test!(q{      x  => x });
    test!(q{ (    x) => x });
    test!(q{ (int x) => x }, false);

    test!(q{ (    x){ return x; } });
    test!(q{ (int x){ return x; } }, false);

    test!(q{ function     (    x) => x });
    test!(q{ function     (int x) => x }, false);
    test!(q{ function int (    x) => x }, false);
    test!(q{ function int (int x) => x }, false);

    test!(q{ delegate     (    x) => x });
    test!(q{ delegate     (int x) => x }, false);
    test!(q{ delegate int (    x) => x }, false);
    test!(q{ delegate int (int x) => x }, false);

    test!(q{ function     (    x){ return x; } });
    test!(q{ function     (int x){ return x; } }, false);
    test!(q{ function int (    x){ return x; } }, false);
    test!(q{ function int (int x){ return x; } }, false);

    test!(q{ delegate     (    x){ return x; } });
    test!(q{ delegate     (int x){ return x; } }, false);
    test!(q{ delegate int (    x){ return x; } }, false);
    test!(q{ delegate int (int x){ return x; } }, false);

    // This is problematic case, and should be disallowed in the future.
    alias f = x => y;
    int y = 10;
    assert(f(1) == 10);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12508

interface A12508(T)
{
    T getT();
}

class C12508 : A12508!double
{
    double getT() { return 1; }
}

void f12508(A12508!double delegate() dg)
{
    auto a = dg();
    assert(a !is null);
    assert(a.getT() == 1.0);    // fails!
}

void t12508(T)(A12508!T delegate() dg)
{
    auto a = dg();
    assert(a !is null);
    assert(a.getT() == 1.0);    // fails!
}

ref alias Dg12508 = A12508!double delegate();
void t12508(T)(Dg12508 dg) {}

void test12508()
{
    f12508({ return new C12508(); });
    t12508({ return new C12508(); });
    static assert(!__traits(compiles, x12508({ return new C12508(); })));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13879

void test13879()
{
    bool function(int)[2] funcs1 = (int x) => true; // OK
    assert(funcs1[0] is funcs1[1]);
    funcs1[0] = x => true;                          // OK
    bool function(int)[2] funcs2 = x => true;       // OK <- Error
    assert(funcs2[0] is funcs2[1]);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14745

void test14745()
{
    // qualified nested functions
    auto foo1() pure immutable { return 0; }
    auto foo2() pure const { return 0; }

    // qualified lambdas (== implicitly marked as delegate literals)
    auto lambda1 = () pure immutable { return 0; };
    auto lambda2 = () pure const { return 0; };
    static assert(is(typeof(lambda1) : typeof(&foo1)));
    static assert(is(typeof(lambda2) : typeof(&foo2)));

    // qualified delegate literals
    auto dg1 = delegate () pure immutable { return 0; };
    auto dg2 = delegate () pure const { return 0; };
    static assert(is(typeof(dg1) : typeof(&foo1)));
    static assert(is(typeof(dg2) : typeof(&foo2)));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15794

struct Foo15794
{
    static void fun(Holder)()
    {
        int i = Holder.fn();
    }
}

struct Holder15794(alias Fn)
{
    alias fn = Fn;
}

void gun15794(alias fn, U...)()
{
    Foo15794.fun!(Holder15794!fn)();
}

void test15794()
{
    gun15794!(() => 0)(); // Line 26
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=16271

ref auto funa16271(alias dg, T)(ref T a)
{
    return dg(a);
}

ref auto func16271(alias dg)()
{
    return dg();
}

void assign16271(T)(ref T a, T b)
{
    alias fun = ref (ref a) => a;
    fun(a) = b;
}

void test16271()
{
    int x;
    (delegate ref => x)() = -1;     assert(x == -1);
    (ref () => x )() = 1;           assert(x == 1);
    func16271!(ref () => x) = 2;    assert(x == 2);
    assign16271(x, 3);              assert(x == 3);

    alias alx = func16271!(ref () => x);
    alx = 4;    assert(x == 4);

    alias alf = ref (ref a) => a;
    auto  auf = ref (ref int a) => a;
    alf(x) = 5;    assert(x == 5);
    auf(x) = 6;    assert(x == 6);

    assert((funa16271!(         ref    (ref a) => a)(x) += 1) == 7 );
    assert((funa16271!(function ref    (ref a) => a)(x) += 1) == 8 );
    assert((funa16271!(function ref int(ref a) => a)(x) += 1) == 9 );
    assert((funa16271!(delegate ref    (ref a) => a)(x) += 1) == 10);
    assert((funa16271!(delegate ref int(ref a) => a)(x) += 1) == 11);
    assert(x == 11);

    alias aldc  = ref () @trusted @nogc { return x; };
    auto  audc  = ref () @safe nothrow  { return x; };
    alias alfuc = function ref (ref x) @trusted { return x; };
    alias aldec = delegate ref () @trusted { return x; };
    aldc()   = 12;    assert(x == 12);
    audc()   = 13;    assert(x == 13);
    alfuc(x) = 14;    assert(x == 14);
    aldec()  = 15;    assert(x == 15);

    template T()
    {
        int x;
        alias alf = ref () => x;
        auto auf = ref () => x;
    }
    T!().alf() = 1;  assert(T!().x == 1);
    T!().auf() = 2;  assert(T!().x == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=24525
void test24525()
{
    int a;
    auto ref () {return a;}() = 1;
    assert(a == 1);

    ref () {return a;}() = 2;
    assert(a == 2);
}

/***************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test4v();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test3235();
    test6714();
    test7202();
    test7288();
    test7499();
    test7500();
    test7525();
    test7582();
    test7649();
    test7650();
    test7705();
    test7713();
    test7743();
    test7761();
    test7941();
    test8005();
    test8198();
    test8226();
    test8241();
    test8242();
    test8315();
    test8397();
    test8496();
    test8575();
    test9153();
    test9393();
    test9415();
    test9628();
    test9928();
    test10133();
    test10219();
    test10288();
    test10336();
    test10928();
    test11661();
    test11774();
    test12421();
    test12508();
    test13879();
    test14745();
    test15794();
    test16271();
    test24525();

    printf("Success\n");
    return 0;
}
