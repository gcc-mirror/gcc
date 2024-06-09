// PERMUTE_ARGS:
// EXTRA_SOURCES: imports/mangle10077.d
// EXTRA_FILES: imports/testmangle.d
// UNICODE_NAMES:
/*
TEST_OUTPUT:
---
_D7imports10testmangle12detectMangleFPSQBlQBg6DetectZQq
_D7imports10testmangle__T10DetectTmplTiZQpFNaNbNiNfZv
true
false
---
*/

import imports.testmangle;

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10077
// pragma(mangle)

pragma(mangle, "_test10077a_") int test10077a;
static assert(test10077a.mangleof == "_test10077a_");

__gshared pragma(mangle, "_test10077b_") ubyte test10077b;
static assert(test10077b.mangleof == "_test10077b_");

pragma(mangle, "_test10077c_") void test10077c() {}
static assert(test10077c.mangleof == "_test10077c_");

pragma(mangle, "_test10077f_") __gshared char test10077f;
static assert(test10077f.mangleof == "_test10077f_");

pragma(mangle, "_test10077g_") @system { void test10077g() {} }
static assert(test10077g.mangleof == "_test10077g_");

template getModuleInfo(alias mod)
{
    pragma(mangle, "_D"~mod.mangleof~"12__ModuleInfoZ") static __gshared extern ModuleInfo mi;
    enum getModuleInfo = &mi;
}

void test10077h()
{
    assert(getModuleInfo!(object).name == "object");
}

//UTF-8 chars
__gshared extern pragma(mangle, "test_эльфийские_письмена_9") ubyte test10077i_evar;

void test10077i()
{
    import imports.mangle10077;

    setTest10077i();
    assert(test10077i_evar == 42);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13050

void func13050(int);
template decl13050(Arg)
{
    void decl13050(Arg);
}
template problem13050(Arg)
{
    pragma(mangle, "foobar")
        void problem13050(Arg);
}
template workaround13050(Arg)
{
    pragma(mangle, "foobar")
        void func(Arg);
    alias workaround13050 = func;
}

static assert(is(typeof(&func13050) == void function(int)));
static assert(is(typeof(&decl13050!int) == void function(int)));
static assert(is(typeof(&problem13050!int) == void function(int)));
static assert(is(typeof(&workaround13050!int) == void function(int)));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=2774

int foo2774(int n) { return 0; }
static assert(foo2774.mangleof == "_D6mangle7foo2774FiZi");

class C2774
{
    int foo2774() { return 0; }
}
static assert(C2774.foo2774.mangleof == "_D6mangle5C27747foo2774MFZi");

template TFoo2774(T) {}
static assert(TFoo2774!int.mangleof == "6mangle"~tl!"15"~"__T8TFoo2774TiZ");

void test2774()
{
    int foo2774(int n) { return 0; }
    static assert(foo2774.mangleof == "_D6mangle8test2774FZ7foo2774MFNaNbNiNfiZi");
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8847

auto S8847()
{
    static struct Result
    {
        inout(Result) get() inout { return this; }
    }
    return Result();
}

void test8847a()
{
    auto a = S8847();
    auto b = a.get();
    alias typeof(a) A;
    alias typeof(b) B;
    assert(is(A == B), A.stringof~ " is different from "~B.stringof);
}

// --------

enum result8847a = "S6mangle9iota8847aFZ6Result";
enum result8847b = "S6mangle9iota8847bFZ4iotaMFZ6Result";
enum result8847c = "C6mangle9iota8847cFZ6Result";
enum result8847d = "C6mangle9iota8847dFZ4iotaMFZ6Result";

auto iota8847a()
{
    static struct Result
    {
        this(int) {}
        inout(Result) test() inout { return cast(inout)Result(0); }
    }
    static assert(Result.mangleof == result8847a);
    return Result.init;
}
auto iota8847b()
{
    auto iota()
    {
        static struct Result
        {
            this(int) {}
            inout(Result) test() inout { return cast(inout)Result(0); }
        }
        static assert(Result.mangleof == result8847b);
        return Result.init;
    }
    return iota();
}
auto iota8847c()
{
    static class Result
    {
        this(int) {}
        inout(Result) test() inout { return cast(inout)new Result(0); }
    }
    static assert(Result.mangleof == result8847c);
    return Result.init;
}
auto iota8847d()
{
    auto iota()
    {
        static class Result
        {
            this(int) {}
            inout(Result) test() inout { return cast(inout)new Result(0); }
        }
        static assert(Result.mangleof == result8847d);
        return Result.init;
    }
    return iota();
}
void test8847b()
{
    static assert(typeof(iota8847a().test()).mangleof == result8847a);
    static assert(typeof(iota8847b().test()).mangleof == result8847b);
    static assert(typeof(iota8847c().test()).mangleof == result8847c);
    static assert(typeof(iota8847d().test()).mangleof == result8847d);
}

// --------

struct Test8847
{
    enum result1 = "S6mangle8Test8847"~tl!("8")~"__T3fooZ"~id!("3foo","Qf")~"MFZ6Result";
    enum result2 = "S6mangle8Test8847"~tl!("8")~"__T3fooZ"~id!("3foo","Qf")~"MxFiZ6Result";

    auto foo()()
    {
        static struct Result
        {
            inout(Result) get() inout { return this; }
        }
        static assert(Result.mangleof == Test8847.result1);
        return Result();
    }
    auto foo()(int n) const
    {
        static struct Result
        {
            inout(Result) get() inout { return this; }
        }
        static assert(Result.mangleof == Test8847.result2);
        return Result();
    }
}
void test8847c()
{
    static assert(typeof(Test8847().foo( ).get()).mangleof == Test8847.result1);
    static assert(typeof(Test8847().foo(1).get()).mangleof == Test8847.result2);
}

// --------

void test8847d()
{
    enum resultS = "S6mangle9test8847dFZ3fooMFZ3barMFZ3bazMFZ1S";
    enum resultX = "S6mangle9test8847dFZ3fooMFZ1X";
    // Return types for test8847d and bar are mangled correctly,
    // and return types for foo and baz are not mangled correctly.

    auto foo()
    {
        struct X { inout(X) get() inout { return inout(X)(); } }
        string bar()
        {
            auto baz()
            {
                struct S { inout(S) get() inout { return inout(S)(); } }
                return S();
            }
            static assert(typeof(baz()      ).mangleof == resultS);
            static assert(typeof(baz().get()).mangleof == resultS);
            return "";
        }
        return X();
    }
    static assert(typeof(foo()      ).mangleof == resultX);
    static assert(typeof(foo().get()).mangleof == resultX);
}

// --------

void test8847e()
{
    enum resultHere = "6mangle"~"9test8847eFZ"~tl!"8"~"__T3fooZ"~id!("3foo","Qf");
    enum resultBar =  "S"~resultHere~"MFNaNfNgiZ3Bar";
    static if(BackRefs) {} else
      enum resultFoo = "_D"~resultHere~"MFNaNbNiNfNgiZNg"~resultBar;   // added 'Nb'

    // Make template function to infer 'nothrow' attributes
    auto foo()(inout int) pure @safe
    {
        struct Bar {}
        static assert(Bar.mangleof == resultBar);
        return inout(Bar)();
    }

    import core.demangle : demangle, demangleType;
    auto bar = foo(0);
    static assert(typeof(bar).stringof == "Bar");
    static assert(typeof(bar).mangleof == resultBar);
    enum fooDemangled = "pure nothrow @nogc @safe inout(mangle.test8847e().foo!().foo(inout(int)).Bar) mangle.test8847e().foo!().foo(inout(int))";

    static if (BackRefs)
      static assert(demangle(foo!().mangleof) == fooDemangled);
    else
      static assert(foo!().mangleof == resultFoo);
}

// --------

pure f8847a()
{
    struct S {}
    return S();
}

pure
{
    auto f8847b()
    {
        struct S {}
        return S();
    }
}

static assert(typeof(f8847a()).mangleof == "S6mangle6f8847aFNaZ1S");
static assert(typeof(f8847b()).mangleof == "S6mangle6f8847bFNaZ1S");

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12352

auto bar12352()
{
    struct S { int var; void func() {} }

    static assert(!__traits(compiles, bar12352.mangleof));   // forward reference to bar
    static assert(S     .mangleof ==  "S6mangle8bar12352FZ1S");
    static assert(S.func.mangleof == "_D6mangle8bar12352FZ1S4funcMFZv");

    return S();
}
static assert(       bar12352        .mangleof == "_D6mangle8bar12352FNaNbNiNfZS"~id!("6mangle8bar12352FZ","QBbQxFZ","QL2H")~"1S");
static assert(typeof(bar12352())     .mangleof ==  "S6mangle8bar12352FZ1S");
static assert(typeof(bar12352()).func.mangleof == "_D6mangle8bar12352FZ1S4funcMFZv");

auto baz12352()
{
    class C { int var; void func() {} }

    static assert(!__traits(compiles, baz12352.mangleof));   // forward reference to baz
    static assert(C     .mangleof ==  "C6mangle8baz12352FZ1C");
    static assert(C.func.mangleof == "_D6mangle8baz12352FZ1C4funcMFZv");

    return new C();
}
static assert(       baz12352      .mangleof == "_D6mangle8baz12352FNaNbNfZC"~id!("6mangle8baz12352FZ","QzQuFZ","QL2F")~"1C");
static assert(typeof(baz12352())     .mangleof ==  "C6mangle8baz12352FZ1C");
static assert(typeof(baz12352()).func.mangleof == "_D6mangle8baz12352FZ1C4funcMFZv");

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9525

void f9525(T)(in T*) { }

void test9525()
{
    enum result1 = "S6mangle8test9525FZ"~tl!"26"~"__T5test1S"~tl!"13"~id!("6mangle","QBc")~"5f9525Z"~id!("5test1","Qr")~"MFZ1S";
    enum result2 = "S6mangle8test9525FZ"~tl!"26"~"__T5test2S"~tl!"13"~id!("6mangle","QBc")~"5f9525Z"~id!("5test2","Qr")~"MFNaNbZ1S";

    bool test1(alias a)()
    {
        static struct S {}
        static assert(S.mangleof == result1);
        S s;
        a(&s);  // Error: Cannot convert &S to const(S*) at compile time
        return true;
    }
    enum evalTest1 = test1!f9525();

    bool test2(alias a)() pure nothrow
    {
        static struct S {}
        static assert(S.mangleof == result2);
        S s;
        a(&s);  // Error: Cannot convert &S to const(S*) at compile time
        return true;
    }
    enum evalTest2 = test2!f9525();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10249

template Seq10249(T...) { alias Seq10249 = T; }

mixin template Func10249(T)
{
    void func10249(T) {}
}
mixin Func10249!long;
mixin Func10249!string;

void f10249(long) {}

class C10249
{
    mixin Func10249!long;
    mixin Func10249!string;
    static assert(Seq10249!(.func10249)[0].mangleof == "6mangle9func10249");           // <- 9func10249
    static assert(Seq10249!( func10249)[0].mangleof == "6mangle6C102499func10249");    // <- 9func10249

static: // necessary to make overloaded symbols accessible via __traits(getOverloads, C10249)
    void foo(long) {}
    void foo(string) {}
    static assert(Seq10249!(foo)[0].mangleof                                   ==   "6mangle6C102493foo");         // <- _D6mangle6C102493fooFlZv
    static assert(Seq10249!(__traits(getOverloads, C10249, "foo"))[0].mangleof == "_D6mangle6C102493fooFlZv");     // <-
    static assert(Seq10249!(__traits(getOverloads, C10249, "foo"))[1].mangleof == "_D6mangle6C102493fooFAyaZv");   // <-

    void g(string) {}
    alias bar = .f10249;
    alias bar =  g;
    static assert(Seq10249!(bar)[0].mangleof                                   ==   "6mangle6C102493bar");         // <- _D6mangle1fFlZv
    static assert(Seq10249!(__traits(getOverloads, C10249, "bar"))[0].mangleof == "_D6mangle6f10249FlZv");         // <-
    static assert(Seq10249!(__traits(getOverloads, C10249, "bar"))[1].mangleof == "_D6mangle6C102491gFAyaZv");     // <-
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11718

struct Ty11718(alias sym) {}

auto fn11718(T)(T a)   { return Ty11718!(a).mangleof; }
auto fn11718(T)() { T a; return Ty11718!(a).mangleof; }

void test11718()
{
    string TyName(string tail)()
    {
        enum s = "__T7Ty11718" ~ tail;
        enum len = unsignedToString(s.length);
        return "S6mangle" ~ tl!(len) ~ s;
    }
    string fnName(string paramPart)()
    {
        enum s = "_D6mangle"~tl!("35")~"__T7fn11718T"~
                 "S6mangle9test11718FZ1AZ7fn11718"~paramPart~"1a"~
                 "S6mangle9test11718FZ1A";
        enum len = unsignedToString(s.length);
        return tl!len ~ s;
    }
    enum result1 = TyName!("S" ~ fnName!("F"~"S6mangle9test11718FZ1A"~"Z") ~ "Z") ~ "7Ty11718";
    enum result2 = TyName!("S" ~ fnName!("F"~""                      ~"Z") ~ "Z") ~ "7Ty11718";

    struct A {}
    static if (BackRefs)
    {
        static assert(fn11718(A.init) == "S6mangle__T7Ty11718S_DQv__T7fn11718TSQBk9test11718FZ1AZQBcFQxZ1aQBcZQCf");
        static assert(fn11718!A()     == "S6mangle__T7Ty11718S_DQv__T7fn11718TSQBk9test11718FZ1AZQBcFZ1aQBaZQCd");
    }
    else
    {
        pragma(msg, fn11718(A.init));
        static assert(fn11718(A.init) == result1);
        static assert(fn11718!A()     == result2);
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11776

struct S11776(alias fun) { }

void test11776()
{
    auto g = ()
    {
        if (1)
            return; // fill tf->next
        if (1)
        {
            auto s = S11776!(a => 1)();
            static assert(typeof(s).mangleof ==
                "S"~"6mangle"~tl!("56")~
                ("__T"~"6S11776"~"S"~tl!("42")~
                 (id!("6mangle","Qs")~"9test11776"~"FZ"~"9__lambda1MFZ"~id!("9__lambda1","Qn"))~"Z"
                 )~id!("6S11776", "QBm"));
        }
    };
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12044

struct S12044(T)
{
    void f()()
    {
        new T[1];
    }

    bool opEquals(O)(O)
    {
        f();
    }
}

void test12044()
{
    ()
    {
        enum E { e }
        auto arr = [E.e];
        S12044!E s;
    }
    ();
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12217

void test12217(int)
{
    static struct S {}
    void bar() {}
    int var;
    template X(T) {}

    static assert(    S.mangleof ==  "S6mangle9test12217FiZ1S");
    static assert(  bar.mangleof == "_D6mangle9test12217FiZ3barMFNaNbNiNfZv");
    static assert(  var.mangleof == "_D6mangle9test12217FiZ3vari");
    static assert(X!int.mangleof ==   "6mangle9test12217FiZ"~tl!("8")~"__T1XTiZ");
}

void test12217() {}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12231

void func12231a()()
if (is(typeof({
        class C {}
        static assert(C.mangleof ==
            "C6mangle"~tl!("16")~"__U10func12231aZ"~id!("10func12231a","Qn")~"FZ9__lambda1MFZ1C");
            //         ###            L                       #
    })))
{}

void func12231b()()
if (is(typeof({
        class C {}        static assert(C.mangleof ==
            "C6mangle"~tl!("16")~"__U10func12231bZ"~id!("10func12231b","Qn")~"FZ9__lambda1MFZ1C");
            //         L__L           L                       LL
      })) &&
    is(typeof({
        class C {}
        static assert(C.mangleof ==
            "C6mangle"~tl!("16")~"__U10func12231bZ"~id!("10func12231b","Qn")~"FZ9__lambda2MFZ1C");
            //         L__L           L                       LL
    })))
{}

void func12231c()()
if (is(typeof({
        class C {}
        static assert(C.mangleof ==
            "C6mangle"~tl!("16")~"__U10func12231cZ"~id!("10func12231c","Qn")~"FZ9__lambda1MFZ1C");
            //         L__L           L                       LL
    })))
{
    (){
        class C {}
        static assert(C.mangleof ==
            "C6mangle"~tl!("16")~"__T10func12231cZ"~id!("10func12231c","Qn")~"FZ9__lambda1MFZ1C");
            //         L__L           L                       LL
    }();
}

void func12231c(X)()
if (is(typeof({
        class C {}
    static assert(C.mangleof ==
            "C6mangle"~tl!("20")~"__U10func12231cTAyaZ"~id!("10func12231c","Qr")~"FZ9__lambda1MFZ1C");
            //         L__L           L___L                       LL
    })))
{
    (){
        class C {}
        static assert(C.mangleof ==
            "C6mangle"~tl!("20")~"__T10func12231cTAyaZ"~id!("10func12231c","Qr")~"FZ9__lambda1MFZ1C");
            //         L__L           L___L                       LL
    }();
}

void test12231()
{
    func12231a();

    func12231b();

    func12231c();
    func12231c!string();
}

/***************************************************/

class CC
{
    int* p;

    int* member() scope
    {
        return p;
    }
}

static assert(CC.member.mangleof == "_D6mangle2CC6memberMFNlZPi");

/***************************************************/

void fooA(void delegate (scope void delegate()) dg)
{
}
void fooB(void delegate (void delegate()) scope dg)
{
}

//pragma(msg, fooA.mangleof);
//pragma(msg, fooB.mangleof);
static assert(typeof(fooA).mangleof != typeof(fooB).mangleof);


/***************************************************/

@live int testLive() { return 42; }

static assert(testLive.mangleof == "_D6mangle8testLiveFNmZi");

/***************************************************/

alias noreturn = typeof(*null);
alias fpd = noreturn function();
int funcd(fpd);
static assert(funcd.mangleof == "_D6mangle5funcdFPFZNnZi");

/***************************************************/

struct S21753 { void function() f1; }
void fun21753(S21753 v)() {}
alias fl21753 = (){};
static assert((fun21753!(S21753(fl21753))).mangleof == "_D6mangle__T8fun21753VSQv6S21753S1f_DQBj10" ~ fl21753.stringof ~ "MFNaNbNiNfZvZQCbQp");

/***************************************************/
void main()
{
    test10077h();
    test10077i();
    test12044();
}
