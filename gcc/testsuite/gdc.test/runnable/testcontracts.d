/* PERMUTE_ARGS: -inline -g -O
TEST_OUTPUT:
---
runnable/testcontracts.d(323): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(324): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(325): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(326): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(328): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(329): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(330): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(331): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(502): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(503): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(504): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(505): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
runnable/testcontracts.d(505): Deprecation: usage of the `body` keyword is deprecated. Use `do` instead.
---
*/
extern(C) int printf(const char*, ...);

/*******************************************/

class A
{
    int x = 7;

    int foo(int i)
    in
    {
        printf("A.foo.in %d\n", i);
        assert(i == 2);
        assert(x == 7);
        printf("A.foo.in pass\n");
    }
    out (result)
    {
        assert(result & 1);
        assert(x == 7);
    }
    do
    {
        return i;
    }
}

class B : A
{
    override int foo(int i)
    in
    {
        float f;
        printf("B.foo.in %d\n", i);
        assert(i == 4);
        assert(x == 7);
        f = f + i;
    }
    out (result)
    {
        assert(result < 8);
        assert(x == 7);
    }
    do
    {
        return i - 1;
    }
}

void test1()
{
    auto b = new B();
    b.foo(2);
    b.foo(4);
}

/*******************************************/

class A2
{
    int x = 7;

    int foo(int i)
    in
    {
        printf("A2.foo.in %d\n", i);
        assert(i == 2);
        assert(x == 7);
        printf("A2.foo.in pass\n");
    }
    out (result)
    {
        assert(result & 1);
        assert(x == 7);
    }
    do
    {
        return i;
    }
}

class B2 : A2
{
    override int foo(int i)
    in
    {
        float f;
        printf("B2.foo.in %d\n", i);
        assert(i == 4);
        assert(x == 7);
        f = f + i;
    }
    out (result)
    {
        assert(result < 8);
        assert(x == 7);
    }
    do
    {
        return i - 1;
    }
}

class C : B2
{
    override int foo(int i)
    in
    {
        float f;
        printf("C.foo.in %d\n", i);
        assert(i == 6);
        assert(x == 7);
        f = f + i;
    }
    out (result)
    {
        assert(result == 1 || result == 3 || result == 5);
        assert(x == 7);
    }
    do
    {
        return i - 1;
    }
}

void test2()
{
    auto c = new C();
    c.foo(2);
    c.foo(4);
    c.foo(6);
}

/*******************************************/

void fun(int x)
in {
    if (x < 0) throw new Exception("a");
}
do {
}

void test3()
{
    fun(1);
}

/*******************************************/

interface Stack {
    int pop()
//   in { printf("pop.in\n"); }
    out(result) {
        printf("pop.out\n");
        assert(result == 3);
    }
}

class CC : Stack
{
    int pop()
    //out (result) { printf("CC.pop.out\n"); } do
    {
        printf("CC.pop.in\n");
        return 3;
    }
}

void test4()
{
    auto cc = new CC();
    cc.pop();
}

/*******************************************/

int mul100(int n)
out(result)
{
    assert(result == 500);
}
do
{
    return n * 100;
}

void test5()
{
    mul100(5);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=3273

// original case
struct Bug3273
{
    ~this() {}
    invariant() {}
}

// simplest case
ref int func3273()
out(r)
{
    // Regression check of https://issues.dlang.org/show_bug.cgi?id=3390
    static assert(!__traits(compiles, r = 1));
}
do
{
    static int dummy;
    return dummy;
}

void test6()
{
    func3273() = 1;
    assert(func3273() == 1);
}

/*******************************************/

/+
// https://issues.dlang.org/show_bug.cgi?id=3722

class Bug3722A
{
    void fun() {}
}
class Bug3722B : Bug3722A
{
    override void fun() in { assert(false); } do {}
}

void test6()
{
    auto x = new Bug3722B();
    x.fun();
}
+/

/*******************************************/

auto test7foo()
in{
    ++cnt;
}do{
    ++cnt;
    return "str";
}

void test7()
{
    cnt = 0;
    assert(test7foo() == "str");
    assert(cnt == 2);
}

/*******************************************/

auto foo8()
out(r){
    ++cnt;
    assert(r == 10);
}do{
    ++cnt;
    return 10;
}

auto bar8()
out{
    ++cnt;
}do{
    ++cnt;
}

void test8()
{
    cnt = 0;
    assert(foo8() == 10);
    assert(cnt == 2);

    cnt = 0;
    bar8();
    assert(cnt == 2);
}

/*******************************************/
// from fail317

void test9()
{
  {
    auto f1 = function() do { }; // fine
    auto f2 = function() in { } do { }; // fine
    auto f3 = function() out { } do { }; // error
    auto f4 = function() in { } out { } do { }; // error

    auto d1 = delegate() do { }; // fine
    auto d2 = delegate() in { } do { }; // fine
    auto d3 = delegate() out { } do { }; // error
    auto d4 = delegate() in { } out { } do { }; // error
  }
  {
    auto f1 = function() body { }; // fine
    auto f2 = function() in { } body { }; // fine
    auto f3 = function() out { } body { }; // error
    auto f4 = function() in { } out { } body { }; // error

    auto d1 = delegate() body { }; // fine
    auto d2 = delegate() in { } body { }; // fine
    auto d3 = delegate() out { } body { }; // error
    auto d4 = delegate() in { } out { } body { }; // error
  }
}

/*******************************************/

auto test10() do { return 3; }
auto test11()() do { return 3; }

auto test12()
{
    auto test10() do { return 3; }
    auto test11()() do { return 3; }
    return 3;
}


void test13()
{
    int function() fp13;
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=4785

int cnt;

auto foo4785()
in{
    int r;
    ++cnt;
}
out(r){
    assert(r == 10);
    ++cnt;
}do{
    ++cnt;
    int r = 10;
    return r;
}
void test4785()
{
    cnt = 0;
    assert(foo4785() == 10);
    assert(cnt == 3);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=5039

class C5039 {
    int x;

    invariant() {
        assert( x < int.max );
    }

    auto foo() {
        return x;
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=5204

interface IFoo5204
{
    IFoo5204 bar()
    out {}
}
class Foo5204 : IFoo5204
{
    Foo5204 bar() { return null; }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6417

class Bug6417
{
    void bar()
    in
    {
        int i = 14;
        assert(i == 14);
        auto dg = (){
            //printf("in: i = %d\n", i);
            assert(i == 14, "in contract failure");
        };
        dg();
    }
    out
    {
        int j = 10;
        assert(j == 10);
        auto dg = (){
            //printf("out: j = %d\n", j);
            assert(j == 10, "out contract failure");
        };
        dg();
    }
    do {}
}

void test6417()
{
    (new Bug6417).bar();
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6549

class C6549
{
    static int ocount = 0;
    static int icount = 0;

    abstract int foo(int)
    in { ++icount; }
    out { ++ocount; }
}

class CD6549 : C6549
{
    override int foo(int)
    in { assert(false); }
    do { return 10; }
}

abstract class D6549
{
    static int icount = 0;
    static int ocount = 0;

    int foo(int)
    in { ++icount; }
    out { ++ocount; }
}

class DD6549 : D6549
{
    override int foo(int)
    in { assert(false); }
    do { return 10; }
}

void test6549()
{
    auto c = new CD6549;
    c.foo(10);
    assert(C6549.icount == 1);
    assert(C6549.ocount == 1);

    auto d = new DD6549;
    d.foo(10);
    assert(D6549.icount == 1);
    assert(D6549.ocount == 1);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7218

void test7218()
{
  {
    size_t foo()  in{}  out{}  do{ return 0; } // OK
    size_t bar()  in{}/*out{}*/do{ return 0; } // OK
    size_t hoo()/*in{}*/out{}  do{ return 0; } // NG1
    size_t baz()/*in{}  out{}*/do{ return 0; } // NG2
  }
  {
    size_t goo()  in{}  out{}  body{ return 0; } // OK
    size_t gar()  in{}/*out{}*/body{ return 0; } // OK
    size_t gob()/*in{}*/out{}  body{ return 0; } // NG1
    size_t gaz()/*in{}  out{}*/body{ return 0; } // NG2
  }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7335

class A7335
{
    int mValue = 10;

    void setValue(int newValue)
    in { }
    out { assert(mValue == 3); }
    do
    {
        mValue = newValue;
    }
}

class B7335 : A7335
{
    override void setValue(int newValue)
    in { assert(false); }
    out { assert(mValue == 3); }
    do
    {
        mValue = newValue;
    }
}

class C7335 : A7335
{
    override void setValue(int newValue)
    in { int a = newValue; }
    out { assert(mValue == 3); }
    do
    {
        mValue = newValue;
    }
}

void test7335()
{
    A7335 aObject = new B7335();
    aObject.setValue(3);

    A7335 bObject = new C7335();
    bObject.setValue(3);    // <<<<<  will crash because undefined mValue in the
                            // A7335.setValue().out-block.
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7517

void test7517()
{
    static string result;

    interface I
    {
        static I self;

        void setEnable()
        in
        {
            assert(self is this);
            result ~= "I.setEnable.in/";
            assert(!enabled);
        }
        out
        {
            assert(self is this);
            result ~= "I.setEnable.out/";
            assert( enabled);
        }

        void setDisable()
        in
        {
            assert(self is this);
            result ~= "I.setDisable.in/";
            assert( enabled);
        }
        out
        {
            assert(self is this);
            result ~= "I.setDisable.out/";
            assert(!enabled);
        }

        @property bool enabled() const;
    }

    class C : I
    {
        static C self;

        void setEnable()
        in {}       // supply in-contract to invoke I.setEnable.in
        do
        {
            assert(self is this);
            result ~= "C.setEnable/";
            _enabled = true;
        }

        void setDisable()
        {
            assert(self is this);
            result ~= "C.setDisable/";
            _enabled = false;
        }

        @property bool enabled() const
        {
            assert(self is this);
            result ~= "C.enabled/";
            return _enabled;
        }

        bool _enabled;
    }

    C c = C.self = new C;
    I i = I.self = c;

    result = null;
    i.setEnable();
    assert(result == "I.setEnable.in/C.enabled/C.setEnable/I.setEnable.out/C.enabled/");

    result = null;
    i.setDisable();
    assert(result == "C.setDisable/I.setDisable.out/C.enabled/");
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7699

class P7699
{
    void f(int n) in {
        assert (n);
    } do { }
}
class D7699 : P7699
{
    override void f(int n) in { } do { }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7883

// Segmentation fault
class AA7883
{
    int foo()
    out (r1) { }
    do { return 1; }
}

class BA7883 : AA7883
{
    override int foo()
    out (r2) { }
    do { return 1; }
}

class CA7883 : BA7883
{
    override int foo()
    do { return 1; }
}

// Error: undefined identifier r2, did you mean variable r3?
class AB7883
{
    int foo()
    out (r1) { }
    do { return 1; }
}

class BB7883 : AB7883
{
    override int foo()
    out (r2) { }
    do { return 1; }

}

class CB7883 : BB7883
{
    override int foo()
    out (r3) { }
    do { return 1; }
}

// Error: undefined identifier r3, did you mean variable r4?
class AC7883
{
    int foo()
    out (r1) { }
    do { return 1; }
}

class BC7883 : AC7883
{
    override int foo()
    out (r2) { }
    do { return 1; }
}

class CC7883 : BC7883
{
    override int foo()
    out (r3) { }
    do { return 1; }
}

class DC7883 : CC7883
{
    override int foo()
    out (r4) { }
    do { return 1; }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7892

struct S7892
{
    @disable this();
    this(int x) {}
}

S7892 f7892()
out (result) {}     // case 1
do
{
    return S7892(1);
}

interface I7892
{
    S7892 f();
}
class C7892
{
    invariant() {}  // case 2

    S7892 f()
    {
        return S7892(1);
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8066

struct CLCommandQueue
{
    invariant() {}

//private:
    int enqueueNativeKernel()
    {
        assert(0, "implement me");
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8073

struct Container8073
{
    int opApply (int delegate(ref int) dg) { return 0; }
}

class Bug8073
{
    static int test;
    int foo()
    out(r) { test = 7; } do
    {
        Container8073 ww;
        foreach( xxx ; ww ) {  }
        return 7;
    }

    ref int bar()
    out { } do
    {
        Container8073 ww;
        foreach( xxx ; ww ) {  }
        test = 7;
        return test;
    }
}
void test8073()
{
    auto c = new Bug8073();
    assert(c.foo() == 7);
    assert(c.test == 7);

    auto p = &c.bar();
    assert(p == &c.test);
    assert(*p == 7);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8093

void test8093()
{
    static int g = 10;
    static int* p;

    enum fdo = q{
        static struct S {
            int opApply(scope int delegate(ref int) dg) { return dg(g); }
        }
        S s;
        foreach (ref e; s)
            return g;
        assert(0);
    };

    ref int foo_ref1() out(r) { assert(&r is &g && r == 10); }
    do { mixin(fdo); }

    ref int foo_ref2()
    do { mixin(fdo); }

    { auto q = &foo_ref1(); assert(q is &g && *q == 10); }
    { auto q = &foo_ref2(); assert(q is &g && *q == 10); }

    int foo_val1() out(r) { assert(&r !is &g && r == 10); }
    do { mixin(fdo); }

    int foo_val2()
    do { mixin(fdo); }

    { auto n = foo_val1(); assert(&n !is &g && n == 10); }
    { auto n = foo_val2(); assert(&n !is &g && n == 10); }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9383

class A9383
{
    static void delegate() dg;
    static int val;

    void failInBase() { assert(typeid(this) is typeid(A9383)); }

    // in-contract tests
    void foo1(int i) in  { A9383.val = i; failInBase; } do { }                        // no closure
    void foo2(int i) in  { A9383.val = i; failInBase; } do { int x; dg = { ++x; }; }  // closure [local]
    void foo3(int i) in  { A9383.val = i; failInBase; } do {        dg = { ++i; }; }  // closure [parameter]
    void foo4(int i) in  { A9383.val = i; failInBase; } do { }                        // no closure
    void foo5(int i) in  { A9383.val = i; failInBase; } do { }                        // no closure
    void foo6(int i) in  { A9383.val = i; failInBase; } do { int x; dg = { ++x; }; }  // closure [local]
    void foo7(int i) in  { A9383.val = i; failInBase; } do {        dg = { ++i; }; }  // closure [parameter]

    // out-contract tests
    void bar1(int i) out { A9383.val = i;             } do { }                        // no closure
    void bar2(int i) out { A9383.val = i;             } do { int x; dg = { ++x; }; }  // closure [local]
    void bar3(int i) out { A9383.val = i;             } do {        dg = { ++i; }; }  // closure [parameter]
    void bar4(int i) out { A9383.val = i;             } do { }                        // no closure
    void bar5(int i) out { A9383.val = i;             } do { }                        // no closure
    void bar6(int i) out { A9383.val = i;             } do { int x; dg = { ++x; }; }  // closure [local]
    void bar7(int i) out { A9383.val = i;             } do {        dg = { ++i; }; }  // closure [parameter]
}

class B9383 : A9383
{
    static int val;

    // in-contract tests
    override void foo1(int i) in  { B9383.val = i; } do { }                           // -> no closure
    override void foo2(int i) in  { B9383.val = i; } do { int x; dg = { ++x; }; }     // -> closure [local] appears
    override void foo3(int i) in  { B9383.val = i; } do {        dg = { ++i; }; }     // -> closure [parameter]
    override void foo4(int i) in  { B9383.val = i; } do { int x; dg = { ++x; }; }     // -> closure [local] appears
    override void foo5(int i) in  { B9383.val = i; } do {        dg = { ++i; }; }     // -> closure [parameter] appears
    override void foo6(int i) in  { B9383.val = i; } do { }                           // -> closure [local] disappears
    override void foo7(int i) in  { B9383.val = i; } do { }                           // -> closure [parameter] disappears

    // out-contract tests
    override void bar1(int i) out { B9383.val = i; } do { }                           // -> no closure
    override void bar2(int i) out { B9383.val = i; } do { int x; dg = { ++x; }; }     // -> closure [local] appears
    override void bar3(int i) out { B9383.val = i; } do {        dg = { ++i; }; }     // -> closure [parameter]
    override void bar4(int i) out { B9383.val = i; } do { int x; dg = { ++x; }; }     // -> closure [local] appears
    override void bar5(int i) out { B9383.val = i; } do {        dg = { ++i; }; }     // -> closure [parameter] appears
    override void bar6(int i) out { B9383.val = i; } do { }                           // -> closure [local] disappears
    override void bar7(int i) out { B9383.val = i; } do { }                           // -> closure [parameter] disappears
}

void test9383()
{
    auto a = new A9383();
    auto b = new B9383();

    // base class in-contract is used from derived class.       // base                   derived
    b.foo1(101); assert(A9383.val == 101 && B9383.val == 101);  // no closure          -> no closure
    b.foo2(102); assert(A9383.val == 102 && B9383.val == 102);  // closure [local]     -> closure [local] appears
    b.foo3(103); assert(A9383.val == 103 && B9383.val == 103);  // closure [parameter] -> closure [parameter]
    b.foo4(104); assert(A9383.val == 104 && B9383.val == 104);  // no closure          -> closure [local] appears
    b.foo5(105); assert(A9383.val == 105 && B9383.val == 105);  // no closure          -> closure [parameter] appears
    b.foo6(106); assert(A9383.val == 106 && B9383.val == 106);  // closure [local]     -> closure [local] disappears
    b.foo7(107); assert(A9383.val == 107 && B9383.val == 107);  // closure [parameter] -> closure [parameter] disappears

    // base class out-contract is used from derived class.      // base                   derived
    b.bar1(101); assert(A9383.val == 101 && B9383.val == 101);  // no closure          -> no closure
    b.bar2(102); assert(A9383.val == 102 && B9383.val == 102);  // closure [local]     -> closure [local] appears
    b.bar3(103); assert(A9383.val == 103 && B9383.val == 103);  // closure [parameter] -> closure [parameter]
    b.bar4(104); assert(A9383.val == 104 && B9383.val == 104);  // no closure          -> closure [local] appears
    b.bar5(105); assert(A9383.val == 105 && B9383.val == 105);  // no closure          -> closure [parameter] appears
    b.bar6(106); assert(A9383.val == 106 && B9383.val == 106);  // closure [local]     -> closure [local] disappears
    b.bar7(107); assert(A9383.val == 107 && B9383.val == 107);  // closure [parameter] -> closure [parameter] disappears

    // in-contract in base class.
    a.foo1(101); assert(A9383.val == 101);      // no closure
    a.foo2(102); assert(A9383.val == 102);      // closure [local]
    a.foo3(103); assert(A9383.val == 103);      // closure [parameter]

    // out-contract in base class.
    a.bar1(101); assert(A9383.val == 101);      // no closure
    a.bar2(102); assert(A9383.val == 102);      // closure [local]
    a.bar3(103); assert(A9383.val == 103);      // closure [parameter]
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15524
// Different from issue 9383 cases, closed variable size is bigger than REGSIZE.

class A15524
{
    static void delegate() dg;
    static string val;

    void failInBase() { assert(typeid(this) is typeid(A15524)); }

    // in-contract tests
    void foo1(string s) in  { A15524.val = s; failInBase; } do { }                                // no closure
    void foo2(string s) in  { A15524.val = s; failInBase; } do { string x; dg = { x = null; }; }  // closure [local]
    void foo3(string s) in  { A15524.val = s; failInBase; } do {           dg = { s = null; }; }  // closure [parameter]
    void foo4(string s) in  { A15524.val = s; failInBase; } do { }                                // no closure
    void foo5(string s) in  { A15524.val = s; failInBase; } do { }                                // no closure
    void foo6(string s) in  { A15524.val = s; failInBase; } do { string x; dg = { x = null; }; }  // closure [local]
    void foo7(string s) in  { A15524.val = s; failInBase; } do {           dg = { s = null; }; }  // closure [parameter]

    // out-contract tests
    void bar1(string s) out { A15524.val = s;             } do { }                                // no closure
    void bar2(string s) out { A15524.val = s;             } do { string x; dg = { x = null; }; }  // closure [local]
    void bar3(string s) out { A15524.val = s;             } do {           dg = { s = null; }; }  // closure [parameter]
    void bar4(string s) out { A15524.val = s;             } do { }                                // no closure
    void bar5(string s) out { A15524.val = s;             } do { }                                // no closure
    void bar6(string s) out { A15524.val = s;             } do { string x; dg = { x = null; }; }  // closure [local]
    void bar7(string s) out { A15524.val = s;             } do {           dg = { s = null; }; }  // closure [parameter]
}

class B15524 : A15524
{
    static string val;

    // in-contract tests
    override void foo1(string s) in  { B15524.val = s; } do { }                                   // -> no closure
    override void foo2(string s) in  { B15524.val = s; } do { string x; dg = { x = null; }; }     // -> closure [local] appears
    override void foo3(string s) in  { B15524.val = s; } do {           dg = { s = null; }; }     // -> closure [parameter]
    override void foo4(string s) in  { B15524.val = s; } do { string x; dg = { x = null; }; }     // -> closure [local] appears
    override void foo5(string s) in  { B15524.val = s; } do {           dg = { s = null; }; }     // -> closure [parameter] appears
    override void foo6(string s) in  { B15524.val = s; } do { }                                   // -> closure [local] disappears
    override void foo7(string s) in  { B15524.val = s; } do { }                                   // -> closure [parameter] disappears

    // out-contract tests
    override void bar1(string s) out { B15524.val = s; } do { }                                   // -> no closure
    override void bar2(string s) out { B15524.val = s; } do { string x; dg = { x = null; }; }     // -> closure [local] appears
    override void bar3(string s) out { B15524.val = s; } do {           dg = { s = null; }; }     // -> closure [parameter]
    override void bar4(string s) out { B15524.val = s; } do { string x; dg = { x = null; }; }     // -> closure [local] appears
    override void bar5(string s) out { B15524.val = s; } do {           dg = { s = null; }; }     // -> closure [parameter] appears
    override void bar6(string s) out { B15524.val = s; } do { }                                   // -> closure [local] disappears
    override void bar7(string s) out { B15524.val = s; } do { }                                   // -> closure [parameter] disappears
}

void test15524()
{
    auto a = new A15524();
    auto b = new B15524();

    // base class in-contract is used from derived class.           // base                   derived
    b.foo1("1"); assert(A15524.val == "1" && B15524.val == "1");    // no closure          -> no closure
    b.foo2("2"); assert(A15524.val == "2" && B15524.val == "2");    // closure [local]     -> closure [local] appears
    b.foo3("3"); assert(A15524.val == "3" && B15524.val == "3");    // closure [parameter] -> closure [parameter]
    b.foo4("4"); assert(A15524.val == "4" && B15524.val == "4");    // no closure          -> closure [local] appears
    b.foo5("5"); assert(A15524.val == "5" && B15524.val == "5");    // no closure          -> closure [parameter] appears
    b.foo6("6"); assert(A15524.val == "6" && B15524.val == "6");    // closure [local]     -> closure [local] disappears
    b.foo7("7"); assert(A15524.val == "7" && B15524.val == "7");    // closure [parameter] -> closure [parameter] disappears

    // base class out-contract is used from derived class.          // base                   derived
    b.bar1("1"); assert(A15524.val == "1" && B15524.val == "1");    // no closure          -> no closure
    b.bar2("2"); assert(A15524.val == "2" && B15524.val == "2");    // closure [local]     -> closure [local] appears
    b.bar3("3"); assert(A15524.val == "3" && B15524.val == "3");    // closure [parameter] -> closure [parameter]
    b.bar4("4"); assert(A15524.val == "4" && B15524.val == "4");    // no closure          -> closure [local] appears
    b.bar5("5"); assert(A15524.val == "5" && B15524.val == "5");    // no closure          -> closure [parameter] appears
    b.bar6("6"); assert(A15524.val == "6" && B15524.val == "6");    // closure [local]     -> closure [local] disappears
    b.bar7("7"); assert(A15524.val == "7" && B15524.val == "7");    // closure [parameter] -> closure [parameter] disappears

    // in-contract in base class.
    a.foo1("1"); assert(A15524.val == "1");     // no closure
    a.foo2("2"); assert(A15524.val == "2");     // closure [local]
    a.foo3("3"); assert(A15524.val == "3");     // closure [parameter]

    // out-contract in base class.
    a.bar1("1"); assert(A15524.val == "1");     // no closure
    a.bar2("2"); assert(A15524.val == "2");     // closure [local]
    a.bar3("3"); assert(A15524.val == "3");     // closure [parameter]
}

void test15524a()
{
    auto t1 = new Test15524a();
    t1.infos["first"] = 0; //t1.add("first");
    t1.add("second");

    auto t2 = new Test15524b();
    t2.add("second");
}

class Test15524a
{
    int[string] infos;

    void add(string key)
    in
    {
        assert(key !in infos); // @@@ crash here at second
    }
    do
    {
        auto item = new class
        {
            void notCalled()
            {
                infos[key] = 0;
                    // affects, key parameter is made a closure variable.
            }
        };
    }
}

class Test15524b
{
    void add(string key)
    in
    {
        assert(key == "second"); // @@@ fails
    }
    do
    {
        static void delegate() dg;
        dg = { auto x = key; };
            // affects, key parameter is made a closure variable.
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10479

class B10479
{
    B10479 foo()
    out { } do { return null; }
}

class D10479 : B10479
{
    override D10479 foo() { return null; }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10596

class Foo10596
{
    auto bar()
    out (result) { }
    do { return 0; }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10721

class Foo10721
{
    this()
    out { }
    do { }

    ~this()
    out { }
    do { }
}

struct Bar10721
{
    this(this)
    out { }
    do { }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10981

class C10981
{
    void foo(int i) pure
    in { assert(i); }
    out { assert(i); }
    do {}
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14779

class C14779
{
    final void foo(int v)
    in  { assert(v == 0); }
    out { assert(v == 0); }
    do
    {
    }
}

void test14779()
{
    auto c = new C14779();
    c.foo(0);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15984

I15984 i15984;
C15984 c15984;

void check15984(T)(const char* s, T this_, int i)
{
    printf("%s this = %p, i = %d\n", s, this_, i);
    static if (is(T == I15984)) assert(this_ is i15984);
    else static if (is(T == C15984)) assert(this_ is c15984);
    else static assert(0);
    assert(i == 5);
}

interface I15984
{
    void f1(int i)
    in  { check15984("I.f1.i", this, i); assert(0); }
    out { check15984("I.f1.o", this, i); }

    int[3] f2(int i)
    in  { check15984("I.f2.i", this, i); assert(0); }
    out { check15984("I.f2.o", this, i); }

    void f3(int i)
    in  { void nested() { check15984("I.f3.i", this, i); } nested(); assert(0); }
    out { void nested() { check15984("I.f3.o", this, i); } nested(); }

    void f4(out int i, lazy int j)
    in  { }
    out { }
}

class C15984 : I15984
{
    void f1(int i)
    in  { check15984("C.f1.i", this, i); }
    out { check15984("C.f1.o", this, i); }
    do  { check15984("C.f1  ", this, i); }

    int[3] f2(int i)
    in  { check15984("C.f2.i", this, i); }
    out { check15984("C.f2.o", this, i); }
    do  { check15984("C.f2  ", this, i); return [0,0,0]; }

    void f3(int i)
    in  { void nested() { check15984("C.f3.i", this, i); } nested(); }
    out { void nested() { check15984("C.f3.o", this, i); } nested(); }
    do  { check15984("C.f3  ", this, i); }

    void f4(out int i, lazy int j)
    in  { assert(0); }
    do  { i = 10; }
}

void test15984()
{
    c15984 = new C15984;
    i15984 = c15984;
    printf("i = %p\n", i15984);
    printf("c = %p\n", c15984);
    printf("====\n");
    i15984.f1(5);
    printf("====\n");
    i15984.f2(5);
    printf("====\n");
    i15984.f3(5);
    int i;
    i15984.f4(i, 1);
    assert(i == 10);
}

/*******************************************/

//******************************************/
// DIP 1009

int dip1009_1(int x)
  in  (x > 0, "x must be positive!")
  out (r; r < 0, "r must be negative!")
  in (true, "cover trailing comma case",)
  out (; true, "cover trailing comma case",)
{
    return -x;
}

int dip1009_2(int x)
  in  (x > 0)
  out (r; r < 0)
{
    return -x;
}

int dip1009_3(int x)
in  (x > 0,)
out (r; r < 0,)
do
{
    return -x;
}

void dip1009_4(int x)
  in  (x > 0)
  out (; x > 1)
{
    x += 1;
}

interface DIP1009_5
{
    void dip1009_5(int x)
      in  (x > 0)
      out (; x > 1);
}

int dip1009_6(int x, int y)
  in  (x > 0)
  out (r; r > 1)
  out (; x > 0)
  in  (y > 0)
  in  (x + y > 1)
  out (r; r > 1)
{
    return x+y;
}

int dip1009_7(int x)
  in (x > 0)
  in { assert(x > 1); }
  out { assert(x > 2); }
  out (; x > 3)
  out (r; r > 3)
{
    x += 2;
    return x;
}

class DIP1009_8
{
    private int x = 4;
    invariant (x > 0, "x must stay positive");
    invariant (x > 1, "x must be greater than one",);
    invariant (x > 2);
    invariant (x > 3,);
    void foo(){ x = 5; }
}

/*******************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
//    test6();
    test7();
    test8();
    test9();
    test4785();
    test6417();
    test6549();
    test7218();
    test7335();
    test7517();
    test8073();
    test8093();
    test9383();
    test15524();
    test15524a();
    test14779();
    test15984();
    dip1009_1(1);
    dip1009_2(1);
    dip1009_3(1);
    dip1009_4(1);
    dip1009_6(1, 1);
    dip1009_7(3);
    new DIP1009_8().foo();

    printf("Success\n");
    return 0;
}
