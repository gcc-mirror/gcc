/*
RUN_OUTPUT:
---
Foo3.func()
Code3.func()
Foo4.func()
Foo5.func()
b.x = 5
x = 5
duff_for(1, 11)
fid = 1, 2
foo12
foo12
foo13 j = 1
foo13 j = 1
x14 = 6
x15 = 6
bar15() = 5
x16 = 6
bar() = 5
x17 = 5
b.x17 = 5
x17 = 3
x17 = 5
x17 = 4
x17 = 3
x17 = 5
in C20.f()
B22.foo()
5
5
a = 0
int
int
int
int
foo 1
foo 2
0 0
two
one
one
Class39 dtor
Mixed-in dtor
Mixed-in dtor
Base39 dtor
Success
---
*/

module mixin1;

import core.stdc.stdio;

alias TypeTuple(T...) = T;

/*******************************************/

mixin template Foo(T)
{
    T x;
}

mixin Foo!(uint);

struct Bar
{
    template Abc(T)
    {
        T y;
    }

    template Def(T)
    {
        T z;
    }
}

mixin Bar.Abc!(int);

Bar b;
mixin typeof(b).Def!(int);

void test1()
{
    x = 3;
    assert(x == 3);
    y = 4;
    assert(y == 4);
    z = 5;
    assert(z == 5);
}

/*******************************************/

template Foo2(T)
{
    T x2 = T.sizeof;
}

mixin Foo2!(uint) B2;
mixin Foo2!(long) C2;
mixin Foo2!(int);

void test2()
{
    B2.x2 = 3;
    assert(B2.x2 == 3);
    assert(C2.x2 == long.sizeof);
//    assert(x2 == int.sizeof);
}

/*******************************************/

template Foo3(T)
{
    int func() { printf("Foo3.func()\n"); return 1; }
}

class Bar3
{
    mixin Foo3!(int);
}

class Code3 : Bar3
{
    override int func() { printf("Code3.func()\n"); return 2; }
}

void test3()
{
    int i;

    Bar3 b = new Bar3();
    i = b.func();
    assert(i == 1);

    b = new Code3();
    i = b.func();
    assert(i == 2);
}

/*******************************************/

template Foo4(T)
{
    int func() { printf("Foo4.func()\n"); return 1; }
}

struct Bar4
{
    mixin Foo4!(int);
}

void test4()
{
    int i;

    Bar4 b;
    i = b.func();
    assert(i == 1);
}

/*******************************************/

template Foo5()
{
    int func() { printf("Foo5.func()\n"); return 1; }
}

struct Bar5
{
    mixin Foo5;
}

void test5()
{
    int i;

    Bar5 b;
    i = b.func();
    assert(i == 1);
}

/*******************************************/

template Foo6()
{
    int x = 5;
}

struct Bar6
{
    mixin Foo6;
}

void test6()
{
    int i;

    Bar6 b;
    i = b.x;
    assert(i == 5);
    assert(b.sizeof == int.sizeof);
}

/*******************************************/

template Foo7()
{
    int x = 5;
}

class Bar7
{
    int y = 6;
    mixin Foo7;
}

void test7()
{
    int i;

    Bar7 b = new Bar7();
    i = b.x;
    printf("b.x = %d\n", b.x);
    assert(i == 5);
}

/*******************************************/

template Foo8()
{
    int x = 5;
    int bar() { return 7; }
}

void test8()
{
    mixin Foo8;
    printf("x = %d\n", x);
    assert(x == 5);
    assert(bar() == 7);
}

/*******************************************/

template Foo9()
{
    int abc() { return y; }
}

void test9()
{
    int y = 8;
    mixin Foo9;
    assert(abc() == 8);
}

/*******************************************/

template Foo10(alias b)
{
    typeof(b) abc() { return b; }
}

void test10()
{
    int y = 8;
    mixin Foo10!(y);
    assert(abc() == 8);
}


/*******************************************/

template Foo11(alias b)
{
    int abc() { return b; }
}

void test11()
{
    int y = 8;
    mixin Foo11!(y) B;
    assert(B.abc() == 8);
}

/*******************************************/

template duff_for(alias id1, alias id2, alias s)
{

    void duff_for()
    {
        printf("duff_for(%d, %d)\n", id1, id2);
        typeof(id1) id = id1;
        printf("fid = %d, %d\n", id, (id2 - id) % 8);
        switch ((id2 - id) % 8)
        {
        case 0:
         while (id != id2)
         {
             printf("wid = %d\n", id);
             s(); ++id;
             goto case;
        case 7: s(); ++id; goto case;
        case 6: s(); ++id; goto case;
        case 5: s(); ++id; goto case;
        case 4: s(); ++id; goto case;
        case 3: s(); ++id; goto case;
        case 2: s(); ++id; goto case;
        case 1: s(); ++id;
             break;
        default: assert(0);
         }
        }
    }
}

void foo12() { printf("foo12\n"); }

void test12()
{
    int i = 1;
    int j = 11;

    mixin duff_for!(i, j, delegate void() { foo12(); });
    duff_for();
}

/*******************************************/

template duff(alias id1, alias id2, alias s)
{

    void duff()
    {
        s();
        s();
    }
}

void foo13(int j)
{
    printf("foo13 j = %d\n", j);
    assert(j == 1);
}

void test13()
{
    int i = 1;
    int j = 11;

    mixin duff!(i, j, delegate { foo13(i); });
    duff();
}

/*******************************************/

template Foo14()
{
    int x14 = 5;
}

void test14()
{
    int x14 = 6;
    mixin Foo14;
    printf("x14 = %d\n", x14);
    assert(x14 == 6);
}

/*******************************************/

template Foo15()
{
    int x15 = 5;

    int bar15() { return x15; }
}

int x15 = 6;
mixin Foo15;

void test15()
{

    printf("x15 = %d\n", x15);
    printf("bar15() = %d\n", bar15());
    assert(x15 == 6);
    assert(bar15() == 5);
}

/*******************************************/

template Foo16()
{
    int x16 = 5;

    int bar() { return x16; }
}

mixin Foo16 A16;
int x16 = 6;
mixin Foo16 B16;

void test16()
{

    printf("x16 = %d\n", x16);
    printf("bar() = %d\n", A16.bar());
    assert(x16 == 6);
    assert(A16.x16 == 5);
    assert(B16.x16 == 5);
    assert(A16.bar() == 5);
    assert(B16.bar() == 5);
}

/*******************************************/

template Foo17()
{
    int x17 = 5;
}

mixin Foo17;

struct Bar17
{
    mixin Foo17;
}

void test17()
{
    printf("x17 = %d\n", x17);          // prints 5
    assert(x17 == 5);
    {   Bar17 b;
        int x17 = 3;

        printf("b.x17 = %d\n", b.x17);  // prints 5
        assert(b.x17 == 5);
        printf("x17 = %d\n", x17);      // prints 3
        assert(x17 == 3);
        {
            mixin Foo17;
            printf("x17 = %d\n", x17);  // prints 5
            assert(x17 == 5);
            x17 = 4;
            printf("x17 = %d\n", x17);  // prints 4
            assert(x17 == 4);
        }
        printf("x17 = %d\n", x17);      // prints 3
        assert(x17 == 3);
    }
    printf("x17 = %d\n", x17);          // prints 5
    assert(x17 == 5);
}

/*******************************************/

template Foo18() { int z = 3; }

struct Bar18(alias Tmpl)
{
    mixin Tmpl;
}

Bar18!(Foo18) b18;

void test18()
{
    assert(b18.z == 3);
}

/*******************************************/

template Mix1(T)
{
    int foo19(T a) { return 2*a; }
}

template Mix2(T)
{
    mixin Mix1!(T);

    int bar19(T a) { return foo19(a); }
}

mixin Mix2!(int);

void test19()
{
    int i;

    i = bar19(7);
    assert(i == 14);
}

/*******************************************/

interface A20 { int f(); }

template Foo20()
{
    int f()
    {
        printf("in C20.f()\n");
        return 6;
    }
}

class C20 : A20
{
    mixin Foo20;
//    void f() { printf("in C20.f()\n"); }
}

void test20()
{
    C20 c = new C20();
    int i = c.f();
    assert(i == 6);
}

/*******************************************/

template Mix21() { this(int x) { printf("mix1\n"); }}

class Bar21
{
     int myx;
     mixin Mix21; // wouldn't compile

     this() { myx = 15; }

//     mixin Mix21; // placing it here compiles
}

void test21()
{
     Bar21 bar = new Bar21();
}

/*******************************************/

template A22(T)
{
    this()
    {   int i;
        i = super.foo();
        assert(i == 67);
    }
}


class B22
{
    int foo() { printf("B22.foo()\n"); return 67; }
}

class C22 : B22
{
    mixin A22!(C22);
}

void test22()
{
    C22 c = new C22;
}


/*******************************************/

template Foo23()
{
     const int x = 5;
}

class C23
{
     mixin Foo23 F;
}

struct D23
{
     mixin Foo23 F;
}

void test23()
{
     C23 c = new C23;

     printf("%d\n",c.F.x);
     assert(c.F.x == 5);

     D23 d;

     printf("%d\n",d.F.x);
     assert(d.F.x == 5);
}

/*******************************************/

template T24()
{
   void foo24() { return cast(void)0; }
//   alias foo24 foo24;
}

mixin T24;

void test24()
{
   foo24();
}


/*******************************************/

template ctor25()
{
 this() { this(null); }
 this( Object o ) {}
}

class Foo25
{
 mixin ctor25;
}

void test25()
{
 Foo25 foo = new Foo25();
}


/*******************************************/

template Get26(T)
{
    Reader get (ref T x)
    {
        return this;
    }
}

class Reader
{
    mixin Get26!(byte) bar;
    alias bar.get get;
    mixin Get26!(int) beq;
    alias beq.get get;
}

void test26()
{
    Reader r = new Reader;
    Reader s;
    byte q;
    s = r.get (q);
    assert(s == r);
}

/*******************************************/

template Template(int L)
{
    int i = L;
    int foo(int b = Template!(9).i) {
        return b;
    }
}

void test27()
{
    int i = 10;
    int foo(int b = Template!(9).i) {
        return b;
    }
    assert(foo()==9);
}

/*******************************************/

template Blah28(int a, alias B)
{
   mixin Blah28!(a-1, B);
   //mixin Blah28!(0, B);
}

template Blah28(int a:0, alias B)
{
}

void test28()
{
   int a;
   mixin Blah28!(5,a);
   printf("a = %d\n", a);
}

/*******************************************/

template T29()
{
    int x;
}

struct S29
{
    mixin T29;
    int y;
}

const S29 s29 = { x:2, y:3 };

void test29()
{
    assert(s29.x == 2);
    assert(s29.y == 3);
}

/*******************************************/

class A30
{
    template ctor(Type)
    {
        this(Type[] arr)
        {
            foreach(Type v; arr)
            {
                const str = typeid(typeof(v)).toString();
                printf("%.*s\n", cast(int)str.length, str.ptr);
            }
        }
    }

    mixin ctor!(int);
}

void test30()
{
    static int[] ints = [0,1,2,3];
    A30 a = new A30(ints);
}

/*******************************************/

template Share(T) {
  const bool opEquals(ref const T x) { return true; }
}

struct List31(T) {

    //int opEquals(List31 x) { return 0; }
    mixin Share!(List31);
}

void test31()
{
  List31!(int) x;
  List31!(int) y;
  int i = x == y;
  assert(i == 1);
}

/*******************************************/

template Blah(int a, alias B)
{
   mixin Blah!(a-1, B);
}

template Blah(int a:0, alias B)
{
    int foo()
    {   return B + 1;
    }
}

void test32()
{
   int a = 3;
   mixin Blah!(5,a);

   assert(foo() == 4);
}

/*******************************************/

template T33( int i )
{
    int foo()
    {
        printf("foo %d\n", i );
        return i;
    }
}


class C33
{
    mixin T33!( 1 ) t1;
    mixin T33!( 2 ) t2;
}

void test33()
{
    int i;
    C33 c1 = new C33;
    i = c1.t1.foo();
    assert(i == 1);
    i = c1.t2.foo();
    assert(i == 2);
}


/*******************************************/

template mix34()
{
    int i;
    void print()
    {
        printf( "%d %d\n", i, j );
        assert(i == 0);
        assert(j == 0);
    }
}

void test34()
{
    int j;
    mixin mix34!();

    print();
    //printf( "%i\n", i );
}

/*******************************************/

mixin T35!(int) m35;

template T35(t)
{
    t a;
}

void test35()
{
   m35.a = 3;
}

/*******************************************/

struct Foo36
{
    int a;
    mixin T!(int) m;
    template T(t)
    {
        t b;
    }
    int c;
}

void test36()
{
   Foo36 f;
   assert(f.sizeof == 12);

   f.a = 1;
   f.m.b = 2;
   f.c = 3;

   assert(f.a == 1);
   assert(f.m.b == 2);
   assert(f.c == 3);
}

/*******************************************/

template Foo37()
{
    template func() {
        int func() {
            return 6;
        }
    }
}

class Baz37
{
    mixin Foo37 bar;
}

void test37()
{
    Baz37 b = new Baz37;
    auto i = b.bar.func!()();
    assert(i == 6);
    i = (new Baz37).bar.func!()();
    assert(i == 6);
}

/*******************************************/

template Foo38()
{
    int a = 4;

    ~this()
    {
        printf("one\n");
        assert(a == 4);
        assert(b == 5);
        c++;
    }
}

class Outer38
{   int b = 5;

    static int c;

    mixin Foo38!() bar;
    mixin Foo38!() abc;

    ~this()
    {
        printf("two\n");
        assert(b == 5);
        assert(c == 0);
        c++;
    }
}

void test38()
{
    Outer38 o = new Outer38();
    destroy(o);
    assert(Outer38.c == 3);
}

/*******************************************/

template TDtor()
{
    ~this()
    {
        printf("Mixed-in dtor\n");
    }
}

class Base39
{
    ~this()
    {
        printf("Base39 dtor\n");
    }
}

class Class39 : Base39
{
    mixin TDtor A;
    mixin TDtor B;

    ~this()
    {
        printf("Class39 dtor\n");
    }
}

void test39()
{
    scope test = new Class39;
}


/*******************************************/

template Mix40()
{
  int  i;
}

struct Z40
{
  union { mixin Mix40; }
}

void test40()
{
    Z40 z;
    z.i = 3;
}

/*******************************************/


class X41(P...)
{
    alias P[0] Q;
    mixin Q!();
}

template MYP()
{
    void foo() { }
}

void test41()
{
    X41!(MYP) x;
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=2245

template TCALL2245a(ARGS...)
{
    int makecall(ARGS args)
    {
        return args.length;
    }
}

template TCALL2245b(int n)
{
    int makecall2(ARGS...)(ARGS args) if (ARGS.length == n)
    {
        return args.length;
    }
}

class C2245
{
    mixin TCALL2245a!();
    mixin TCALL2245a!(int);
    mixin TCALL2245a!(int,int);

    mixin TCALL2245b!(0);
    mixin TCALL2245b!(1);
    mixin TCALL2245b!(2);
}

struct S2245
{
    mixin TCALL2245a!();
    mixin TCALL2245a!(int);
    mixin TCALL2245a!(int,int);

    mixin TCALL2245b!(0);
    mixin TCALL2245b!(1);
    mixin TCALL2245b!(2);
}

void test2245()
{
    auto c = new C2245;
    assert(c.makecall() == 0);
    assert(c.makecall(0) == 1);
    assert(c.makecall(0,1) == 2);

    assert(c.makecall2() == 0);
    assert(c.makecall2(0) == 1);
    assert(c.makecall2(0,1) == 2);

    assert(c.makecall2!()() == 0);
    assert(c.makecall2!(int)(0) == 1);
    assert(c.makecall2!(int, int)(0,1) == 2);

    auto s = S2245();
    assert(s.makecall() == 0);
    assert(s.makecall(0) == 1);
    assert(s.makecall(0,1) == 2);

    assert(s.makecall2() == 0);
    assert(s.makecall2(0) == 1);
    assert(s.makecall2(0,1) == 2);

    assert(s.makecall2!()() == 0);
    assert(s.makecall2!(int)(0) == 1);
    assert(s.makecall2!(int, int)(0,1) == 2);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=2481

template M2481() { int i; }
class Z2481a { struct { mixin M2481!(); } }
class Z2481b { struct { int i; } }

void test2481()
{
    Z2481a z1;
    Z2481b z2;
    static assert(z1.i.offsetof == z2.i.offsetof);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=2740

interface IFooable2740
{
    bool foo();
}
abstract class CFooable2740
{
    bool foo();
}

mixin template MFoo2740()
{
    override bool foo() { return true; }
}

class Foo2740i1 : IFooable2740
{
    override bool foo() { return false; }
    mixin MFoo2740;
}
class Foo2740i2 : IFooable2740
{
    mixin MFoo2740;
    override bool foo() { return false; }
}

class Foo2740c1 : CFooable2740
{
    override bool foo() { return false; }
    mixin MFoo2740;
}
class Foo2740c2 : CFooable2740
{
    mixin MFoo2740;
    override bool foo() { return false; }
}

void test2740()
{
    {
        auto p = new Foo2740i1();
        IFooable2740 i = p;
        assert(p.foo() == false);
        assert(i.foo() == false);
    }
    {
        auto p = new Foo2740i2();
        IFooable2740 i = p;
        assert(p.foo() == false);
        assert(i.foo() == false);
    }

    {
        auto p = new Foo2740c1();
        CFooable2740 i = p;
        assert(p.foo() == false);
        assert(i.foo() == false);
    }
    {
        auto p = new Foo2740c2();
        CFooable2740 i = p;
        assert(p.foo() == false);
        assert(i.foo() == false);
    }
}

/*******************************************/

mixin template MTestFoo()
{
    int foo(){ return 2; }
}
class TestFoo
{
    mixin MTestFoo!() test;
    int foo(){ return 1; }
}
void test42()
{
    auto p = new TestFoo();
    assert(p.foo() == 1);
    assert(p.test.foo() == 2);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=7744

class ZeroOrMore7744(Expr)
{
    enum name = "ZeroOrMore7744!("~Expr.name~")";
}
class Range7744(char begin, char end)
{
    enum name = "Range7744!("~begin~","~end~")";
}

mixin(q{
    class RubySource7744 : ZeroOrMore7744!(DecLiteral7744)
    {
    }
    class DecLiteral7744 : Range7744!('0','9')
    {
    }
});

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=8032

mixin template T8032()
{
    void f() { }
}

class A8032a
{
    mixin T8032; // Named mixin causes the error too
    void f() { }
}
class B8032a : A8032a
{
    override void f() { }
}

class A8032b
{
    void f() { }
    mixin T8032; // Named mixin causes the error too
}
class B8032b : A8032b
{
    override void f() { }
}

/*********************************************/
// https://issues.dlang.org/show_bug.cgi?id=9417

mixin template Foo9417()
{
    void foo() {}
}

void test9417()
{
    struct B
    {
        mixin Foo9417;
    }
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11487

template X11487()
{
    struct R()
    {
        C11487 c;

        ~this()
        {
            static assert(is(typeof(c.front) == void));
        }
    }
    template Mix(alias R)
    {
        R!() range;
        @property front() inout {}
    }
}

class C11487
{
    alias X11487!() M;
    mixin M.Mix!(M.R);
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11767

mixin template M11767()
{
    struct S11767 {}
}
mixin M11767!();
mixin M11767!();    // OK
static assert(!__traits(compiles, S11767));

void test11767()
{
    mixin M11767!();
    alias S1 = S11767;
    {
        mixin M11767!();
        alias S2 = S11767;
        static assert(!is(S1 == S2));
        static assert(S1.mangleof == "S6mixin19test11767FZ8__mixin16S11767");
        static assert(S2.mangleof == "S6mixin19test11767FZ8__mixin26S11767");
    }
    mixin M11767!();
    static assert(!__traits(compiles, S11767));
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12023

void Delete12023(Object obj) {}

template MessageCode12023()
{
    alias typeof(this) C;

    struct MessageDeinitHelper
    {
        C m_outer;

        ~this()
        {
            m_outer.DoDeinitMessaging();
        }
    }

    CToClient toClient = null;
    TypeTuple!(CToClient) toClients;

    class CToClient {}

    void DoDeinitMessaging()
    {
        Delete12023(toClient);
        Delete12023(toClients);
    }
}

class TurretCannon12023(ProjectileClass)
{
    mixin MessageCode12023;
}

void test12023()
{
    auto tc = new TurretCannon12023!Object();
}

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14243

mixin template Mix14243a(int n)
{
    static assert(n > 0);
    import core.stdc.stdio;
    enum { enumMember = 1 }

    auto a = A14243(n);
}

mixin template Mix14243b(int n)
{
    static if (n > 0)
    {
        auto b = A14243(n);
    }
}

template foo14243(alias v) { auto bar() { return &v; } }
mixin template Mix14243c(alias v)
{
    // instantiate template in TemplateMixin
    auto c = foo14243!v.bar();
}

mixin template Mix14243d(int n)
{
    // Type declaration in TemplateMixin
    struct NS { int x = n; }
    mixin("auto d" ~ ('0' + n) ~ " = NS();");
}

mixin template Mix14243e(int n)
{
@safe:
nothrow:
    int foo() { return var; }

static:
    struct S { int x; void f() {} }
    int bar() { return n; }
}

int test14243()
{
    int[] ctor;
    int[] dtor;
    struct A14243
    {
        int x;
        this(int x) { ctor ~= x; this.x = x; }
        ~this()     { dtor ~= x; }
    }

    {
        /**/
        assert(ctor == [] && dtor == []);         mixin Mix14243a!(1);
        assert(ctor == [1] && dtor == []);        mixin Mix14243b!(12) b1;
        assert(ctor == [1,12] && dtor == []);     mixin Mix14243b!(24) b2;
        assert(ctor == [1,12,24] && dtor == []);
        assert(a.x == 1);
        static assert(!__traits(compiles, b > 0));  // ambiguous symbol access
        assert(b1.b.x == 12);
        assert(b2.b.x == 24);

        int x;
        mixin Mix14243c!(x);
        assert(c == &x);

        mixin Mix14243d!(1);
        mixin Mix14243d!(2);
        static assert(!is(typeof(d1) == typeof(d2)));
        assert(d1.x == 1);
        assert(d2.x == 2);

        assert(ctor == [1,12,24] && dtor == []);
    }
    assert(ctor == [1,12,24] && dtor == [24,12,1]);

    {
        int var = 1;
        mixin Mix14243e!12;
        static assert(is(typeof(&foo) == int delegate() @safe nothrow pure @nogc));
        static assert(is(typeof(&bar) == int function() @safe nothrow pure @nogc));
        static assert(S.sizeof == int.sizeof);  // s is static struct
        assert(foo() == 1);
        assert(bar() == 12);
    }
    return 1;
}
static assert(test14243()); // changed to be workable

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10492

class TestClass10492 {}

mixin template mix10492(string name)
{
    mixin("scope " ~ name ~ " = new TestClass10492;" );
}

void test10492()
{
    mixin mix10492!("var");
}

/*******************************************/

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
    test2245();
    test2740();
    test42();
    test9417();
    test11767();
    test12023();
    test14243();
    test10492();

    printf("Success\n");
    return 0;
}
