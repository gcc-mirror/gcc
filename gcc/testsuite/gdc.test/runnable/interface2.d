// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

/*******************************************************/

interface Foo { int bar(); }

void* p1;

class Bar : Foo
{
    int bar()
    {
        printf("Bar.bar(this = %p)\n", this);
        p1 = cast(void*)this;
        return 0;
    }
}

void test1()
{
    Bar b = new Bar();
    Foo f = b;

    printf("b = %p\n", b);
    printf("f = %p\n", f);
    assert(cast(void*)b !is cast(void*)f);

    printf("f.class = '%.*s'\n", cast(int)f.classinfo.name.length, f.classinfo.name.ptr);
    assert(f.classinfo.name == "interface2.Foo");

    f.bar();
    assert(p1 is cast(void*)b);

    Bar b2 = cast(Bar)f;
    printf("cast(Bar)f = %p\n", b2);
    assert(b is b2);

    destroy(f);
}

/*******************************************************/

interface A {}
interface B:A {}
interface C {}
class D:B,C {}

void test2()
{
    D x = new D();
    printf("D: %p\n",x);
    Object o = x;
    printf("o: %p\n",o);
    B b = cast(B)o;
    printf("B: %p\n",b);
    C c = cast(C)o;    // boom
    printf("C: %p\n",c);
}

/*******************************************************/

interface B3
{
    void close();
}

interface C3 : B3
{
}

class A3 : B3
{
    void close()
    {
    }
}

class D3 : A3
{
}

class E3 : D3, C3
{
}

void test3()
{
    C3 c = new E3();
    destroy(c);
}


/*******************************************************/

interface K
{
}

interface X
{
}

interface Y : X
{
}

class Z : Y
{
}

void test4()
{
    Z z = new Z();
    if (cast(K) z)
    {
        printf("not ok\n");
        assert(0);
    }
}

/*******************************************************/

interface I5
{
    char M ();
}

interface J5 : I5
{
    char N ();
}

class A5 : I5
{
    char M () { printf("M()\n"); return 'M'; }
}

class B5 : A5, J5
{
    char N () { printf("N()\n"); return 'N'; }
}

void test5()
{
    I5 f = new B5 ();

    char c = f.M();
    assert(c == 'M');
}

/*******************************************************/

interface A6
{
    void ma ();
}

interface B6
{
    void mb ();
}

class C6 : A6, B6
{
    void ma () { }
    void mb () { }
}

void test6()
{
    A6 x = new C6 ();

    assert (cast (B6) x);
}

/*******************************************************/

interface D7 { int foo(); }

class A7 : D7 { int foo() { return 1; } }

class B7 : A7
{
    override int foo() { return 2; }
    D7 me() { return this; }
}

void test7()
{
    A7 a = new A7;
    B7 b = new B7;
    assert(b.me().foo() != a.foo());
}

/*******************************************************/

interface D8 { void foo(); }

class A8 : D8 { void foo() { printf("A8.foo()\n"); } }

class B8 : A8 {}

void test8()
{
    B8 b = new B8();
    D8 d = cast(D8) b;
    d.foo();
}

/*******************************************************/

interface IA9
{
    int i1();
}

interface IB9
{
    int i2();
}

interface IC9 : IA9, IB9
{
}

class C9 : IC9
{
    int i1() { printf("i1\n"); return 1; }
    int i2() { printf("i2\n"); return 2; }
}

void f9(IA9 i1, IB9 i2)
{
    int i;

    printf("f9\n");
    i = i1.i1();
    assert(i == 1);
    i = i2.i2();
    assert(i == 2);
}

void test9()
{
    IC9 i3 = new C9();
    C9 c = new C9();
    f9(c, c);
    //printf("c = %p, IC9 = %p, IA9 = %p, IB9 = %p\n", c, i3, cast(IA9)i3, cast(IB9)i3);
    f9(i3, i3);
}


/*******************************************************/

interface IOne
{
    int one ();
}

interface ITwo
{
    int two ();
}

interface IThree : IOne, ITwo
{
    int three ();
}

class Three : IThree
{
    int one ()   { printf ("one\n"); return 1; }
    int two ()   { printf ("two\n"); return 2; }
    int three () { printf ("three\n"); return 3; }
}


void test10()
{
    int i;
    IThree three = new Three;

    i = three.one();
    assert(i == 1);
    i = three.two();
    assert(i == 2);
    i = three.three();
    assert(i == 3);

    ITwo two = cast(ITwo) three;
    i = two.two();
    assert(i == 2);
}


/*******************************************************/

interface A11{
}

interface B11 : A11{
}

class MyClass : B11{
}

void test11()
{
    B11 b = new MyClass();
    Object o = cast(Object)b;
    printf("o = %p\n", o);
}

/*******************************************************/

interface I12
{
    int foo();
}

class IA12 : I12
{
    int foo() { return 1; }
}

class A12
{
    I12 i;

    I12 clone() { return i; }
}

class B12 : A12
{
    IA12 ia;

    override IA12 clone()   // covariant return value
    out (result)
    {
        printf("B12.clone()\n");
    }
    do
    {
        return ia;
    }
}

void test12()
{
    IA12 ia = new IA12;
    assert(ia.foo() == 1);

    I12 i = ia;
    assert(i.foo() == 1);

    A12 a = new A12;
    a.i = i;
    assert(a.clone().foo() == 1);

    B12 b = new B12;
    b.ia = ia;
    assert(b.clone().foo() == 1);

    a = b;
    assert(a.clone().foo() == 1);
}

/*******************************************************/

class I13
{
    int foo() { return 0; }
}

class IA13 : I13
{
    override int foo() { return 1; }
}

class A13
{
    I13 i;

    I13 clone() { return i; }
}

class B13 : A13
{
    IA13 ia;

    override IA13 clone()
    out (result)
    {
        printf("B13.clone()\n");
    }
    do { return ia; }
}

void test13()
{
    IA13 ia = new IA13;
    assert(ia.foo() == 1);

    I13 i = ia;
    assert(i.foo() == 1);

    A13 a = new A13;
    a.i = i;
    assert(a.clone().foo() == 1);

    B13 b = new B13;
    b.ia = ia;
    assert(b.clone().foo() == 1);

    a = b;
    assert(a.clone().foo() == 1);

    bar(&b.clone);
}

void bar(IA13 delegate() dg)
{
}

/*******************************************************/

interface I14
{
        I14 clone();
}

interface BabyI14: I14
{
}

class A14: BabyI14
{
    int x;
    BabyI14 clone()
    {
        A14 a = new A14;
        a.x = x;
        return a;
    }
}

I14 foo14(I14 i)
{
    return i.clone();
}

void test14()
{
    A14 a = new A14;
    a.x = 3;
    a = cast(A14)a.clone();
    assert(a.x == 3);

    A14 b = cast(A14)foo14(a);
    a.x = 4;
    assert(b.x == 3);
}

/*******************************************************/

interface I15
{
        Object clone();
}

class A15 : I15
{
    int x;
    A15 clone() { return this; }
}

void test15()
{
    A15 a = new A15;
    a.x = 3;

    A15 a1 = a.clone();
    assert(a1.x == 3);

    I15 i = a1;
    Object o = i.clone();
    A15 a2 = cast(A15) o;
    assert(a2.x == 3);
}

/*******************************************************/

interface I16 {}

class A16
{
    I16 foo()
    {
        printf("Called A.foo\n");
        return new B16(42);
    }
}

class B16 : A16, I16
{
    int data;

    this(int d) { data = d; }

    override B16 foo()
    {
        printf("Called B.foo\n");
        return new B16(69);
    }
}

void test16()
{
    B16 b = new B16(105);
    b.foo();
    A16 a = b;
    a.foo();
    printf("foo\n");
    B16 b2 = cast(B16) a.foo();
}


/*******************************************************/

interface Father
{
    int showData();
}

class Mother
{
    Father test() {
        printf("Called Mother.test\n");
        return new Child(42);
    }
}

class Child : Mother, Father {
    int data;

    this(int d) { data = d; }

    override Child test()
    {
        printf("Called Child.test\n");
        return new Child(69);
    }

    int showData() {
        printf("%d\n", data);
        return data;
    }
}

void test17()
{
    Child aChild = new Child(105);
    Mother childsMum = aChild;

    aChild.test();
    Father mumTest = childsMum.test();
    int i;
    i = aChild.showData();
    assert(i == 105);
    i = mumTest.showData();
    assert(i == 69);
}


/*******************************************************/

int status18;

interface I18 {
    int showData();
}

class Parent18 {
    I18 test() {
        status18 += 7;
        return new Child18(42);
    }
}

class Child18 : Parent18, I18 {
    int data;

    this(int d) {
        data = d;
    }

    override Child18 test() {
        status18 += 1;
        return new Child18(69);
    }

    override
     int showData(){
        return data;
    }
}

void test18()
{
    Child18 a = new Child18(105);
    assert(a);
    assert(status18 == 0);
    assert(a.data == 105);

    Parent18 p = a;
    assert(a);
    assert(status18 == 0);

    a.test();
    assert(status18 == 1);

    I18 i = p.test();
    assert(i);
    assert(status18 == 2);

    assert(a.data == 105);
    assert(a.showData() == 105);
    assert(i.showData() == 69);
}

/*******************************************************/


interface IFoo19 {
}

interface ICov19 {
    IFoo19 covfunc();
}

class Child19 : ICov19, IFoo19 {
    Child19 covfunc() {
        printf("in Child19.covfunc()\n");
        return this;
    }
}

void test19()
{
    Child19 c = new Child19();
    ICov19 icov = c;

    IFoo19 ifoo = icov.covfunc();

    printf("c = %p\n", c);
    printf("icov = %p\n", icov);
    printf("ifoo = %p\n", ifoo);

    assert(cast(void*)c + (2*(void*).sizeof) == cast(void*)icov);
    assert(cast(void*)c + (3*(void*).sizeof) == cast(void*)ifoo);

    string s = ifoo.classinfo.name;
    printf("%.*s\n", cast(int)s.length, s.ptr);
    assert(s == "interface2.IFoo19");

    s = (cast(Object)ifoo).toString;
    printf("%.*s\n", cast(int)s.length, s.ptr);
    assert(s == "interface2.Child19");
}

/*******************************************************/

interface Iface1
{
    Iface2 func1();
}

interface Iface2
{
    Iface1 func2();
}

class C1_20 : Iface1
{
    C2_20 func1(){ return null; }
}

class C2_20 : Iface2
{
    C1_20 func2(){ return null; }
}

void test20()
{
    C1_20 c1 = new C1_20();
    printf("c1.func1() == %p\n", c1.func1());
    assert(c1.func1() is null);
    printf("test1\n");

    C2_20 c2 = new C2_20();
    printf("c2.func2() == %p\n", c2.func2());
    assert(c2.func2() is null);
}

/*******************************************************/

interface I21
{
    int test(int);
}

class C21 : I21
{
    int test(int i){
        return i+1;
    }
}

void test21()
{
    C21[I21] aa;

    C21 o = new C21();

    aa[o] = o;

    I21 i = aa[o];
    assert(i.test(3) == 4);
}


/*******************************************************/

interface IFoo22
{
   int foo();
}

class Foo22: IFoo22
{
   final int foo() { return 7; }
}

void test22()
{
    Foo22 f = new Foo22;
    assert(f.foo() == 7);

    IFoo22 i = f;
    assert(i.foo() == 7);
}

/*******************************************************/

interface IFoo23
{
   int foo();
}

class Foo23: IFoo23
{
   final int foo() { return 7; }
}

class Baz23 : Foo23
{
}

void test23()
{
    Baz23 f = new Baz23;
    assert(f.foo() == 7);

    IFoo23 i = f;
    assert(i.foo() == 7);
}

/*******************************************************/

interface I24B() : I24A
{
}

interface I24A
{
    I24B!() func ();
}

class Foo24 : I24B!()
{
    I24B!() func()
    {
        return null;
    }
}

void test24()
{
    auto foo = new Foo24();

    foo.func();
    printf("foo.func() call passed\n");

    I24A ifA = foo;
    assert(ifA !is null);
    ifA.func();
}

/*******************************************************/

interface IA25
{
    char a();
}

interface IB25
{
    char b();
}

interface IC25 : IA25, IB25
{
    char c();
}

interface ID25
{
    char d();
}

interface IE25 : IC25, ID25
{
    char e();
}

class Foo25 : IE25
{
    char a() { return('a'); }
    char b() { return('b'); }
    char c() { return('c'); }
    char d() { return('d'); }
    char e() { return('e'); }
}

void test25()
{
    auto foo = new Foo25;
    printf("Foo: %c %c %c %c %c\n", foo.a, foo.b, foo.c, foo.d, foo.e);
    IA25 a = foo;
    printf("A: %c\n", a.a);
    assert(a.a == 'a');
    IB25 b = foo;
    printf("B: %c\n", b.b);
    assert(b.b == 'b');
    IC25 c = foo;
    printf("C: %c %c %c\n", c.a, c.b, c.c);
    assert(c.a == 'a');
    assert(c.b == 'b');
    assert(c.c == 'c');
    ID25 d = foo;
    printf("D: %c\n", d.d);
    assert(d.d == 'd');
    IE25 e = foo;
    printf("E: %c %c %c %c %c\n", e.a, e.b, e.c, e.d, e.e);
    assert(e.a == 'a');
    assert(e.b == 'b');
    assert(e.c == 'c');
    assert(e.d == 'd');
    assert(e.e == 'e');

    b = e;
    printf("IB25: %c\n", b.b);
    assert(b.b == 'b');
}

/*******************************************************/

interface VisualElement {
   void draw();
}

interface Actor {
}

interface VisualActor : Actor, VisualElement {
}

class Sprite3 : Actor, VisualActor {
    override void draw() { }
}

void test26()
{
}

/*******************************************************/

interface I27
{
    static int foo() { return 3; }

    final int bar() { return 7 + abc(); }

    int abc();
}

class C27 : I27
{
    int x;
    int abc() { return x * 10; }
}

void test27()
{
    C27 c = new C27();
    c.x = 8;
    I27 i = c;
    assert(i.foo() == 3);
    assert(I27.foo() == 3);
    assert(i.bar() == 87);
}

/*******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=1747
// https://issues.dlang.org/show_bug.cgi?id=2013

void test1747()
{
    interface IA          { int mA(); }
    interface IB : IA     { int mB(); }
    interface IC : IB     { }
    interface ID : IA, IC { int mD(); }

    // offset:   0   +n  +n + ptrsize
    //                  (IA)
    //                   IB
    //               IA, IC
    static class C : ID
    {
        int mA() { return 1; }
        int mB() { return 2; }
        int mD() { return 3; }
    }

    C c = new C;    void* pc  = *cast(void**)&c;
    ID id = c;      void* pid = *cast(void**)&id;
    IC ic = c;      void* pic = *cast(void**)&ic;
    IB ib = c;      void* pib = *cast(void**)&ib;
    IA ia = c;      void* pia = *cast(void**)&ia;

    //printf(" c = %p\n", pc);
    //printf("id = %p\n", pid);
    //printf("ic = %p\n", pic);
    //printf("ib = %p\n", pib);
    //printf("ia = %p\n", pia);

    size_t n = pid - pc;
    assert(pic == pc + n + (void*).sizeof);
    assert(pib == pc + n + (void*).sizeof);     // OK <- NG
    assert(pia == pc + n);

    assert(id.mA() == 1);
    assert(id.mB() == 2);   // OK <- NG (bugzilla 2013 case)
    assert(id.mD() == 3);

    assert(ic.mA() == 1);
    assert(ic.mB() == 2);   // OK <- NG (bugzilla 2013 case)

    assert(ib.mA() == 1);
    assert(ib.mB() == 2);   // OK <- NG

    assert(ia.mA() == 1);
}

/*******************************************************/

private interface IFoo
{
   void foo();
}

void test2553()
{
    IFoo foo;
    if (0)
        foo.foo;
}

/*******************************************************/

interface I2524
{
    void foo();
}

class C2524 : I2524
{
    final override void foo() { }
}

/*******************************************************/

interface Test4088 {}

bool foo4088(Test4088 x, Test4088 y)
{
    return x == y;
}

/*******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7950

template TypeTuple7950(T...){alias T TypeTuple7950;}
interface I7950a {} // ok
interface I7950b : I7950a, TypeTuple7950!() {} // fail

/*******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10007

struct A10007 {}

interface IFoo10007
{
    void bar(ref const A10007);
}

class Foo10007 : IFoo10007
{
    void bar(ref const A10007 a) {}
    void bar(    const A10007 a) { return this.bar(a); }
}

/*******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10744

interface A10744
{
    int x();
    Foo10744 foo();
}

class B10744 : A10744
{
    int x() { return 0; }
    Bar10744 foo() { return null; }
}

class Foo10744 { }
class Bar10744 : Foo10744 { }

interface C10744
{
    int x();
    Baz10744 foo();
}

class D10744 : C10744
{
    int x() { return 0; }
    Qux10744 foo() { return null; }
}

interface Baz10744 { }
interface Qux10744 : Baz10744 { }

/*******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11034

class A11034(T)
{
    A11034!int view() { return null; }
}
class B11034(T) : A11034!int
{
override:
    C11034!int view() { return null; }
}
class C11034(T) : B11034!int {}

void test11034()
{
    auto b = new B11034!int;

    // Check that B!int.view() overrides A!int.view()
    auto tiobj = typeid(Object);
    assert(typeid(A11034!int).vtbl.length == tiobj.vtbl.length + 1);
    assert(typeid(B11034!int).vtbl.length == tiobj.vtbl.length + 1);
}

/*******************************************************/

void testTypeid()
{
    interface I
    {
    }

    interface J : I
    {
    }

    class C : J
    {
    }

    class D : C
    {
    }

    D d = new D();
    Object o = d;
    I i = d;

    assert(typeid(typeof(o)) is typeid(Object));
    assert(typeid(o) is typeid(D));
    assert(o.classinfo is typeid(D));
    assert(typeid(typeof(i)) is typeid(I));
    assert(typeid(i) !is typeid(J));
    assert(i.classinfo !is typeid(J));
}

/*******************************************************/

extern (C++)
{
    interface IA47
    {
        char a();
    }

    interface IB47
    {
        char b();
    }

    interface IC47 : IA47, IB47
    {
        char c();
    }

    interface ID47
    {
        char d();
    }

    interface IE47 : IC47, ID47
    {
        char e();
    }

    class Foo47 : IE47
    {
        int x = 9;
        char a() { printf("a.this = %p\n", this); return('a'); }
        char b() { printf("b.this = %p\n", this); return('b'); }
        char c() { printf("c.this = %p\n", this); return('c'); }
        char d() { printf("d.this = %p\n", this); return('d'); }
        char e() { printf("e.this = %p\n", this); return('e'); }
    }
}

void test15647()
{
    auto foo = new Foo47;
    printf("Foo: %p %c %c %c %c %c\n", foo, foo.a, foo.b, foo.c, foo.d, foo.e);
    IA47 a = foo;
    printf("A: %c\n", a.a);
    assert(a.a == 'a');
    IB47 b = foo;
    printf("B: %c\n", b.b);
    assert(b.b == 'b');
    IC47 c = foo;
    printf("C: %p %c %c %c\n", c, c.a, c.b, c.c);
    assert(c.a == 'a');
    assert(c.b == 'b');
    assert(c.c == 'c');
    ID47 d = foo;
    printf("D: %c\n", d.d);
    assert(d.d == 'd');
    IE47 e = foo;
    printf("E: %c %c %c %c %c\n", e.a, e.b, e.c, e.d, e.e);
    assert(e.a == 'a');
    assert(e.b == 'b');
    assert(e.c == 'c');
    assert(e.d == 'd');
    assert(e.e == 'e');

    b = e;
    printf("IB47: %c\n", b.b);
    assert(b.b == 'b');
}

/*******************************************************/

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
    test1747();
    test2553();
    test11034();
    testTypeid();
    test15647();

    printf("Success\n");
    return 0;
}
