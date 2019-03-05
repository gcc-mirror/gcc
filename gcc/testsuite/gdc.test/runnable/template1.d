// REQUIRED_ARGS:

module template1;

import core.stdc.stdio : printf;
import core.vararg;

/******************************************/

template TFoo1(T,U)
{
    int foo(T t, U u)
    {
        return 3;
    }
}


alias TFoo1!(int, char) Foo1;


void test1()
{
    int i = Foo1.foo(1, 2);
    assert(i == 3);
}

/******************************************/

template TFoo2(T,U)
{
    T x = 4;
    U y;
}


alias TFoo2!(int, char) Foo2;


void test2()
{
    assert(Foo2.x + Foo2.y == 0x103);
    Foo2.x = 3;
    Foo2.y = 7;
    assert(Foo2.x + Foo2.y == 10);
}

/******************************************/

template TFoo3(T,U)
{
    class Bar
    {
        T x = 4;
        U y;
    }
}


alias TFoo3!(int, char) Foo3;


void test3()
{
    Foo3.Bar b = new Foo3.Bar();

    assert(b.x == 4);
    assert(b.y == 0xFF);
}

/******************************************/

template TFoo4(T,U)
{
    T x;
    U y;
}

template TFoo4(T:T,U:T)
{
    T a;
    U b;
}

template TFoo4(T:uint, U:uint)
{
    T c;
    U d;
}

alias TFoo4!(int, char) Foo4x;

void test4()
{
    alias TFoo4!(int, char) Foo4;
    int* x = &Foo4.c;
    char* y = &Foo4.d;

    alias TFoo4!(uint, char**) Foo4_2;
    uint* x2 = &Foo4_2.x;
    char*** y2 = &Foo4_2.y;

    alias TFoo4!(int, int) Foo4_3;
    int* x3 = &Foo4_3.a;
    int* y3 = &Foo4_3.b;

    alias TFoo4!(uint, uint) Foo4_4;
    uint* x4 = &Foo4_4.c;
    uint* y4 = &Foo4_4.d;
}


/******************************************/

template TtoUx(T, U)
{
    T toUx(U[] s)
    {
        uint v = 0;

        if (v != cast(T)v)
            return 3;

        return cast(T)v;
    }

}

alias TtoUx!(ubyte, char).toUx toUbyte;
alias TtoUx!(ushort, char).toUx toUshort;

void test5()
{
}


/******************************************/

template TtoUx6(T, U)
{
    T toUx(U[] s)
    {
        uint v = 0;

        if (v != cast(T)v)
            return 3;

        return cast(T)v;
    }

}

alias TtoUx6!(ubyte, char) t6;

void test6()
{
}

/******************************************/

template A7(T) {
    T id(T t) {
        return t;
    }
}

alias A7!(int) a;

void test7()
{
    printf("%d\r\n", a.id(3));
    assert(a.id(3) == 3);
}

/******************************************/

template Swapper(T)
{
    void Swap(ref T a, ref T b)
    {
        T temp = a; a = b; b = temp;
    }
}

void test8()
{
    alias Swapper!(int) IntSwap;
    int a=1,b=2;
    IntSwap.Swap(a,b);
    printf("a=%d,b=%d\n",a,b); // prints 2,1
    assert(a == 2);
    assert(b == 1);
}


/******************************************/

template Foo9(T)
{
    class B
    {
        T data;
    }
}

void test9()
{
    (new Foo9!(int).B).data += 4;
}


/******************************************/

template A10(T) {
}

template B10(T) {
    alias A10!(int) a;
}

void test10()
{
    alias B10!(int) b;
}

/******************************************/

template A11(T) {
    T idf(T t) {
        return t;
    }
}

template B11(T) {
    private alias A11!(T) a;
    T same(T t) {
        return a.idf(t);
    }
}

void test11()
{
    alias B11!(int) b;
    //printf("%d\r\n", b.same(10));
    assert(b.same(10) == 10);
}


/******************************************/

template A12(T) {
    class B {
        invariant() {
            assert(1);
        }
    T ide(T t) {
        return t;
    }
    }
}

void test12()
{
    alias A12!(int) a;
    a.B b = new a.B();
    printf("%d\r\n", b.ide(10));
    assert(b.ide(10) == 10);
}


/******************************************/

template A13(T) {
    public interface I {
        public T i();
    }
}

class B13 : A13!(int).I {
    public int i() {
        return 42;
    }
}

void test13()
{
    B13 b = new B13();
    A13!(int).I i = b;
    assert(b.i() == 42);
    assert(i.i() == 42);
}


/******************************************/

class B14
{
}

template A14(T, U) {

    private U u;

    static this()
    {
        u = new U();
    }
}

alias A14!(int, B14) t14;

void test14()
{
}


/******************************************/

template A15(T) {
    public interface Init {
        public T init();
    }
}
template A15(T : int) {
    public class Init {
        public T init() {
            return 42;
        };
    }
}
template A15(T : float) {
    public class Init {
        public T init() {
            return 3.25;
        };
    }
}

template TB15(T, U) {
    private U initializer;
    private void setInitializer(U init) {
        initializer = init;
    }
    public class B {
        private T _value;
        public this() {
            this._value = initializer.init();
        }
        public T value() {
            return this._value;
        }
    }
}
template TB15(T) {
    private alias TB15!(T, A15!(T).Init) tb;
    private void setInitializer(A15!(T).Init init) {
        tb.setInitializer(init);
    }
    public class B : tb.B {
    }
}

void test15()
{
    alias TB15!(int, A15!(int).Init) tb;
    tb.setInitializer(new A15!(int).Init());
    tb.B b = new tb.B();
    int i;
    i = b.value();
    assert(i == 42);

    alias TB15!(float) tb2;
    tb2.setInitializer(new A15!(float).Init());
    tb2.B b2 = new tb2.B();
    assert(b2.value() == 3.25);
}

/******************************************/

template foo16(U : int, int T : 9+1)
{
    U x = T;
}

alias foo16!(int, 10) bar16;

void test16()
{
    int i;

    i = bar16.x;
    assert(i == 10);
    assert(foo16!(int, 10).x == 10);
}

/******************************************/

template VecTemplate(tfloat)
{
    struct Vector
    {
        tfloat d;
    }
}

void test17()
{
    with (VecTemplate!(int)) // crash DMD
    {
    }
}

/******************************************/

template Bomb (T)
{
   void foo (T *parm)
   {
   }
}

template Name (T)
{
   T y;

   void test ()
   {
      Bomb!(T).foo (&y);
   }
}

alias Name!(int) a18;
alias Name!(ubyte) b18;

void test18()
{
}

/******************************************/

template one20( T )
{
  alias T function () safeptr;
}

template one20( T1, T2 )
{
  alias int function(int) safeptr;
}

alias one20!( int ) A;
A.safeptr foo20;

alias one20!( int, int ) B;
B.safeptr bar20;


int func_bar(int i) { return 2; }

void test20()
{
    bar20 = &func_bar;
}

/******************************************/

class A21 { int x; }
class B21 : A21 { int y; }

void abc21(B21* b) { }

template TFoo21(T : A21, U : T*)
{
    void test()
    {
        assert(T.sizeof == B21.sizeof);
        U u;
        abc21(u);
    }
}

alias TFoo21!(B21, B21*) bar21;

void test21()
{
    bar21.test();
}

/******************************************/

template Bug22(T : Object) {
    int print() {
        printf("Bug22(T : Object).print()\r\n");
        return 1;
    }
}
template Bug22(T) {
    int print() {
        printf("Bug22(T).print()\r\n");
        return 2;
    }
}
template TTest22(T) {
    private alias Bug22!(T) bug;
    class Test {
        int test() {
            return bug.print();
        }
    }
}

void test22()
{
    alias TTest22!(int).Test Test1;
    alias TTest22!(Test1).Test Test2;
    alias TTest22!(Object).Test Test3;
    Test1 test1 = new Test1();
    Test2 test2 = new Test2();
    Test3 test3 = new Test3();
    int i;

    i = test1.test();
    assert(i == 2);
    i = test2.test();
    assert(i == 1);
    i = test3.test();
    assert(i == 1);
}


/******************************************/

template T23()
{
     struct Rank
     {
     }
}

template A23()
{
    struct Array
    {
        alias T23!().Rank Rank1;

        Rank1 data;
    }
}

alias A23!().Array Array_int23;

void test23()
{
}


/******************************************/

template TList24(T)
{
    class Node
    {
    }
    class List
    {
        Node m_first = null;
    }
}

void test24()
{
  alias TList24!(uint).List UIntList;
}


/******************************************/

template TList25(T)
{
    class Node
    {
        Node prev;
        Node next;
        T Value;
    }
    class List
    {
        Node m_first = null;
        Node m_last = null;
        void AddFront(T _Value)
        {
            Node cur = new Node;
            with (cur)
            {
                next = m_first;
                prev = null;
                Value = _Value;
                if (next !is null)
                    next.prev = cur;
            }
            m_first = null;
            if (m_last is null)
                m_last = cur;
        }
    }
}

void test25()
{
  alias TList25!(uint).List UIntList;
  alias TList25!(uint).Node UIntNode;
  UIntList list;
  UIntNode node;
  for (int i = 1; i <= 10; i++)
        {} //list.AddFront(i);
}


/******************************************/

template Foo26(T)
{
    void doIt() {
        printf("Foo26(T)\r\n");
    }
}

template Foo26(T : T[])
{
    private alias Foo26!(T) bug;
    void doIt() {
        printf("Foo26(T[])\r\n");
        bug.doIt();
    }
}

void test26()
{
    alias Foo26!(int[]) foo;
    foo.doIt();
}


/******************************************/

template Foo27(T)
{
    public const T[] empty = [];
}

void test27()
{
    alias Foo27!(int) bug;
}


/******************************************/

template A28(T) {
    public bool all(in T[] array, bool function (T) predicate) {
        for (int i = 0; i < array.length; i++) {
            if (!predicate(array[i])) {
                return false;
            }
        }
        return true;
    }
}

void test28()
{
    static bool isVowel(char c) {
        return (c == 'a') || (c == 'e') || (c == 'i') || (c == 'o') || (c == 'u');
    }

    alias A28!(char) arrays;
    assert(arrays.all("aeiouoeieuiei", &isVowel));
    assert(arrays.all("aeoiuaioeuioaeuiouoiaeu", &isVowel));
    assert(!arrays.all("aaeiouioeujiurioue", &isVowel));
    assert(!arrays.all("bjkqwkjbwqjbkwb", &isVowel));
    assert(arrays.all("", &isVowel));
    printf("A28(char).all tests passed!\r\n");
}


/******************************************/

public template TRange29(T) {
    debug private bool recursing = false;
    public class Range {
        private T _lower;
        private T _upper;
        public this(T lower, T upper) {
            this._lower = lower;
            this._upper = upper;
        }
        public T lower() {
            return this._lower;
        }
        public T upper() {
            return this._upper;
        }
        public bool contains(T item) {
            return (lower() <= item) && (item <= upper());
        }
        public bool intersects(Range other)
        in {
            assert(other !is null);
        } out (result) {
            debug {
                if (!recursing) {
                    recursing = true;
                    assert(result == other.intersects(this));
                } else {
                    recursing = false;
                }
            }
        } body {
            return contains(other.lower()) || contains(other.upper()) || other.includes(this);
        }
        public bool includes(Range other)
        in {
            assert(other !is null);
        } out (result) {
            assert(result == (contains(other.lower()) && contains(other.upper())));
        } body {
            return contains(other.lower()) && contains(other.upper());
        }
    }
}

void test29()
{
    alias TRange29!(int).Range Range;
    Range r1 = new Range(1, 10);
    Range r2 = new Range(5, 15);
    assert(r1.intersects(r2) == 1);
}


/******************************************/

template TCopy30(T)
{
    void copy(out T to, T from)
    {
        to = from;
    }
}

template TCopy30(T : string)
{
    void copy(out string to, in string from)
    {
        printf("Specialization\n");
        to = from;
    }
}

void test30()
{
    int i = 0;
    float f = 0;
    string s;

    alias TCopy30!(int) copyint;
    alias TCopy30!(string) copystr;

    copyint.copy(i, 3);
    printf("%d\n", i);
    assert(i == 3);

    copystr.copy(s, "Here it comes");
    printf("%.*s\n", s.length, s.ptr);
    assert(s == "Here it comes");
}

/******************************************/

import std.string;

template Foo31(alias X)
{
        alias X.toStringz y;
}

void test31()
{
    alias Foo31!(std.string) bar;
}


/******************************************/

shared int x32;

template Foo32(alias X)
{
    static shared int* p = &X;
}

alias Foo32!(x32) abc32;

void test32()
{
    alias Foo32!(x32) bar;

    *bar.p = 3;
    assert(x32 == 3);

    *abc32.p = 4;
    assert(x32 == 4);
}

/******************************************/

shared int x33;

template Foo33(alias X)
{
    static shared int* p = &X;
}

template Bar33(alias T)
{
    alias T!(x33) abc;
}

void test33()
{
    alias Bar33!(Foo33) bar;

    *bar.abc.p = 3;
    assert(x33 == 3);
}

/******************************************/

shared int x34;

template Foo34(alias X)
{
    static shared int* p = &X;
}

template Bar34(alias T)
{
    alias T.p q;
}

void test34()
{
    alias Foo34!(x34) foo;
    alias Bar34!(foo) bar;

    *bar.q = 3;
    assert(x34 == 3);
}

/******************************************/

class Foo35
{
    static int p;
}

template Bar35(alias T)
{
    alias T.p q;
}

void test35()
{
    alias Bar35!(Foo35) bar;

    bar.q = 3;
    assert(Foo35.p == 3);
}

/******************************************/

template Bar36(T)
{
    class Bar36
    {
        static T x;
    };
}

void test36()
{
    Bar36!(int).x = 3;
}

/******************************************/

class Bar37(T)
{
    static T x;
}


void test37()
{
    Bar37!(int).x = 3;
}

/******************************************/

class Bar38(T)
{
    static T x = 3;
}


void test38()
{
    int i = template1.Bar38!(int).x;
    assert(i == 3);

    int j = Bar38!(int).x;
    assert(j == 3);
}

/******************************************/

class Bar39(T)
{
    alias T x;
}


void test39()
{
    Bar39!(int).x y = 3;
    assert(y == 3);
}


/******************************************/

template Bar40(T)
{
    alias T Bar40;
}


void test40()
{
    Bar40!(int) y = 3;
    assert(y == 3);
}

/******************************************/

template Bar41(T)
{
    alias T Bar41;
}


void test41()
{
    template1.Bar41!(int) y = 3;
    assert(y == 3);

    assert(template1.Bar41!(int).sizeof == int.sizeof);
}

/******************************************/

template Bar42(T) { T t; }

typeof(Bar42!(int).t) bar42;

void test42()
{
    bar42 = 5;
}

/******************************************/

template factor43(int n : 1)
{
  enum { value = 1 }
}

template factor43(int n)
{
  enum { value = n*factor43!(n-1).value }
}

void test43()
{

  int i = factor43!(3).value;

  printf("%d\n",i);
  assert(i == 6);
}


/******************************************/

template factorial1(int n : 1)
{
    const int x = 1;
}

template factorial1(int n)
{
    const int x = n*.factorial1!(n-1).x;
}

template factorial2(int n : 1)
{
    const int factorial2 = 1;
}

template factorial2(int n)
{
    const int factorial2 = n*.factorial2!(n-1);
}

template factorial3(int n : 1)
{
    enum { x = 1 }
}

template factorial3(int n)
{
    enum { x = n*.factorial3!(n-1).x }
}

template factorial4(int n : 1)
{
    enum { factorial4 = 1 }
}

template factorial4(int n)
{
    enum { factorial4 = n*.factorial4!(n-1) }
}

void test44()
{

  int i = factorial1!(4).x;
  printf("%d\n",i);
  assert(i == 24);

  i = factorial2!(4);
  printf("%d\n",i);
  assert(i == 24);

  i = factorial3!(4).x;
  printf("%d\n",i);
  assert(i == 24);

  i = factorial4!(4);
  printf("%d\n",i);
  assert(i == 24);
}


/******************************************/

template factor45(int n)
{
    int value()
    {
        if (n==0 || n==1)
            return 1;
        return n * factor45!(n-1).value();
    }
}

template factor45(int n : 0)
{
    int value()
    {
        return 1;
    }
}

template factor45(int n : 1)
{
    int value()
    {
        return 1;
    }
}

void test45()
{
    int i;

    i = factor45!(4).value();
    printf( "%d\n", i);
    assert(i == 24);
}


/******************************************/

template sqrt46(int n, int lo, int hi : lo)
{
    enum { result = lo }
}

void test46()
{
    int i;

    i = sqrt46!(1, 24, 24).result;
    printf("i = %d\n", i);
    assert(i == 24);
}

/******************************************/

template sqrt47(int n, int lo, int hi)
{
    enum { mid = (lo + hi + 1) / 2 }

    enum { result = (n < mid * mid) ? sqrt47!(n, lo, mid - 1).result
                                    : sqrt47!(n, mid, hi).result }
}

template sqrt47(int n, int lo, int hi : lo)
{
    enum { result = lo }
}

template sqrt47(int n)
{
    enum { sqrt47 = .sqrt47!(n, 1, n).result }
}

void test47()
{
    int i;

    i = sqrt47!(24);
    printf("i = %d\n", i);
}

/******************************************/

class Foo48 (T)
{
    alias T Type;

    class Inner (U)
    {
        alias U Type;
    };
};

struct Bar48 (alias TT)
{
    alias TT!(int).Type A;
    alias TT!(int).Inner!(A).Type B;
};

void test48()
{
    Bar48!(Foo48).A x;
    Bar48!(Foo48).B y;

    int *p;

    p = &x;
    p = &y;
}


/******************************************/

struct Foo49(T)
{
    static Foo49 bar(T c1)
    {
        Foo49 rtn; // Error here
        return rtn;
    }
}

void test49()
{
    alias Foo49!(double) vector;

    vector.bar(1);
}

/******************************************/

struct Foo50(T)
{
  T x = 0;

  static Foo50 bar(T c1)
  {
    .Foo50!(typeof(c1)) rtn;
    rtn.x = c1;
    return rtn;
  }

  static .Foo50!(T) barx(T c1)
  {
    Foo50 rtn;
    rtn.x = c1;
    return rtn;
  }
}

void test50()
{
  alias Foo50!(double) vector;

  vector xAxis = vector.bar(1);
}

/******************************************/

struct Foo51(T)
{
  T x = 0;
  .Foo51!(long)* p;

  static Foo51 bar(T c1)
  {
    .Foo51!(typeof(c1)) rtn;
    rtn.x = c1;
    return rtn;
  }

  static .Foo51!(T) barx(T c1)
  {
    Foo51 rtn;
    .Foo51!(int)* c;
    rtn.x = c1;
    return rtn;
  }
}

void test51()
{
  alias Foo51!(double) vector;

  vector xAxis = vector.bar(1);
}


/******************************************/

interface Interface(T)
{
    void foo52();
}

void bar52(Interface!(Object) i)
{
    i.foo52();
}

class Abstract(T) : Interface!(T)
{
    abstract void foo52();
}

class Concrete(T) : Abstract!(T)
{
    override void foo52() { printf("Concrete.foo52(this = %p)\n", this); }
}

class Sub(T) : Concrete!(T)
{
}

void test52()
{
    Sub!(Object) s = new Sub!(Object)();
    s.foo52();
    bar52(s);
}


/******************************************/

class Foo53
{
    template tmethod (T)
    {
        public static void tmethod (T param)
        {
            printf("param = %d\n", param);
            assert(param == 42);
        }
    }
}


void test53()
{
    Foo53 foo = new Foo53;

    Foo53.tmethod!(int)(42);
}


/******************************************/

class Foo54
{
   template func(W) {
     static void foo(W w) { printf("W_I %d\n", w); assert(w == 3); }
     static int xx;
   }
}

void test54() {

   Foo54 c = new Foo54();
   c.func!(int).foo(3);
   c.func!(int).xx = 4;

}

/******************************************/

template T55(S)
{
    struct Foo55
    {
        static Foo55 test(Foo55 f)
        {
            Foo55 a = f;
            return f;
        }
    }
}

alias T55!(char).Foo55 Foo55;
alias T55!(char).Foo55 Bar55;


void test55()
{
    Bar55 a;
    Foo55 b;
    b.test(a);
    Bar55.test(a);
}

/******************************************/

template CT56(T)
{
  class C
  {
    const char[][1] arrArr=["foo" ];
  }
}

void test56()
{
  alias CT56!(int) Ct;
  Ct.C c= new Ct.C();
  printf("%.*s\n", c.arrArr[0].length, c.arrArr[0].ptr);
  assert(c.arrArr[0] == "foo");
}


/******************************************/

template foo57(T : int = int)
{
    T x = 3;
}

void test57()
{
    printf("%d\n", foo57!().x);
    assert(foo57!().x == 3);
}

/******************************************/

template Foo58(T, U = T)
{
    U x = 3;
}

void test58()
{
    alias Foo58!(int) f;
    assert(f.x == 3);
    assert(f.x.sizeof == 4);
}

/******************************************/

template Foo59(T, U = T*)
{
    shared T x = 3;
    shared U px = &x;
}

void test59()
{
    alias Foo59!(uint) f;
    assert(f.x == 3);
    assert(f.x.sizeof == 4);
    assert(*f.px == 3);

    alias Foo59!(long) g;
    assert(g.x == 3);
    assert(g.x.sizeof == 8);
    assert(*g.px == 3);
}

/******************************************/

class A60
{}

template B60(T, U = short)
{
        struct Thing
        {
                T       t;
                U       u;
        };
}

template C60(T, U = A60)
{
        class C60
                : U
        {}

        class C2
        {};
}

void test60()
{
        B60!(int, long).Thing   thing1;
        B60!(int).Thing         thing2;

        printf("thing1.sizeof: %u\n", thing1.sizeof);
        printf("thing2.sizeof: %u\n", thing2.sizeof);

        assert(thing1.sizeof == long.alignof + long.sizeof);
        assert(thing2.sizeof == 8);

        C60!(int /*,A60*/ )     container1;

        printf("container1.sizeof: %u\n", container1.sizeof);
        assert(container1.sizeof == (void*).sizeof);
}

/******************************************/

struct Foo61
{
    int a;

    template Bar(T)
    {
        T abc() { return a; }
    }

    int def() { return 4; }
}

void test61()
{
    Foo61 *f = new Foo61();
    int i;

    f.a = 3;
    i = f.def();
    assert(i == 4);
    i = f.Bar!(int).abc();
    assert(i == 3);

    Foo61 g;
    g.a = 3;
    i = g.def();
    assert(i == 4);
    i = g.Bar!(int).abc();
    assert(i == 3);
}

/******************************************/

class Foo62(T)
{
    template Bar(T)
    {
        int func() { return 3; }
    }
}

void test62()
{
    Foo62!(int) x = new Foo62!(int);

    assert(x.Bar!(int).func() == 3);
}

/******************************************/

class Foo63(T)
{
    template Bar(T)
    {
        int func() { this.def(); return 3; }
        int func2() { return 4; }
    }

    void def()
    {
        assert(Bar!(T).func2() == 4);
    }
}

void test63()
{
    Foo63!(int) x = new Foo63!(int);

    assert(x.Bar!(int).func() == 3);
    x.def();
}

/******************************************/

struct XVector(qfloat)
{
    qfloat x;qfloat y;qfloat z;

    static int opCall (qfloat x, qfloat y, qfloat z) { return 8; }
}

void test64()
{
    int i;
    i = XVector!(int)(1,2,3);
    assert(i == 8);
    i = XVector!(real).opCall(1,2,3);
    assert(i == 8);
}

/******************************************/
// http://www.digitalmars.com/d/archives/28052.html

alias int value_type;

struct Foo65
{
    uint length() { return 47; }

    size_t test()
    {
        value_type[] e = new value_type[length];
        return e.length;
    }
}

void test65()
{
    Foo65 f;

    assert(f.test() == 47);
}

/******************************************/

class Thing66
{
        template print(T2)
        {
                void print(T2 t)
                {
                    printf("t = %d\n", t);
                    assert(t == 10);
                }
        }
}


void test66()
{
        Thing66 thing = new Thing66;

        thing.print!(int)(10);
}

/******************************************/

template Foo67(alias T)
{
    void Foo67()
    {
        printf("T = '%.*s'\n", T.length, T.ptr);
        assert(T == "hello");
    }
}

void test67()
{
    static string x = "hello";

    Foo67!(x)();
}


/******************************************/

template T68(int a) {
    int vec[a];
}

void test68()
{
        int i;

        i = T68!(4>1?4:1).vec[0];
        assert(i == 0);
        i = T68!(4==1?1:(1==1?4:(4>1?1:4))).vec[0];
        assert(i == 0);
}

/******************************************/

size_t printx(string s)
{
    printf("s = '%.*s'\n", s.length, s.ptr);
    return s.length;
}

size_t printx(int i)
{
    printf("i = %d\n", i);
    return 28;
}

template Foo69(alias T)
{
 size_t Foo69()
 {
  return printx(T);
 }
}

void test69()
{
 static string x = "hello";
 static string z = "abc";
 static int y=100;
 size_t i;

 alias Foo69!(x) foox;
 alias Foo69!(y) fooy;

    i = Foo69!(x)();
    assert(i == 5);
    i = Foo69!(y)();
    assert(i == 28);
    i = Foo69!(z)();
    assert(i == 3);
    i = foox();
    assert(i == 5);
    i = fooy();
    assert(i == 28);
}

/******************************************/

template temptt70(alias func)
{
   void temp()
   {
        func();
   }
}

int x70;

void myfunc70()
{
    printf("myfunc70()\n");
    x70 = 6;
}

alias temptt70!(myfunc70).temp foo70;

void test70()
{
   foo70();
   assert(x70 == 6);
}

/******************************************/

struct A71(T)
{
    alias .A71!(T) AT;
    int x;
}

alias A71!(int) Aint71;

void test71()
{
    Aint71.AT a;
    a.x = 3;
}

/******************************************/

template foo72(T)
{
    char[] foo72(T d)
    {
        uint sz = typeof(d[0]).sizeof * 2;
        return null;
    }
}

void test72()
{
    static ulong[5] a = [0,1,2,3,4];
    static uint[5] b = [0,1,2,3,4];
    char[] r;
    r = foo72!(ulong[5])(a); printf("%.*s\n", r.length, r.ptr);
    r = foo72!(uint[5])(b);  printf("%.*s\n", r.length, r.ptr);
}


/******************************************/

alias int Int73;
class Test73(T = Int73);
alias Test73!() Foo73;

void test73()
{
}

/******************************************/

class A74
{
    alias A74 atype;
    int x;
}


class B74(R, int V = R.sizeof)
{
    int v = V;
}

void test74()
{
    B74!(A74,3) b = new B74!(A74,3)();
    assert(b.v == 3);

    B74!(A74) c = new B74!(A74)();
    assert(c.v == A74.sizeof);
}


/******************************************/

interface NotionalRange75(V)
{
}

class MatchedNotionalRange75(R)
    : NotionalRange75!(R.value_type)
{
}

class Range75
{
    alias   int   value_type;
}

class List75
{

    MatchedNotionalRange75!(Range75) x;
}

void test75()
{
}


/******************************************/

interface Indian(T)
{
}

interface Iterable(T)
{
  Indian!(T) foo();
}

class Lope(T) : Iterable!(T)
{
  Indian!(T) foo()
  {
        return new Corn!(T);
  }
}

class Corn(T) : Indian!(T)
{
}

void test76()
{
  Lope!(int) x = new Lope!(int);
}


/******************************************/

class RawFile
{
}

class Stream : RawFile
{
        template readLineT(T) { bool readLineT()
        {
                if (super)
                        return false;
                return true;
        }}

        bool readLine()
        {
                return readLineT!(int)();
        }
}

void test77()
{
}


/******************************************/

class Four(U, V, X, Y)
{
  U i;  V j;  X k;  Y l;
}

template WhatFour(U,V,X,Y)
{
    int func(Four!(U,V,X,Y) four)
    {
        printf("general template\n");
        return 1;
    }
}

template WhatFour(U:int,V,X,Y)
{
    int func(Four!(int,V,X,Y) four)
    {
        printf("specialization:: first int\n");
        return 2;
    }
}

template WhatFour(U,V:U,X,Y:X)
{
    int func(Four!(U,U,X,X) four)
    {
        printf("specialization:: first two equal, second two equal\n");
        return 3;
    }
}

alias WhatFour!(int,float,char,bool).func whatfour;
alias WhatFour!(float,float,char,bool).func whatfour;
alias WhatFour!(float,float,char,char).func whatfour;
alias WhatFour!(int,int,float,char).func whatfour;  // ambiguous match

void test78()
{ int j;

  Four!(int,float,char,bool) f;
  Four!(float,float,char,bool) g;
  Four!(float,float,char,char) h;
  Four!(int,int,float,char) i;

  j = whatfour(f);
  assert(j == 2);
  j = whatfour(g);
  assert(j == 1);
  j = whatfour(h);
  assert(j == 3);
  j = whatfour(i);
  assert(j == 2);

  /*
  will print:
specialization:: first int
general template
specialization:: first two equal, second two equal
specialization:: first int
  */
}


/******************************************/
// http://www.digitalmars.com/pnews/read.php?server=news.digitalmars.com&group=digitalmars.D.bugs&artnum=2117

class Conversion(T,U){
        alias char Small;
        class Big{
                char[2] dummy;
        }
        static Small Test(U u);
        static Big Test(...);
        static T MakeT();
        enum {
                exists = (Test(MakeT())).sizeof == (Small).sizeof
        }
}

void variadicDummy(...){
}

void test79()
{
        variadicDummy(Conversion!(double,int).exists);
}

/******************************************/

class A80(T)
{
   T s;

   int foo(int delegate (T) d) { return 3 + d(s); }

   int bar()
   {
     return foo(delegate int (T t) { return 6 + t.x; });
   }
}

class B80: A80!(B80)
{
    int x = 20;
}

class C80: A80!(C80)
{
    int y = 3;
    int x = 10;
}

void test80()
{
    B80 b = new B80();
    C80 c = new C80();

    b.s = b;
    c.s = c;

    assert(b.bar() == 9+20);
    assert(c.bar() == 9+10);
}

/******************************************/

struct T81(FOO)
{
        S81 s;
}

struct S81
{
        T81!(int)* pt;
}

void test81()
{
}

/******************************************/

T foo82(T : const(U)*, U=char)(T t)
{
    return null;
}

void test82()
{   int i;
    const int ci;

    //writeln(typeid(typeof(foo82(&ci))));
    //writeln(typeid(typeof(foo82(&i))));
    assert(typeof(foo82(&ci)).stringof == "const(int)*");
    assert(typeof(foo82(&i)).stringof == "int*");
}

/******************************************/

struct A83
{
    void foo(int) {}
    void bar(T)(T) {}
}

void test83()
{
    A83 a;
    a.foo = 5;
    a.bar = 6;
}

/******************************************/

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
//    test19();
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
    test42();
    test43();
    test44();
    test45();
    test46();
    test47();
    test48();
    test49();
    test50();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test58();
    test59();
    test60();
    test61();
    test62();
    test63();
    test64();
    test65();
    test66();
    test67();
    test68();
    test69();
    test70();
    test71();
    test72();
    test73();
    test74();
    test75();
    test76();
    test77();
    test78();
    test79();
    test80();
    test81();
    test82();
    test83();

    printf("Success\n");
    return 0;
}
