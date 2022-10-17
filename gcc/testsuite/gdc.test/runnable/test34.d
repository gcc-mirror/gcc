module test34;

import core.exception;
import core.stdc.stdio;
import core.vararg;

/************************************************/

class Foo {}
class Bar {}

void test1()
{
    TypeInfo ti_foo = typeid(Foo);
    TypeInfo ti_bar = typeid(Bar);

    auto hfoo = ti_foo.toHash();
    auto hbar = ti_bar.toHash();
    assert(hfoo != hbar);

    auto e = (ti_foo == ti_bar);
    assert(!e);

    auto c = (ti_foo.opCmp(ti_bar) == 0);
    assert(!c);
}


/************************************************/

void test2()
{
  assert( [2,3]!=[2,4] );
  assert( [3,2]!=[4,2] );
  assert( !([2,3]==[2,4]) );
  assert( ([2,3]==[2,3]) );
}

/************************************************/

struct Struct
{
    int langID;
    long _force_nrvo;
}

Struct[1] table;

Struct getfirst()
{
    foreach(v; table) {
        assert(v.langID == 1);
        return v;
    }
    assert(0);
}

Struct getsecond()
{
    foreach(ref v; table) {
        assert(v.langID == 1);
        return v;
    }
    assert(0);
}

void test3()
{
    table[0].langID = 1;

    auto v = getfirst();
    assert(v.langID == 1);

    v = getsecond();
    assert(v.langID == 1);
}

/************************************************/

class ExOuter
{
    class ExInner
    {
        this()
        {
            typeof(this.outer) X;
            static assert(is(typeof(X) == ExOuter));
        }
    }
}

void test4()
{
}

/************************************************/

int status5;

struct MyStruct5
{
}

void rec5(int i, MyStruct5 s)
{
    if( i > 0 )
    {
        status5++;
        rec5(i-1, s);
    }
}

void test5()
{
    assert(status5==0);
    MyStruct5 st;
    rec5(1030, st);
    assert(status5==1030);
}

/************************************************/

class C6
{
    const int a;

    this()
    {
        a = 3;
    }

    this(int x)
    {
        this();
    }
}

void test6()
{
}

/************************************************/

template parseUinteger(string s)
{
    static if (s.length == 0)
    {   const char[] value = "";
        const char[] rest = "";
    }
    else static if (s[0] >= '0' && s[0] <= '9')
    {   const char[] value = s[0] ~ parseUinteger!(s[1..$]).value;
        const char[] rest = parseUinteger!(s[1..$]).rest;
    }
    else
    {   const char[] value = "";
        const char[] rest = s;
    }
}

template parseInteger(string s)
{
    static if (s.length == 0)
    {   const char[] value = "";
        const char[] rest = "";
    }
    else static if (s[0] >= '0' && s[0] <= '9')
    {   const char[] value = s[0] ~ parseUinteger!(s[1..$]).value;
        const char[] rest = parseUinteger!(s[1..$]).rest;
    }
    else static if (s.length >= 2 &&
                s[0] == '-' && s[1] >= '0' && s[1] <= '9')
    {   const char[] value = s[0..2] ~ parseUinteger!(s[2..$]).value;
        const char[] rest = parseUinteger!(s[2..$]).rest;
    }
    else
    {   const char[] value = "";
        const char[] rest = s;
    }
}

void test7()
{
    assert(parseUinteger!("1234abc").value == "1234");
    assert(parseUinteger!("1234abc").rest == "abc");
    assert(parseInteger!("-1234abc").value == "-1234");
    assert(parseInteger!("-1234abc").rest == "abc");
}

/************************************************/

struct Foo8 { }

enum Enum { RED }

//typedef int myint;

alias int myalias;

void test8()
{
/+
    assert((1+2).stringof == "1 + 2");
    assert(Foo8.stringof == "Foo8");
    assert(test.Foo8.stringof == "test.Foo8");
    assert(int.stringof == "int");
    assert((int*[5][]).stringof == "int*[5][]");
    assert(Enum.RED.stringof == "Enum.RED");
    assert(test.myint.stringof == "test.myint");
    assert(myalias.stringof == "myalias");
    assert((5).stringof == "5");
    assert(typeof(5).stringof == "typeof(5)");
+/
}

/************************************************/

/+
class Base9 {
    public void fnc(){
    }
}

class Foo9 : Base9 {
    alias Base9.fnc fnc;
    public void fnc(){
    }
    static this(){
        alias void function() T;
        T ptr = & fnc;
    }
}
+/

void test9()
{
}

/************************************************/

bool isalnum(dchar c) { return c>='0' && c >= '9'; }

char[] toHtmlFilename(char[] fname)
{
    foreach (ref c; fname)
    {
        if (!isalnum(c) && c != '.' && c != '-')
            c = '_';
    }
    return fname;
}

void test10()
{
}

/************************************************/

class A34 { }
class B34 : A34 { }

void test11()
{
  A34 test=new B34;
  assert(test.toString == "test34.B34");
  A34 test_2=cast(A34)(new B34);
  assert(test_2.toString == "test34.B34");
}

/************************************************/

template Foo12(T: T[U], U)
{
    alias int Foo12;
}

void test12()
{
    Foo12!(int[long]) x;
    assert(is(typeof(x) == int));
}

/************************************************/

class C13
{
    int a = 4;
    this()
    {
        printf("C13.this()\n");
        assert(a == 4);
        a = 5;
    }
}

void test13()
{
    C13 c = cast(C13)Object.factory("test34.C13");
    assert(c.a == 5);
    Object o = Object.factory("test35.C13");
    assert(o is null);
}

/************************************************/

class Base15 {
        int func(int a) { return 1; }
}


class Foo15 : Base15 {
        alias Base15.func func;
}


class Bar15 : Foo15 {
        alias Foo15.func func;
        int func(string a) { return 2; }
}

void test15()
{
    Bar15 b = new Bar15();
    assert(b.func("hello") == 2);
    assert(b.func(5) == 1);
}

/************************************************/

struct Basic16(T, U) {}

struct Iterator16(T : Basic16!(T, U), U)
{
    static void Foo()
    {
        assert(is(T == int));
        assert(is(U == float));
    }
}

void test16()
{
    Iterator16!(Basic16!(int, float)).Foo();
}

/************************************************/

struct S17(T)
{
    struct iterator {}
}

int insert17(T) (S17!(T) lst, S17!(T).iterator i)
{
    return 3;
}

void test17()
{
    S17!(int) a;
    S17!(int).iterator i;
    auto x = insert17(a, i);
    assert(x == 3);
}

/************************************************/

void test18()
{
    real t = 0.;
    for(int i=0; i<10; i++)
    {
        t += 1.;
        real r =  (2*t);
        printf("%Lg  %Lg  %Lg\n", t, r, 2*t);
        assert(2*t == (i+1)*2);
    }
}

/************************************************/

void test19()
{
    char c = '3';
    void[] ca = cast(void[])[c];
    char[] x = cast(char[])ca;
    assert(x[0] == '3');
}

/************************************************/

enum type20
{
    a,
    b,
}

class myclass20
{
    template XX(uint a, uint c)
    {
        static uint XX(){ return (a*256+c);}
    }
    void testcase()
    {
        switch (cast(uint)type20.a)
        {
            case XX!(cast(uint)type20.a,cast(uint)type20.b)():
                break;
            default: assert(0);
        }
    }
}

void test20()
{
}

/************************************************/

struct S21
{
    alias int Foo;
    int x;
}

void test21()
{
    S21 s;
    typeof(s).Foo j;
    assert(is(typeof(j) == int));
}

/************************************************/

void test22()
{
    auto i = 3, j = 4;
    assert(is(typeof(i) == int));
    assert(is(typeof(j) == int));
}

/************************************************/

static m23 = 5, n23 = 6;

void test23()
{
    auto i = 3, j = 4;
    assert(is(typeof(i) == int));
    assert(is(typeof(j) == int));
    assert(is(typeof(m23) == int));
    assert(is(typeof(n23) == int));
}

/************************************************/

const int a24 = 0;
const int foo24 = 4;
const int[1] bar24 = [foo24 * 2];
const int zap24 = (1 << bar24[a24]);

void test24()
{
    assert(zap24 == 256);
}

/************************************************/

struct List25(T) {  }
struct CircularQueue25(T) {  }

void front25(T)(ref List25!(T) list) {  }
void front25(T)(ref CircularQueue25!(T) queue) {  }

void test25()
{
    List25!(int) x;
    front25(x);
}

/************************************************/

struct Foo26
{
    const string x;
}

static Foo26 foo26 = {"something"};

void test26()
{
    assert(foo26.x == "something");
}

/************************************************/

template Mang(alias F)
{
    class G { }
    alias void function (G ) H;
    const string mangledname = H.mangleof;
}

template moo(alias A)
{
    const string a = Mang!(A).mangledname;
    static assert(Mang!(A).mangledname == a); // FAILS !!!
}

void test27()
{
    int q;
    string b = moo!(q).a;
}

/************************************************/

struct Color
{
    static void fromRgb(uint rgb)
    {
    }

    static void fromRgb(ubyte alpha, uint rgb)
    {
    }
}

void test28()
{
    Color.fromRgb(0);
    Color.fromRgb(cast(uint)0);
}

/************************************************/

void test29()
{
  const char[] t="abcd";
  const ubyte[] t2=cast(ubyte[])t;
  const char[] t3=['a','b','c','d'];
  const ubyte[] t4=cast(ubyte[])t3;
  assert(t4[1] == 'b');
}

/************************************************/

void test30()
{
  const char[] test = "" ~ 'a' ~ 'b' ~ 'c';
  char[] test2 = (cast(char[])null)~'a'~'b'~'c';
  const char[] test3 = (cast(char[])null)~'a'~'b'~'c';
  char[] test4 = (cast(char[])[])~'a'~'b'~'c';
  const char[] test5 = (cast(char[])[])~'a'~'b'~'c';
  const char[] test6 = null;
  const char[] test7 = test6~'a'~'b'~'c';
}

/************************************************/

class C31
{
    synchronized invariant() { int x; }
}

void test31()
{
}

/************************************************/

ulong foo32()
{
        return cast(ulong) (cast(ulong) 1176576512 + cast(float) -2);
}

void test32()
{
        assert(foo32()==1176576510);
}

/************************************************/

class RangeCoder
{
    uint[258] cumCount; // 256 + end + total
    uint lower;
    uint upper;
    ulong range;

    this() {
        for (int i=0; i<cumCount.length; i++)
            cumCount[i] = i;
        lower = 0;
        upper = 0xffffffff;
        range = 0x100000000;
    }

    void encode(uint symbol) {
        uint total = cumCount[$ - 1];
        // "Error: Access Violation" in following line
        upper = lower + cast(uint)((cumCount[symbol+1] * range) / total) - 1;
        lower = lower + cast(uint)((cumCount[symbol]   * range) / total);
    }
}

void test33()
{
    RangeCoder rc = new RangeCoder();
    rc.encode(77);
}

/************************************************/

struct Vector34
{
    float x, y, z;

    public static Vector34 opCall(float x = 0, float y = 0, float z = 0)
    {
        Vector34 v;

        v.x = x;
        v.y = y;
        v.z = z;

        return v;
    }

    public string toString()
    {
        return formatImpl("<%f, %f, %f>", x, y, z);
    }

    private static string formatImpl(string fmt, ...)
    {
        string ret = "<";
        bool comma;
        foreach (arg; _arguments)
        {
            assert(arg is typeid(float));
            if (comma)
                ret ~= ", ";
            auto f = va_arg!float(_argptr);
            if (f == 1)
                ret ~= "1.000000";
            else if (f == 0)
                ret ~= "0.000000";
            else
                assert(0);
            comma = true;
        }
        ret ~= ">";
        return ret;
    }
}

string format34(string fmt, ...)
{
    assert(_arguments[0] is typeid(Vector34));
    auto arg = va_arg!Vector34(_argptr);
    return arg.toString();
}

class Foo34
{
    private Vector34 v;

    public this()
    {
        v = Vector34(1, 0, 0);
    }

    public void foo()
    {
        bar();
    }

    private void bar()
    {
        auto s = foobar();
        assert(format34("%s", s) == "<1.000000, 0.000000, 0.000000>");
    }

    public Vector34 foobar()
    {
        return v;
    }
}

void test34()
{
    Foo34 f = new Foo34();
    f.foo();
}


/************************************************/

void foo35()
{
    uint a;
    uint b;
    uint c;
    extern (Windows) int function(int i, int j, int k) xxx;

    a = 1;
    b = 2;
    c = 3;

    xxx = cast(typeof(xxx))(a + b);
    throw new Exception("xxx");
    xxx( 4, 5, 6 );
}

void test35()
{
}

/************************************************/

void test36()
{
    int* p = void, c = void;
}

/************************************************/

void test37()
{
    synchronized
    {
        synchronized
        {
            printf("Hello world!\n");
        }
    }
}

/************************************************/

struct Rect {
    int left, top, right, bottom;
}

void test38()
{
    print38(sizeTest(false));
    print38(sizeTest(true));
    print38(defaultRect);
}

static Rect sizeTest(bool empty) {
    if (empty) {
        Rect result;
        return result;
        //return Rect.init;
    } else {
        return defaultRect;
        /+Rect result = defaultRect;
        return result;+/
    }
}

void print38(Rect r) {
    printf("(%d, %d)-(%d, %d)\n", r.left, r.top, r.right, r.bottom);
    assert(r.left == 0);
    assert(r.right == 0);
    assert(r.top == 0);
    assert(r.bottom == 0);
}

Rect defaultRect() {
    return Rect.init;
}

/************************************************/

void varargs39(...)
{
    if (_arguments[0] is typeid(double[]))
    {
        auto arg = va_arg!(double[])(_argptr);
        assert(arg.length == 1 && arg[0] == 1 || arg[0] == 2);
    }
    else if (_arguments[0] is typeid(double[][]))
    {
        auto arg = va_arg!(double[][])(_argptr);
        assert(arg == [[1],[2]]);
    }
    else if (_arguments[0] is typeid(double[1][]))
    {
        auto arg = va_arg!(double[1][])(_argptr);
        assert(arg == [[1], [2]]);
    }
    else
        assert(0);
}

void test39()
{
   double[][] foo = [[1.0],[2.0]];

   varargs39(foo[0]); // --> [1] , ok
   varargs39(foo[1]); // --> [2] , ok
   varargs39(foo);    // --> [[1],4.63919e-306]  ack!

   double[1][2] bar;
   bar[0][0] = 1.0;
   bar[1][0] = 2.0;

   varargs39(bar);    // Error: Access violation
}

/************************************************/

void varargs40(...)
{
    if (_arguments[0] is typeid(int[char]))
    {
        auto x = va_arg!(int[char])(_argptr);
        assert(x == ['b':123]);
    }
    else if (_arguments[0] is typeid(int))
    {
        auto x = va_arg!int(_argptr);
        assert(x == 123);
    }
    else
        assert(0);
}

void test40()
{
    int[char] x;
    x['b'] = 123;
    varargs40(x);
    varargs40(x['b']);
}

/************************************************/

void test41()
{
}

/************************************************/

enum Enum42 {
        A = 1
}

void test42() {
        Enum42[] enums = new Enum42[1];
        assert(enums[0] == Enum42.A);
}


/************************************************/

struct A43 {}

struct B43(L) {
  A43 l;
}

void test43()
{
  A43 a;
  auto b = B43!(A43)(a);
}

/************************************************/

void test44()
{
    int[ const char[] ] a = ["abc":3, "def":4];
}

/************************************************/

void varargs45(...)
{
    if (_arguments[0] is typeid(const(char[3])[]))
    {
        auto a = va_arg!(const(char[3])[])(_argptr);
        assert(a == ["abc", "def"]);
    }
    else if (_arguments[0] is typeid(const(char)[][]))
    {
        auto b = va_arg!(const(char)[][])(_argptr);
        assert(b == ["abc", "def"]);
    }
    else
        assert(0);
}

void test45()
{
    const(char)[3][] a = ["abc", "def"];
    varargs45(a);
    const(char)[][2] b = ["abc", "def"];
    varargs45(b);
}

/************************************************/

struct bignum
{
    bool smaller()
    {
        if (true) return false;
        else      return false;
        assert(0);
    }

    void equal()
    {
        if (!smaller)
            return;
    }
}

void test46()
{
}

/************************************************/

static size_t myfind(string haystack, char needle) {
  foreach (i, c ; haystack) {
    if (c == needle) return i;
  }
  return size_t.max;
}

static size_t skip_digits(string s) {
  foreach (i, c ; s) {
    if (c < '0' || c > '9') return i;
  }
  return s.length;
}

static uint matoi(string s) {
  uint result = 0;
  foreach (c ; s) {
    if (c < '0' || c > '9') break;
    result = result * 10 + (c - '0');
  }
  return result;
}

enum { leading, skip, width, modifier, format, fmt_length, extra };

static string GetFormat(string s) {
  uint pos = 0;
  string result;
  // find the percent sign
  while (pos < s.length && s[pos] != '%') {
    ++pos;
  }
  const leading_chars = pos;
  result ~= cast(char) pos;
  if (pos < s.length) ++pos; // go right after the '%'
  // skip?
  if (pos < s.length && s[pos] == '*') {
    result ~= 1;
    ++pos;
  } else {
    result ~= 0;
  }
  // width?
  result ~= cast(char) matoi(s);
  pos += skip_digits(s[pos .. $]);
  // modifier?
  if (pos < s.length && myfind("hjlLqtz", s[pos]) != size_t.max) {
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@
    static if (true) {
      result ~= s[pos++];
    } else {
      result ~= s[pos];
      ++pos;
    }
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@
  } else {
    result ~= '\0';
  }
  return result;
}

void test47()
{
  static string test = GetFormat(" %*Lf");
  assert(test[modifier] == 'L', "`" ~ test[modifier] ~ "'");
}

/************************************************/

class B48() {}
class C48   {}

int foo48()(B48!()) { return 1; }
int foo48()(C48 c) { return 2; }

void test48()
{
    auto i = foo48(new B48!());
    assert(i == 1);

    i = foo48(new C48);
    assert(i == 2);
}

/************************************************/

void test49()
{
    struct A { int v; }

    A a = A(10);

  version (all)
  {
    if (a == a.init) { assert(0); }
    else { assert(a.v == 10); }
  }
  else
  {
    if (a == a.init) { assert(a.v == 10); }
    else { assert(0); }
  }

    a.v = 100;
    if (a == a.init) { assert(0); }
    else { assert(a.v == 100); }

    a = A(1000);
    if (a == a.init) { assert(0); }
    else { assert(a.v == 1000); }

  version (all)
    assert(a.init.v == 0);
  else
    assert(a.init.v == 10);
}

/************************************************/

struct S51
{
    int i = 3;
    void div() { assert(i == 3); }
}

void test51()
{
    S51().div();
}

/************************************************/

void test52()
{
    struct Foo {
        alias int Y;
    }
    with (Foo) {
         Y y;
    }
}

/************************************************/

struct TestStruct
{
    int dummy0 = 0;
    int dummy1 = 1;
    int dummy2 = 2;
}

void func53(TestStruct[2] testarg)
{
    assert(testarg[0].dummy0 == 0);
    assert(testarg[0].dummy1 == 1);
    assert(testarg[0].dummy2 == 2);

    assert(testarg[1].dummy0 == 0);
    assert(testarg[1].dummy1 == 1);
    assert(testarg[1].dummy2 == 2);
}

TestStruct[2] m53;

void test53()
{
    func53(m53);
}

/************************************************/

void test54()
{
    double a = 0;
    double b = 1;
    // Internal error: ..\ztc\cg87.c 3233
    a += ((1? b: 1+1i)*1i).re;
    assert(a == 0);
    // Internal error: ..\ztc\cod2.c 1680
    a += ((b?1:b-1i)*1i).re;
    assert(a == 0);
}

/************************************************/

class B55 {}
class D55 : B55 {}

template foo55(S, T : S) { }    // doesn't work

alias foo55!(B55, D55) bar55;

void test55()
{
}

/************************************************/

template t56() { alias Object t56; }
static assert(t56!().stringof == "Object");

void test56()
{
}

/************************************************/

void test57()
{
    alias long[char[]] AA;

    static if (is(AA T : T[U], U : const char[]))
    {
        assert(is(T == long));
        assert(is(U == const(char)[]));
    }

    static if (is(AA A : A[B], B : int))
    {
        assert(0);
    }

    static if (is(int[10] W : W[V], int V))
    {
        assert(is(W == int));
        assert(V == 10);
    }

    static if (is(int[10] X : X[Y], int Y : 5))
    {
        assert(0);
    }
}

/************************************************/

static this()
{
   printf("one\n");
}

static this()
{
   printf("two\n");
}

static ~this()
{
   printf("~two\n");
}

static ~this()
{
   printf("~one\n");
}


void test59()
{
}

/************************************************/

class C60
{
    extern (C++) int bar60(int i, int j, int k)
    {
        printf("this = %p\n", this);
        printf("i = %d\n", i);
        printf("j = %d\n", j);
        printf("k = %d\n", k);
        assert(i == 4);
        assert(j == 5);
        assert(k == 6);
        return 1;
    }
}


extern (C++)
        int foo60(int i, int j, int k)
{
    printf("i = %d\n", i);
    printf("j = %d\n", j);
    printf("k = %d\n", k);
    assert(i == 1);
    assert(j == 2);
    assert(k == 3);
    return 1;
}

void test60()
{
    foo60(1, 2, 3);

    C60 c = new C60();
    c.bar60(4, 5, 6);
}

/***************************************************/

template Foo61(alias a) {}

struct Bar61 {}
const Bar61 bar61 = {};

alias Foo61!(bar61) baz61;

void test61()
{
}

/************************************************/

T foo62(T)(lazy T value)
{
    return value;
}

void test62()
{
    foo62(new float[1]);
}

/************************************************/

void main()
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
    test42();
    test43();
    test44();
    test45();
    test46();
    test47();
    test48();
    test49();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test59();
    test60();
    test61();
    test62();

    printf("Success\n");
}
