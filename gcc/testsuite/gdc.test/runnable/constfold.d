#! blah
// RUNNABLE_PHOBOS_TEST
static assert(__LINE__ == 3); // fails as __LINE__ is 2

import std.stdio;
import std.math : signbit, sqrt;


/************************************/

static assert(-(1) == -1);
static assert(-(6i) == -6i);
static assert(-(1 + 6i) == -1 - 6i);

static assert(!27 == 0);
static assert(!0 == 1);
static assert(!6.2 == 0);
static assert(!0.0 == 1);
static assert(!3.7i == 0);
static assert(!0.0i == 1);
static assert(!(2+3.7i) == 0);
static assert(!(0+3.7i) == 0);
static assert(!(2+0.0i) == 0);
static assert(!(0+0.0i) == 1);

static assert(-6i + 2i == -4i);
static assert(6i - 1i == 5i);

static assert((3.6 + 7.2i) / (1 + 0i) == 3.6 + 7.2i);
static assert((3.6 + 7.2i) / (0.0 + 1i) == 7.2 - 3.6i);

static assert((6 % 4) == 2);
static assert((6u % 4u) == 2u);

static assert((cast(byte)0x109 >> 1) == 4);
static assert((cast(byte)-1 >> 1) == -1);
static assert((cast(ubyte)0x109 >> 1) == 4);

static assert((cast(short)0x10009 >> 1) == 4);
static assert((cast(short)-1 >> 1) == -1);
static assert((cast(ushort)0x10009 >> 1) == 4);

static assert((cast(long)0x1_0000_0000_0009 >> 1) == 0x8000_0000_0004);
static assert((cast(long)-1L >> 1) == -1);
static assert((cast(ulong)0x10009 >> 1) == 0x8004);

static assert((cast(byte)0x109 >>> 1) == 4);
static assert((cast(byte)-1 >>> 1) == int.max);
static assert((cast(ubyte)0x109 >>> 1) == 4);

static assert((cast(short)0x10009 >>> 1) == 4);
static assert((cast(short)-1 >>> 1) == int.max);
static assert((cast(ushort)0x10009 >>> 1) == 4);

static assert((cast(long)0x1_0000_0000_0009 >>> 1) == 0x8000_0000_0004);
static assert((cast(long)-1L >>> 1) == long.max);
static assert((cast(ulong)0x10009 >>> 1) == 0x8004);

static assert((3 ^ 5) == 6);

static assert((0 && 0) == 0);
static assert((0 && 5) == 0);
static assert((10 && 0) == 0);
static assert((58 && 10000) == 1);

static assert((0.0 && 0.0) == 0);
static assert((0.0 && 5.1) == 0);
static assert((10.0 && 0.0) == 0);
static assert((58.6 && 10000.7) == 1);

static assert((0 || 0) == 0);
static assert((0 || 5) == 1);
static assert((10 || 0) == 1);
static assert((58 || 10000) == 1);

static assert((0.0 || 0.0) == 0);
static assert((0.0 || 5.1) == 1);
static assert((10.0 || 0.0) == 1);
static assert((58.6 || 10000.7) == 1);

static assert((5 < 3) == 0);
static assert((5 < 5) == 0);
static assert((5 < 6) == 1);
static assert((5 <= 3) == 0);
static assert((5 <= 5) == 1);
static assert((5 <= 6) == 1);
static assert((5 > 3) == 1);
static assert((5 > 5) == 0);
static assert((5 > 6) == 0);
static assert((5 >= 3) == 1);
static assert((5 >= 5) == 1);
static assert((5 >= 6) == 0);

static assert((-5 < -3) == 1);
static assert((-5 < -5) == 0);
static assert((-5 < -6) == 0);
static assert((-5 <= -3) == 1);
static assert((-5 <= -5) == 1);
static assert((-5 <= -6) == 0);
static assert((-5 > -3) == 0);
static assert((-5 > -5) == 0);
static assert((-5 > -6) == 1);
static assert((-5 >= -3) == 0);
static assert((-5 >= -5) == 1);
static assert((-5 >= -6) == 1);

static assert((5u < 3u) == 0);
static assert((5u < 5u) == 0);
static assert((5u < 6u) == 1);
static assert((5u <= 3u) == 0);
static assert((5u <= 5u) == 1);
static assert((5u <= 6u) == 1);
static assert((5u > 3u) == 1);
static assert((5u > 5u) == 0);
static assert((5u > 6u) == 0);
static assert((5u >= 3u) == 1);
static assert((5u >= 5u) == 1);
static assert((5u >= 6u) == 0);

static assert((-5u < 3) == 0);
static assert((-5u <= 3) == 0);
static assert((-5u > 3) == 1);
static assert((-5u >= 3) == 1);

static assert((-5 < 3u) == 0);
static assert((-5 <= 3u) == 0);
static assert((-5 > 3u) == 1);
static assert((-5 >= 3u) == 1);

static assert((5.2 <    double.nan) == 0);
static assert((5.2 <=   double.nan) == 0);
static assert((5.2 >    double.nan) == 0);
static assert((5.2 >=   double.nan) == 0);

static assert((double.nan <    6.2) == 0);
static assert((double.nan <=   6.2) == 0);
static assert((double.nan >    6.2) == 0);
static assert((double.nan >=   6.2) == 0);

static assert((double.nan <    double.nan) == 0);
static assert((double.nan <=   double.nan) == 0);
static assert((double.nan >    double.nan) == 0);
static assert((double.nan >=   double.nan) == 0);

static assert((5.2 <    6.2) == 1);
static assert((5.2 <=   6.2) == 1);
static assert((5.2 >    6.2) == 0);
static assert((5.2 >=   6.2) == 0);

static assert((5.2 <    5.2) == 0);
static assert((5.2 <=   5.2) == 1);
static assert((5.2 >    5.2) == 0);
static assert((5.2 >=   5.2) == 1);

static assert((7.2 <    6.2) == 0);
static assert((7.2 <=   6.2) == 0);
static assert((7.2 >    6.2) == 1);
static assert((7.2 >=   6.2) == 1);

static assert((7.2i < 6.2i) == 0);


static assert((7.2i == 6.2i) == 0);
static assert((7.2i != 6.2i) == 1);
static assert((7.2 == 6.2) == 0);
static assert((7.2 != 6.2) == 1);

static assert((7.2i == 7.2i) == 1);
static assert((7.2i != 7.2i) == 0);
static assert((7.2 == 7.2) == 1);
static assert((7.2 != 7.2) == 0);

static assert((7.2 == double.nan) == 0);
static assert((7.2 != double.nan) == 1);
static assert((double.nan == double.nan) == 0);
static assert((double.nan != double.nan) == 1);
static assert((double.nan == 7.2) == 0);
static assert((double.nan != 7.2) == 1);

static assert((5 is 5) == 1);
static assert((5 is 4) == 0);
static assert((5 !is 5) == 0);
static assert((5 !is 4) == 1);

static assert((5.1 is 5.1) == 1);
static assert((5.1 is 4.1) == 0);
static assert((5.1 !is 5.1) == 0);
static assert((5.1 !is 4.1) == 1);

static assert((5.1 is 5.1i) == 0);
static assert((5.1 !is 5.1i) == 1);

static assert((5 ? 2 : 3) == 2);
static assert((0 ? 2 : 3) == 3);
static assert((5.0 ? 2 : 3) == 2);
static assert((0.0 ? 2 : 3) == 3);

static assert("abc" == "abc");

//static assert("abc"w.sizeof == 6);
//static assert("\U00010000bc"w.sizeof == 8);

static assert([1,2,3][1] == 2);
static assert([1,2,3] ~ [4] == [1,2,3,4]);
static assert([1,2,3][1..3] == [2,3]);

static assert(['a','b','c','d'] == "abcd");
static assert("efgh" == ['e','f','g','h']);
static assert("efgi" != ['e','f','g','h']);

static assert((2 ^^ 8) == 256);
static assert((3 ^^ 8.0) == 6561);
static assert((4.0 ^^ 8) == 65536);
static assert((5.0 ^^ 8.0) == 390625);

static assert((0.5 ^^ 3) == 0.125);
static assert((1.5 ^^ 3.0) == 3.375);
static assert((2.5 ^^ 3) == 15.625);
static assert((3.5 ^^ 3.0) == 42.875);

static assert(((-2) ^^ -5.0) == -0.031250);
static assert(((-2.0) ^^ -6) == 0.015625);
static assert(((-2.0) ^^ -7.0) == -0.0078125);

static assert((144 ^^ 0.5) == 12);
static assert((1089 ^^ 0.5) == 33);
static assert((1764 ^^ 0.5) == 42);
static assert((650.25 ^^ 0.5) == 25.5);


void test1()
{
    int x;
    int y;
    int* p;

    p = &x + cast(size_t)&y;
    p = &x + 2;
    p = 4 + &y;
    p = &x - 1;

    assert((&x is &x) == 1);
    assert((&x is &y) == 0);
    assert((&x !is &x) == 0);
    assert((&x !is &y) == 1);
}

/************************************/

void test2()
{
    // This test only tests undefined, architecture-dependant behavior.
    // E.g. the result of converting a float whose value doesn't fit into the integer
    // leads to an undefined result.
    version (DigitalMars)
    {
        float f = float.infinity;
        int i = cast(int) f;
        writeln(i);
        writeln(cast(int)float.max);
        assert(i == cast(int)float.max);
        assert(i == 0x80000000);
    }
}

/************************************/

void test3()
{
     real n = -0.0;
     const real m = -0.0;

     creal c = -0.0 + 3i;
     creal d = n + 3i;
     creal e = m + 3i;

     // should print "11111"
     writeln(signbit(n), signbit(m),
        signbit(c.re), signbit(d.re), signbit(e.re));

     assert(signbit(n) == 1);
     assert(signbit(m) == 1);
     assert(signbit(c.re) == 1);
     assert(signbit(d.re) == 1);
     assert(signbit(e.re) == 1);
}

/************************************/

struct A4 { char [] a; }
struct B4 { long x; }
struct C4 { int a;
    static C4 opCall(int b) { C4 q; q.a=b; return q; }
}
static assert(!is(typeof( (){ A4 s; B4 q = s; })));
static assert(!is(typeof( (){ B4 x =1L; })));
static assert(is(typeof( (){ C4 g = 7; })));
static assert(is(typeof( (){ C4 g = 7; C4 h = g;})));

/************************************/

alias uint DWORD;
MY_API_FUNCTION lpStartAddress;
extern (Windows) alias DWORD function(void*) MY_API_FUNCTION;
static assert(MY_API_FUNCTION.stringof == "extern (Windows) uint function(void*)");

/************************************/

enum bug6 = cast(void*)0xFEFEFEFE;
static assert(bug6 is bug6);

/************************************/

struct S7{
   double z;
}

int bug7(int x) {  return x; }

S7 s7;
double e7 = 4;
const double d7 = 4;

static assert(!is(typeof(bug7(cast(long)e7))));
static assert(!is(typeof(bug7(cast(long)s7))));
version (LDC) {} else // cast in LDC undefined result w/ x > long.max
static assert(!is(typeof(bug7(cast(long)3.256679e30))));

static assert(is(typeof(bug7(cast(long)d7))));
static assert(is(typeof(bug7(cast(long)3.256679e4))));

/************************************/

class C8 {
    int x;
}
alias C8.x F8;
static assert(is(typeof(F8) == int));
static assert(is(typeof(C8.x) == int));

/************************************/

int foo9() {
   int u = cast(int)(0x1_0000_0000L);
   while (u) {
      if (u) {
         assert(u!=0);
        }
      assert(u!=0);
   }
   return 2;
}

static assert(foo9()==2);

/************************************/
// Bugzilla 6077

void test6077() {
    static string scat(string s1, string s2)
    {
        return s1 ~ s2;
    }

    static string scatass(string s1, string s2)
    {
        s1 ~= s2;
        return s1;
    }

    static string[] arycats(string[] ary, string s)
    {
        return ary ~ s;
    }

    static string[] scatary(string s, string[] ary)
    {
        return s ~ ary;
    }

    static string[] arycatasss(string[] ary, string s)
    {
        ary ~= s;
        return ary;
    }

    static assert(scat(null, null) is null);
    static assert(scatass(null, null) is null);
    static assert(arycats(null, null) == cast(string[])[null]);
    static assert(scatary(null, null) == cast(string[])[null]);
    static assert(arycatasss(null, null) == cast(string[])[null]);
}

/************************************/

int test4()
{
    int i;

    dchar  d;
    d  >>= 1;
    d >>>= 1;
    d  <<= 1;
    d = d  >> 1;
    d = d >>> 1;
    d = d  << 1;
    wchar  w;
    w  >>= 1;
    w >>>= 1;
    w  <<= 1;
    w = w  >> 1;
    w = w >>> 1;
    i = w  << 1; // promoted to int
    char   c;
    c  >>= 1;
    c >>>= 1;
    c  <<= 1;
    c = c  >> 1;
    c = c >>> 1;
    i = c  << 1; // promoted to int
    return d + w + c + i;
}

static assert(test4() == 24666);

/************************************/
// 8400

void test8400()
{
    immutable a = [1,2];
    int[a.length+0] b; // ok
    int[a.length  ] c; // error
}

/************************************/
// 8939

void foo8939(T)(ref T) { } // same for `auto ref`
void bar8939(ref const int) { }
void bar8939(ref const S8939) { }

static struct S8939 { int n; }

const gn8939 = 1; // or `immutable`
const gs8939 = S8939(3);
static assert(__traits(compiles, foo8939(gn8939), bar8939(gn8939)));
static assert(__traits(compiles, foo8939(gs8939), bar8939(gs8939)));

void test8939()
{
    foo8939(gn8939), bar8939(gn8939);
    foo8939(gs8939), bar8939(gs8939);

    const ln8939 = 1;
    const ls8939 = S8939(3);
    foo8939(ln8939), bar8939(ln8939);
    foo8939(ls8939), bar8939(ls8939);
}

class C8939regression
{
    const int n1 = 0;
    const int n2 = 0;
    const int n3 = 0;
    const int n4 = 1;

    int refValue(V)(ref V var)
    {
        return 0;
    }

    void foo()
    {
        string[2] str;
        refValue(str[n1]);

        int[] da;
        refValue(da[n2]);

        int n; int* p = &n;
        refValue(*cast(int*)(p + n3));

        refValue([1,2,n4].ptr[0]);
    }
}

/************************************/
// 9058

template TypeTuple9058(TL...) { alias TypeTuple9058 = TL; }
template EnumMembers9058(T)
{
    alias EnumMembers9058 = TypeTuple9058!(Foo9058.A, Foo9058.B);
}
enum Foo9058 { A, B }
size_t bar9058(size_t n)
{
    return 0;
}
void test9058()
{
    Foo9058 x = [EnumMembers9058!Foo9058][bar9058($)];
}

/************************************/
// 11159

void test11159()
{
    import std.math : pow;
    enum ulong
        e_2_pow_64 = 2uL^^64,
        e_10_pow_19 = 10uL^^19,
        e_10_pow_20 = 10uL^^20;
    assert(e_2_pow_64 == pow(2uL, 64));
    assert(e_10_pow_19 == pow(10uL, 19));
    assert(e_10_pow_20 == pow(10uL, 20));
}

/************************************/
// 12306

void test12306()
{
    struct Point3D { ubyte x, y, z; }

    enum      Point3D pt1 = {x:1, y:1, z:1};
    const     Point3D pt2 = {x:1, y:1, z:1};
    immutable Point3D pt3 = {x:1, y:1, z:1};

    int[pt1.z][pt1.y][pt1.x] a1;
    int[pt2.z][pt2.y][pt2.x] a2;
    int[pt3.z][pt3.y][pt3.x] a3;

    ubyte a = 1;
    const     Point3D ptx = {x:a, y:1, z:1};

    static assert(!__traits(compiles, { int[ptx.z][ptx.y][ptx.x] ax; }));
}

/************************************/
// 13977

void test13977()
{
    bool cond(bool b) { return b; }
    int x = 0;
    void check(int n = 1) { x = n; }

    cond(true) && check();
    assert(x == 1); x = 0;

    cond(false) && check();
    assert(x == 0); x = 0;

    true && check();
    assert(x == 1); x = 0;

    false && check();
    assert(x == 0); x = 0;
    (int[]).init && check();
    assert(x == 0); x = 0;
    Object.init && check();
    assert(x == 0);

    check(2);
    false && check();
    assert(x == 2); x = 0;
}

/************************************/
// 13978

void test13978()
{
    bool cond(bool b) { return b; }
    int x = 0;
    void check(int n = 1) { x = n; }

    cond(true) || check();
    assert(x == 0); x = 0;

    cond(false) || check();
    assert(x == 1); x = 0;

    true || check();
    assert(x == 0); x = 0;

    false || check();
    assert(x == 1); x = 0;
    (int[]).init || check();
    assert(x == 1); x = 0;
    Object.init || check();
    assert(x == 1); x = 0;

    check(2);
    true || check();
    assert(x == 2); x = 0;
}

/************************************/
// Pull Request 3697

void test3697and()
{
    enum x = 0;
    auto y = x && 1 / x;
}

void test3697or()
{
    enum x = 0;
    enum y = 1;
    auto z = y || 1 / x;
}

/************************************/
// 14459

void test14459()
{
    const char* s0 = "hi0";
    const(char)* p0 = s0;
    assert(p0 == s0);

    const char* s1 = "hi1";
    const char* s2 = "hi2";
    const char* s3 = "hi3";
    const char* s4 = "hi4";
    const char* s5 = "hi5";
    const char* s6 = "hi6";
    const char* s7 = "hi7";
    const char* s8 = "hi8";
    const char* s9 = "hi9";
    const char* s10 = "hi10";
    const char* s11 = "hi11";
    const char* s12 = "hi12";
    const char* s13 = "hi13";
    const char* s14 = "hi14";
    const char* s15 = "hi15";
    assert(p0 == s0);           // ok
    const char* s16 = "hi16";
    assert(p0 == s0);           // ok <- fails
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=15607

static immutable char[2][4] code_base = [ "??", 12 ];
static assert(code_base[0] == "??");
static assert(code_base[1] == [12, 12]);
static assert(code_base[2] == typeof(code_base[2]).init);

/************************************/

int main()
{
    test1();
    test2();
    test3();
    test3697and();
    test3697or();
    test6077();
    test8400();
    test8939();
    test9058();
    test11159();
    test13977();
    test13978();
    test14459();

    printf("Success\n");
    return 0;
}
