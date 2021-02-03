// PERMUTE_ARGS: -inline
/*
TEST_OUTPUT:
---
compilable/interpret3.d(2914): Deprecation: `case` variables have to be `const` or `immutable`
compilable/interpret3.d(6351): Deprecation: identity comparison of static arrays implicitly coerces them to slices, which are compared by reference
---
*/

template compiles(int T)
{
    bool compiles = true;
}

alias TypeTuple(T...) = T;

/**************************************************
    3901 Arbitrary struct assignment, ref return
**************************************************/

struct ArrayRet
{
    int x;
}

int arrayRetTest(int z)
{
    ArrayRet[6] w;
    int q = (w[3].x = z);
    return q;
}
static assert(arrayRetTest(51) == 51);

// Bugzilla 3842 -- must not segfault
int ice3842(int z)
{
    ArrayRet w;
    return arrayRetTest((*(&w)).x);
}
static assert(true || is(typeof(compiles!(ice3842(51)))));

int arrayret2()
{
    int[5] a;
    int[3] b;
    b[] = a[1 .. $-1] = 5;
    return b[1];
}
static assert(arrayret2() == 5);

struct DotVarTest
{
    ArrayRet z;
}

struct DotVarTest2
{
    ArrayRet z;
    DotVarTest p;
}

int dotvar1()
{
    DotVarTest w;
    w.z.x = 3;
    return w.z.x;
}
static assert(dotvar1() == 3);

int dotvar2()
{
    DotVarTest2[4] m;
    m[2].z.x = 3;
    m[1].p.z.x = 5;
    return m[2].z.x + 7;
}
static assert(dotvar2() == 10);


struct RetRefStruct
{
    int x;
    char c;
}

// Return value reference tests, for D2 only.

ref RetRefStruct reffunc1(ref RetRefStruct a)
{
    int y = a.x;
    return a;
}

ref RetRefStruct reffunc2(ref RetRefStruct a)
{
    RetRefStruct z = a;
    return reffunc1(a);
}

ref int reffunc7(ref RetRefStruct aa)
{
    return reffunc1(aa).x;
}

ref int reffunc3(ref int a)
{
    return a;
}

struct RefTestStruct
{
    RetRefStruct r;

    ref RefTestStruct reffunc4(ref RetRefStruct[3] a)
    {
        return this;
    }

    ref int reffunc6()
    {
        return this.r.x;
    }
}

ref RetRefStruct reffunc5(ref RetRefStruct[3] a)
{
    int t = 1;
    for (int i = 0; i < 10; ++i)
    {
        if (i == 7)
            ++t;
    }
    return a[reffunc3(t)];
}

int retRefTest1()
{
    RetRefStruct b = RetRefStruct(0, 'a');
    reffunc1(b).x = 3;
    return b.x - 1;
}

int retRefTest2()
{
    RetRefStruct b = RetRefStruct(0, 'a');
    reffunc2(b).x = 3;
    RetRefStruct[3] z;
    RefTestStruct w;
    w.reffunc4(z).reffunc4(z).r.x = 4;
    assert(w.r.x == 4);
    w.reffunc6() = 218;
    assert(w.r.x == 218);
    z[2].x = 3;
    int q = 4;
    int u = reffunc5(z).x + reffunc3(q);
    assert(u == 7);
    reffunc5(z).x += 7;
    assert(z[2].x == 10);
    RetRefStruct m = RetRefStruct(7, 'c');
    m.x = 6;
    reffunc7(m) += 3;
    assert(m.x == 9);
    return b.x - 1;
}

int retRefTest3()
{
    RetRefStruct b = RetRefStruct(0, 'a');
    auto deleg = function (RetRefStruct a){ return a; };
    typeof(deleg)[3] z;
    z[] = deleg;
    auto y = deleg(b).x + 27;
    b.x = 5;
    assert(y == 27);
    y = z[1](b).x + 22;
    return y - 1;
}

int retRefTest4()
{
    RetRefStruct b = RetRefStruct(0, 'a');
    reffunc3(b.x) = 218;
    assert(b.x == 218);
    return b.x;
}

static assert(retRefTest1() == 2);
static assert(retRefTest2() == 2);
static assert(retRefTest3() == 26);
static assert(retRefTest4() == 218);

/**************************************************
    Bug 7887 assign to returned reference
**************************************************/

bool test7887()
{
    ref int f(ref int x) { return x; }
    int a;
    f(a) = 42;
    return (a == 42);
}
static assert(test7887());

/**************************************************
    Bug 7473 struct non-ref
**************************************************/

struct S7473
{
    int i;
}

static assert({
    S7473 s = S7473(1);
    assert(s.i == 1);
    bug7473(s);
    assert(s.i == 1);
    return true;
}());

void bug7473(S7473 s)
{
    s.i = 2;
}

struct S7473b
{
    S7473 m;
}

static assert({
    S7473b s = S7473b(S7473(7));
    assert(s.m.i == 7);
    bug7473b(s);
    assert(s.m.i == 7);
    return true;
}());

void bug7473b(S7473b s)
{
    s.m.i = 2;
}

/**************************************************
    Bug 4389
**************************************************/

int bug4389()
{
    string s;
    dchar c = '\u2348';
    s ~= c;
    assert(s.length == 3);
    dchar d = 'D';
    s ~= d;
    assert(s.length == 4);
    s = "";
    s ~= c;
    assert(s.length == 3);
    s ~= d;
    assert(s.length == 4);
    string z;
    wchar w = '\u0300';
    z ~= w;
    assert(z.length == 2);
    z = "";
    z ~= w;
    assert(z.length == 2);
    return 1;
}

static assert(bug4389());

// ICE(constfold.c)
int ice4389()
{
    string s;
    dchar c = '\u2348';
    s ~= c;
    s = s ~ "xxx";
   return 1;
}

static assert(ice4389());

// ICE(expression.c)
string ice4390()
{
    string s;
    dchar c = '`';
    s ~= c;
    s ~= c;
   return s;
}

static assert(mixin(ice4390()) == ``);

// bug 5248 (D1 + D2)
struct Leaf5248
{
    string Compile_not_ovloaded()
    {
        return "expression";
    }
}
struct Matrix5248
{
    Leaf5248 Right;

    string Compile()
    {
        return Right.Compile_not_ovloaded();
    }
};

static assert(Matrix5248().Compile());

/**************************************************
    4837   >>>=
**************************************************/

bool bug4837()
{
    ushort x = 0x89AB;
    x >>>= 4;
    assert(x == 0x89A);
    byte y = 0x7C;
    y >>>= 2;
    assert(y == 0x1F);
    return true;
}

static assert(bug4837());

/**************************************************
  10252 shift out of range
**************************************************/

int lshr10252(int shift)
{
     int a = 5;
     return a << shift;
}

int rshr10252(int shift)
{
     int a = 5;
     return a >> shift;
}

int ushr10252(int shift)
{
     int a = 5;
     return a >>> shift;
}

static assert( is(typeof(compiles!(lshr10252( 4)))));
static assert(!is(typeof(compiles!(lshr10252(60)))));
static assert( is(typeof(compiles!(rshr10252( 4)))));
static assert(!is(typeof(compiles!(rshr10252(80)))));
static assert( is(typeof(compiles!(ushr10252( 2)))));
static assert(!is(typeof(compiles!(ushr10252(60)))));

/**************************************************
  1982 CTFE null problems
**************************************************/

enum a1982 = [1, 2, 3];
static assert(a1982 !is null);

string foo1982() { return null; }
static assert(foo1982() is null);
static assert(!foo1982().length);

static assert(null is null);

/**************************************************
  7988 CTFE return values should be allowed in compile-time expressions
**************************************************/

class X7988 { int y; this() { y = 2; } }
static assert((new X7988).y == 2);

/**************************************************
  8253 ICE: calling of member function of non-CTFE class variable
**************************************************/

class Bug8253
{
    bool j()
    {
        return true;
    }
}
Bug8253 m8253;
static assert(!is(typeof(compiles!(m8253.j()))));

/**************************************************
  8285 Issue with slice returned from CTFE function
**************************************************/

string foo8285()
{
    string s = "ab";
    return s[0 .. $];
}

template T8285b(string s) { }

template T8285a()
{
    enum s = foo8285();
    alias T8285b!(s) t2;
}

int bar8285()
{
    alias T8285a!() t1;
    return 0;
}

int baz8285(int x)
{
    return 0;
}

static assert(baz8285(bar8285()) == 0);

// test case 2

string xbar8285()
{
    string s = "ab";
    return s[0 .. $];
}

template xT8285a()
{
    enum xT8285a = xbar8285()[0 .. $];
}

string xbaz8285()
{
    return xT8285a!();
}

string xfoo8285(string s)
{
    return s;
}

static assert(xfoo8285(xbaz8285()) == "ab");

/**************************************************
  'this' parameter bug revealed during refactoring
**************************************************/

int thisbug1(int x) { return x; }

struct ThisBug1
{
    int m = 1;
    int wut()
    {
        return thisbug1(m);
    }
}

int thisbug2()
{
    ThisBug1 spec;
    return spec.wut();
}

static assert(thisbug2());

/**************************************************
   6972 ICE with cast()cast()assign
**************************************************/

int bug6972()
{
    ubyte n = 6;
    n /= 2u;
    return n;
}
static assert(bug6972() == 3);

/**************************************************
    Bug 6164
**************************************************/

size_t bug6164()
{
    int[] ctfe2(int n)
    {
        int[] r = [];
        if (n != 0)
            r ~= [1] ~ ctfe2(n - 1);
        return r;
    }
    return ctfe2(2).length;
}
static assert(bug6164() == 2);

/**************************************************
    Interpreter code coverage tests
**************************************************/

int cov1(int a)
{
    a %= 15382;
    a /= 5;
    a = ~ a;
    bool c = (a == 0);
    bool b = true && c;
    assert(b == 0);
    b = false && c;
    assert(b == 0);
    b = false || c;
    assert(b == 0);
    a ^= 0x45349;
    a = ~ a;
    a &= 0xFF3F;
    a >>>= 1;
    a = a ^ 0x7393;
    a = a >> 1;
    a = a >>> 1;
    a = a | 0x010101;
    return a;
}
static assert(cov1(534564) == 71589);

int cov2()
{
    int i = 0;
    do
    {
        goto DOLABEL;
    DOLABEL:
        if (i != 0)
        {
            goto IFLABEL;
    IFLABEL:
            switch(i)
            {
            case 3:
                break;
            case 6:
                goto SWITCHLABEL;
    SWITCHLABEL:
                i = 27;
                goto case 3;
            default:
                assert(0);
            }
            return i;
        }
        i = 6;
    } while(true);
    return 88; // unreachable
}
static assert(cov2() == 27);

template CovTuple(T...)
{
    alias T CovTuple;
}

alias CovTuple!(int, long) TCov3;

int cov3(TCov3 t)
{
    TCov3 s;
    s = t;
    assert(s[0] == 1);
    assert(s[1] == 2);
    return 7;
}
static assert(cov3(1, 2) == 7);

int badassert1(int z)
{
    assert(z == 5, "xyz");
    return 1;
}

size_t badslice1(int[] z)
{
    return z[0 .. 3].length;
}

size_t badslice2(int[] z)
{
    return z[0 .. badassert1(1)].length;
}

size_t badslice3(int[] z)
{
    return z[badassert1(1) .. 2].length;
}

static assert(!is(typeof(compiles!(badassert1(67)))));
static assert( is(typeof(compiles!(badassert1(5)))));
static assert(!is(typeof(compiles!(badslice1([1,2])))));
static assert(!is(typeof(compiles!(badslice2([1,2])))));
static assert(!is(typeof(compiles!(badslice3([1,2,3])))));

/*******************************************/

int bug7894()
{
    for (int k = 0; k < 2; ++k)
    {
        goto Lagain;
Lagain:
        ;
    }
    int m = 1;
    do
    {
        ++m;
        goto Ldo;
Ldo: ;
    } while (m < 3);
    assert(m == 3);

    return 1;
}
static assert(bug7894());

/*******************************************/

size_t bug5524(int x, int[] more...)
{
    int[0] zz;
    assert(zz.length == 0);
    return 7 + more.length + x;
}
static assert(bug5524(3) == 10);


// 5722

static assert(("" ~ "\&copy;"[0]).length == 1);
const char[] null5722 = null;
static assert((null5722 ~ "\&copy;"[0]).length == 1);
static assert(("\&copy;"[0] ~ null5722).length == 1);

/*******************************************
 * Tests for CTFE Array support.
 * Including bugs 1330, 3801, 3835, 4050,
 * 4051, 5147, and major functionality
 *******************************************/

char[] bug1330StringIndex()
{
    char[] blah = "foo".dup;
    assert(blah == "foo");
    char[] s = blah[0 .. 2];
    blah[0] = 'h';
    assert(s == "ho");
    s[0] = 'm';
    return blah;
}
static assert(bug1330StringIndex() == "moo");
static assert(bug1330StringIndex() == "moo"); // check we haven't clobbered any string literals

int[] bug1330ArrayIndex()
{
    int[] blah = [1,2,3];
    int[] s = blah;
    s = blah[0 .. 2];
    int z = blah[0] = 6;
    assert(z == 6);
    assert(blah[0] == 6);
    assert(s[0] == 6);
    assert(s == [6, 2]);
    s[1] = 4;
    assert(z == 6);
    return blah;
}
static assert(bug1330ArrayIndex() == [6, 4, 3]);
static assert(bug1330ArrayIndex() == [6, 4, 3]); // check we haven't clobbered any literals

char[] bug1330StringSliceAssign()
{
    char[] blah = "food".dup;
    assert(blah == "food");
    char[] s = blah[1 .. 4];
    blah[0 .. 2] = "hc";
    assert(s == "cod");
    s[0 .. 2] = ['a', 'b'];   // Mix string + array literal
    assert(blah == "habd");
    s[0 .. 2] = "mq";
    return blah;
}
static assert(bug1330StringSliceAssign() == "hmqd");
static assert(bug1330StringSliceAssign() == "hmqd");

int[] bug1330ArraySliceAssign()
{
    int[] blah = [1, 2, 3, 4];
    int[] s = blah[1 .. 4];
    blah[0 .. 2] = [7, 9];
    assert(s == [9, 3, 4]);
    s[0 .. 2] = [8, 15];
    return blah;
}
static assert(bug1330ArraySliceAssign() == [7, 8, 15, 4]);

int[] bug1330ArrayBlockAssign()
{
    int[] blah = [1, 2, 3, 4, 5];
    int[] s = blah[1 .. 4];
    blah[0 .. 2] = 17;
    assert(s == [17, 3, 4]);
    s[0 .. 2] = 9;
    return blah;
}
static assert(bug1330ArrayBlockAssign() == [17, 9, 9, 4, 5]);

char[] bug1330StringBlockAssign()
{
    char[] blah = "abcde".dup;
    char[] s = blah[1 .. 4];
    blah[0 .. 2] = 'x';
    assert(s == "xcd");
    s[0 .. 2] = 'y';
    return blah;
}
static assert(bug1330StringBlockAssign() == "xyyde");

int assignAA(int x)
{
    int[int] aa;
    int[int] cc = aa;
    assert(cc.values.length == 0);
    assert(cc.keys.length == 0);
    aa[1] = 2;
    aa[x] = 6;
    int[int] bb = aa;
    assert(bb.keys.length == 2);
    assert(cc.keys.length == 0); // cc is not affected to aa, because it is null
    aa[500] = 65;
    assert(bb.keys.length == 3); // but bb is affected by changes to aa
    return aa[1] + aa[x];
}
static assert(assignAA(12) == 8);

template Compileable(int z) { bool OK; }

int arraybounds(int j, int k)
{
    int[] xxx = [1, 2, 3, 4, 5];
    int[] s = xxx[1 .. $];
    s = s[j .. k]; // slice of slice
    return s[$ - 1];
}
static assert(!is(typeof(Compileable!(arraybounds(1, 14)))));
static assert(!is(typeof(Compileable!(arraybounds(15, 3)))));
static assert(arraybounds(2, 4) == 5);

int arraybounds2(int j, int k)
{
    int[] xxx = [1, 2, 3, 4, 5];
    int[] s = xxx[j .. k]; // direct slice
    return 1;
}
static assert(!is(typeof(Compileable!(arraybounds2(1, 14)))));
static assert(!is(typeof(Compileable!(arraybounds2(15, 3)))));
static assert(arraybounds2(2, 4) == 1);

int bug5147a()
{
    int[1][2] a = 37;
    return a[0][0];
}
static assert(bug5147a() == 37);

int bug5147b()
{
    int[4][2][3][17] a = 37;
    return a[0][0][0][0];
}
static assert(bug5147b() == 37);

int setlen()
{
    int[][] zzz;
    zzz.length = 2;
    zzz[0].length = 10;
    assert(zzz.length == 2);
    assert(zzz[0].length == 10);
    assert(zzz[1].length == 0);
    return 2;
}
static assert(setlen() == 2);

int[1][1] bug5147()
{
    int[1][1] a = 1;
    return a;
}
static assert(bug5147() == [[1]]);

enum int[1][1] enum5147 = bug5147();
static assert(enum5147 == [[1]]);

immutable int[1][1] bug5147imm = bug5147();

// Index referencing
int[2][2] indexref1()
{
    int[2][2] a = 2;
    a[0] = 7;

    int[][] b = [null, null];
    b[0 .. $] = a[0][0 .. 2];
    assert(b[0][0] == 7);
    assert(b[0][1] == 7);
    int[] w;
    w = a[0];
    assert(w[0] == 7);
    w[0 .. $] = 5;
    assert(a[0] != [7, 7]);
    assert(a[0] == [5, 5]);
    assert(b[0] == [5, 5]);
    return a;
}
int[2][2] indexref2()
{
    int[2][2] a = 2;
    a[0] = 7;

    int[][2] b = null;
    b[0 .. $] = a[0];
    assert(b[0][0] == 7);
    assert(b[0][1] == 7);
    assert(b == [[7, 7], [7, 7]]);
    int[] w;
    w = a[0];
    assert(w[0] == 7);
    w[0 .. $] = 5;
    assert(a[0] != [7, 7]);
    assert(a[0] == [5, 5]);
    assert(b[0] == [5, 5]);
    return a;
}
int[2][2] indexref3()
{
    int[2][2] a = 2;
    a[0]=7;

    int[][2] b = [null, null];
    b[0 .. $] = a[0];
    assert(b[0][0] == 7);
    assert(b[0][1] == 7);
    int[] w;
    w = a[0];
    assert(w[0] == 7);
    w[0 .. $] = 5;
    assert(a[0] != [7, 7]);
    assert(a[0] == [5, 5]);
    assert(b[0] == [5, 5]);
    return a;
}
int[2][2] indexref4()
{
    int[2][2] a = 2;
    a[0] = 7;

    int[][2] b =[[1, 2, 3], [1, 2, 3]]; // wrong code
    b[0] = a[0];
    assert(b[0][0] == 7);
    assert(b[0][1] == 7);
    int[] w;
    w = a[0]; //[0 .. $];
    assert(w[0] == 7);
    w[0 .. $] = 5;
    assert(a[0] != [7, 7]);
    assert(a[0] == [5, 5]);
    assert(b[0] == [5, 5]);
    return a;
}
static assert(indexref1() == [[5, 5], [2, 2]]);
static assert(indexref2() == [[5, 5], [2, 2]]);
static assert(indexref3() == [[5, 5], [2, 2]]);
static assert(indexref4() == [[5, 5], [2, 2]]);

int staticdynamic()
{
    int[2][1] a = 2;
    assert(a == [[2, 2]]);

    int[][1] b = a[0][0 .. 1];
    assert(b[0] == [2]);
    auto k = b[0];
    auto m = a[0][0 .. 1];
    assert(k == [2]);
    assert(m == k);
    return 0;
}
static assert(staticdynamic() == 0);

int chainassign()
{
    int[4] x = 6;
    int[] y = new int[4];
    auto k = (y[] = (x[] = 2));
    return k[0];
}
static assert(chainassign() == 2);

// index assignment
struct S3801
{
    char c;
    int[3] arr;

    this(int x, int y)
    {
        c = 'x';
        arr[0] = x;
        arr[1] = y;
    }
}

int bug3801()
{
    S3801 xxx = S3801(17, 67);
    int[] w = xxx.arr;
    xxx.arr[1] = 89;
    assert(xxx.arr[0] == 17);
    assert(w[1] == 89);
    assert(w == [17, 89, 0]);
    return xxx.arr[1];
}

enum : S3801 { bug3801e = S3801(17, 18) }
static assert(bug3801e.arr == [17, 18, 0]);

immutable S3801 bug3801u = S3801(17, 18);
static assert(bug3801u.arr == [17, 18, 0]);
static assert(bug3801() == 89);

int bug3835()
{
    int[4] arr;
    arr[] = 19;
    arr[0] = 4;
    int kk;
    foreach (ref el; arr)
    {
        el += 10;
        kk = el;
    }
    assert(arr[2] == 29);
    arr[0] += 3;
    return arr[0];
}
static assert(bug3835() == 17);

auto bug5852(const(string) s)
{
    string[] r;
    r ~= s;
    assert(r.length == 1);
    return r[0].length;
}
static assert(bug5852("abc") == 3);

// 7217

struct S7217 { int[] arr; }

bool f7217()
{
    auto s = S7217();
    auto t = s.arr;
    return true;
}
static assert(f7217());

/*******************************************
    Set array length
*******************************************/

static assert(
{
    struct W { int[] z; }
    W w;
    w.z.length = 2;
    assert(w.z.length == 2);
    w.z.length = 6;
    assert(w.z.length == 6);
    return true;
}());

// 7185 char[].length = n

bool bug7185()
{
    auto arr = new char[2];
    auto arr2 = new char[2];
    arr2[] = "ab";
    arr.length = 1;
    arr2.length = 7;
    assert(arr.length == 1);
    assert(arr2.length == 7);
    assert(arr2[0 .. 2] == "ab");
    return true;
}
static assert(bug7185());

bool bug9908()
{
    static const int[3] sa = 1;
    return sa == [1, 1, 1];
}
static assert(bug9908());

/*******************************************
    6934
*******************************************/

struct Struct6934
{
    int[] x = [1, 2];
}

void bar6934(ref int[] p)
{
    p[0] = 12;
    assert(p[0] == 12);
    p[0 .. 1] = 17;
    assert(p[0] == 17);
    p = p[1 .. $];
}

int bug6934()
{
    Struct6934 q;
    bar6934(q.x);
    int[][] y = [[2, 5], [3, 6, 8]];
    bar6934(y[0]);
    return 1;
}
static assert(bug6934());

/*******************************************
             Bug 5671
*******************************************/

static assert(['a', 'b'] ~ "c" == "abc");

/*******************************************
      8624
*******************************************/

int evil8624()
{
    long  m =  0x1_0000_0000L;
    assert(m != 0);
    long[] a = [0x1_0000_0000L];
    long[] b = [0x4_0000_0000L];
    assert(a[] != b[]);
    return 1;
}
static assert(evil8624());

/*******************************************
        8644 array literal >,<
*******************************************/

int bug8644()
{
    auto m = "a";
    auto z = ['b'];
    auto c = "b7";
    auto d = ['b', '6'];
    assert(m < z);
    assert(z > m);
    assert(z <= c);
    assert(c > z);
    assert(c > d);
    assert(d >= d);
    return true;
}
static assert(bug8644());

/*******************************************
        Bug 6159
*******************************************/

struct A6159 {}

static assert({ return A6159.init is A6159.init; }());
static assert({ return [1] is [1]; }());

/*******************************************
        Bug 5685
*******************************************/

string bug5685()
{
    return "xxx";
}
struct Bug5865
{
    void test1()
    {
        enum file2 = (bug5685())[0 .. $];
    }
}

/*******************************************
    6235 - Regression ICE on $ in template
*******************************************/

struct Bug6235(R)
{
    enum XXX = is(typeof(R.init[0 .. $]) : const ubyte[]);
}

Bug6235!(ubyte[]) bug6235;

/*******************************************
    8673 ICE
*******************************************/

enum dollar8673 = [0][(() => $ - 1)()];

/*******************************************
        Bug 5840
*******************************************/

struct Bug5840
{
    string g;
    int w;
}

int bug5840(int u)
{
    // check for clobbering
    Bug5840 x = void;
    x.w = 4;
    x.g = "3gs";
    if (u == 1)
        bug5840(2);
    if (u == 2)
    {
        x.g = "abc";
        x.w = 3465;
    }
    else
    {
        assert(x.g == "3gs");
        assert(x.w == 4);
    }
    return 56;
}
static assert(bug5840(1) == 56);

/*******************************************
        7810
*******************************************/

int bug7810()
{
    int[1][3] x = void;
    x[0] = [2];
    x[1] = [7];
    assert(x[0][0] == 2);

    char[1][3] y = void;
    y[0] = "a";
    y[1] = "b";
    assert(y[0][0] == 'a');

    return 1;
}
static assert(bug7810());

struct Bug7810
{
    int w;
}
int bug7810b(T)(T[] items...)
{
    assert(items[0] == Bug7810(20));
    return 42;
}
static assert(bug7810b(Bug7810(20), Bug7810(10)) == 42);

/*******************************************
    std.datetime ICE (30 April 2011)
*******************************************/

struct TimeOfDayZ
{
public:
    this(int hour) { }
    invariant() { }
}
const testTODsThrownZ = TimeOfDayZ(0);

/*******************************************
        Bug 5954
*******************************************/

struct Bug5954
{
    int x;
    this(int xx)
    {
        this.x = xx;
    }
}
void bug5954()
{
    enum f = Bug5954(10);
    static assert(f.x == 10);
}

/*******************************************
        Bug 5972
*******************************************/

int bug5972()
{
    char[] z = "abc".dup;
    char[][] a = [null, null];
    a[0]  = z[0 .. 2];
    char[] b = a[0];
    assert(b == "ab");
    a[0][1] = 'q';
    assert(a[0] == "aq");
    assert(b == "aq");
    assert(b[1] == 'q');
    //a[0][0 .. $ - 1][0 .. $] = a[0][0 .. $ - 1][0 .. $];  // overlap
    return 56;
}
static assert(bug5972() == 56);

/*******************************************
    2.053beta [CTFE]ICE 'global errors'
*******************************************/

int wconcat(wstring replace)
{
    wstring value;
    value  = "A"w;
    value = value ~ replace;
    return 1;
}
static assert(wconcat("X"w));

/*******************************************
    10397 string concat
*******************************************/

static assert(!is(typeof(compiles!("abc" ~ undefined))));
static assert(!is(typeof(compiles!(otherundefined ~ "abc"))));

/*******************************************
    9634 struct concat
*******************************************/

struct Bug9634
{
    int raw;
}

bool bug9634()
{
    Bug9634[] jr = [Bug9634(42)];

    Bug9634[] ir = null ~ jr;
    Bug9634[] kr = jr ~ null;
    Bug9634[] mr = jr ~ jr;

    jr[0].raw = 6;
    assert(ir[0].raw == 42);
    assert(kr[0].raw == 42);
    assert(jr[0].raw == 6);
    assert(&mr[0] != &mr[1]);
    return true;
}

static assert(bug9634());

/*******************************************
    Bug 4001: A Space Oddity
*******************************************/

int space() { return 4001; }

void oddity4001(int q)
{
    const int bowie = space();
    static assert(space() == 4001);
    static assert(bowie == 4001);
}

/*******************************************
    Bug 3779
*******************************************/

static const bug3779 = ["123"][0][$ - 1];

/*******************************************
    Bug 8893 ICE with bad struct literal
*******************************************/

struct Foo8893
{
    char[3] data;
}
int bar8893(Foo8893 f)
{
    return f.data[0];
}
static assert(!is(typeof(compiles!(bar8893(Foo8893(['a','b']))))));

/*******************************************
    non-Cow struct literals
*******************************************/

struct Zadok
{
    int[3] z;
    char[4] s = void;
    ref int[] fog(ref int[] q) { return q; }
    int bfg()
    {
        z[0] = 56;
        auto zs = z[];
        fog(zs) = [56, 6, 8];

        assert(z[0] == 56);
        assert(z[1] == 61);
        assert(z[2] == 61);

        assert(zs[0] == 56);
        assert(zs[1] == 6);
        return zs[2];
    }
}

struct Vug
{
    Zadok p;
    int[] other;
}

int quop()
{
    int[] heap = new int[5];
    heap[] = 738;
    Zadok pong;
    pong.z = 3;
    int[] w = pong.z;
    assert(w[0] == 3);
    Zadok phong;
    phong.z = 61;
    pong = phong;
    assert(w[0] == 61);
    Vug b = Vug(Zadok(17, "abcd"));
    b = Vug(Zadok(17, "abcd"), heap);
    b.other[2] = 78;
    assert(heap[2] == 78);
    char[] y = b.p.s;
    assert(y[2] == 'c');
    phong.s = ['z','x','f', 'g'];
    w = b.p.z;
    assert(y[2] == 'c');
    assert(w[0] == 17);
    b.p = phong;
    assert(y[2] == 'f');

    Zadok wok = Zadok(6, "xyzw");
    b.p = wok;
    assert(y[2] == 'z');
    b.p = phong;
    assert(w[0] == 61);
    Vug q;
    q.p = pong;
    return pong.bfg();
}

static assert(quop() == 8);
static assert(quop() == 8); // check for clobbering

/**************************************************
   Bug 5676 tuple assign of struct that has void opAssign
**************************************************/

struct S5676
{
    int x;
    void opAssign(S5676 rhs) { x = rhs.x; }
}

struct Tup5676(E...)
{
    E g;
    void foo(E values) { g = values; }
}

bool ice5676()
{
    Tup5676!(S5676) q;
    q.foo(S5676(3));
    assert(q.g[0].x == 3);
    return true;
}

static assert(ice5676());

/**************************************************
   Bug 5682 Wrong CTFE with operator overloading
**************************************************/

struct A
{
    int n;
    auto opBinary(string op : "*")(A rhs)
    {
        return A(n * rhs.n);
    }
}

A foo(A[] lhs, A[] rhs)
{
    A current;
    for (size_t k = 0; k < rhs.length; ++k)
    {
        current = lhs[k] * rhs[k];
    }
    return current;
}

auto test()
{
    return foo([A(1), A(2)], [A(3), A(4)]);
}

static assert(test().n == 8);

/**************************************************
   Attempt to modify a read-only string literal
**************************************************/
struct Xarg
{
    char[] s;
}

int zfs(int n)
{
    char[] m = "exy".dup;
    if (n == 1)
    {
        // it's OK to cast to const, then cast back
        string ss = cast(string)m;
        m = cast(char[])ss;
        m[2]='q';
        return 56;
    }
    auto q = Xarg(cast(char[])"abc");
    assert(q.s[1] == 'b');
    if (n == 2)
        q.s[1] = 'p';
    else if (n == 3)
        q.s[0 .. $] = 'p';
    char* w = &q.s[2];
    if (n == 4)
        *w = 'z';
    return 76;
}

static assert(!is(typeof(compiles!(zfs(2)))));
static assert(!is(typeof(compiles!(zfs(3)))));
static assert(!is(typeof(compiles!(zfs(4)))));
static assert( is(typeof(compiles!(zfs(1)))));
static assert( is(typeof(compiles!(zfs(5)))));

/**************************************************
   .dup must protect string literals
**************************************************/

string mutateTheImmutable(immutable string _s)
{
    char[] s = _s.dup;
    foreach (ref c; s)
        c = 'x';
    return s.idup;
}

string doharm(immutable string _name)
{
    return mutateTheImmutable(_name[2 .. $].idup);
}

enum victimLiteral = "CL_INVALID_CONTEXT";

enum thug = doharm(victimLiteral);
static assert(victimLiteral == "CL_INVALID_CONTEXT");

/**************************************************
        Use $ in a slice of a dotvar slice
**************************************************/

int sliceDollar()
{
    Xarg z;
    z.s = new char[20];
    z.s[] = 'b';
    z.s = z.s[2 .. $ - 2];
    z.s[$ - 2] = 'c';
    return z.s[$ - 2];
}
static assert(sliceDollar() == 'c');

/**************************************************
   Variation of 5972 which caused segfault
**************************************************/

int bug5972crash()
{
    char[] z = "abc".dup;
    char[][] a = [null, null];
    a[0] = z[0 .. 2];
    a[0][1] = 'q';
    return 56;
}
static assert(bug5972crash() == 56);

/**************************************************
   String slice assignment through ref parameter
**************************************************/

void popft(A)(ref A a)
{
    a = a[1 .. $];
}

int sdfgasf()
{
    auto scp = "abc".dup;
    popft(scp);
    return 1;
}
static assert(sdfgasf() == 1);

/**************************************************
   8830 slice of slice.ptr
**************************************************/

string bug8830(string s)
{
    auto ss = s[1 .. $];
    return ss.ptr[0 .. 2];
}
static assert(bug8830("hello") == "el");

/**************************************************
   8608 ICE
**************************************************/

void bug8608(ref int m) {}
void test8608()
{
    int z;
    int foo(bool b)
    {
        if (b)
            bug8608(z);
        return 1;
    }
    static assert( is(typeof(compiles!(foo(false)))));
    static assert(!is(typeof(compiles!(foo(true) ))));
}

/**************************************************
   Bug 7770
**************************************************/

immutable char[] foo7770 = "abcde";

int bug7770a(string a)
{
    return 1;
}

bool bug7770b(char c)
{
    return true;
}

static assert(bug7770a(foo7770[0 .. $]));
static assert(bug7770b(foo7770[$ - 2]));

void baz7770()
{
    static assert(bug7770a(foo7770[0 .. $]));
    static assert(bug7770b(foo7770[$ - 2]));
}

/**************************************************
   8601 ICE
**************************************************/

dchar bug8601(dstring s)
{
    dstring w = s[1 .. $];
    return w[0];
}

enum dstring e8601 = [cast(dchar)'o', 'n'];
static assert(bug8601(e8601) == 'n');

/**************************************************
   Bug 6015
**************************************************/

struct Foo6015
{
    string field;
}

bool func6015(string input)
{
    Foo6015 foo;
    foo.field = input[0 .. $];
    assert(foo.field == "test");
    foo.field = "test2";
    assert(foo.field != "test");
    assert(foo.field == "test2");
    return true;
}

static assert(func6015("test"));

/**************************************************
   Bug 6001
**************************************************/

void bug6001e(ref int[] s)
{
    int[] r = s;
    s ~= 0;
}
bool bug6001f()
{
    int[] s;
    bug6001e(s);
    return true;
}
static assert(bug6001f());

// Assignment to AAs

void blah(int[char] as)
{
    auto k = [6: as];
    as = k[6];
}
int blaz()
{
    int[char] q;
    blah(q);
    return 67;
}
static assert(blaz() == 67);

void bug6001g(ref int[] w)
{
    w = [88];
    bug6001e(w);
    w[0] = 23;
}

bool bug6001h()
{
    int[] s;
    bug6001g(s);
    assert(s.length == 2);
    assert(s[1] == 0);
    assert(s[0] == 23);
    return true;
}
static assert(bug6001h());

/**************************************************
   10243 wrong code *&arr as ref parameter
   10551 wrong code (&arr)[0] as ref parameter
**************************************************/

void bug10243(ref int n)
{
    n = 3;
}

void bug10551(int* p)
{
    bug10243(p[0]);
}

bool test10243()
{
    int[1] arr;
    bug10243(*arr.ptr);
    assert(arr[0] == 3);
    int[1] arr2;
    bug10551(arr2.ptr);
    assert(arr2[0] == 3);
    int v;
    bug10551(&v);
    assert(v == 3);
    return true;
}

static assert(test10243());

/**************************************************
   Bug 4910
**************************************************/

int bug4910(int a)
{
    return a;
}

static int var4910;
static assert(!is(typeof(Compiles!(bug4910(var4910)))));

static assert(bug4910(123));

/**************************************************
    Bug 5845 - Regression(2.041)
**************************************************/

void test5845(ulong cols) {}

uint solve(bool niv, ref ulong cols)
{
    if (niv)
        solve(false, cols);
    else
        test5845(cols);
    return 65;
}

ulong nqueen(int n)
{
    ulong cols = 0;
    return solve(true, cols);
}

static assert(nqueen(2) == 65);

/**************************************************
    Bug 5258
**************************************************/

struct Foo5258 { int x; }
void bar5258(int n, ref Foo5258 fong)
{
    if (n)
        bar5258(n - 1, fong);
    else
        fong.x++;
}
int bug5258()
{
    Foo5258 foo5258 = Foo5258();
    bar5258(1, foo5258);
    return 45;
}
static assert(bug5258() == 45);

struct Foo5258b { int[2] r; }
void baqopY(int n, ref int[2] fongo)
{
    if (n)
        baqopY(n - 1, fongo);
    else
        fongo[0]++;
}
int bug5258b()
{
    Foo5258b qq;
    baqopY(1, qq.r);
    return 618;
}
static assert(bug5258b() == 618);

// Notice that this case involving reassigning the dynamic array
struct Foo5258c { int[] r; }
void baqop(int n, ref int[] fongo)
{
    if (n)
        baqop(n - 1, fongo);
    else
    {
        fongo = new int[20];
        fongo[0]++;
    }
}
size_t bug5258c()
{
    Foo5258c qq;
    qq.r = new int[30];
    baqop(1, qq.r);
    return qq.r.length;
}
static assert(bug5258c() == 20);

/**************************************************
    Bug 6049
**************************************************/

struct Bug6049
{
    int m;
    this(int x) {  m = x; }
    invariant() { }
}

const Bug6049[] foo6049 = [Bug6049(6),  Bug6049(17)];

static assert(foo6049[0].m == 6);

/**************************************************
    Bug 6052
**************************************************/

struct Bug6052
{
    int a;
}

bool bug6052()
{
    Bug6052[2] arr;
    for (int i = 0; i < 2; ++ i)
    {
        Bug6052 el = {i};
        Bug6052 ek = el;
        arr[i] = el;
        el.a = i + 2;
        assert(ek.a == i);      // ok
        assert(arr[i].a == i);  // fail
    }
    assert(arr[1].a == 1);  // ok
    assert(arr[0].a == 0);  // fail
    return true;
}

static assert(bug6052());

bool bug6052b()
{
    int[][1] arr;
    int[1] z = [7];
    arr[0] = z;
    assert(arr[0][0] == 7);
    arr[0] = z;
    z[0] = 3;
    assert(arr[0][0] == 3);
    return true;
}

static assert(bug6052b());

struct Bug6052c
{
    int x;
    this(int a) { x = a; }
}

int bug6052c()
{
    Bug6052c[] pieces = [];
    for (int c = 0; c < 2; ++ c)
        pieces ~= Bug6052c(c);
    assert(pieces[1].x == 1);
    assert(pieces[0].x == 0);
    return 1;
}
static assert(bug6052c() == 1);
static assert(bug6052c() == 1);


static assert({
    Bug6052c[] pieces = [];
    pieces.length = 2;
    int c = 0;
    pieces[0] = Bug6052c(c);
    ++c;
    pieces[1] = Bug6052c(c);
    assert(pieces[0].x == 0);
    return true;
}());

static assert({
    int[1][] pieces = [];
    pieces.length = 2;
    for (int c = 0; c < 2; ++ c)
        pieces[c][0] = c;
    assert(pieces[1][0] == 1);
    assert(pieces[0][0] == 0);
    return true;
}());

static assert({
    Bug6052c[] pieces = [];
    for (int c = 0; c < 2; ++ c)
        pieces ~= Bug6052c(c);
    assert(pieces[1].x == 1);
    assert(pieces[0].x == 0);
    return true;
}());

static assert({
    int[1] z = 7;
    int[1][] pieces = [z,z];
    pieces[1][0]=3;
    assert(pieces[0][0] == 7);
    pieces = pieces ~ [z,z];
    pieces[3][0] = 16;
    assert(pieces[2][0] == 7);
    pieces = [z,z] ~ pieces;
    pieces[5][0] = 16;
    assert(pieces[4][0] == 7);
    return true;
}());

/**************************************************
    Bug 6749
**************************************************/

struct CtState
{
    string code;
}

CtState bug6749()
{
    CtState[] pieces;
    CtState r = CtState("correct");
    pieces ~= r;
    r = CtState("clobbered");
    return pieces[0];
}
static assert(bug6749().code == "correct");

/**************************************************
    Index + slice assign to function returns
**************************************************/

int[] funcRetArr(int[] a)
{
    return a;
}

int testFuncRetAssign()
{
    int[] x = new int[20];
    funcRetArr(x)[2] = 4;
    assert(x[2] == 4);
    funcRetArr(x)[] = 27;
    assert(x[15] == 27);
    return 5;
}
static assert(testFuncRetAssign() == 5);

int keyAssign()
{
    int[int] pieces;
    pieces[3] = 1;
    pieces.keys[0] = 4;
    pieces.values[0] = 27;
    assert(pieces[3] == 1);
    return 5;
}
static assert(keyAssign() == 5);

/**************************************************
    Bug 6054 -- AA literals
**************************************************/

enum x6054 = {
    auto p = {
        int[string] pieces;
        pieces[['a'].idup] = 1;
        return pieces;
    }();
    return p;
}();

/**************************************************
    Bug 6077
**************************************************/

enum bug6077 = {
    string s;
    string t;
    return s ~ t;
}();

/**************************************************
    Bug 6078 -- Pass null array by ref
**************************************************/

struct Foo6078
{
    int[] bar;
}

static assert({
    Foo6078 f;
    int i;
    foreach (ref e; f.bar)
    {
        i += e;
    }
    return i;
}() == 0);

int bug6078(ref int[] z)
{
    int[] q = z;
    return 2;
}

static assert({
    Foo6078 f;
    return bug6078(f.bar);
}() == 2);

/**************************************************
    Bug 6079 -- Array bounds checking
**************************************************/

static assert(!is(typeof(compiles!({
    int[] x = [1, 2, 3, 4];
    x[4] = 1;
    return true;
}()
))));

/**************************************************
    Bug 6100
**************************************************/

struct S6100
{
    int a;
}

S6100 init6100(int x)
{
    S6100 s = S6100(x);
    return s;
}

static const S6100[2] s6100a = [init6100(1), init6100(2)];
static assert(s6100a[0].a == 1);

/**************************************************
    Bug 4825 -- failed with -inline
**************************************************/

int a4825()
{
    int r;
    return r;
}

int b4825()
{
    return a4825();
}

void c4825()
{
    void d()
    {
        auto e = b4825();
    }
    static const int f = b4825();
}

/**************************************************
    Bug 5708 -- failed with -inline
**************************************************/

string b5708(string s) { return s; }
string a5708(string s) { return b5708(s); }

void bug5708()
{
    void m() { a5708("lit"); }
    static assert(a5708("foo") == "foo");
    static assert(a5708("bar") == "bar");
}

/**************************************************
    Bug 6120 -- failed with -inline
**************************************************/

struct Bug6120(T)
{
    this(int x) { }
}
static assert({
    auto s = Bug6120!int(0);
    return true;
}());

/**************************************************
    Bug 6123 -- failed with -inline
**************************************************/

struct Bug6123(T)
{
    void f() {}
    // can also trigger if the struct is normal but f is template
}
static assert({
    auto piece = Bug6123!int();
    piece.f();
    return true;
}());

/**************************************************
    Bug 6053 -- ICE involving pointers
**************************************************/

static assert({
    int* a = null;
    assert(a is null);
    assert(a == null);
    return true;
}());

static assert({
    int b;
    int* a = &b;
    assert(a !is null);
    *a = 7;
    assert(b == 7);
    assert(*a == 7);
    return true;
}());

int dontbreak6053()
{
    auto q = &dontbreak6053;
    void caz() {}
    auto tr = &caz;
    return 5;
}
static assert(dontbreak6053());

static assert({
    int a;
    *(&a) = 15;
    assert(a == 15);
    assert(*(&a) == 15);
    return true;
}());

static assert({
    int a = 5, b = 6, c = 2;
    assert(*(c ? &a : &b) == 5);
    assert(*(!c ? &a : &b) == 6);
    return true;
}());

static assert({
    int a, b, c;
    (c ? a : b) = 1;
    return true;
}());

static assert({
    int a, b, c = 1;
    int* p = &a;
    (c ? *p : b) = 51;
    assert(a == 51);
    return true;
}());

/**************************************************
  Pointer arithmetic, dereference, and comparison
**************************************************/

// dereference null pointer
static assert(!is(typeof(compiles!({
    int a, b, c = 1;
    int* p;
    (c ? *p : b) = 51;
    return 6;
}()
))));
static assert(!is(typeof(compiles!({
    int* a = null;
    assert(*a != 6);
    return 72;
}()
))));

// cannot <, > compare pointers to different arrays
static assert(!is(typeof(compiles!({
    int[5] a, b;
    bool c = (&a[0] > &b[0]);
    return 72;
}()
))));

// can ==, is, !is, != compare pointers for different arrays
static assert({
    int[5] a;
    int[5] b;
    assert(!(&a[0] == &b[0]));
    assert(&a[0] != &b[0]);
    assert(!(&a[0] is &b[0]));
    assert(&a[0] !is &b[0]);
    return 72;
}());

static assert({
    int[5] a;
    a[0] = 25;
    a[1] = 5;
    int* b = &a[1];
    assert(*b == 5);
    *b = 34;
    int c = *b;
    *b += 6;
    assert(b == &a[1]);
    assert(b != &a[0]);
    assert(&a[0] < &a[1]);
    assert(&a[0] <= &a[1]);
    assert(!(&a[0] >= &a[1]));
    assert(&a[4] > &a[0]);
    assert(c == 34);
    assert(*b == 40);
    assert(a[1] == 40);
    return true;
}());

static assert({
    int[12] x;
    int* p = &x[10];
    int* q = &x[4];
    return p - q;
}() == 6);

static assert({
    int[12] x;
    int* p = &x[10];
    int* q = &x[4];
    q = p;
    assert(p == q);
    q = &x[4];
    assert(p != q);
    q = q + 6;
    assert(q is p);
    return 6;
}() == 6);

static assert({
    int[12] x;
    int[] y = x[2 .. 8];
    int* p = &y[4];
    int* q = &x[6];
    assert(p == q);
    p = &y[5];
    assert(p > q);
    p = p + 5; // OK, as long as we don't dereference
    assert(p > q);
    return 6;
}() == 6);

static assert({
    char[12] x;
    const(char)* p = "abcdef";
    const (char)* q = p;
    q = q + 2;
    assert(*q == 'c');
    assert(q > p);
    assert(q - p == 2);
    assert(p - q == -2);
    q = &x[7];
    p = &x[1];
    assert(q>p);
    return 6;
}() == 6);

// Relations involving null pointers
bool nullptrcmp()
{
    // null tests
    void* null1 = null, null2 = null;
    int x = 2;
    void* p = &x;
    assert(null1 == null2);
    assert(null1 is null2);
    assert(null1 <= null2);
    assert(null1 >= null2);
    assert(!(null1 > null2));
    assert(!(null2 > null1));
    assert(null1 != p);
    assert(null1 !is p);
    assert(p != null1);
    assert(p !is null1);
    assert(null1 <= p);
    assert(p >= null2);
    assert(p > null1);
    assert(!(null1 > p));
    return true;
}
static assert(nullptrcmp());

/**************************************************
 10840 null pointer in dotvar
**************************************************/

struct Data10840
{
    bool xxx;
}

struct Bug10840
{
    Data10840* _data;
}

bool bug10840(int n)
{
    Bug10840 stack;
    if (n == 1)
    {
        // detect deref through null pointer
        return stack._data.xxx;
    }
    // Wrong-code for ?:
    return stack._data ? false : true;
}

static assert(bug10840(0));
static assert(!is(typeof(Compileable!(bug10840(1)))));

/**************************************************
  8216 ptr inside a pointer range
**************************************************/

// Four-pointer relations. Return true if [p1 .. p2] points inside [q1 .. q2]
// (where the end points don't coincide).
bool ptr4cmp(void* p1, void* p2, void* q1, void* q2)
{
// Each compare can be written with <, <=, >, or >=.
// Either && or || can be used, giving 32 permutations.
// Additionally each compare can be negated with !, yielding 128 in total.
    bool b1 = (p1 > q1 && p2 <= q2);
    bool b2 = (p1 > q1 && p2 < q2);
    bool b3 = (p1 >= q1 && p2 <= q2);
    bool b4 = (p1 >= q1 && p2 < q2);

    bool b5 = (q1 <= p1 && q2 > p2);
    bool b6 = (q1 <= p1 && q2 >= p2);
    bool b7 = (p2 <= q2 && p1 > q1);
    bool b8 = (!(p1 <= q1) && p2 <= q2);
    bool b9 = (!(p1 <= q1) && !(p2 > q2));
    bool b10 = (!!!(p1 <= q1) && !(p2 > q2));

    assert(b1 == b2 && b1 == b3 && b1 == b4 && b1 == b5 && b1 == b6);
    assert(b1 == b7 && b1 == b8 && b1 == b9 && b1 == b10);

    bool c1 = (p1 <= q1 || p2 > q2);
    assert(c1 == !b1);
    bool c2 = (p1 < q1 || p2 >= q2);
    bool c3 = (!(q1 <= p1) || !(q2 >= p2));
    assert(c1 == c2 && c1 == c3);
    return b1;
}

bool bug8216()
{
    int[4] a;
    int[13] b;
    int v;
    int* p = &v;
    assert(!ptr4cmp(&a[0], &a[3], p, p));
    assert(!ptr4cmp(&b[2], &b[9], &a[1], &a[2]));
    assert(!ptr4cmp(&b[1], &b[9], &b[2], &b[8]));
    assert( ptr4cmp(&b[2], &b[8], &b[1], &b[9]));
    return 1;
}
static assert(bug8216());

/**************************************************
  6517 ptr++, ptr--
**************************************************/

int bug6517()
{
    int[] arr = [1, 2, 3];
    auto startp = arr.ptr;
    auto endp = arr.ptr + arr.length;

    for (; startp < endp; startp++) {}
    startp = arr.ptr;
    assert(startp++ == arr.ptr);
    assert(startp != arr.ptr);
    assert(startp-- != arr.ptr);
    assert(startp == arr.ptr);

    return 84;
}
static assert(bug6517() == 84);

/**************************************************
  Out-of-bounds pointer assignment and deference
**************************************************/

int ptrDeref(int ofs, bool wantDeref)
{
    int[5] a;
    int* b = &a[0];
    b = b + ofs; // OK
    if (wantDeref)
        return *b; // out of bounds
    return 72;
}

static assert(!is(typeof(compiles!(ptrDeref(-1, true)))));
static assert( is(typeof(compiles!(ptrDeref(4, true)))));
static assert( is(typeof(compiles!(ptrDeref(5, false)))));
static assert(!is(typeof(compiles!(ptrDeref(5, true)))));
static assert(!is(typeof(compiles!(ptrDeref(6, false)))));
static assert(!is(typeof(compiles!(ptrDeref(6, true)))));

/**************************************************
  Pointer +=
**************************************************/
static assert({
    int[12] x;
    int zzz;
    assert(&zzz);
    int* p = &x[10];
    int* q = &x[4];
    q = p;
    assert(p == q);
    q = &x[4];
    assert(p != q);
    q += 4;
    assert(q == &x[8]);
    q = q - 2;
    q = q + 4;
    assert(q is p);
    return 6;
}() == 6);

/**************************************************
  Reduced version of bug 5615
**************************************************/

const(char)[] passthrough(const(char)[] x)
{
    return x;
}

sizediff_t checkPass(Char1)(const(Char1)[] s)
{
    const(Char1)[] balance = s[1 .. $];
    return passthrough(balance).ptr - s.ptr;
}
static assert(checkPass("foobar") == 1);

/**************************************************
  Pointers must not escape from CTFE
**************************************************/

struct Toq
{
    const(char)* m;
}

Toq ptrRet(bool b)
{
    string x = "abc";
    return Toq(b ? x[0 .. 1].ptr : null);
}

static assert(is(typeof(compiles!({
    enum Toq boz = ptrRet(false); // OK - ptr is null
    Toq z = ptrRet(true); // OK -- ptr doesn't escape
    return 4;
}()
))));

static assert(!is(typeof(compiles!({
    enum Toq boz = ptrRet(true); // fail - ptr escapes
    return 4;
}()
))));

/**************************************************
    Pointers to struct members
**************************************************/

struct Qoz
{
    int w;
    int[3] yof;
}

static assert({
    int[3] gaz;
    gaz[2] = 3156;
    Toq z = ptrRet(true);
    auto p = z.m;
    assert(*z.m == 'a');
    assert(*p == 'a');
    auto q = &z.m;
    assert(*q == p);
    assert(**q == 'a');
    Qoz g = Qoz(2, [5, 6, 7]);
    auto r = &g.w;
    assert(*r == 2);
    r = &g.yof[1];
    assert(*r == 6);
    g.yof[0] = 15;
    ++r;
    assert(*r == 7);
    r -= 2;
    assert(*r == 15);
    r = &gaz[0];
    r += 2;
    assert(*r == 3156);
    return *p;
}() == 'a');

struct AList
{
    AList* next;
    int value;
    static AList* newList()
    {
        AList[] z = new AList[1];
        return &z[0];
    }
    static AList* make(int i, int j)
    {
        auto r = newList();
        r.next = (new AList[1]).ptr;
        r.value = 1;
        AList* z = r.next;
        (*z).value = 2;
        r.next.value = j;
        assert(r.value == 1);
        assert(r.next.value == 2);
        r.next.next = &(new AList[1])[0];
        assert(r.next.next != null);
        assert(r.next.next);
        r.next.next.value = 3;
        assert(r.next.next.value == 3);
        r.next.next = newList();
        r.next.next.value = 9;
        return r;
    }
    static int checkList()
    {
        auto r = make(1,2);
        assert(r.value == 1);
        assert(r.next.value == 2);
        assert(r.next.next.value == 9);
        return 2;
    }
}

static assert(AList.checkList() == 2);

/**************************************************
    7194 pointers as struct members
**************************************************/

struct S7194 { int* p, p2; }

int f7194()
{
    assert(S7194().p == null);
    assert(!S7194().p);
    assert(S7194().p == S7194().p2);
    S7194 s = S7194();
    assert(!s.p);
    assert(s.p == null);
    assert(s.p == s.p2);
    int x;
    s.p = &x;
    s.p2 = s.p;
    assert(s.p == &x);
    return 0;
}

int g7194()
{
    auto s = S7194();
    assert(s.p);  // should fail
    return 0;
}

static assert(f7194() == 0);
static assert(!is(typeof(compiles!(g7194()))));

/**************************************************
    7248 recursive struct pointers in array
**************************************************/

struct S7248 { S7248* ptr; }

bool bug7248()
{
    S7248[2] sarr;
    sarr[0].ptr = &sarr[1];
    sarr[0].ptr = null;
    S7248* t = sarr[0].ptr;
    return true;
}
static assert(bug7248());

/**************************************************
    7216 calling a struct pointer member
**************************************************/

struct S7216
{
    S7216* p;
    int t;

    void f() { }
    void g() { ++t; }
}

bool bug7216()
{
    S7216 s0, s1;
    s1.t = 6;
    s0.p = &s1;
    s0.p.f();
    s0.p.g();
    assert(s1.t == 7);
    return true;
}

static assert(bug7216());

/**************************************************
    10858 Wrong code with array of pointers
**************************************************/

bool bug10858()
{
    int*[4] x;
    x[0] = null;
    assert(x[0] == null);
    return true;
}
static assert(bug10858());

/**************************************************
    12528 - painting inout type for value type literals
**************************************************/

inout(T)[] dup12528(T)(inout(T)[] a)
{
    inout(T)[] res;
    foreach (ref e; a)
        res ~= e;
    return res;
}

enum arr12528V1 = dup12528([0]);
enum arr12528V2 = dup12528([0, 1]);
static assert(arr12528V1 == [0]);
static assert(arr12528V2 == [0, 1]);

/**************************************************
    9745 Allow pointers to static variables
**************************************************/

shared int x9745;
shared int[5] y9745;

shared(int)* bug9745(int m)
{
    auto k = &x9745;
    auto j = &x9745;
    auto p = &y9745[0];
    auto q = &y9745[3];
    assert(j - k == 0);
    assert(j == k);
    assert(q - p == 3);
    --q;
    int a = 0;
    assert(p + 2 == q);
    if (m == 7)
    {
        auto z1 = y9745[0 .. 2]; // slice global pointer
    }
    if (m == 8)
        p[1] = 7; // modify through a pointer
    if (m == 9)
         a = p[1]; // read from a pointer
    if (m == 0)
        return &x9745;
    return &y9745[1];
}

int test9745(int m)
{
    bug9745(m);
    // type painting
    shared int* w = bug9745(0);
    return 1;
}

shared int* w9745a = bug9745(0);
shared int* w9745b = bug9745(1);
static assert( is(typeof(compiles!(test9745(6)))));
static assert(!is(typeof(compiles!(test9745(7)))));
static assert(!is(typeof(compiles!(test9745(8)))));
static assert(!is(typeof(compiles!(test9745(9)))));

// pointers cast from an absolute address
// (mostly applies to fake pointers, eg Windows HANDLES)
bool test9745b()
{
    void* b6 = cast(void*)0xFEFEFEFE;
    void* b7 = cast(void*)0xFEFEFEFF;
    assert(b6 is b6);
    assert(b7 != b6);
    return true;
}
static assert(test9745b());

/**************************************************
    9364 ICE with pointer to local struct
**************************************************/

struct S9364
{
    int i;
}

bool bug9364()
{
    S9364 s;
    auto k = (&s).i;
    return 1;
}

static assert(bug9364());

/**************************************************
    10251 Pointers to const globals
**************************************************/

static const int glob10251 = 7;

const(int)* bug10251()
{
   return &glob10251;
}

static a10251 = &glob10251; //  OK
static b10251 = bug10251();

/**************************************************
    4065 [CTFE] AA "in" operator doesn't work
**************************************************/

bool bug4065(string s)
{
    enum int[string] aa = ["aa":14, "bb":2];
    int* p = s in aa;
    if (s == "aa")
        assert(*p == 14);
    else if (s == "bb")
        assert(*p == 2);
    else
        assert(!p);
    int[string] zz;
    assert(!("xx" in zz));
    bool c = !p;
    return cast(bool)(s in aa);
}

static assert(!bug4065("xx"));
static assert( bug4065("aa"));
static assert( bug4065("bb"));

/**************************************************
    12689 - assigning via pointer from 'in' expression
**************************************************/

int g12689()
{
    int[int] aa;
    aa[1] = 13;
    assert(*(1 in aa) == 13);
    *(1 in aa) = 42;
    return aa[1];
}
static assert(g12689() == 42);

/**************************************************
    Pointers in ? :
**************************************************/

static assert({
    int[2] x;
    int* p = &x[1];
    return p ? true: false;
}());

/**************************************************
    Pointer slicing
**************************************************/

int ptrSlice()
{
    auto arr = new int[5];
    int* x = &arr[0];
    int[] y = x[0 .. 5];
    x[1 .. 3] = 6;
    ++x;
    x[1 .. 3] = 14;
    assert(arr[1] == 6);
    assert(arr[2] == 14);
    //x[-1 .. 4] = 5;   // problematic because negative lower boundary will throw RangeError in runtime
    (x - 1)[0 .. 3] = 5;
    int[] z = arr[1 .. 2];
    z.length = 4;
    z[$ - 1] = 17;
    assert(arr.length == 5);
    return 2;
}
static assert(ptrSlice() == 2);

/**************************************************
    6344 - create empty slice from null pointer
**************************************************/

static assert({
    char* c = null;
    auto m = c[0 .. 0];
    return true;
}());

/**************************************************
    8365 - block assignment of enum arrays
**************************************************/

enum E8365 { first = 7, second, third, fourth }
static assert({ E8365[2]       x; return x[0];       }() == E8365.first);
static assert({ E8365[2][2]    x; return x[0][0];    }() == E8365.first);
static assert({ E8365[2][2][2] x; return x[0][0][0]; }() == E8365.first);

/**************************************************
    4448 - labelled break + continue
**************************************************/

int bug4448()
{
    int n = 2;
L1:
    do
    {
        switch(n)
        {
        case 5:
            return 7;
        default:
            n = 5;
            break L1;
        }
        int w = 7;
    } while (0);
    return 3;
}
static assert(bug4448() == 3);

int bug4448b()
{
    int n = 2;
L1:
    for (n = 2; n < 5; ++n)
    {
        for (int m = 1; m < 6; ++m)
        {
            if (n < 3)
            {
                assert(m == 1);
                continue L1;
            }
        }
        break;
    }
    return 3;
}
static assert(bug4448b() == 3);

/**************************************************
    6985 - non-constant case
**************************************************/

int bug6985(int z)
{
    int q = z * 2 - 6;
    switch(z)
    {
    case q:
        q = 87;
        break;
    default:
    }
    return q;
}
static assert(bug6985(6) == 87);

/**************************************************
    6281 - [CTFE] A null pointer '!is null' returns 'true'
**************************************************/

static assert(!{
    auto p = null;
    return p !is null;
}());

static assert(!{
    auto p = null;
    return p != null;
}());

/**************************************************
    6331 - evaluate SliceExp on if condition
**************************************************/

bool bug6331(string s)
{
    if (s[0 .. 1])
        return true;
    return false;
}
static assert(bug6331("str"));

/**************************************************
    6283 - assign to AA with slice as index
**************************************************/

static assert({
    immutable p = "pp";
    int[string] pieces = [p: 0];
    pieces["qq"] = 1;
    return true;
}());

static assert({
    immutable renames = [0: "pp"];
    int[string] pieces;
    pieces[true ? renames[0] : "qq"] = 1;
    pieces["anything"] = 1;
    return true;
}());

static assert({
    immutable qq = "qq";
    string q = qq;
    int[string] pieces = ["a":1];
    pieces[q] = 0;
    string w = "ab";
    int z = pieces[w[0 .. 1]];
    assert(z == 1);
    return true;
}());

/**************************************************
    6282 - dereference 'in' of an AA
**************************************************/

static assert({
    int[] w = new int[4];
    w[2] = 6;
    auto c = [5: w];
    auto kk  = (*(5 in c))[2];
    (*(5 in c))[2] = 8;
    (*(5 in c))[1 .. $ - 2] = 4;
    auto a = [4:"1"];
    auto n = *(4 in a);
    return n;
}() == "1");

/**************************************************
    6337 - member function call on struct literal
**************************************************/

struct Bug6337
{
    int k;
    void six()
    {
        k = 6;
    }
    int ctfe()
    {
        six();
        return k;
    }
}
static assert(Bug6337().ctfe() == 6);

/**************************************************
    6603 call manifest function pointer
**************************************************/

int f6603(int a) { return a + 5; }
enum bug6603 = &f6603;
static assert(bug6603(6) == 11);

/**************************************************
    6375
**************************************************/

struct D6375
{
    int[] arr;
}
A6375 a6375(int[] array)
{
    return A6375(array);
}
struct A6375
{
    D6375* _data;
    this(int[] arr)
    {
        _data = new D6375;
        _data.arr = arr;
    }
    int[] data()
    {
        return _data.arr;
    }
}
static assert({
    int[] a = [1, 2];
    auto app2 = a6375(a);
    auto data = app2.data();
    return true;
}());

/**************************************************
    6280 Converting pointers to bool
**************************************************/

static assert({
    if ((0 in [0:0])) {}
    if ((0 in [0:0]) && (0 in [0:0])) {}
    return true;
}());

/**************************************************
    6276 ~=
**************************************************/

struct Bug6276
{
    int[] i;
}
static assert({
    Bug6276 foo;
    foo.i ~= 1;
    foo.i ~= 2;
    return true;
}());

/**************************************************
    6374   ptr[n] = x, x = ptr[n]
**************************************************/

static assert({
    int[] arr = [1];
    arr.ptr[0] = 2;
    auto k = arr.ptr[0];
    assert(k == 2);
    return arr[0];
}() == 2);

/**************************************************
    6306  recursion and local variables
**************************************************/

void recurse6306()
{
    bug6306(false);
}

bool bug6306(bool b)
{
    int x = 0;
    if (b)
        recurse6306();
    assert(x == 0);
    x = 1;
    return true;
}
static assert(bug6306(true));

/**************************************************
    6386  ICE on unsafe pointer cast
**************************************************/

static assert(!is(typeof(compiles!({
    int x = 123;
    int* p = &x;
    float z;
    float* q = cast(float*)p;
    return true;
}()
))));

static assert({
    int[] x = [123, 456];
    int* p = &x[0];
    auto m = cast(const(int)*)p;
    auto q = p;
    return *q;
}());

/**************************************************
    6420  ICE on dereference of invalid pointer
**************************************************/

static assert({
    // Should compile, but pointer can't be dereferenced
    int x = 123;
    int* p = cast(int*)x;
    auto q = cast(char*)x;
    auto r = cast(char*)323;
    // Valid const-changing cast
    const float *m = cast(immutable float*)[1.2f,2.4f,3f];
    return true;
}()
);

static assert(!is(typeof(compiles!({
    int x = 123;
    int* p = cast(int*)x;
    int a = *p;
    return true;
}()
))));

static assert(!is(typeof(compiles!({
    int* p = cast(int*)123;
    int a = *p;
    return true;
}()
))));

static assert(!is(typeof(compiles!({
    auto k = cast(int*)45;
    *k = 1;
    return true;
}()
))));

static assert(!is(typeof(compiles!({
    *cast(float*)"a" = 4.0;
    return true;
}()
))));

static assert(!is(typeof(compiles!({
    float f = 2.8;
    long *p = &f;
    return true;
}()
))));

static assert(!is(typeof(compiles!({
    long *p = cast(long*)[1.2f, 2.4f, 3f];
    return true;
}()
))));

/**************************************************
    6250  deref pointers to array
**************************************************/

int[]* simple6250(int[]* x) { return x; }

void swap6250(int[]* lhs, int[]* rhs)
{
    int[] kk = *lhs;
    assert(simple6250(lhs) == lhs);
    lhs = simple6250(lhs);
    assert(kk[0] == 18);
    assert((*lhs)[0] == 18);
    assert((*rhs)[0] == 19);
    *lhs = *rhs;
    assert((*lhs)[0] == 19);
    *rhs = kk;
    assert(*rhs == kk);
    assert(kk[0] == 18);
    assert((*rhs)[0] == 18);
}

int ctfeSort6250()
{
     int[][2] x;
     int[3] a = [17, 18, 19];
     x[0] = a[1 .. 2];
     x[1] = a[2 .. $];
     assert(x[0][0] == 18);
     assert(x[0][1] == 19);
     swap6250(&x[0], &x[1]);
     assert(x[0][0] == 19);
     assert(x[1][0] == 18);
     a[1] = 57;
     assert(x[0][0] == 19);
     return x[1][0];
}

static assert(ctfeSort6250() == 57);

/**************************************************/

long[]* simple6250b(long[]* x) { return x; }

void swap6250b(long[]* lhs, long[]* rhs)
{
    long[] kk = *lhs;
    assert(simple6250b(lhs) == lhs);
    lhs = simple6250b(lhs);
    assert(kk[0] == 18);
    assert((*lhs)[0] == 18);
    assert((*rhs)[0] == 19);
    *lhs = *rhs;
    assert((*lhs)[0] == 19);
    *rhs = kk;
    assert(*rhs == kk);
    assert(kk[0] == 18);
    assert((*rhs)[0] == 18);
}

long ctfeSort6250b()
{
     long[][2] x;
     long[3] a = [17, 18, 19];
     x[0] = a[1 .. 2];
     x[1] = a[2 .. $];
     assert(x[0][0] == 18);
     assert(x[0][1] == 19);
     swap6250b(&x[0], &x[1]);
     assert(x[0][0] == 19);
     assert(x[1][0] == 18);
     a[1] = 57;
     assert(x[0][0] == 19);
     return x[1][0];
}

static assert(ctfeSort6250b() == 57);

/**************************************************
    6672 circular references in array
**************************************************/

void bug6672(ref string lhs, ref string rhs)
{
    auto tmp = lhs;
    lhs = rhs;
    rhs = tmp;
}

static assert({
    auto kw = ["a"];
    bug6672(kw[0], kw[0]);
    return true;
}());

void slice6672(ref string[2] agg, ref string lhs)
{
    agg[0 .. $] = lhs;
}

static assert({
    string[2] kw = ["a", "b"];
    slice6672(kw, kw[0]);
    assert(kw[0] == "a");
    assert(kw[1] == "a");
    return true;
}());

// an unrelated rejects-valid bug
static assert({
    string[2] kw = ["a", "b"];
    kw[0 .. 2] = "x";
    return true;
}());

void bug6672b(ref string lhs, ref string rhs)
{
    auto tmp = lhs;
    assert(tmp == "a");
    lhs = rhs;
    assert(tmp == "a");
    rhs = tmp;
}

static assert({
    auto kw=["a", "b"];
    bug6672b(kw[0], kw[1]);
    assert(kw[0] == "b");
    assert(kw[1] == "a");
    return true;
}());

/**************************************************
    6399 (*p).length = n
**************************************************/

struct A6399
{
    int[] arr;
    int subLen()
    {
        arr = [1, 2, 3, 4, 5];
        arr.length -= 1;
        return cast(int)arr.length;
    }
}

static assert({
    A6399 a;
    return a.subLen();
}() == 4);

/**************************************************
    7789 (*p).length++ where *p is null
**************************************************/

struct S7789
{
    size_t foo()
    {
        _ary.length += 1;
        return _ary.length;
    }

    int[] _ary;
}

static assert(S7789().foo());

/**************************************************
    6418 member named 'length'
**************************************************/

struct Bug6418
{
    size_t length() { return 189; }
}
static assert(Bug6418.init.length == 189);

/**************************************************
    4021 rehash
**************************************************/

bool bug4021()
{
    int[int] aa = [1: 1];
    aa.rehash;
    return true;
}
static assert(bug4021());

/**************************************************
    11629 crash on AA.rehash
**************************************************/

struct Base11629
{
    alias T = ubyte, Char = char;
    alias String = immutable(Char)[];

    const Char[T] toChar;

    this(int _dummy)
    {
        Char[T] toCharTmp = [0:'A'];

        toChar = toCharTmp.rehash;
    }
}
enum ct11629 = Base11629(4);

/**************************************************
    3512 foreach (dchar; string)
    6558 foreach (int, dchar; string)
**************************************************/

bool test3512()
{
    string s = "öhai";
    int q = 0;

    foreach (wchar c; s)
    {
        if (q == 2)
            assert(c == 'a');
        ++q;
    }
    assert(q == 4);

    // _aApplycd1
    foreach (dchar c; s)
    {
        ++q;
        if (c == 'h')
            break;
    }
    assert(q == 6);

    // _aApplycw2
    foreach (ptrdiff_t i, wchar c; s)
    {
        assert(i >= 0 && i < s.length);
    }

    // _aApplycd2
    foreach (ptrdiff_t i, dchar c; s)
    {
        assert(i >= 0 && i < s.length);
    }

    wstring w = "xüm";

    // _aApplywc1
    foreach (char c; w)
    {
        ++q;
    }
    assert(q == 10);

    // _aApplywd1
    foreach (dchar c; w)
    {
        ++q;
    }
    assert(q == 13);

    // _aApplywc2
    foreach (ptrdiff_t i, char c; w)
    {
        assert(i >= 0 && i < w.length);
    }

    // _aApplywd2
    foreach (ptrdiff_t i, dchar c; w)
    {
        assert(i >= 0 && i < w.length);
    }

    dstring d = "yäq";

    // _aApplydc1
    q = 0;
    foreach (char c; d)
    {
        ++q;
    }
    assert(q == 4);

    // _aApplydw1
    q = 0;
    foreach (wchar c; d)
    {
        ++q;
    }
    assert(q == 3);

    // _aApplydc2
    foreach (ptrdiff_t i, char c; d)
    {
        assert(i >= 0 && i < d.length);
    }
    // _aApplydw2
    foreach (ptrdiff_t i, wchar c; d)
    {
        assert(i >= 0 && i < d.length);
    }

    dchar[] dr = "squop"d.dup;

    foreach (ptrdiff_t n, char c; dr)
    {
        if (n == 2)
            break;
        assert(c != 'o');
    }

    // _aApplyRdc1
    foreach_reverse (char c; dr)
    {}

    // _aApplyRdw1
    foreach_reverse (wchar c; dr)
    {}

    // _aApplyRdc2
    foreach_reverse (ptrdiff_t n, char c; dr)
    {
        if (n == 4)
            break;
        assert(c != 'o');
    }

    // _aApplyRdw2
    foreach_reverse (ptrdiff_t i, wchar c; dr)
    {
        assert(i >= 0 && i < dr.length);
    }

    q = 0;
    wstring w2 = ['x', 'ü', 'm']; // foreach over array literals
    foreach_reverse (ptrdiff_t n, char c; w2)
    {
        ++q;
        if (c == 'm') assert(n == 2 && q == 1);
        if (c == 'x') assert(n == 0 && q == 4);
    }
    return true;
}
static assert(test3512());

/**************************************************
    6510 ICE only with -inline
**************************************************/

struct Stack6510
{
    struct Proxy
    {
        void shrink() {}
    }
    Proxy stack;
    void pop()
    {
        stack.shrink();
    }
}

int bug6510()
{
    static int used()
    {
        Stack6510 junk;
        junk.pop();
        return 3;
    }
    return used();
}

void test6510()
{
    static assert(bug6510() == 3);
}

/**************************************************
    6511   arr[] shouldn't make a copy
**************************************************/

T bug6511(T)()
{
    T[1] a = [1];
    a[] += a[];
    return a[0];
}
static assert(bug6511!ulong() == 2);
static assert(bug6511!long() == 2);

/**************************************************
    6512   new T[][]
**************************************************/

bool bug6512(int m)
{
    auto x = new int[2][][](m, 5);
    assert(x.length == m);
    assert(x[0].length == 5);
    assert(x[0][0].length == 2);
    foreach (i; 0.. m)
        foreach (j; 0 .. 5)
            foreach (k; 0 .. 2)
                x[i][j][k] = k + j*10 + i*100;
    foreach (i; 0.. m)
        foreach (j; 0 .. 5)
            foreach (k; 0 .. 2)
                assert(x[i][j][k] == k + j*10 + i*100);
    return true;
}
static assert(bug6512(3));

/**************************************************
    6516   ICE(constfold.c)
**************************************************/

dstring bug6516()
{
    return cast(dstring)new dchar[](0);
}

static assert(bug6516() == ""d);

/**************************************************
    6727   ICE(interpret.c)
**************************************************/

const(char)* ice6727(const(char)* z) { return z; }
static assert({
    auto q = ice6727("a".dup.ptr);
    return true;
}());

/**************************************************
    6721   Cannot get pointer to start of char[]
**************************************************/

static assert({
    char[] c1 = "".dup;
    auto p = c1.ptr;
    string c2 = "";
    auto p2 = c2.ptr;
    return 6;
}() == 6);

/**************************************************
    6693   Assign to null AA
**************************************************/

struct S6693
{
    int[int] m;
}

static assert({
    int[int][int] aaa;
    aaa[3][1] = 4;
    int[int][3] aab;
    aab[2][1] = 4;
    S6693 s;
    s.m[2] = 4;
    return 6693;
}() == 6693);

/**************************************************
    7602   Segfault AA.keys on null AA
**************************************************/

string[] test7602()
{
    int[string] array;
    return array.keys;
}

enum bug7602 = test7602();

/**************************************************
    6739   Nested AA assignment
**************************************************/

static assert({
    int[int][int][int] aaa;
    aaa[3][1][6] = 14;
    return aaa[3][1][6];
}() == 14);

static assert({
    int[int][int] aaa;
    aaa[3][1] = 4;
    aaa[3][3] = 3;
    aaa[1][5] = 9;
    auto kk = aaa[1][5];
    return kk;
}() == 9);

/**************************************************
    6751   ref AA assignment
**************************************************/

void bug6751(ref int[int] aa)
{
    aa[1] = 2;
}

static assert({
    int[int] aa;
    bug6751(aa);
    assert(aa[1] == 2);
    return true;
}());

void bug6751b(ref int[int][int] aa)
{
    aa[1][17] = 2;
}

struct S6751
{
    int[int][int] aa;
    int[int] bb;
}

static assert({
    S6751 s;
    bug6751b(s.aa);
    assert(s.aa[1][17] == 2);
    return true;
}());

static assert({
    S6751 s;
    s.aa[7][56] = 57;
    bug6751b(s.aa);
    assert(s.aa[1][17] == 2);
    assert(s.aa[7][56] == 57);
    bug6751c(s.aa);
    assert(s.aa.keys.length == 1);
    assert(s.aa.values.length == 1);
    return true;
}());

static assert({
    S6751 s;
    s.bb[19] = 97;
    bug6751(s.bb);
    assert(s.bb[1] == 2);
    assert(s.bb[19] == 97);
    return true;
}());

void bug6751c(ref int[int][int] aa)
{
    aa = [38: [56 : 77]];
}

/**************************************************
   7790   AA foreach ref
**************************************************/

struct S7790
{
    size_t id;
}

size_t bug7790(S7790[string] tree)
{
    foreach (k, ref v; tree)
        v.id = 1;
    return tree["a"].id;
}

static assert(bug7790(["a":S7790(0)]) == 1);

/**************************************************
    6765   null AA.length
**************************************************/

static assert({
    int[int] w;
    return w.length;
}() == 0);

/**************************************************
    6769   AA.keys, AA.values with -inline
**************************************************/

static assert({
    double[char[3]] w = ["abc" : 2.3];
    double[] z = w.values;
    return w.keys.length;
}() == 1);

/**************************************************
    4022   AA.get
**************************************************/

static assert({
    int[int] aa = [58: 13];
    int r = aa.get(58, 1000);
    assert(r == 13);
    r = aa.get(59, 1000);
    return r;
}() == 1000);

/**************************************************
    6775 AA.opApply
**************************************************/

static assert({
    int[int] aa = [58: 17, 45:6];
    int valsum = 0;
    int keysum = 0;
    foreach (m; aa)  // aaApply
    {
        valsum += m;
    }
    assert(valsum == 17 + 6);
    valsum = 0;
    foreach (n, m; aa)  // aaApply2
    {
        valsum += m;
        keysum += n;
    }
    assert(valsum == 17 + 6);
    assert(keysum == 58 + 45);
    // Check empty AA
    valsum = 0;
    int[int] bb;
    foreach (m; bb)
    {
        ++valsum;
    }
    assert(valsum == 0);
    return true;
}());

/**************************************************
    7890   segfault struct with AA field
**************************************************/

struct S7890
{
    int[int] tab;
}

S7890 bug7890()
{
    S7890 foo;
    foo.tab[0] = 0;
    return foo;
}

enum e7890 = bug7890();

/**************************************************
    AA.remove
**************************************************/

static assert({
    int[int] aa = [58: 17, 45:6];
    aa.remove(45);
    assert(aa.length == 1);
    aa.remove(7);
    assert(aa.length == 1);
    aa.remove(58);
    assert(aa.length == 0);
    return true;
}());

/**************************************************
    try, finally
**************************************************/

static assert({
    int n = 0;

    try
    {
        n = 1;
    }
    catch (Exception e)
    {}
    assert(n == 1);

    try
    {
        n = 2;
    }
    catch (Exception e)
    {}
    finally
    {
        assert(n == 2);
        n = 3;
    }
    assert(n == 3);
    return true;
}());

/**************************************************
    6800 bad pointer casts
**************************************************/

bool badpointer(int k)
{
    int m = 6;
    int* w =  &m;
    assert(*w == 6);
    int[3] a = [17, 2, 21];
    int* w2 = &a[2];
    assert(*w2 == 21);

    // cast int* to uint* is OK
    uint* u1 = cast(uint*)w;
    assert(*u1 == 6);
    uint* u2 = cast(uint*)w2;
    assert(*u2 == 21);
    uint* u3 = cast(uint*)&m;
    assert(*u3 == 6);
    // cast int* to void* is OK
    void* v1 = cast(void*)w;
    void* v3 = &m;
    void* v4 = &a[0];
    // cast from void* back to int* is OK
    int* t3 = cast(int*)v3;
    assert(*t3 == 6);
    int* t4 = cast(int*)v4;
    assert(*t4 == 17);
    // cast from void* to uint* is OK
    uint* t1 = cast(uint*)v1;
    assert(*t1 == 6);
    // and check that they're real pointers
    m = 18;
    assert(*t1 == 18);
    assert(*u3 == 18);

    int** p = &w;

    if (k == 1) // bad reinterpret
        double *d1 = cast(double*)w;
    if (k == 3) // bad reinterpret
        char* d3 = cast(char*)w2;
    if (k == 4) {
        void* q1 = cast(void*)p;    // OK-void is int*
        void* *q = cast(void**)p;   // OK-void is int
    }
    if (k == 5)
        void*** q = cast(void***)p;  // bad: too many *
    if (k == 6) // bad reinterpret through void*
        double* d1 = cast(double*)v1;
    if (k == 7)
        double* d7 = cast(double*)v4;
    if (k == 8)
        ++v4; // can't do pointer arithmetic on void*
    return true;
}
static assert(badpointer(4));
static assert(!is(typeof(compiles!(badpointer(1)))));
static assert( is(typeof(compiles!(badpointer(2)))));
static assert(!is(typeof(compiles!(badpointer(3)))));
static assert( is(typeof(compiles!(badpointer(4)))));
static assert(!is(typeof(compiles!(badpointer(5)))));
static assert(!is(typeof(compiles!(badpointer(6)))));
static assert(!is(typeof(compiles!(badpointer(7)))));
static assert(!is(typeof(compiles!(badpointer(8)))));

/**************************************************
    10211 Allow casts S**->D**, when S*->D* is OK
**************************************************/

int bug10211()
{
    int m = 7;
    int* x = &m;
    int** y = &x;
    assert(**y == 7);
    uint* p = cast(uint*)x;
    uint** q = cast(uint**)y;
    return 1;
}

static assert(bug10211());

/**************************************************
    10568 CTFE rejects function pointer safety casts
**************************************************/

@safe void safetyDance() {}

int isItSafeToDance()
{
    void function() @trusted yourfriends = &safetyDance;
    void function() @safe nofriendsOfMine = yourfriends;
    return 1;
}

static assert(isItSafeToDance());

/**************************************************
    12296 CTFE rejects const compatible AA pointer cast
**************************************************/

int test12296()
{
    immutable x = [5 : 4];
    auto aa = &x;
    const(int[int])* y = aa;
    return 1;
}
static assert(test12296());

/**************************************************
    9170 Allow reinterpret casts float<->int
**************************************************/

int f9170(float x)
{
    return *(cast(int*)&x);
}

float i9170(int x)
{
    return *(cast(float*)&x);
}

float u9170(uint x)
{
    return *(cast(float*)&x);
}

int f9170arr(float[] x)
{
    return *(cast(int*)&(x[1]));
}

long d9170(double x)
{
    return *(cast(long*)&x);
}

int fref9170(ref float x)
{
    return *(cast(int*)&x);
}

long dref9170(ref double x)
{
    return *(cast(long*)&x);
}

bool bug9170()
{
    float f = 1.25;
    double d = 1.25;
    assert(f9170(f) == 0x3FA0_0000);
    assert(fref9170(f) == 0x3FA0_0000);
    assert(d9170(d) == 0x3FF4_0000_0000_0000L);
    assert(dref9170(d) == 0x3FF4_0000_0000_0000L);
    float [3] farr = [0, 1.25, 0];
    assert(f9170arr(farr) == 0x3FA0_0000);
    int i = 0x3FA0_0000;
    assert(i9170(i) == 1.25);
    uint u = 0x3FA0_0000;
    assert(u9170(u) == 1.25);
    return true;
}

static assert(bug9170());

/**************************************************
    6792 ICE with pointer cast of indexed array
**************************************************/

struct S6792
{
    int i;
}

static assert({
    {
        void* p;
        p = [S6792(1)].ptr;
        S6792 s = *(cast(S6792*)p);
        assert(s.i == 1);
    }
    {
        void*[] ary;
        ary ~= [S6792(2)].ptr;
        S6792 s = *(cast(S6792*)ary[0]);
        assert(s.i == 2);
    }
    {
        void*[7] ary;
        ary[6]= [S6792(2)].ptr;
        S6792 s = *(cast(S6792*)ary[6]);
        assert(s.i == 2);
    }
    {
        void* p;
        p = [S6792(1)].ptr;
        void*[7] ary;
        ary[5]= p;
        S6792 s = *(cast(S6792*)ary[5]);
        assert(s.i == 1);
    }
    {
        S6792*[string] aa;
        aa["key"] = [S6792(3)].ptr;
        const(S6792) s = *(cast(const(S6792)*)aa["key"]);
        assert(s.i == 3);
    }
    {
        S6792[string] blah;
        blah["abc"] = S6792(6);
        S6792*[string] aa;
        aa["kuy"] = &blah["abc"];
        const(S6792) s = *(cast(const(S6792)*)aa["kuy"]);
        assert(s.i == 6);

        void*[7] ary;
        ary[5]= &blah["abc"];
        S6792 t = *(cast(S6792*)ary[5]);
        assert(t.i == 6);

        int q = 6;
        ary[3]= &q;
        int gg = *(cast(int*)(ary[3]));
    }
    return true;
}());

/**************************************************
   7780 array cast
**************************************************/

int bug7780(int testnum)
{
    int[] y = new int[2];
    y[0] = 2000000;
    if (testnum == 1)
    {
        void[] x = y;
        return (cast(byte[])x)[1];
    }
    if (testnum == 2)
    {
        int[] x = y[0 .. 1];
        return (cast(byte[])x)[1];
    }
    return 1;
}

static assert( is(typeof(compiles!(bug7780(0)))));
static assert(!is(typeof(compiles!(bug7780(1)))));
static assert(!is(typeof(compiles!(bug7780(2)))));

/**************************************************
    14028 - static array pointer that refers existing array elements.
**************************************************/

int test14028a(size_t ofs)(bool ct)
{
    int[4] a;
    int[2]* p;
    int num = ofs;

    if (ct)
        p = cast(int[2]*)&a[ofs];    // SymOffExp
    else
        p = cast(int[2]*)&a[num];    // CastExp + AddrExp

    // pointers comparison
    assert(cast(void*)a.ptr <= cast(void*)p);
    assert(cast(void*)a.ptr <= cast(void*)&(*p)[0]);
    assert(cast(void*)&a[0] <= cast(void*)p);

    return 1;
}
static assert(test14028a!0(true));
static assert(test14028a!0(false));
static assert(test14028a!3(true));
static assert(test14028a!3(false));
static assert(!is(typeof(compiles!(test14028a!4(true)))));
static assert(!is(typeof(compiles!(test14028a!4(false)))));

int test14028b(int num)
{
    int[4] a;
    int[2]* p;

    if (num == 1)
    {
        p = cast(int[2]*)&a[0]; // &a[0..2];
        (*p)[0] = 1;            // a[0] = 1
        (*p)[1] = 2;            // a[1] = 2
        assert(a == [1,2,0,0]);
        p = p + 1;              // &a[0] -> &a[2]
        (*p)[0] = 3;            // a[2] = 3
        (*p)[1] = 4;            // a[3] = 4
        assert(a == [1,2,3,4]);
    }
    if (num == 2)
    {
        p = cast(int[2]*)&a[1]; // &a[1..3];
        (*p)[0] = 1;            // a[1] = 1
        p = p + 1;              // &a[1..3] -> &a[3..5]
        (*p)[0] = 2;            // a[3] = 2
        assert(a == [0,1,0,2]);
    }
    if (num == 3)
    {
        p = cast(int[2]*)&a[1]; // &a[1..3];
        (*p)[0] = 1;            // a[1] = 1
        p = p + 1;              // &a[1..3] -> &a[3..5]
        (*p)[0] = 2;            // a[3] = 2
        (*p)[1] = 3;            // a[4] = 3 (CTFE error)
    }
    if (num == 4)
    {
        p = cast(int[2]*)&a[0]; // &a[0..2];
        p = p + 1;              // &a[0..2] -> &a[2..4]
        p = p + 1;              // &a[2..4] -> &a[4..6] (ok)
    }
    if (num == 5)
    {
        p = cast(int[2]*)&a[1]; // &a[1..3];
        p = p + 2;              // &a[1..3] -> &a[5..7] (CTFE error)
    }
    return 1;
}
static assert(test14028b(1));
static assert(test14028b(2));
static assert(!is(typeof(compiles!(test14028b(3)))));
static assert(test14028b(4));
static assert(!is(typeof(compiles!(test14028b(5)))));

/**************************************************
    10275 cast struct literals to immutable
**************************************************/

struct Bug10275
{
    uint[] ivals;
}

Bug10275 bug10275()
{
    return Bug10275([1, 2, 3]);
}

int test10275()
{
    immutable(Bug10275) xxx = cast(immutable(Bug10275))bug10275();
    return 1;
}

static assert(test10275());

/**************************************************
    6851 passing pointer by argument
**************************************************/

void set6851(int* pn)
{
    *pn = 20;
}
void bug6851()
{
    int n = 0;
    auto pn = &n;
    *pn = 10;
    assert(n == 10);
    set6851(&n);
}
static assert({ bug6851(); return true; }());

/**************************************************
    7876
**************************************************/

int* bug7876(int n) @system
{
    int x;
    auto ptr = &x;
    if (n == 2)
        ptr = null;
    return ptr;
}

struct S7876
{
    int* p;
}

S7876 bug7876b(int n) @system
{
    int x;
    S7876 s;
    s.p = &x;
    if (n == 11)
        s.p = null;
    return s;
}

int test7876(int n)
{
    if (n >= 10)
    {
        S7876 m = bug7876b(n);
        return 1;
    }
    int* p = bug7876(n);
    return 1;
}

static assert( is(typeof(compiles!(test7876(2)))));
static assert(!is(typeof(compiles!(test7876(0)))));
static assert( is(typeof(compiles!(test7876(11)))));
static assert(!is(typeof(compiles!(test7876(10)))));

/**************************************************
    11824
**************************************************/

int f11824(T)()
{
    T[] arr = new T[](1);
    T* getAddr(ref T a)
    {
        return &a;
    }
    getAddr(arr[0]);
    return 1;
}
static assert(f11824!int());        // OK
static assert(f11824!(int[])());    // OK <- NG

/**************************************************
    6817 if converted to &&, only with -inline
**************************************************/

static assert({
    void toggle()
    {
        bool b;
        if (b)
            b = false;
    }
    toggle();
    return true;
}());

/**************************************************
    cast to void
**************************************************/

static assert({
    cast(void)(71);
    return true;
}());

/**************************************************
    6816 nested function can't access this
**************************************************/

struct S6816
{
    size_t foo()
    {
        return (){ return value +1 ; }();
    }
    size_t value;
}

enum s6816 = S6816().foo();

/**************************************************
    7277 ICE nestedstruct.init.tupleof
**************************************************/

struct Foo7277
{
    int a;
    int func()
    {
        int b;
        void nested()
        {
            b = 7;
            a = 10;
        }
        nested();
        return a+b;
    }
}

static assert(Foo7277().func() == 17);

/**************************************************
    10217 ICE. CTFE version of 9315
**************************************************/

bool bug10217()
{
    struct S
    {
        int i;
        void bar() {}
    }
    auto yyy = S.init.tupleof[$ - 1];
    assert(!yyy);
    return 1;
}

static assert(bug10217());

/**************************************************
    8276 ICE
**************************************************/

void bug8676(int n)
{
    const int X1 = 4 + n;
    const int X2 = 4;
    int X3 = 4;
    int bar1() { return X1; }
    int bar2() { return X2; }
    int bar3() { return X3; }
    static assert(!is(typeof(compiles!(bar1()))));
    static assert( is(typeof(compiles!(bar2()))));
    static assert(!is(typeof(compiles!(bar3()))));
}

/**************************************************
    classes and interfaces
**************************************************/

interface SomeInterface
{
    int daz();
    float bar(char);
    int baz();
}

interface SomeOtherInterface
{
    int xxx();
}

class TheBase : SomeInterface, SomeOtherInterface
{
    int q = 88;
    int rad = 61;
    int a = 14;
    int somebaseclassfunc() { return 28; }
    int daz() { return 0; }
    int baz() { return 0; }
    int xxx() { return 762; }
    int foo() { return q; }
    float bar(char c) { return 3.6; }
}

class SomeClass : TheBase, SomeInterface
{
    int gab = 9;
    int fab;
    int a = 17;
    int b = 23;
    override int foo() { return gab + a; }
    override float bar(char c) { return 2.6; }
    int something() { return 0; }
    override int daz() { return 0; }
    override int baz() { return 0; }
}

class Unrelated : TheBase
{
    this(int x) { a = x; }
}

auto classtest1(int n)
{
    SomeClass c = new SomeClass;
    assert(c.a == 17);
    assert(c.q == 88);
    TheBase d = c;
    assert(d.a == 14);
    assert(d.q == 88);
    if (n == 7)
    {
        // bad cast -- should fail
        Unrelated u = cast(Unrelated)d;
        assert(u is null);
    }
    SomeClass e = cast(SomeClass)d;
    d.q = 35;
    assert(c.q == 35);
    assert(c.foo() == 9 + 17);
    ++c.a;
    assert(c.foo() == 9 + 18);
    assert(d.foo() == 9 + 18);
    d = new TheBase;
    SomeInterface fc = c;
    SomeOtherInterface ot = c;
    assert(fc.bar('x') == 2.6);
    assert(ot.xxx() == 762);
    fc = d;
    ot = d;
    assert(fc.bar('x') == 3.6);
    assert(ot.xxx() == 762);

    Unrelated u2 = new Unrelated(7);
    assert(u2.a == 7);
    return 6;
}
static assert(classtest1(1));
static assert(classtest1(2));
static assert(classtest1(7)); // bug 7154

// can't initialize enum with not null class
SomeClass classtest2(int n)
{
    return n == 5 ? (new SomeClass) : null;
}
static assert( is(typeof((){ enum const(SomeClass) xx = classtest2(2);}())));
static assert(!is(typeof((){ enum const(SomeClass) xx = classtest2(5);}())));

class RecursiveClass
{
   int x;
   this(int n) { x = n; }
   RecursiveClass b;
   void doit() { b = new RecursiveClass(7); b.x = 2;}
}

int classtest3()
{
    RecursiveClass x = new RecursiveClass(17);
    x.doit();
    RecursiveClass y = x.b;
    assert(y.x == 2);
    assert(x.x == 17);
    return 1;
}

static assert(classtest3());

/**************************************************
    12016 class cast and qualifier reinterpret
**************************************************/

class B12016 { }

class C12016 : B12016 { }

bool f12016(immutable B12016 b)
{
    assert(b);
    return true;
}

static assert(f12016(new immutable C12016));

/**************************************************
    10610 ice immutable implicit conversion
**************************************************/

class Bug10610(T)
{
    int baz() immutable
    {
        return 1;
    }
    static immutable(Bug10610!T) min = new Bug10610!T();
}

void ice10610()
{
   alias T10610 = Bug10610!(int);
   static assert (T10610.min.baz());
}

/**************************************************
    13141 regression fix caused by 10610
**************************************************/

struct MapResult13141(alias pred)
{
    int[] range;
    @property empty() { return range.length == 0; }
    @property front() { return pred(range[0]); }
    void popFront() { range = range[1 .. $]; }
}

string[] array13141(R)(R r)
{
    typeof(return) result;
    foreach (e; r)
        result ~= e;
    return result;
}

//immutable string[] splitterNames = [4].map!(e => "4").array();
immutable string[] splitterNames13141 = MapResult13141!(e => "4")([4]).array13141();

/**************************************************
    11587 AA compare
**************************************************/

static assert([1:2, 3:4] == [3:4, 1:2]);

/**************************************************
    14325 more AA comparisons
**************************************************/

static assert([1:1] != [1:2, 2:1]);      // OK
static assert([1:1] != [1:2]);           // OK
static assert([1:1] != [2:1]);           // OK <- Error
static assert([1:1, 2:2] != [3:3, 4:4]); // OK <- Error

/**************************************************
    7147 typeid()
**************************************************/

static assert({
    TypeInfo xxx = typeid(Object);
    TypeInfo yyy = typeid(new Error("xxx"));
    return true;
}());

int bug7147(int n)
{
    Error err = n ? new Error("xxx") : null;
    TypeInfo qqq = typeid(err);
    return 1;
}

// Must not segfault if class is null
static assert(!is(typeof(compiles!(bug7147(0)))));
static assert( is(typeof(compiles!(bug7147(1)))));


/**************************************************
    14123 - identity TypeInfo objects
**************************************************/

static assert({
    bool eq(TypeInfo t1, TypeInfo t2)
    {
        return t1 is t2;
    }

    class C {}
    struct S {}

    assert( eq(typeid(C), typeid(C)));
    assert(!eq(typeid(C), typeid(Object)));
    assert( eq(typeid(S), typeid(S)));
    assert(!eq(typeid(S), typeid(int)));
    assert( eq(typeid(int), typeid(int)));
    assert(!eq(typeid(int), typeid(long)));

    Object o = new Object;
    Object c = new C;
    assert( eq(typeid(o), typeid(o)));
    assert(!eq(typeid(c), typeid(o)));
    assert(!eq(typeid(o), typeid(S)));

    return 1;
}());

/**************************************************
    6885 wrong code with new array
**************************************************/

struct S6885
{
    int p;
}

int bug6885()
{
    auto array = new double[1][2];
    array[1][0] = 6;
    array[0][0] = 1;
    assert(array[1][0] == 6);

    auto barray = new S6885[2];
    barray[1].p = 5;
    barray[0].p = 2;
    assert(barray[1].p == 5);
    return 1;
}

static assert(bug6885());

/**************************************************
    6886 ICE with new array of dynamic arrays
**************************************************/

int bug6886()
{
    auto carray = new int[][2];
    carray[1] = [6];
    carray[0] = [4];
    assert(carray[1][0] == 6);
    return 1;
}

static assert(bug6886());

/**************************************************
    10198 Multidimensional struct block initializer
**************************************************/

struct Block10198
{
    int val[3][4];
}

int bug10198()
{
    Block10198 pp = Block10198(67);
    assert(pp.val[2][3] == 67);
    assert(pp.val[1][3] == 67);
    return 1;
}
static assert(bug10198());

/**************************************************
    14440 Multidimensional block initialization should create distinct arrays for each elements
**************************************************/

struct Matrix14440(E, size_t row, size_t col)
{
    E[col][row] array2D;

    @safe pure nothrow
    this(E[row * col] numbers...)
    {
        foreach (r; 0 .. row)
        {
            foreach (c; 0 .. col)
            {
                array2D[r][c] = numbers[r * col + c];
            }
        }
    }
}

void test14440()
{
    // Replace 'enum' with 'auto' here and it will work fine.
    enum matrix = Matrix14440!(int, 3, 3)(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
    );

    static assert(matrix.array2D[0][0] == 1);
    static assert(matrix.array2D[0][1] == 2);
    static assert(matrix.array2D[0][2] == 3);
    static assert(matrix.array2D[1][0] == 4);
    static assert(matrix.array2D[1][1] == 5);
    static assert(matrix.array2D[1][2] == 6);
    static assert(matrix.array2D[2][0] == 7);
    static assert(matrix.array2D[2][1] == 8);
    static assert(matrix.array2D[2][2] == 9);
}

/****************************************************
 * Exception chaining tests from xtest46.d
 ****************************************************/

class A75
{
    pure static void raise(string s)
    {
        throw new Exception(s);
    }
}

int test75()
{
    int x = 0;
    try
    {
        A75.raise("a");
    }
    catch (Exception e)
    {
        x = 1;
    }
    assert(x == 1);
    return 1;
}
static assert(test75());

/****************************************************
 * Exception chaining tests from test4.d
 ****************************************************/

int test4_test54()
{
    int status = 0;

    try
    {
        try
        {
            status++;
            assert(status == 1);
            throw new Exception("first");
        }
        finally
        {
            status++;
            assert(status == 2);
            status++;
            throw new Exception("second");
        }
    }
    catch (Exception e)
    {
        assert(e.msg == "first");
        assert(e.next.msg == "second");
    }
    return true;
}

static assert(test4_test54());

void foo55()
{
    try
    {
        Exception x = new Exception("second");
        throw x;
    }
    catch (Exception e)
    {
        assert(e.msg == "second");
    }
}

int test4_test55()
{
    int status = 0;
    try
    {
        try
        {
            status++;
            assert(status == 1);
            Exception x = new Exception("first");
            throw x;
        }
        finally
        {
            status++;
            assert(status == 2);
            status++;
            foo55();
        }
    }
    catch (Exception e)
    {
        assert(e.msg == "first");
        assert(status == 3);
    }
    return 1;
}

static assert(test4_test55());

/****************************************************
 * Exception chaining tests from eh.d
 ****************************************************/

void bug1513outer()
{
    int result1513;

    void bug1513a()
    {
         throw new Exception("d");
    }

    void bug1513b()
    {
        try
        {
            try
            {
                bug1513a();
            }
            finally
            {
                result1513 |= 4;
                throw new Exception("f");
            }
        }
        catch (Exception e)
        {
            assert(e.msg == "d");
            assert(e.next.msg == "f");
            assert(!e.next.next);
        }
    }

    void bug1513c()
    {
        try
        {
            try
            {
                throw new Exception("a");
            }
            finally
            {
                result1513 |= 1;
                throw new Exception("b");
            }
        }
        finally
        {
            bug1513b();
            result1513 |= 2;
            throw new Exception("c");
        }
    }

    void bug1513()
    {
        result1513 = 0;
        try
        {
            bug1513c();
        }
        catch (Exception e)
        {
            assert(result1513 == 7);
            assert(e.msg == "a");
            assert(e.next.msg == "b");
            assert(e.next.next.msg == "c");
        }
    }

    bug1513();
}

void collideone()
{
    try
    {
        throw new Exception("x");
    }
    finally
    {
        throw new Exception("y");
    }
}

void doublecollide()
{
    try
    {
        try
        {
            try
            {
                throw new Exception("p");
            }
            finally
            {
                throw new Exception("q");
            }
        }
        finally
        {
            collideone();
        }
    }
    catch (Exception e)
    {
        assert(e.msg == "p");
        assert(e.next.msg == "q");
        assert(e.next.next.msg == "x");
        assert(e.next.next.next.msg == "y");
        assert(!e.next.next.next.next);
    }
}

void collidetwo()
{
    try
    {
        try
        {
            throw new Exception("p2");
        }
        finally
        {
            throw new Exception("q2");
        }
    }
    finally
    {
        collideone();
    }
}

void collideMixed()
{
    int works = 6;
    try
    {
        try
        {
            try
            {
                throw new Exception("e");
            }
            finally
            {
                throw new Error("t");
            }
        }
        catch (Exception f)
        {
            // Doesn't catch, because Error is chained to it.
            works += 2;
        }
    }
    catch (Error z)
    {
        works += 4;
        assert(z.msg == "t"); // Error comes first
        assert(z.next is null);
        assert(z.bypassedException.msg == "e");
    }
    assert(works == 10);
}

class AnotherException : Exception
{
    this(string s)
    {
        super(s);
    }
}

void multicollide()
{
    try
    {
        try
        {
            try
            {
                try
                {
                    throw new Exception("m2");
                }
                finally
                {
                    throw new AnotherException("n2");
                }
            }
            catch (AnotherException s)
            {
                // Not caught -- we needed to catch the root cause "m2", not
                // just the collateral "n2" (which would leave m2 uncaught).
                assert(0);
            }
        }
        finally
        {
            collidetwo();
        }
    }
    catch (Exception f)
    {
        assert(f.msg == "m2");
        assert(f.next.msg == "n2");
        Throwable e = f.next.next;
        assert(e.msg == "p2");
        assert(e.next.msg == "q2");
        assert(e.next.next.msg == "x");
        assert(e.next.next.next.msg == "y");
        assert(!e.next.next.next.next);
    }
}

int testsFromEH()
{
    bug1513outer();
    doublecollide();
    collideMixed();
    multicollide();
    return 1;
}
static assert(testsFromEH());

/**************************************************
    With + synchronized statements + bug 6901
**************************************************/

struct With1
{
    int a;
    int b;
}

class Foo6
{
}

class Foo32
{
    struct Bar
    {
        int x;
    }
}

class Base56
{
    private string myfoo;
    private string mybar;

    // Get/set properties that will be overridden.
    void foo(string s) { myfoo = s; }
    string foo() { return myfoo; }

    // Get/set properties that will not be overridden.
    void bar(string s) { mybar = s; }
    string bar() { return mybar; }
}

class Derived56 : Base56
{
    alias Base56.foo foo; // Bring in Base56's foo getter.
    override void foo(string s) { super.foo = s; } // Override foo setter.
}

int testwith()
{
    With1 x = With1(7);
    with (x)
    {
        a = 2;
    }
    assert(x.a == 2);

    // from test11.d
    Foo6 foo6 = new Foo6();

    with (foo6)
    {
        int xx;
        xx = 4;
    }
    with (new Foo32)
    {
        Bar z;
        z.x = 5;
    }
    Derived56 d = new Derived56;
    with (d)
    {
        foo = "hi";
        d.foo = "hi";
        bar = "hi";
        assert(foo == "hi");
        assert(d.foo == "hi");
        assert(bar == "hi");
    }
    int w = 7;
    synchronized
    {
        ++w;
    }
    assert(w == 8);
    return 1;
}

static assert(testwith());

/**************************************************
    9236 ICE  switch with(EnumType)
**************************************************/

enum Command9236
{
    Char,
    Any,
};

bool bug9236(Command9236 cmd)
{
    int n = 0;
    with (Command9236) switch (cmd)
    {
    case Any:
        n = 1;
        break;
    default:
        n = 2;
    }
    assert(n == 1);

    switch (cmd) with (Command9236)
    {
    case Any:
        return true;
    default:
        return false;
    }
}

static assert(bug9236(Command9236.Any));

/**************************************************
    6416 static struct declaration
**************************************************/

static assert({
    static struct S { int y = 7; }
    S a;
    a.y += 6;
    assert(a.y == 13);
    return true;
}());

/**************************************************
    10499 static template struct declaration
**************************************************/

static assert({
    static struct Result() {}
    return true;
}());

/**************************************************
    13757 extern(C) alias declaration
**************************************************/

static assert({
    alias FP1 = extern(C) int function();
    alias extern(C) int function() FP2;
    return true;
}());

/**************************************************
    6522 opAssign + foreach ref
**************************************************/

struct Foo6522
{
    bool b = false;
    void opAssign(int x)
    {
        this.b = true;
    }
}

bool foo6522()
{
    Foo6522[1] array;
    foreach (ref item; array)
        item = 1;
    return true;
}

static assert(foo6522());

/**************************************************
    7245 pointers + foreach ref
**************************************************/

int bug7245(int testnum)
{
    int[3] arr;
    arr[0] = 4;
    arr[1] = 6;
    arr[2] = 8;
    int* ptr;

    foreach (i, ref p; arr)
    {
        if (i == 1)
            ptr = &p;
        if (testnum == 1)
            p = 5;
    }

    return *ptr;
}

static assert(bug7245(0) == 6);
static assert(bug7245(1) == 5);

/**************************************************
    8498 modifying foreach
    7658 foreach ref
    8539 nested funcs, ref param, -inline
**************************************************/

int bug8498()
{
    foreach (ref i; 0 .. 5)
    {
        assert(i == 0);
        i = 100;
    }
    return 1;
}
static assert(bug8498());

string bug7658()
{
    string[] children = ["0"];
    foreach (ref child; children)
        child = "1";
    return children[0];
}

static assert(bug7658() == "1");

int bug8539()
{
    static void one(ref int x)
    {
        x = 1;
    }
    static void go()
    {
        int y;
        one(y);
        assert(y == 1); // fails with -inline
    }
    go();
    return 1;
}

static assert(bug8539());

/**************************************************
    7874, 13297, 13740 - better lvalue handling
**************************************************/

int bug7874(int x){ return ++x = 1; }
static assert(bug7874(0) == 1);

// ----

struct S13297
{
    int* p;
}
void f13297(ref int* p)
{
    p = cast(int*) 1;
    assert(p); // passes
}
static assert(
{
    S13297 s;
    f13297(s.p);
    return s.p != null; // false
}());

// ----

class R13740
{
    int e;
    bool empty = false;
    @property ref front() { return e; }
    void popFront() { empty = true; }
}
static assert({
    auto r = new R13740();
    foreach (ref e; r)
        e = 42;
    assert(r.e == 42); /* fails in CTFE */

    return true;
}());

/**************************************************
    6919
**************************************************/

void bug6919(int* val)
{
    *val = 1;
}
void test6919()
{
    int n;
    bug6919(&n);
    assert(n == 1);
}
static assert({ test6919(); return true; }());

void bug6919b(string* val)
{
    *val = "1";
}

void test6919b()
{
    string val;
    bug6919b(&val);
    assert(val == "1");
}
static assert({ test6919b(); return true; }());

/**************************************************
    6995
**************************************************/

struct Foo6995
{
    static size_t index(size_t v)()
    {
        return v;
    }
}

static assert(Foo6995.index!(27)() == 27);

/**************************************************
    7043 ref with -inline
**************************************************/

int bug7043(S)(ref int x)
{
    return x;
}

static assert({
    int i = 416;
    return bug7043!(char)(i);
}() == 416);

/**************************************************
    6037 recursive ref
**************************************************/

void bug6037(ref int x, bool b)
{
    int w = 3;
    if (b)
    {
        bug6037(w, false);
        assert(w == 6);
    }
    else
    {
        x = 6;
        assert(w == 3); // fails
    }
}

int bug6037outer()
{
    int q;
    bug6037(q, true);
    return 401;
}

static assert(bug6037outer() == 401);

/**************************************************
    14299 - [REG2.067a], more than one depth of recursive call with ref
**************************************************/

string gen14299(int max, int idx, ref string name)
{
    string ret;
    name = [cast(char)(idx + '0')];
    ret ~= name;
    if (idx < max)
    {
        string subname;
        ret ~= gen14299(max, idx + 1, subname);
    }
    ret ~= name;
    return ret;
}
string test14299(int max)
{
    string n;
    return gen14299(max, 0, n);
}
static assert(test14299(1) ==     "0110");      // OK <- fail
static assert(test14299(2) ==    "012210");     // OK <- ICE
static assert(test14299(3) ==   "01233210");
static assert(test14299(4) ==  "0123443210");
static assert(test14299(5) == "012345543210");

/**************************************************
    7940 wrong code for complicated assign
**************************************************/

struct Bug7940
{
    int m;
}

struct App7940
{
    Bug7940[] x;
}

int bug7940()
{
    Bug7940[2] y;
    App7940 app;
    app.x = y[0 .. 1];
    app.x[0].m = 12;
    assert(y[0].m == 12);
    assert(app.x[0].m == 12);
    return 1;
}

static assert(bug7940());

/**************************************************
    10298 wrong code for struct array literal init
**************************************************/

struct Bug10298
{
    int m;
}

int bug10298()
{
    Bug10298[1] y = [Bug10298(78)];
    y[0].m = 6;
    assert(y[0].m == 6);

    // Root cause
    Bug10298[1] x;
    x[] = [cast(const Bug10298)(Bug10298(78))];
    assert(x[0].m == 78);
    return 1;
}

static assert(bug10298());

/**************************************************
    7266 dotvar ref parameters
**************************************************/

struct S7266 { int a; }

bool bug7266()
{
    S7266 s;
    s.a = 4;
    bar7266(s.a);
    assert(s.a == 5);
    out7266(s.a);
    assert(s.a == 7);
    return true;
}

void bar7266(ref int b)
{
    b = 5;
    assert(b == 5);
}

void out7266(out int b)
{
    b = 7;
    assert(b == 7);
}

static assert(bug7266());

/**************************************************
    9982 dotvar assign through pointer
**************************************************/

struct Bug9982
{
    int a;
}

int test9982()
{
    Bug9982 x;
    int*q = &x.a;
    *q = 99;
    assert(x.a == 99);
    return 1;
}

static assert(test9982());

// 9982, rejects-valid case

struct SS9982
{
    Bug9982 s2;
    this(Bug9982 s1)
    {
        s2.a = 6;
        emplace9982(&s2, s1);
        assert(s2.a == 3);
    }
}

void emplace9982(Bug9982* chunk, Bug9982 arg)
{
    *chunk = arg;
}

enum s9982 = Bug9982(3);
enum p9982 = SS9982(s9982);

/**************************************************
    11618 dotvar assign through casted pointer
**************************************************/

struct Tuple11618(T...)
{
    T field;
    alias field this;
}

static assert({
    Tuple11618!(immutable dchar) result = void;
    auto addr = cast(dchar*)&result[0];
    *addr = dchar.init;
    return (result[0] == dchar.init);
}());

/**************************************************
    7143 'is' for classes
**************************************************/

class C7143
{
    int x;
}

int bug7143(int test)
{
    C7143 c = new C7143;
    C7143 d = new C7143;
    if (test == 1)
    {
        if (c)
            return c.x + 8;
        return -1;
    }
    if (test == 2)
    {
        if (c is null)
            return -1;
        return c.x + 45;
    }
    if (test == 3)
    {
        if (c is c)
            return 58;
    }
    if (test == 4)
    {
        if (c !is c)
            return -1;
        else
            return 48;
    }
    if (test == 6)
        d = c;
    if (test == 5 || test == 6)
    {
        if (c is d)
            return 188;
        else
            return 48;
    }
    return -1;
}

static assert(bug7143(1) == 8);
static assert(bug7143(2) == 45);
static assert(bug7143(3) == 58);
static assert(bug7143(4) == 48);
static assert(bug7143(5) == 48);
static assert(bug7143(6) == 188);

/**************************************************
    7147 virtual function calls from base class
**************************************************/

class A7147
{
    int foo() { return 0; }

    int callfoo()
    {
        return foo();
    }
}

class B7147 : A7147
{
    override int foo() { return 1; }
}

int test7147()
{
    A7147 a = new B7147;
    return a.callfoo();
}

static assert(test7147() == 1);

/**************************************************
    7158
**************************************************/

class C7158
{
    bool b() { return true; }
}
struct S7158
{
    C7158 c;
}

bool test7158()
{
    S7158 s = S7158(new C7158);
    return s.c.b;
}
static assert(test7158());

/**************************************************
    8484
**************************************************/

class C8484
{
    int n;
    int b() { return n + 3; }
}

struct S
{
    C8484 c;
}

int t8484(ref C8484 c)
{
    return c.b();
}

int test8484()
{
    auto s = S(new C8484);
    s.c.n = 4;
    return t8484(s.c);
}
static assert(test8484() == 7);

/**************************************************
    7419
**************************************************/

struct X7419
{
    double x;
    this(double x)
    {
        this.x = x;
    }
}

void bug7419()
{
    enum x = {
        auto p = X7419(3);
        return p.x;
    }();
    static assert(x == 3);
}

/**************************************************
    9445 ice
**************************************************/

template c9445(T...) { }

void ice9445(void delegate() expr, void function() f2)
{
    static assert(!is(typeof(c9445!(f2()))));
    static assert(!is(typeof(c9445!(expr()))));
}

/**************************************************
    10452 delegate ==
**************************************************/

struct S10452
{
    bool func() { return true; }
}

struct Outer10452
{
    S10452 inner;
}

class C10452
{
    bool func() { return true; }
}

bool delegate() ref10452(ref S10452 s)
{
    return &s.func;
}

bool test10452()
{
    bool delegate() bar = () { return true; };

    assert(bar !is null);
    assert(bar is bar);

    S10452 bag;
    S10452[6] bad;
    Outer10452 outer;
    C10452 tag = new C10452;

    auto rat = &outer.inner.func;
    assert(rat == rat);
    auto tat = &tag.func;
    assert(tat == tat);

    auto bat = &outer.inner.func;
    auto mat = &bad[2].func;
    assert(mat is mat);
    assert(rat == bat);

    auto zat = &bag.func;
    auto cat = &bag.func;
    assert(zat == zat);
    assert(zat == cat);

    auto drat = ref10452(bag);
    assert(cat == drat);
    assert(drat == drat);
    drat = ref10452(bad[2]);
    assert( drat == mat);
    assert(tat != rat);
    assert(zat != rat);
    assert(rat != cat);
    assert(zat != bar);
    assert(tat != cat);
    cat = bar;
    assert(cat == bar);
    return true;
}
static assert(test10452());

/**************************************************
    7162 and 4711
**************************************************/

void f7162() { }

bool ice7162()
{
    false && f7162();
    false || f7162();
    false && f7162();  // bug 4711
    true && f7162();
    return true;
}

static assert(ice7162());

/**************************************************
    8857, only with -inline (creates an &&)
**************************************************/

struct Result8857 { char[] next; }

void bug8857()()
{
    Result8857 r;
    r.next = null;
    if (true)
    {
       auto next = r.next;
    }
}
static assert({
    bug8857();
    return true;
}());

/**************************************************
    7527
**************************************************/

struct Bug7527
{
    char[] data;
}

int bug7527()
{
    auto app = Bug7527();

    app.data.ptr[0 .. 1] = "x";
    return 1;
}

static assert(!is(typeof(compiles!(bug7527()))));

/**************************************************
    7527
**************************************************/

int bug7380;

static assert(!is(typeof( compiles!(
    (){
        return &bug7380;
    }()
))));

/**************************************************
    7165
**************************************************/

struct S7165
{
    int* ptr;
    bool f() const { return !!ptr; }
}

static assert(!S7165().f());

/**************************************************
    7187
**************************************************/

int[] f7187() { return [0]; }
int[] f7187b(int n) { return [0]; }

int g7187(int[] r)
{
    auto t = r[0 .. 0];
    return 1;
}

static assert(g7187(f7187()));
static assert(g7187(f7187b(7)));

struct S7187 { const(int)[] field; }

const(int)[] f7187c()
{
    auto s = S7187([0]);
    return s.field;
}

bool g7187c(const(int)[] r)
{
    auto t = r[0 .. 0];
    return true;
}

static assert(g7187c(f7187c()));


/**************************************************
    6933 struct destructors
**************************************************/

struct Bug6933
{
    int x = 3;
    ~this() { }
}

int test6933()
{
    Bug6933 q;
    assert(q.x == 3);
    return 3;
}

static assert(test6933());

/**************************************************
    7197
**************************************************/

int foo7197(int[] x...)
{
    return 1;
}
template bar7197(y...)
{
    enum int bar7197 = foo7197(y);
}
enum int bug7197 = 7;
static assert(bar7197!(bug7197));

/**************************************************
    Enum string compare
**************************************************/

enum EScmp : string { a = "aaa" }

bool testEScmp()
{
    EScmp x = EScmp.a;
    assert(x < "abc");
    return true;
}

static assert(testEScmp());

/**************************************************
    7667
**************************************************/

bool baz7667(int[] vars...)
{
     return true;
}

struct S7667
{
    static void his(int n)
    {
        static assert(baz7667(2));
    }
}

bool bug7667()
{
    S7667 unused;
    unused.his(7);
    return true;
}
enum e7667 = bug7667();

/**************************************************
    7536
**************************************************/

bool bug7536(string expr)
{
    return true;
}

void vop()
{
    const string x7536 = "x";
    static assert(bug7536(x7536));
}

/**************************************************
    6681 unions
**************************************************/

struct S6681
{
    this(int a, int b) { this.a = b; this.b = a; }
    union
    {
        ulong g;
        struct { int a, b; };
    }
}

static immutable S6681 s6681 = S6681(0, 1);

bool bug6681(int test)
{
    S6681 x = S6681(0, 1);
    x.g = 5;
    auto u = &x.g;
    auto v = &x.a;
    long w = *u;
    int  z;
    assert(w == 5);
    if (test == 4)
        z = *v; // error
    x.a = 2; // invalidate g, and hence u.
    if (test == 1)
        w = *u; // error
    z = *v;
    assert(z == 2);
    x.g = 6;
    w = *u;
    assert(w == 6);
    if (test == 3)
        z = *v;
    return true;
}
static assert(bug6681(2));
static assert(!is(typeof(compiles!(bug6681(1)))));
static assert(!is(typeof(compiles!(bug6681(3)))));
static assert(!is(typeof(compiles!(bug6681(4)))));

/**************************************************
    9113 ICE with struct in union
**************************************************/

union U9113
{
    struct M
    {
        int y;
    }
    int xx;
}

int bug9113(T)()
{
    U9113 x;
    x.M.y = 10; // error, need 'this'
    return 1;
}

static assert(!is(typeof(compiles!(bug9113!(int)()))));

/**************************************************
    Creation of unions
**************************************************/

union UnionTest1
{
    int x;
    float y;
}

int uniontest1()
{
    UnionTest1 u = UnionTest1(1);
    return 1;
}

static assert(uniontest1());

/**************************************************
    6438 void
**************************************************/

struct S6438
{
    int a;
    int b = void;
}

void fill6438(int[] arr, int testnum)
{
    if (testnum == 2)
    {
        auto u = arr[0];
    }
    foreach (ref x; arr)
        x = 7;
    auto r = arr[0];
    S6438[2] s;
    auto p = &s[0].b;
    if (testnum == 3)
    {
        auto v = *p;
    }
}

bool bug6438(int testnum)
{
    int[4] stackSpace = void;
    fill6438(stackSpace[], testnum);
    assert(stackSpace == [7, 7, 7, 7]);
    return true;
}

static assert( is(typeof(compiles!(bug6438(1)))));
static assert(!is(typeof(compiles!(bug6438(2)))));
static assert(!is(typeof(compiles!(bug6438(3)))));

/**************************************************
    10994 void static array members
**************************************************/

struct Bug10994
{
    ubyte[2] buf = void;
}

static bug10994 = Bug10994.init;

/**************************************************
    10937 struct inside union
**************************************************/

struct S10937
{
    union
    {
        ubyte[1] a;
        struct
        {
            ubyte b;
        }
    }

    this(ubyte B)
    {
        if (B > 6)
            this.b = B;
        else
            this.a[0] = B;
    }
}

enum test10937 = S10937(7);
enum west10937 = S10937(2);

/**************************************************
    13831
**************************************************/

struct Vector13831()
{
}

struct Coord13831
{
    union
    {
        struct { short x; }
        Vector13831!() vector;
    }
}

struct Chunk13831
{
    this(Coord13831)
    {
        coord = coord;
    }

    Coord13831 coord;

    static const Chunk13831* unknownChunk = new Chunk13831(Coord13831());
}

/**************************************************
    7732
**************************************************/

struct AssociativeArray
{
    int* impl;
    int f()
    {
        if (impl !is null)
            auto x = *impl;
        return 1;
    }
}

int test7732()
{
    AssociativeArray aa;
    return aa.f;
}

static assert(test7732());

/**************************************************
    7784
**************************************************/
struct Foo7784
{
    void bug()
    {
        tab["A"] = Bar7784(&this);
        auto pbar = "A" in tab;
        auto bar = *pbar;
    }

    Bar7784[string] tab;
}

struct Bar7784
{
    Foo7784* foo;
    int val;
}

bool ctfe7784()
{
    auto foo = Foo7784();
    foo.bug();
    return true;
}

static assert(ctfe7784());

/**************************************************
    7781
**************************************************/

static assert(({ return; }(), true));

/**************************************************
    7785
**************************************************/

bool bug7785(int n)
{
    int val = 7;
    auto p = &val;
    if (n == 2)
    {
        auto ary = p[0 .. 1];
    }
    auto x = p[0];
    val = 6;
    assert(x == 7);
    if (n == 3)
        p[0 .. 1] = 1;
    return true;
}

static assert(bug7785(1));
static assert(!is(typeof(compiles!(bug7785(2)))));
static assert(!is(typeof(compiles!(bug7785(3)))));

/**************************************************
    7987
**************************************************/

class C7987
{
    int m;
}

struct S7987
{
    int* p;
    C7987 c;
}

bool bug7987()
{
    int[7] q;
    int[][2] b = q[0 .. 5];
    assert(b == b);
    assert(b is b);
    C7987 c1 = new C7987;
    C7987 c2 = new C7987;
    S7987 s, t;
    s.p = &q[0];
    t.p = &q[1];
    assert(s != t);
    s.p = &q[1];
    /*assert(s == t);*/     assert(s.p == t.p);
    s.c = c1;
    t.c = c2;
    /*assert(s != t);*/     assert(s.c !is t.c);
    assert(s !is t);
    s.c = c2;
    /*assert(s == t);*/     assert(s.p == t.p && s.c is t.c);
    assert(s is t);
    return true;
}

static assert(bug7987());

/**************************************************
    10579 typeinfo.func() must not segfault
**************************************************/

static assert(!is(typeof(compiles!(typeid(int).toString.length))));

class Bug10579
{
    int foo() { return 1; }
}
Bug10579 uninitialized10579;

static assert(!is(typeof(compiles!(uninitialized10579.foo()))));

/**************************************************
    10804 mixin ArrayLiteralExp typed string
**************************************************/

void test10804()
{
    String identity(String)(String a) { return a; }

    string cfun()
    {
        char[] s;
        s.length = 8 + 2 + (2) + 1 + 2;
        s[] = "identity(`Ω`c)"c[];
        return cast(string)s;   // Return ArrayLiteralExp as the CTFE result
    }
    {
        enum a1 = "identity(`Ω`c)"c;
        enum a2 = cfun();
        static assert(cast(ubyte[])mixin(a1) == [0xCE, 0xA9]);
        static assert(cast(ubyte[])mixin(a2) == [0xCE, 0xA9]);  // should pass
    }

    wstring wfun()
    {
        wchar[] s;
        s.length = 8 + 2 + (2) + 1 + 2;
        s[] = "identity(`\U0002083A`w)"w[];
        return cast(wstring)s;  // Return ArrayLiteralExp as the CTFE result
    }
    {
        enum a1 = "identity(`\U0002083A`w)"w;
        enum a2 = wfun();
        static assert(cast(ushort[])mixin(a1) == [0xD842, 0xDC3A]);
        static assert(cast(ushort[])mixin(a2) == [0xD842, 0xDC3A]);
    }

    dstring dfun()
    {
        dchar[] s;
        s.length = 8 + 2 + (1) + 1 + 2;
        s[] = "identity(`\U00101000`d)"d[];
        return cast(dstring)s;  // Return ArrayLiteralExp as the CTFE result
    }
    {
        enum a1 = "identity(`\U00101000`d)"d;
        enum a2 = dfun();
        static assert(cast(uint[])mixin(a1) == [0x00101000]);
        static assert(cast(uint[])mixin(a2) == [0x00101000]);
    }
}

/******************************************************/

struct B73 {}
struct C73 { B73 b; }
C73 func73() { C73 b = void; b.b = B73(); return b; }
C73 test73 = func73();

/******************************************************/

struct S74
{
    int n[1];
    static S74 test(){ S74 ret = void; ret.n[0] = 0; return ret; }
}

enum Test74 = S74.test();

/******************************************************/

static bool bug8865()
in
{
    int x = 0;
label:
    foreach (i; (++x) .. 3)
    {
        if (i == 1)
            continue label;     // doesn't work.
        else
            break label;        // doesn't work.
    }
}
out
{
    int x = 0;
label:
    foreach (i; (++x) .. 3)
    {
        if (i == 1)
            continue label;     // doesn't work.
        else
            break label;        // doesn't work.
    }
}
body
{
    int x = 0;
label:
    foreach (i; (++x) .. 3)
    {
        if (i == 1)
            continue label;     // works.
        else
            break label;        // works.
    }

    return true;
}
static assert(bug8865());

/******************************************************/
// 15450 labeled foreach + continue/break

static assert({
  L1:
    foreach (l; [0])
        continue L1;

  L2:
    foreach (l; [0])
        break L2;

    return true;
}());

struct Test75
{
    this(int) pure {}
}

/******************************************************/

static assert( __traits(compiles, { static shared(Test75*)   t75 = new shared(Test75)(0);    return t75; }));
static assert( __traits(compiles, { static shared(Test75)*   t75 = new shared(Test75)(0);    return t75; }));
static assert( __traits(compiles, { static __gshared Test75* t75 = new Test75(0);            return t75; }));
static assert( __traits(compiles, { static const(Test75*)    t75 = new const(Test75)(0);     return t75; }));
static assert( __traits(compiles, { static immutable Test75* t75 = new immutable(Test75)(0); return t75; }));
static assert(!__traits(compiles, { static Test75*           t75 = new Test75(0);            return t75; }));
/+
static assert(!__traits(compiles, { enum                 t75 = new shared(Test75)(0); return t75; }));
static assert(!__traits(compiles, { enum                 t75 = new Test75(0);         return t75; }));
static assert(!__traits(compiles, { enum shared(Test75)* t75 = new shared(Test75)(0); return t75; }));
static assert(!__traits(compiles, { enum Test75*         t75 = new Test75(0);         return t75; }));

static assert( __traits(compiles, { enum                    t75 = new const(Test75)(0);     return t75;}));
static assert( __traits(compiles, { enum                    t75 = new immutable(Test75)(0); return t75;}));
static assert( __traits(compiles, { enum const(Test75)*     t75 = new const(Test75)(0);     return t75;}));
static assert( __traits(compiles, { enum immutable(Test75)* t75 = new immutable(Test75)(0); return t75;}));
+/
/******************************************************/

class Test76
{
    this(int) pure {}
}
/+
static assert(!__traits(compiles, { enum                   t76 = new shared(Test76)(0); return t76;}));
static assert(!__traits(compiles, { enum                   t76 = new Test76(0);         return t76;}));
static assert(!__traits(compiles, { enum shared(Test76)    t76 = new shared(Test76)(0); return t76;}));
static assert(!__traits(compiles, { enum Test76            t76 = new Test76(0);         return t76;}));

static assert( __traits(compiles, { enum                   t76 = new const(Test76)(0);     return t76;}));
static assert( __traits(compiles, { enum                   t76 = new immutable(Test76)(0); return t76;}));
static assert( __traits(compiles, { enum const(Test76)     t76 = new const(Test76)(0);     return t76;}));
static assert( __traits(compiles, { enum immutable(Test76) t76 = new immutable(Test76)(0); return t76;}));
+/
/******************************************************/

static assert( __traits(compiles, { static shared Test76    t76 = new shared(Test76)(0);   return t76; }));
static assert( __traits(compiles, { static shared(Test76)   t76 = new shared(Test76)(0);   return t76; }));
static assert( __traits(compiles, { static __gshared Test76 t76 = new Test76(0);           return t76; }));
static assert( __traits(compiles, { static const Test76     t76 = new const(Test76)(0);    return t76; }));
static assert( __traits(compiles, { static immutable Test76 t76 = new immutable Test76(0); return t76; }));
static assert(!__traits(compiles, { static Test76           t76 = new Test76(0);           return t76; }));

/***** Bug 5678 *********************************/

struct Bug5678
{
    this(int) {}
}

static assert(!__traits(compiles, { enum const(Bug5678)* b5678 = new const(Bug5678)(0); return b5678; }));

/**************************************************
    10782 run semantic2 for class field
**************************************************/

enum e10782 = 0;
class C10782 { int x = e10782; }
string f10782()
{
    auto c = new C10782();
    return "";
}
mixin(f10782());

/**************************************************
    10929 NRVO support in CTFE
**************************************************/

struct S10929
{
    this(this)
    {
        postblitCount++;
    }
    ~this()
    {
        dtorCount++;
    }
    int payload;
    int dtorCount;
    int postblitCount;
}

auto makeS10929()
{
    auto s = S10929(42, 0, 0);
    return s;
}

bool test10929()
{
    auto s = makeS10929();
    assert(s.postblitCount == 0);
    assert(s.dtorCount == 0);
    return true;
};
static assert(test10929());

/**************************************************
    9245 - support postblit call on array assignments
**************************************************/

bool test9245()
{
    int postblits = 0;
    struct S
    {
        this(this)
        {
            ++postblits;
        }
    }

    S s;
    S[2] a;
    assert(postblits == 0);

    {
        S[2] arr = s;
        assert(postblits == 2);
        arr[] = s;
        assert(postblits == 4);
        postblits = 0;

        S[2] arr2 = arr;
        assert(postblits == 2);
        arr2 = arr;
        assert(postblits == 4);
        postblits = 0;

        const S[2] constArr = s;
        assert(postblits == 2);
        postblits = 0;

        const S[2] constArr2 = arr;
        assert(postblits == 2);
        postblits = 0;
    }
    {
        S[2][2] arr = s;
        assert(postblits == 4);
        arr[] = a;
        assert(postblits == 8);
        postblits = 0;

        S[2][2] arr2 = arr;
        assert(postblits == 4);
        arr2 = arr;
        assert(postblits == 8);
        postblits = 0;

        const S[2][2] constArr = s;
        assert(postblits == 4);
        postblits = 0;

        const S[2][2] constArr2 = arr;
        assert(postblits == 4);
        postblits = 0;
    }

    return true;
}
static assert(test9245());

/**************************************************
    12906 don't call postblit on blit initializing
**************************************************/

struct S12906 { this(this) { assert(0); } }

static assert({
    S12906[1] arr;
    return true;
}());

/**************************************************
    11510 support overlapped field access in CTFE
**************************************************/

struct S11510
{
    union
    {
        size_t x;
        int* y; // pointer field
    }
}
bool test11510()
{
    S11510 s;

    s.y = [1,2,3].ptr;            // writing overlapped pointer field is OK
    assert(s.y[0 .. 3] == [1,2,3]); // reading valid field is OK

    s.x = 10;
    assert(s.x == 10);

    // There's no reinterpretation between S.x and S.y
    return true;
}
static assert(test11510());

/**************************************************
    11534 - subtitude inout
**************************************************/

struct MultiArray11534
{
    void set(size_t[] sizes...)
    {
        storage = new size_t[5];
    }

    @property auto raw_ptr() inout
    {
        return storage.ptr + 1;
    }
    size_t[] storage;
}

enum test11534 = () {
    auto m = MultiArray11534();
    m.set(3,2,1);
    auto start = m.raw_ptr;   //this trigger the bug
    //auto start = m.storage.ptr + 1; //this obviously works
    return 0;
}();

/**************************************************
    11941 - Regression of 11534 fix
**************************************************/

void takeConst11941(const string[]) {}
string[] identity11941(string[] x) { return x; }

bool test11941a()
{
    struct S { string[] a; }
    S s;

    takeConst11941(identity11941(s.a));
    s.a ~= [];

    return true;
}
static assert(test11941a());

bool test11941b()
{
    struct S { string[] a; }
    S s;

    takeConst11941(identity11941(s.a));
    s.a ~= "foo"; /* Error refers to this line (15), */
    string[] b = s.a[]; /* but only when this is here. */

    return true;
}
static assert(test11941b());

/**************************************************
    11535 - element-wise assignment from string to ubyte array literal
**************************************************/

struct Hash11535
{
    ubyte[6] _buffer;

    void put(scope const(ubyte)[] data...)
    {
        uint i = 0, index = 0;
        auto inputLen = data.length;

        (&_buffer[index])[0 .. inputLen - i] = (&data[i])[0 .. inputLen - i];
    }
}

auto md5_digest11535(T...)(scope const T data)
{
    Hash11535 hash;
    hash.put(cast(const(ubyte[]))data[0]);
    return hash._buffer;
}

static assert(md5_digest11535(`TEST`) == [84, 69, 83, 84, 0, 0]);

/**************************************************
    11540 - goto label + try-catch-finally / with statement
**************************************************/

static assert(()
{
    // enter to TryCatchStatement.body
    {
        bool c = false;
        try
        {
            if (c)  // need to bypass front-end optimization
                throw new Exception("");
            else
            {
                goto Lx;
              L1:
                c = true;
            }
        }
        catch (Exception e) {}

      Lx:
        if (!c)
            goto L1;
    }

    // jump inside TryCatchStatement.body
    {
        bool c = false;
        try
        {
            if (c)  // need to bypass front-end optimization
                throw new Exception("");
            else
                goto L2;
          L2:
            ;
        }
        catch (Exception e) {}
    }

    // exit from TryCatchStatement.body
    {
        bool c = false;
        try
        {
            if (c)  // need to bypass front-end optimization
                throw new Exception("");
            else
                goto L3;
        }
        catch (Exception e) {}

        c = true;
      L3:
        assert(!c);
    }

    return 1;
}());

static assert(()
{
    // enter to TryCatchStatement.catches which has no exception variable
    {
        bool c = false;
        goto L1;
        try
        {
            c = true;
        }
        catch (Exception/* e*/)
        {
          L1:
            ;
        }
        assert(c == false);
    }

    // jump inside TryCatchStatement.catches
    {
        bool c = false;
        try
        {
            throw new Exception("");
        }
        catch (Exception e)
        {
            goto L2;
            c = true;
          L2:
            ;
        }
        assert(c == false);
    }

    // exit from TryCatchStatement.catches
    {
        bool c = false;
        try
        {
            throw new Exception("");
        }
        catch (Exception e)
        {
            goto L3;
            c = true;
        }
      L3:
        assert(c == false);
    }

    return 1;
}());

static assert(()
{
    // enter forward to TryFinallyStatement.body
    {
        bool c = false;
        goto L0;
        c = true;
        try
        {
          L0:
            ;
        }
        finally {}
        assert(!c);
    }

    // enter back to TryFinallyStatement.body
    {
        bool c = false;
        try
        {
            goto Lx;
          L1:
            c = true;
        }
        finally {
        }

      Lx:
        if (!c)
            goto L1;
    }

    // jump inside TryFinallyStatement.body
    {
        try
        {
            goto L2;
          L2: ;
        }
        finally {}
    }

    // exit from TryFinallyStatement.body
    {
        bool c = false;
        try
        {
            goto L3;
        }
        finally {}

        c = true;
      L3:
        assert(!c);
    }

    // enter in / exit out from finally block is rejected in semantic analysis

    // jump inside TryFinallyStatement.finalbody
    {
        bool c = false;
        try
        {
        }
        finally
        {
            goto L4;
            c = true;
          L4:
            assert(c == false);
        }
    }

    return 1;
}());

static assert(()
{
    {
        bool c = false;
        with (Object.init)
        {
            goto L2;
            c = true;
          L2:
            ;
        }
        assert(c == false);
    }

    {
        bool c = false;
        with (Object.init)
        {
            goto L3;
            c = true;
        }
      L3:
        assert(c == false);
    }

    return 1;
}());

/**************************************************
    11627 -  cast dchar to char at compile time on AA assignment
**************************************************/

bool test11627()
{
    char[ubyte] toCharTmp;
    dchar letter = 'A';

    //char c = cast(char)letter;    // OK
    toCharTmp[0] = cast(char)letter;    // NG

    return true;
}
static assert(test11627());

/**************************************************
    11664 - ignore function local static variables
**************************************************/

bool test11664()
{
    static int x;
    static int y = 1;
    return true;
}
static assert(test11664());

/**************************************************
    12110 - operand of dereferencing does not need to be an lvalue
**************************************************/

struct SliceOverIndexed12110
{
    Uint24Array12110* arr;

    @property front(uint val)
    {
        arr.dupThisReference();
    }
}

struct Uint24Array12110
{
    ubyte[] data;

    this(ubyte[] range)
    {
        data = range;
        SliceOverIndexed12110(&this).front = 0;
        assert(data.length == range.length * 2);
    }

    void dupThisReference()
    {
        auto new_data = new ubyte[data.length * 2];
        data = new_data;
    }
}

static m12110 = Uint24Array12110([0x80]);

/**************************************************
    12310 - heap allocation for built-in sclar types
**************************************************/

bool test12310()
{
    auto p1 = new int, p2 = p1;
    assert(*p1 == 0);
    assert(*p2 == 0);
    *p1 = 10;
    assert(*p1 == 10);
    assert(*p2 == 10);

    auto q1 = new int(3), q2 = q1;
    assert(*q1 == 3);
    assert(*q2 == 3);
    *q1 = 20;
    assert(*q1 == 20);
    assert(*q2 == 20);

    return true;
}
static assert(test12310());

/**************************************************
    12499 - initialize TupleDeclaraion in CTFE
**************************************************/

auto f12499()
{
    //Initialize 3 ints to 5.
    TypeTuple!(int, int, int) a = 5;
    return a[0]; //Error: variable _a_field_0 cannot be read at compile time
}
static assert(f12499() == 5);

/**************************************************
    12602 - slice in struct literal members
**************************************************/

struct Result12602
{
    uint[] source;
}

auto wrap12602a(uint[] r)
{
    return Result12602(r);
}

auto wrap12602b(uint[] r)
{
    Result12602 x;
    x.source = r;
    return x;
}

auto testWrap12602a()
{
    uint[] dest = [1, 2, 3, 4];

    auto ra = wrap12602a(dest[0 .. 2]);
    auto rb = wrap12602a(dest[2 .. 4]);

    foreach (i; 0 .. 2)
        rb.source[i] = ra.source[i];

    assert(ra.source == [1,2]);
    assert(rb.source == [1,2]);
    assert(&ra.source[0] == &dest[0]);
    assert(&rb.source[0] == &dest[2]);
    assert(dest == [1,2,1,2]);
    return dest;
}

auto testWrap12602b()
{
    uint[] dest = [1, 2, 3, 4];

    auto ra = wrap12602b(dest[0 .. 2]);
    auto rb = wrap12602b(dest[2 .. 4]);

    foreach (i; 0 .. 2)
        rb.source[i] = ra.source[i];

    assert(ra.source == [1,2]);
    assert(rb.source == [1,2]);
    assert(&ra.source[0] == &dest[0]);
    assert(&rb.source[0] == &dest[2]);
    assert(dest == [1,2,1,2]);
    return dest;
}

auto testWrap12602c()
{
    uint[] dest = [1, 2, 3, 4];

    auto ra = Result12602(dest[0 .. 2]);
    auto rb = Result12602(dest[2 .. 4]);

    foreach (i; 0 .. 2)
        rb.source[i] = ra.source[i];

    assert(ra.source == [1,2]);
    assert(rb.source == [1,2]);
    assert(&ra.source[0] == &dest[0]);
    assert(&rb.source[0] == &dest[2]);
    assert(dest == [1,2,1,2]);
    return dest;
}

auto testWrap12602d()
{
    uint[] dest = [1, 2, 3, 4];

    Result12602 ra; ra.source = dest[0 .. 2];
    Result12602 rb; rb.source = dest[2 .. 4];

    foreach (i; 0 .. 2)
        rb.source[i] = ra.source[i];

    assert(ra.source == [1,2]);
    assert(rb.source == [1,2]);
    assert(&ra.source[0] == &dest[0]);
    assert(&rb.source[0] == &dest[2]);
    assert(dest == [1,2,1,2]);
    return dest;
}

static assert(testWrap12602a() == [1,2,1,2]);
static assert(testWrap12602b() == [1,2,1,2]);
static assert(testWrap12602c() == [1,2,1,2]);
static assert(testWrap12602d() == [1,2,1,2]);

/**************************************************
    12677 - class type initializing from DotVarExp
**************************************************/

final class C12677
{
    TypeTuple!(Object, int[]) _test;
    this()
    {
        auto t0 = _test[0]; //
        auto t1 = _test[1]; //
        assert(t0 is null);
        assert(t1 is null);
    }
}

struct S12677
{
    auto f = new C12677();
}

/**************************************************
    12851 - interpret function local const static array
**************************************************/

void test12851()
{
    const int[5] arr;
    alias staticZip = TypeTuple!(arr[0]);
}

/**************************************************
    13630 - indexing and setting array element via pointer
**************************************************/

struct S13630(T)
{
    T[3] arr;

    this(A...)(auto ref in A args)
    {
        auto p = arr.ptr;

        foreach (ref v; args)
        {
            *p = 0;
        }
    }
}

enum s13630 = S13630!float(1);

/**************************************************
    13827
**************************************************/

struct Matrix13827(T, uint N)
{
    private static defaultMatrix()
    {
        T arr[N];
        return arr;
    }

    union
    {
        T[N] A = defaultMatrix;
        T[N] flat;
    }

    this(A...)(auto ref in A args)
    {
        uint k;

        foreach (ref v; args)
            flat[k++] = cast(T)v;
    }
}
enum m13827 = Matrix13827!(int, 3)(1, 2, 3);

/**************************************************
    13847 - support DotTypeExp
**************************************************/

class B13847
{
    int foo() { return 1; }
}

class C13847 : B13847
{
    override int foo() { return 2; }

    final void test(int n)
    {
        assert(foo() == n);
        assert(B13847.foo() == 1);
        assert(C13847.foo() == 2);
        assert(this.B13847.foo() == 1);
        assert(this.C13847.foo() == 2);
    }
}

class D13847 : C13847
{
    override int foo() { return 3; }
}

static assert({
    C13847 c = new C13847();
    c.test(2);
    assert(c.B13847.foo() == 1);
    assert(c.C13847.foo() == 2);

    D13847 d = new D13847();
    d.test(3);
    assert(d.B13847.foo() == 1);
    assert(d.C13847.foo() == 2);
    assert(d.D13847.foo() == 3);

    c = d;
    c.test(3);
    assert(c.B13847.foo() == 1);
    assert(c.C13847.foo() == 2);
    return true;
}());

/**************************************************
    12495 - cast from string to immutable(ubyte)[]
**************************************************/

string getStr12495()
{
    char[1] buf = void;                     // dummy starting point.
    string s = cast(string)buf[0..0];       // empty slice, .ptr points mutable.
    assert(buf.ptr == s.ptr);
    s ~= 'a';                               // this should allocate.
    assert(buf.ptr != s.ptr);
    return s.idup;                          // this should allocate again, and
                                            // definitely point immutable memory.
}
auto indexOf12495(string s)
{
    auto p1 = s.ptr;
    auto p2 = (cast(immutable(ubyte)[])s).ptr;
    assert(cast(void*)p1 == cast(void*)p2); // OK <- fails
    return cast(void*)p2 - cast(void*)p1;   // OK <- "cannot subtract pointers ..."
}
static assert(indexOf12495(getStr12495()) == 0);

/**************************************************
    13992 - Repainting pointer arithmetic result
**************************************************/

enum hash13992 = hashOf13992("abcd".ptr);

@trusted hashOf13992(const void* buf)
{
    auto data = cast(const(ubyte)*) buf;
    size_t hash;
    data += 2;      // problematic pointer arithmetic
    hash += *data;  // CTFE internal issue was shown by the dereference
    return hash;
}

/**************************************************
    13739 - Precise copy for ArrayLiteralExp elements
**************************************************/

static assert(
{
    int[] a1 = [13];
    int[][] a2 = [a1];
    assert(a2[0] is a1);            // OK
    assert(a2[0].ptr is a1.ptr);    // OK <- NG

    a1[0] = 1;
    assert(a2[0][0] == 1);  // OK <- NG

    a2[0][0] = 2;
    assert(a1[0] == 2);     // OK <- NG

    return 1;
}());

/**************************************************
    14463 - ICE on slice assignment without postblit
**************************************************/

struct Boo14463
{
    private int[1] c;
    this(int[] x)
    {
        c = x;
    }
}
immutable Boo14463 a14463 = Boo14463([1]);

/**************************************************
    13295 - Don't copy struct literal in VarExp::interpret()
**************************************************/

struct S13295
{
    int n;
}

void f13295(ref const S13295 s)
{
    *cast(int*) &s.n = 1;
    assert(s.n == 1);     // OK <- fail
}

static assert(
{
    S13295 s;
    f13295(s);
    return s.n == 1; // true <- false
}());

int foo14061(int[] a)
{
    foreach (immutable x; a)
    {
        auto b = a ~ x;
        return b == [1, 1];
    }
    return 0;
}
static assert(foo14061([1]));

/**************************************************
    14024 - CTFE version
**************************************************/

bool test14024()
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case
    }

    S[4] mem;
    ref S[2] slice(int a, int b) { return mem[a .. b][0 .. 2]; }

    op = null;
    mem[0].x = 'a';
    mem[1].x = 'b';
    mem[2].x = 'x';
    mem[3].x = 'y';
    slice(0, 2) = slice(2, 4);  // [ab] = [xy]
    assert(op == "XaYb", op);

    op = null;
    mem[0].x = 'x';
    mem[1].x = 'y';
    mem[2].x = 'a';
    mem[3].x = 'b';
    slice(2, 4) = slice(0, 2);  // [ab] = [xy]
    assert(op == "XaYb", op);

    op = null;
    mem[0].x = 'a';
    mem[1].x = 'b';
    mem[2].x = 'c';
    slice(0, 2) = slice(1, 3);  // [ab] = [bc]
    assert(op == "BaCb", op);

    op = null;
    mem[0].x = 'x';
    mem[1].x = 'y';
    mem[2].x = 'z';
    slice(1, 3) = slice(0, 2);  // [yz] = [xy]
    assert(op == "YzXy", op);

    return true;
}
static assert(test14024());

/**************************************************
    14304 - cache of static immutable value
**************************************************/

immutable struct Bug14304
{
    string s_name;
    alias s_name this;

    string fun()()
    {
        return "fun";
    }
}
class Buggy14304
{
    static string fun(string str)()
    {
        return str;
    }
    static immutable val = immutable Bug14304("val");
}
void test14304()
{
    enum kt = Buggy14304.fun!(Buggy14304.val);
    static assert(kt == "val");
    enum bt = Buggy14304.val.fun();
    static assert(bt == "fun");
}

/**************************************************
    14371 - evaluate BinAssignExp as lvalue
**************************************************/

int test14371()
{
    int x;
    ++(x += 1);
    return x;
}
static assert(test14371() == 2);

/**************************************************
    7151 - [CTFE] cannot compare classes with ==
**************************************************/

bool test7151()
{
    auto a = new Object;
    return a == a && a != new Object;
}
static assert(test7151());


/**************************************************
    12603 - [CTFE] goto does not correctly call dtors
**************************************************/

struct S12603
{
    this(uint* dtorCalled)
    {
        *dtorCalled = 0;
        this.dtorCalled = dtorCalled;
    }

    @disable this();

    ~this()
    {
        ++*dtorCalled;
    }

    uint* dtorCalled;
}


auto numDtorCallsByGotoWithinScope()
{
    uint dtorCalled;
    {
        S12603 s = S12603(&dtorCalled);
        assert(dtorCalled == 0);
        goto L_abc;
        L_abc:
        assert(dtorCalled == 0);
    }
    assert(dtorCalled == 1);
    return dtorCalled;
}
static assert(numDtorCallsByGotoWithinScope() == 1);


auto numDtorCallsByGotoOutOfScope()
{
    uint dtorCalled;
    {
        S12603 s = S12603(&dtorCalled);
        assert(dtorCalled == 0);
        goto L_abc;
    }
    L_abc:
    assert(dtorCalled == 1);
    return dtorCalled;
}
static assert(numDtorCallsByGotoOutOfScope() == 1);


uint numDtorCallsByGotoDifferentScopeAfter()
{
    uint dtorCalled;
    {
        S12603 s = S12603(&dtorCalled);
        assert(dtorCalled == 0);
    }
    assert(dtorCalled == 1);
    goto L_abc;
    L_abc:
    assert(dtorCalled == 1);
    return dtorCalled;
}
static assert(numDtorCallsByGotoDifferentScopeAfter() == 1);


auto numDtorCallsByGotoDifferentScopeBefore()
{
    uint dtorCalled;
    assert(dtorCalled == 0);
    goto L_abc;
    L_abc:
    assert(dtorCalled == 0);
    {
        S12603 s = S12603(&dtorCalled);
        assert(dtorCalled == 0);
    }
    assert(dtorCalled == 1);
    return dtorCalled;
}
static assert(numDtorCallsByGotoDifferentScopeBefore() == 1);


struct S12603_2
{
    ~this()
    {
        dtorCalled = true;
    }

    bool dtorCalled = false;
}

auto structInCaseScope()
{
    auto charsets = S12603_2();
    switch(1)
    {
    case 0:
        auto set = charsets;
        break;
    default:
        break;
    }
    return charsets.dtorCalled;
}

static assert(!structInCaseScope());

/**************************************************
    15233 - ICE in TupleExp, Copy On Write bug
**************************************************/

alias TT15233(stuff ...) = stuff;

struct Tok15233 {}
enum tup15233 = TT15233!(Tok15233(), "foo");
static assert(tup15233[0] == Tok15233());
static assert(tup15233[1] == "foo");

/**************************************************
    15251 - void cast in ForStatement.increment
**************************************************/

int test15251()
{
    for (ubyte lwr = 19;
        lwr != 20;
        cast(void)++lwr)    // have to to be evaluated with ctfeNeedNothing
    {}
    return 1;
}
static assert(test15251());

/**************************************************
    15998 - Sagfault caused by memory corruption
**************************************************/

immutable string[2] foo15998 = ["",""];
immutable string[2][] bar15998a = foo15998 ~ baz15998;
immutable string[2][] bar15998b = baz15998 ~ foo15998;

auto baz15998()
{
    immutable(string[2])[] r;
    return r;
}

static assert(bar15998a == [["", ""]]);
static assert(bar15998b == [["", ""]]);

/**************************************************
    16094 - Non-overlapped slice assignment on an aggregate
**************************************************/

char[] f16094a()
{
    char[] x = new char[](6);
    x[3..6] = x[0..3];
    return x;
}

int[] f16094b()
{
    int[] x = new int[](6);
    x[3..6] = x[0..3];
    return x;
}

enum copy16094a = f16094a();
enum copy16094b = f16094b();

/**************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17407

bool foo17407()
{
    void delegate ( ) longest_convert;
    return __traits(compiles, longest_convert = &doesNotExists);
}

static assert(!foo17407);

/**************************************************/
// https://issues.dlang.org/show_bug.cgi?id=18057
// Recursive field initializer causes segfault.

struct RBNode(T)
{
    RBNode!T *copy = new RBNode!T;
}

static assert(!__traits(compiles, { alias bug18057 = RBNode!int; }));

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9937

int test9937()
{
    import core.math;

    float x = float.max;
    x *= 2;
    x = toPrec!float(x);
    x /= 2;
    assert(x == float.infinity);
    return 1;
}

static assert(test9937());
