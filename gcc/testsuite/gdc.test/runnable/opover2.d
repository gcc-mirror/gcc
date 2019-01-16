// PERMUTE_ARGS: -inline -O -property
// REQUIRED_ARGS: -dip25

// Test operator overloading

extern (C) int printf(const(char*) fmt, ...);

template Seq(T...){ alias T Seq; }

bool thrown(E, T)(lazy T val)
{
    try { val(); return false; }
    catch (E e) { return true; }
}

void stompStack() { int[256] sa = 0xdeadbeef; }

/**************************************/

class A
{
    string opUnary(string s)()
    {
        printf("A.opUnary!(%.*s)\n", s.length, s.ptr);
        return s;
    }
}

void test1()
{
    auto a = new A();

    +a;
    -a;
    ~a;
    *a;
    ++a;
    --a;

    auto x = a++;
    assert(x == a);
    auto y = a--;
    assert(y == a);
}

/**************************************/

class A2
{
    T opCast(T)()
    {
        auto s = T.stringof;
        printf("A.opCast!(%.*s)\n", s.length, s.ptr);
        return T.init;
    }
}


void test2()
{
    auto a = new A2();

    auto x = cast(int)a;
    assert(x == 0);

    auto y = cast(char)a;
    assert(y == char.init);
}

/**************************************/

struct A3
{
    int opBinary(string s)(int i)
    {
        printf("A.opBinary!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    int opBinaryRight(string s)(int i) if (s == "/" || s == "*")
    {
        printf("A.opBinaryRight!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    T opCast(T)()
    {
        auto s = T.stringof;
        printf("A.opCast!(%.*s)\n", s.length, s.ptr);
        return T.init;
    }
}


void test3()
{
    A3 a;

    a + 3;
    4 * a;
    4 / a;
    a & 5;
}

/**************************************/

struct A4
{
    int opUnary(string s)()
    {
        printf("A.opUnary!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    T opCast(T)()
    {
        auto s = T.stringof;
        printf("A.opCast!(%.*s)\n", s.length, s.ptr);
        return T.init;
    }
}


void test4()
{
    A4 a;

    if (a)
        int x = 3;
    if (!a)
        int x = 3;
    if (!!a)
        int x = 3;
}

/**************************************/

class A5
{
    override bool opEquals(Object o)
    {
        printf("A.opEquals!(%p)\n", o);
        return 1;
    }

    int opUnary(string s)()
    {
        printf("A.opUnary!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    T opCast(T)()
    {
        auto s = T.stringof;
        printf("A.opCast!(%.*s)\n", s.length, s.ptr);
        return T.init;
    }
}

class B5 : A5
{
    override bool opEquals(Object o)
    {
        printf("B.opEquals!(%p)\n", o);
        return 1;
    }
}


void test5()
{
    A5 a = new A5();
    A5 a2 = new A5();
    B5 b = new B5();
    A n = null;

    if (a == a)
        int x = 3;
    if (a == a2)
        int x = 3;
    if (a == b)
        int x = 3;
    if (a == n)
        int x = 3;
    if (n == a)
        int x = 3;
    if (n == n)
        int x = 3;
}

/**************************************/

struct S6
{
    const bool opEquals(ref const S6 b)
    {
        printf("S.opEquals(S %p)\n", &b);
        return true;
    }

    const bool opEquals(ref const T6 b)
    {
        printf("S.opEquals(T %p)\n", &b);
        return true;
    }
}

struct T6
{
    const bool opEquals(ref const T6 b)
    {
        printf("T.opEquals(T %p)\n", &b);
        return true;
    }
/+
    const bool opEquals(ref const S6 b)
    {
        printf("T.opEquals(S %p)\n", &b);
        return true;
    }
+/
}


void test6()
{
    S6 s1;
    S6 s2;

    if (s1 == s2)
        int x = 3;

    T6 t;

    if (s1 == t)
        int x = 3;

    if (t == s2)
        int x = 3;
}

/**************************************/

struct S7
{
    const int opCmp(ref const S7 b)
    {
        printf("S.opCmp(S %p)\n", &b);
        return -1;
    }

    const int opCmp(ref const T7 b)
    {
        printf("S.opCmp(T %p)\n", &b);
        return -1;
    }
}

struct T7
{
    const int opCmp(ref const T7 b)
    {
        printf("T.opCmp(T %p)\n", &b);
        return -1;
    }
/+
    const int opCmp(ref const S7 b)
    {
        printf("T.opCmp(S %p)\n", &b);
        return -1;
    }
+/
}


void test7()
{
    S7 s1;
    S7 s2;

    if (s1 < s2)
        int x = 3;

    T7 t;

    if (s1 < t)
        int x = 3;

    if (t < s2)
        int x = 3;
}

/**************************************/

struct A8
{
    int opUnary(string s)()
    {
        printf("A.opUnary!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    int opIndexUnary(string s, T)(T i)
    {
        printf("A.opIndexUnary!(%.*s)(%d)\n", s.length, s.ptr, i);
        return 0;
    }

    int opIndexUnary(string s, T)(T i, T j)
    {
        printf("A.opIndexUnary!(%.*s)(%d, %d)\n", s.length, s.ptr, i, j);
        return 0;
    }

    int opSliceUnary(string s)()
    {
        printf("A.opSliceUnary!(%.*s)()\n", s.length, s.ptr);
        return 0;
    }

    int opSliceUnary(string s, T)(T i, T j)
    {
        printf("A.opSliceUnary!(%.*s)(%d, %d)\n", s.length, s.ptr, i, j);
        return 0;
    }
}


void test8()
{
    A8 a;

    -a;
    -a[3];
    -a[3, 4];
    -a[];
    -a[5 .. 6];
    --a[3];
}

/**************************************/

struct A9
{
    int opOpAssign(string s)(int i)
    {
        printf("A.opOpAssign!(%.*s)\n", s.length, s.ptr);
        return 0;
    }

    int opIndexOpAssign(string s, T)(int v, T i)
    {
        printf("A.opIndexOpAssign!(%.*s)(%d, %d)\n", s.length, s.ptr, v, i);
        return 0;
    }

    int opIndexOpAssign(string s, T)(int v, T i, T j)
    {
        printf("A.opIndexOpAssign!(%.*s)(%d, %d, %d)\n", s.length, s.ptr, v, i, j);
        return 0;
    }

    int opSliceOpAssign(string s)(int v)
    {
        printf("A.opSliceOpAssign!(%.*s)(%d)\n", s.length, s.ptr, v);
        return 0;
    }

    int opSliceOpAssign(string s, T)(int v, T i, T j)
    {
        printf("A.opSliceOpAssign!(%.*s)(%d, %d, %d)\n", s.length, s.ptr, v, i, j);
        return 0;
    }
}


void test9()
{
    A9 a;

    a += 8;
    a -= 8;
    a *= 8;
    a /= 8;
    a %= 8;
    a &= 8;
    a |= 8;
    a ^= 8;
    a <<= 8;
    a >>= 8;
    a >>>= 8;
    a ~= 8;
    a ^^= 8;

    a[3] += 8;
    a[3] -= 8;
    a[3] *= 8;
    a[3] /= 8;
    a[3] %= 8;
    a[3] &= 8;
    a[3] |= 8;
    a[3] ^= 8;
    a[3] <<= 8;
    a[3] >>= 8;
    a[3] >>>= 8;
    a[3] ~= 8;
    a[3] ^^= 8;

    a[3, 4] += 8;
    a[] += 8;
    a[5 .. 6] += 8;
}

/**************************************/

struct BigInt
{
    int opEquals(T)(T n) const
    {
        return 1;
    }

    int opEquals(T:int)(T n) const
    {
        return 1;
    }

    int opEquals(T:const(BigInt))(T n) const
    {
        return 1;
    }

}

int decimal(BigInt b, const BigInt c)
{
    while (b != c) {
    }
    return 1;
}

/**************************************/

struct Foo10
{
    int opUnary(string op)() { return 1; }
}

void test10()
{
    Foo10 foo;
    foo++;
}

/**************************************/

struct S4913
{
    bool opCast(T : bool)() { return true; }
}

int bug4913()
{
    if (S4913 s = S4913()) { return 83; }
    return 9;
}

static assert(bug4913() == 83);

/**************************************/
// 5551

struct Foo11 {
    Foo11 opUnary(string op:"++")() {
        return this;
    }
    Foo11 opBinary(string op)(int y) {
        return this;
    }
}

void test11()
{
    auto f = Foo11();
    f++;
}

/**************************************/
// 4099

struct X4099
{
    int x;
    alias x this;

    typeof(this) opUnary (string operator) ()
    {
        printf("operator called\n");
        return this;
    }
}

void test4099()
{
    X4099 x;
    X4099 r1 = ++x; //operator called
    X4099 r2 = x++; //BUG! (alias this used. returns int)
}

/**************************************/

void test12()
{
    static int opeq;

    // xopEquals OK
    static struct S1a { const bool opEquals(    const typeof(this) rhs) { ++opeq; return false; } }
    static struct S1b { const bool opEquals(ref const typeof(this) rhs) { ++opeq; return false; } }
    static struct S1c { const bool opEquals(          typeof(this) rhs) { ++opeq; return false; } }

    // xopEquals NG
    static struct S2a {       bool opEquals(          typeof(this) rhs) { ++opeq; return false; } }

    foreach (S; Seq!(S1a, S1b, S1c))
    {
        S s;
        opeq = 0;
        assert(s != s);                     // call opEquals directly
        assert(!typeid(S).equals(&s, &s));  // -> xopEquals (-> __xopEquals) -> opEquals
        assert(opeq == 2);
    }

    foreach (S; Seq!(S2a))
    {
        S s;
        opeq = 0;
        assert(s != s);
        assert(thrown!Error(!typeid(S).equals(&s, &s)));
            // Error("notImplemented") thrown
        assert(opeq == 1);
    }
}

/**************************************/

void test13()
{
    static int opeq;

    struct X
    {
        const bool opEquals(const X){ ++opeq; return false; }
    }
    struct S
    {
        X x;
    }

    S makeS(){ return S(); }

    S s;
    opeq = 0;
    assert(s != s);
    assert(makeS() != s);
    assert(s != makeS());
    assert(makeS() != makeS());
    assert(opeq == 4);

    // built-in opEquals == const bool opEquals(const S rhs);
    assert(s != s);
    assert(opeq == 5);

    // xopEquals
    assert(!typeid(S).equals(&s, &s));
    assert(opeq == 6);
}

/**************************************/

void test14()
{
    static int opeq;

    struct S
    {
        const bool opEquals(T)(const T rhs) { ++opeq; return false; }
    }

    S makeS(){ return S(); }

    S s;
    opeq = 0;
    assert(s != s);
    assert(makeS() != s);
    assert(s != makeS());
    assert(makeS() != makeS());
    assert(opeq == 4);

    // xopEquals (-> __xxopEquals) -> template opEquals
    assert(!typeid(S).equals(&s, &s));
    assert(opeq == 5);
}

/**************************************/

void test15()
{
    struct S
    {
        const bool opEquals(T)(const(T) rhs)
        if (!is(T == S))
        { return false; }

        @disable const bool opEquals(T)(const(T) rhs)
        if (is(T == S))
        { return false; }
    }

    S makeS(){ return S(); }

    S s;
    static assert(!__traits(compiles, s != s));
    static assert(!__traits(compiles, makeS() != s));
    static assert(!__traits(compiles, s != makeS()));
    static assert(!__traits(compiles, makeS() != makeS()));

    // xopEquals (-> __xxopEquals) -> Error thrown
    assert(thrown!Error(!typeid(S).equals(&s, &s)));
}

/**************************************/

void test16()
{
    struct X
    {
        int n;
        const bool opEquals(T)(T t)
        {
            return false;
        }
    }
    struct S
    {
        X x;
    }

    S s1, s2;
    assert(s1 != s2);
        // field template opEquals should call
}

/**************************************/

void test17()
{
    static int opeq = 0;

    struct S
    {
        bool opEquals(ref S rhs) { ++opeq; return false; }
    }
    S[] sa1 = new S[3];
    S[] sa2 = new S[3];
    assert(sa1 != sa2);     // isn't used TypeInfo.equals
    assert(opeq == 1);

    const(S)[] csa = new const(S)[3];
    static assert(!__traits(compiles, csa == sa1));
    static assert(!__traits(compiles, sa1 == csa));
    static assert(!__traits(compiles, csa == csa));
}

/**************************************/
// 3789

bool test3789()
{
    static struct Float
    {
        double x;
    }
    Float f;
    assert(f.x != f.x); // NaN != NaN
    assert(f != f);

    static struct Array
    {
        int[] x;
    }
    Array a1 = Array([1,2,3].dup);
    Array a2 = Array([1,2,3].dup);
    if (!__ctfe)
    {   // Currently doesn't work this in CTFE - may or may not a bug.
        assert(a1.x !is a2.x);
    }
    assert(a1.x == a2.x);
    assert(a1 == a2);

    static struct AA
    {
        int[int] x;
    }
    AA aa1 = AA([1:1,2:2,3:3]);
    AA aa2 = AA([1:1,2:2,3:3]);
    if (!__ctfe)
    {   // Currently doesn't work this in CTFE - may or may not a bug.
        assert(aa1.x !is aa2.x);
    }
    if (!__ctfe)
    {   // This is definitely a bug. Should work in CTFE.
        assert(aa1.x == aa2.x);
        assert(aa1 == aa2);
    }

    if (!__ctfe)
    {   // Currently union operation is not supported in CTFE.
        union U1
        {
            double x;
        }
        static struct UnionA
        {
            int[] a;
            U1 u;
        }
        auto ua1 = UnionA([1,2,3]);
        auto ua2 = UnionA([1,2,3]);
        assert(ua1.u.x is ua2.u.x);
        assert(ua1.u.x != ua2.u.x);
        assert(ua1 == ua2);
        ua1.u.x = 1.0;
        ua2.u.x = 1.0;
        assert(ua1.u.x is ua2.u.x);
        assert(ua1.u.x == ua2.u.x);
        assert(ua1 == ua2);
        ua1.u.x = double.nan;
        assert(ua1.u.x !is ua2.u.x);
        assert(ua1.u.x !=  ua2.u.x);
        assert(ua1 != ua2);

        union U2
        {
            int[] a;
        }
        static struct UnionB
        {
            double x;
            U2 u;
        }
        auto ub1 = UnionB(1.0);
        auto ub2 = UnionB(1.0);
        assert(ub1 == ub2);
        ub1.u.a = [1,2,3].dup;
        ub2.u.a = [1,2,3].dup;
        assert(ub1.u.a !is ub2.u.a);
        assert(ub1.u.a  == ub2.u.a);
        assert(ub1 != ub2);
        ub2.u.a = ub1.u.a;
        assert(ub1.u.a is ub2.u.a);
        assert(ub1.u.a == ub2.u.a);
        assert(ub1 == ub2);
    }

    if (!__ctfe)
    {   // This is definitely a bug. Should work in CTFE.
        static struct Class
        {
            Object x;
        }
        static class X
        {
            override bool opEquals(Object o){ return true; }
        }

        Class c1a = Class(new Object());
        Class c2a = Class(new Object());
        assert(c1a.x !is c2a.x);
        assert(c1a.x != c2a.x);
        assert(c1a != c2a); // Pass, Object.opEquals works like bitwise compare

        Class c1b = Class(new X());
        Class c2b = Class(new X());
        assert(c1b.x !is c2b.x);
        assert(c1b.x == c2b.x);
        assert(c1b == c2b); // Fails, should pass
    }
    return true;
}
static assert(test3789());

/**************************************/
// 10037

struct S10037
{
    bool opEquals(ref const S10037) { assert(0); }
}

struct T10037
{
    S10037 s;
    // Compiler should not generate 'opEquals' here implicitly:
}

struct Sub10037(TL...)
{
    TL data;
    int value;
    alias value this;
}

void test10037()
{
    S10037 s;
    T10037 t;
    static assert( __traits(hasMember, S10037, "opEquals"));
    static assert(!__traits(hasMember, T10037, "opEquals"));
    assert(thrown!Error(s == s));
    assert(thrown!Error(t == t));

    Sub10037!(S10037) lhs;
    Sub10037!(S10037) rhs;
    static assert(!__traits(hasMember, Sub10037!(S10037), "opEquals"));
    assert(lhs == rhs);     // lowered to: lhs.value == rhs.value
}

/**************************************/
// 5810

struct Bug5810
{
    void opUnary(string op)() {}
}

struct Foo5810
{
    Bug5810 x;
    void bar() { x++; }
}

/**************************************/
// 6798

struct Tuple6798(T...)
{
    T field;
    alias field this;

    bool opEquals(Tuple6798 rhs)
    {
        foreach (i, _; T)
        {
            if (this[i] != rhs[i])
                return false;
        }
        return true;
    }
}
auto tuple6798(T...)(T args)
{
    return Tuple6798!T(args);
}

int test6798a()
{
    //import std.typecons;
    alias tuple6798 tuple;

    static struct S1
    {
        auto opDollar(size_t dim)()
        {
            return 99;
        }
        auto opSlice(int dim)(int lwr, int upr)
        {
            return [dim, lwr, upr];
        }

        auto opIndex(A...)(A indices)
        {
            return tuple(" []", indices);
        }
        auto opIndexUnary(string op, A...)(A indices)
        {
            return tuple(op~"[]", indices);
        }
        auto opIndexAssign(A...)(string s, A indices)
        {
            return tuple("[] =", s, indices);
        }
        auto opIndexOpAssign(string op, A...)(string s, A indices)
        {
            return tuple("[]"~op~"=", s, indices);
        }
    }
    S1 s1;
    assert( s1[]       == tuple(" []"));
    assert( s1[10]     == tuple(" []", 10));
    assert( s1[10, 20] == tuple(" []", 10, 20));
    assert( s1[10..20] == tuple(" []", [0, 10, 20]));
    assert(+s1[]       == tuple("+[]"));
    assert(-s1[10]     == tuple("-[]", 10));
    assert(*s1[10, 20] == tuple("*[]", 10, 20));
    assert(~s1[10..20] == tuple("~[]", [0, 10, 20]));
    assert((s1[]       ="x") == tuple("[] =", "x"));
    assert((s1[10]     ="x") == tuple("[] =", "x", 10));
    assert((s1[10, 20] ="x") == tuple("[] =", "x", 10, 20));
    assert((s1[10..20] ="x") == tuple("[] =", "x", [0, 10, 20]));
    assert((s1[]      +="x") == tuple("[]+=", "x"));
    assert((s1[10]    -="x") == tuple("[]-=", "x", 10));
    assert((s1[10, 20]*="x") == tuple("[]*=", "x", 10, 20));
    assert((s1[10..20]~="x") == tuple("[]~=", "x", [0, 10, 20]));
    assert( s1[20..30, 10]           == tuple(" []", [0, 20, 30], 10));
    assert( s1[10, 10..$, $-4, $..2] == tuple(" []", 10, [1,10,99], 99-4, [3,99,2]));
    assert(+s1[20..30, 10]           == tuple("+[]", [0, 20, 30], 10));
    assert(-s1[10, 10..$, $-4, $..2] == tuple("-[]", 10, [1,10,99], 99-4, [3,99,2]));
    assert((s1[20..30, 10]           ="x") == tuple("[] =", "x", [0, 20, 30], 10));
    assert((s1[10, 10..$, $-4, $..2] ="x") == tuple("[] =", "x", 10, [1,10,99], 99-4, [3,99,2]));
    assert((s1[20..30, 10]          +="x") == tuple("[]+=", "x", [0, 20, 30], 10));
    assert((s1[10, 10..$, $-4, $..2]-="x") == tuple("[]-=", "x", 10, [1,10,99], 99-4, [3,99,2]));

    // opIndex exist, but opSlice for multi-dimensional doesn't.
    static struct S2
    {
        auto opSlice(size_t dim)() { return [dim]; }
        auto opSlice()(size_t lwr, size_t upr) { return [lwr, upr]; }

        auto opIndex(A...)(A indices){ return [[indices]]; }
    }
    S2 s2;
    assert(s2[] == [[]]);
    assert(s2[1] == [[1]]);
    assert(s2[1, 2] == [[1, 2]]);
    assert(s2[1..2] == [1, 2]);
    static assert(!__traits(compiles, s2[1, 2..3] ));
    static assert(!__traits(compiles, s2[1..2, 2..3] ));

    // opSlice for multi-dimensional exists, but opIndex for that doesn't.
    static struct S3
    {
        auto opSlice(size_t dim)(size_t lwr, size_t upr) { return [lwr, upr]; }

        auto opIndex(size_t n){ return [[n]]; }
        auto opIndex(size_t n, size_t m){ return [[n, m]]; }
    }
    S3 s3;
    static assert(!__traits(compiles, s3[] ));
    assert(s3[1]    == [[1]]);
    assert(s3[1, 2] == [[1, 2]]);
    static assert(!__traits(compiles, s3[1..2] ));
    static assert(!__traits(compiles, s3[1, 2..3] ));
    static assert(!__traits(compiles, s3[1..2, 2..3] ));

    return 0;
}

int test6798b()
{
    static struct Typedef(T)
    {
        private T Typedef_payload = T.init;

        alias a = Typedef_payload;

        auto ref opIndex(this X, D...)(auto ref D i)    { return a[i]; }
        auto ref opSlice(this X      )()                { return a[]; }
        auto ref opSlice(this X, B, E)(auto ref B b, auto ref E e)
        {
            assert(b == 0 && e == 3);
            return a[b..e];
        }

        template opDispatch(string name)
        {
            // field or property function
            @property auto ref opDispatch(this X)()                { return mixin("a."~name);        }
            @property auto ref opDispatch(this X, V)(auto ref V v) { return mixin("a."~name~" = v"); }
        }

        static if (is(typeof(a) : E[], E))
        {
            auto opDollar() const { return a.length; }
        }
    }

    Typedef!(int[]) dollar2;
    dollar2.length = 3;
    assert(dollar2.Typedef_payload.length == 3);
    assert(dollar2[0 .. $] is dollar2[0 .. 3]);

    return 0;
}

int test6798c()
{
    alias T = Tuple6798!(int, int);
    auto n = T[].init;
    static assert(is(typeof(n[0]) == Tuple6798!(int, int)));

    return 0;
}

void test6798()
{
    static assert(test6798a() == 0);    // CTFE check
    test6798a();
    static assert(test6798b() == 0);
    test6798b();
    static assert(test6798c() == 0);
    test6798c();
}

/**************************************/
// 12382

struct S12382
{
    size_t opDollar() { return 0; }
    size_t opIndex(size_t) { return 0; }
}

S12382 func12382() { return S12382(); }

static assert(S12382.init[$] == 0);
static assert(func12382()[$] == 0);
enum e12382a = S12382.init[$];
enum e12382b = func12382()[$];
static v12382a = S12382.init[$];
static v12382b = func12382()[$];

void test12382()
{
    static assert(S12382.init[$] == 0);
    static assert(func12382()[$] == 0);
    enum e12382a = S12382.init[$];
    enum e12382b = func12382()[$];
    static v12382a = S12382.init[$];
    static v12382b = func12382()[$];
}

/**************************************/
// 12904

struct S12904
{
    void opIndexAssign(U, A...)(U value, A args)
    {
        static assert(0);
    }
    void opSliceAssign(int n)
    {
        assert(n == 10);
    }

    size_t opDollar(size_t dim)()
    {
        return 7;
    }

    int opSlice(size_t dim)(size_t, size_t to)
    {
        assert(to == 7);
        return 1;
    }

    int opIndex(int i1, int i2)
    {
        assert(i1 == 1 && i2 == 1);
        return 10;
    }
}

void test12904()
{
    S12904 s;
    s[] = s[0..$, 1];
    s[] = s[0..$, 0..$];
}

/**************************************/
// 7641

mixin template Proxy7641(alias a)
{
    auto ref opBinaryRight(string op, B)(auto ref B b)
    {
        return mixin("b "~op~" a");
    }
}
struct Typedef7641(T)
{
    private T Typedef_payload;

    this(T init)
    {
        Typedef_payload = init;
    }

    mixin Proxy7641!Typedef_payload;
}

void test7641()
{
    class C {}
    C c1 = new C();
    auto a = Typedef7641!C(c1);
    static assert(!__traits(compiles, { C c2 = a; }));
}

/**************************************/
// 8434

void test8434()
{
    static class Vector2D(T)
    {
        T x, y;

        this(T x, T y) {
            this.x = x;
            this.y = y;
        }

        U opCast(U)() const { assert(0); }
    }

    alias Vector2D!(short) Vector2s;
    alias Vector2D!(float) Vector2f;

    Vector2s vs1 = new Vector2s(42, 23);
    Vector2s vs2 = new Vector2s(42, 23);

    assert(vs1 != vs2);
}

/**************************************/

void test18()
{
    // one dimensional indexing
    static struct IndexExp
    {
        int[] opIndex(int a)
        {
            return [a];
        }

        int[] opIndexUnary(string op)(int a)
        {
            return [a];
        }

        int[] opIndexAssign(int val, int a)
        {
            return [val, a];
        }

        int[] opIndexOpAssign(string op)(int val, int a)
        {
            return [val, a];
        }

        int opDollar()
        {
            return 8;
        }
    }

    IndexExp index;
    // opIndex
    assert(index[8]     == [8]);
    assert(index[$]     == [8]);
    assert(index[$-1]   == [7]);
    assert(index[$-$/2] == [4]);
    // opIndexUnary
    assert(-index[8]     == [8]);
    assert(-index[$]     == [8]);
    assert(-index[$-1]   == [7]);
    assert(-index[$-$/2] == [4]);
    // opIndexAssign
    assert((index[8]     = 2) == [2, 8]);
    assert((index[$]     = 2) == [2, 8]);
    assert((index[$-1]   = 2) == [2, 7]);
    assert((index[$-$/2] = 2) == [2, 4]);
    // opIndexOpAssign
    assert((index[8]     += 2) == [2, 8]);
    assert((index[$]     += 2) == [2, 8]);
    assert((index[$-1]   += 2) == [2, 7]);
    assert((index[$-$/2] += 2) == [2, 4]);

    // opDollar is only one-dimensional
    static assert(!is(typeof(index[$, $])));
    static assert(!is(typeof(-index[$, $])));
    static assert(!is(typeof(index[$, $] = 2)));
    static assert(!is(typeof(index[$, $] += 2)));

    // multi dimensional indexing
    static struct ArrayExp
    {
        int[] opIndex(int a, int b)
        {
            return [a, b];
        }

        int[] opIndexUnary(string op)(int a, int b)
        {
            return [a, b];
        }

        int[] opIndexAssign(int val, int a, int b)
        {
            return [val, a, b];
        }

        int[] opIndexOpAssign(string op)(int val, int a, int b)
        {
            return [val, a, b];
        }

        int opDollar(int dim)()
        {
            return dim;
        }
    }

    ArrayExp array;
    // opIndex
    assert(array[8, 8]     == [8, 8]);
    assert(array[$, $]     == [0, 1]);
    assert(array[$, $-1]   == [0, 0]);
    assert(array[2, $-$/2] == [2, 1]);
    // opIndexUnary
    assert(-array[8, 8]     == [8, 8]);
    assert(-array[$, $]     == [0, 1]);
    assert(-array[$, $-1]   == [0, 0]);
    assert(-array[2, $-$/2] == [2, 1]);
    // opIndexAssign
    assert((array[8, 8]      = 2) == [2, 8, 8]);
    assert((array[$, $]      = 2) == [2, 0, 1]);
    assert((array[$, $-1]    = 2) == [2, 0, 0]);
    assert((array[2, $-$/2]  = 2) == [2, 2, 1]);
    // opIndexOpAssign
    assert((array[8, 8]      += 2) == [2, 8, 8]);
    assert((array[$, $]      += 2) == [2, 0, 1]);
    assert((array[$, $-1]    += 2) == [2, 0, 0]);
    assert((array[2, $-$/2]  += 2) == [2, 2, 1]);

    // one dimensional slicing
    static struct SliceExp
    {
        int[] opSlice(int a, int b)
        {
            return [a, b];
        }

        int[] opSliceUnary(string op)(int a, int b)
        {
            return [a, b];
        }

        int[] opSliceAssign(int val, int a, int b)
        {
            return [val, a, b];
        }

        int[] opSliceOpAssign(string op)(int val, int a, int b)
        {
            return [val, a, b];
        }

        int opDollar()
        {
            return 8;
        }
    }

    SliceExp slice;
    // opSlice
    assert(slice[0 .. 8]     == [0, 8]);
    assert(slice[0 .. $]     == [0, 8]);
    assert(slice[0 .. $-1]   == [0, 7]);
    assert(slice[$-3 .. $-1] == [5, 7]);
    // opSliceUnary
    assert(-slice[0 .. 8]     == [0, 8]);
    assert(-slice[0 .. $]     == [0, 8]);
    assert(-slice[0 .. $-1]   == [0, 7]);
    assert(-slice[$-3 .. $-1] == [5, 7]);
    // opSliceAssign
    assert((slice[0 .. 8]     = 2) == [2, 0, 8]);
    assert((slice[0 .. $]     = 2) == [2, 0, 8]);
    assert((slice[0 .. $-1]   = 2) == [2, 0, 7]);
    assert((slice[$-3 .. $-1] = 2) == [2, 5, 7]);
    // opSliceOpAssign
    assert((slice[0 .. 8]     += 2) == [2, 0, 8]);
    assert((slice[0 .. $]     += 2) == [2, 0, 8]);
    assert((slice[0 .. $-1]   += 2) == [2, 0, 7]);
    assert((slice[$-3 .. $-1] += 2) == [2, 5, 7]);

    // test different kinds of opDollar
    auto dollar(string opDollar)()
    {
        static struct Dollar
        {
            size_t opIndex(size_t a) { return a; }
            mixin(opDollar);
        }
        Dollar d;
        return d[$];
    }
    assert(dollar!q{@property size_t opDollar() { return 8; }}() == 8);
    assert(dollar!q{template opDollar(size_t dim) { enum opDollar = dim; }}() == 0);
    assert(dollar!q{static const size_t opDollar = 8;}() == 8);
    assert(dollar!q{enum opDollar = 8;}() == 8);
    assert(dollar!q{size_t length() { return 8; } alias length opDollar;}() == 8);
}

/**************************************/

void test19()
{
    static struct Foo
    {
        int[] opSlice(int a, int b)
        {
            return [a, b];
        }

        int opDollar(int dim)()
        {
            return dim;
        }
    }

    Foo foo;
    assert(foo[0 .. $] == [0, 0]);
}

/**************************************/
// 9453

struct Foo9453
{
    static int ctor = 0;

    this(string bar) { ++ctor; }

    void opIndex(size_t i) const {}
    void opSlice(size_t s, size_t e) const {}

    size_t opDollar(int dim)() const if (dim == 0) { return 1; }
}

void test9453()
{
    assert(Foo9453.ctor == 0);  Foo9453("bar")[$-1];
    assert(Foo9453.ctor == 1);  Foo9453("bar")[0..$];
    assert(Foo9453.ctor == 2);
}

/**************************************/
// 9496

struct S9496
{
    static S9496* ptr;

    size_t opDollar()
    {
        assert(ptr is &this);
        return 10;
    }
    void opSlice(size_t , size_t)
    {
        assert(ptr is &this);
    }
    void getSlice()
    {
        assert(ptr is &this);
        this[1 .. opDollar()];
        this[1 .. $];
    }
}

void test9496()
{
    S9496 s;
    S9496.ptr = &s;
    s.getSlice();
    s[1 .. $];
}

/**************************************/
// 9689

struct B9689(T)
{
    T val;
    @disable this(this);

    bool opEquals(this X, B)(auto ref B b)
    {
        //pragma(msg, "+", X, ", B = ", B, ", ref = ", __traits(isRef, b));
        return this.val == b.val;
        //pragma(msg, "-", X, ", B = ", B, ", ref = ", __traits(isRef, b));
    }
}

struct S9689
{
    B9689!int num;
}

void test9689()
{
    B9689!S9689 b;
}

/**************************************/
// 9694

struct S9694
{
    bool opEquals(ref S9694 rhs)
    {
        assert(0);
    }
}
struct T9694
{
    S9694 s;
}
void test9694()
{
    T9694 t;
    assert(thrown!Error(typeid(T9694).equals(&t, &t)));
}

/**************************************/
// 10064

void test10064()
{
    static struct S
    {
        int x = 3;

        @disable this();

        this(int)
        { x = 7; }

        int opSlice(size_t, size_t)
        { return 0; }

        @property size_t opDollar()
        {
            assert(x == 7 || x == 3); // fails
            assert(x == 7);
            return 0;
        }
    }
    auto x = S(0)[0 .. $];
}

/**************************************/
// 12585

void test12585()
{
    struct Bar
    {
        int opIndex(size_t index)
        {
            return 0;
        }
    }

    struct Foo
    {
        Bar opIndex(size_t index)
        {
            throw new Exception("Fail");
        }
    }

    Foo foo()
    {
        return Foo();
    }

    void catchStuff(E)(lazy E expression)
    {
        try
            expression();
        catch (Exception e) {}
    }

    catchStuff(foo()[0][0]);          // OK <- NG
    catchStuff(foo().opIndex(0)[0]);  // OK
    catchStuff(foo()[0].opIndex(0));  // OK
    Foo f; catchStuff(f[0][0]);       // OK
}

/**************************************/
// 10394

void test10394()
{
    alias Seq!(int, int) Pair;
    Pair pair;

    struct S1
    {
        int opBinary(string op)(Pair) { return 1;  }
        bool opEquals(Pair) { return true; }
        int opOpAssign(string op)(Pair) { return 1; }
    }
    S1 s1;
    assert((s1 + pair) == 1);
    assert((s1 == pair) == true);
    assert((s1 *= pair) == 1);

    struct S2
    {
        int opBinaryRight(string op)(Pair lhs) { return 1;  }
        int opCmp(Pair) { return -1; }
    }
    S2 s2;
    assert((pair in s2) == 1);
    assert(s2 < pair);
}

/**************************************/
// 10597

struct R10597
{
    void opIndex(int) {}
    void opSlice(int, int) {}
    int opDollar();
}
R10597 r;

struct S10597
{
    static assert(is(typeof(r[0]))); //ok
    static assert(is(typeof(r[$]))); //fails

    static assert(is(typeof(r[0..0]))); //ok
    static assert(is(typeof(r[$..$]))); //fails

    void foo()
    {
        static assert(is(typeof(r[0]))); //ok
        static assert(is(typeof(r[$]))); //ok

        static assert(is(typeof(r[0..0]))); //ok
        static assert(is(typeof(r[$..$]))); //ok
    }
}

static assert(is(typeof(r[0]))); //ok
static assert(is(typeof(r[$]))); //fails

static assert(is(typeof(r[0..0]))); //ok
static assert(is(typeof(r[$..$]))); //fails

void test10597()
{
    static assert(is(typeof(r[0]))); //ok
    static assert(is(typeof(r[$]))); //ok

    static assert(is(typeof(r[0..0]))); //ok
    static assert(is(typeof(r[$..$]))); //ok
}

/**************************************/
// 10567

// doesn't require thunk
struct S10567x1n { int value; int opCmp(ref const S10567x1n rhs) const { return 0; } }

// requires thunk
struct S10567y1n { int value; int opCmp(const S10567y1n rhs) const { return 0; } }
struct S10567y1t { int value; int opCmp(S)(const S rhs) const { return 0; } }

// doesn't support const comparison
struct S10567z1n { int value; int opCmp(const S10567z1n rhs) { return 0; } }
struct S10567z1t { int value; int opCmp(S)(const S rhs) { return 0; } }

/+
struct S10567x2n { S10567x1n s; this(int n) { s = typeof(s)(n); } alias s this; }

struct S10567y2n { S10567y1n s; this(int n) { s = typeof(s)(n); } alias s this; }
struct S10567y2t { S10567y1t s; this(int n) { s = typeof(s)(n); } alias s this; }

struct S10567z2n { S10567z1n s; this(int n) { s = typeof(s)(n); } alias s this; }
struct S10567z2t { S10567z1t s; this(int n) { s = typeof(s)(n); } alias s this; }

struct S10567d1
{
    int value;
    int opDispatch(string name, S)(const S rhs) const if (name == "opCmp")
    { assert(0); }
}
struct S10567d2
{
    int value;
    template opDispatch(string name) if (name == "opCmp")
    {
        int opDispatch(const S rhs) const
        { assert(0); }
    }
}

// recursive alias this + opCmp searching
struct S10567r1
{
    static S10567r2 t;
    ref S10567r2 payload() { return t; }
    alias payload this;

    int opCmp(const S10567r1 s) const { return 0; }
}
struct S10567r2
{
    static S10567r1 s;
    ref S10567r1 payload() { return s; }
    alias payload this;
}
+/

void test10567()
{
    foreach (S; Seq!(S10567x1n/+, S10567x2n+/))
    {
        S sx = S(1);
        S sy = S(2);
        assert(!(sx < sy) && !(sx > sy));
        assert(sx.opCmp(sy) == 0);

        assert(typeid(S).compare(&sx, &sy) == 0);
        static if (is(S == S10567x1n))
            assert(cast(void*)typeid(S).xopCmp == cast(void*)&S.opCmp, S.stringof);
    }

    foreach (S; Seq!(S10567y1n, S10567y1t/+, S10567y2n, S10567y2t+/))
    {
        S sx = S(1);
        S sy = S(2);
        assert(!(sx < sy) && !(sx > sy));
        assert(sx.opCmp(sy) == 0);

        assert(typeid(S).compare(&sx, &sy) == 0);
    }

    foreach (S; Seq!(S10567z1n, S10567z1t/+, S10567z2n, S10567z2t+/))
    {
        S sx = S(1);
        S sy = S(2);
        assert(!(sx < sy) && !(sx > sy));
        assert(sx.opCmp(sy) == 0);

        try
        {
            auto x = typeid(S).compare(&sx, &sy);
            assert(0);
        }
        catch (Error e) { assert(e.msg[$-15 .. $] == "not implemented"); }
    }
/+
    foreach (S; Seq!(S10567d1, S10567d2))
    {
        int[S] aa;
        aa[S(1)] = 10;  aa[S(1)] = 1;
        aa[S(2)] = 20;  aa[S(2)] = 2;
        assert(aa.length == 2);
        foreach (k, v; aa)
            assert(k.value == v);

        S sx = S(1);
        S sy = S(2);

        // Don't invoke opDispatch!"opCmp"
        assert(typeid(S).compare(&sx, &sy) != 0);
    }
+/
}

/**************************************/
// 11062

struct S11062ia
{
    struct S1
    {
        void opIndexAssign(int val, int key) {}
    }
    struct S2
    {
        S1 headers;
    }

    private S2 m_obj;
    @property S2 get() { return m_obj; }
    alias get this;
}

struct S11062sa
{
    struct S1
    {
        void opSliceAssign(int val, int lwr, int upr) {}
    }
    struct S2
    {
        S1 headers;
    }

    private S2 m_obj;
    @property S2 get() { return m_obj; }
    alias get this;
}

void test11062()
{
    auto sia = S11062ia();
    sia.headers[1] = 1;     // bug

    auto ssa = S11062sa();
    ssa.headers[1..2] = 1;  // bug
}

/**************************************/
// 11311

void test11311()
{
    static int ctor, cpctor, dtor;

    static struct S
    {
        this(int)  { ++ctor; }
        this(this) { ++cpctor; }
        ~this()    { ++dtor; }
    }
    static struct Arr
    {
        S data;
        ref S opIndex(int) return { return data; }
        ref S opSlice(int, int) return { return data; }
    }

    {
        Arr a = Arr(S(1));
        assert(ctor == 1);
        assert(cpctor == 0);
        assert(dtor == 0);

        auto getA1() { return a; }
      //getA1().opIndex(1);  // OK
        getA1()[1];          // NG

        assert(ctor == 1);
        assert(cpctor == 1);  // getA() returns a copy of a
        assert(dtor == 1);    // temporary returned by getA() should be destroyed
    }
    assert(dtor == 2);
    assert(ctor + cpctor == dtor);

    ctor = cpctor = dtor = 0;

    {
        Arr a = Arr(S(1));
        assert(ctor == 1);
        assert(cpctor == 0);
        assert(dtor == 0);

        auto getA2() { return a; }
      //getA2().opSlice(1, 2);  // OK
        getA2()[1..2];          // NG

        assert(ctor == 1);
        assert(cpctor == 1);  // getA() returns a copy of a
        assert(dtor == 1);    // temporary returned by getA() should be destroyed
    }
    assert(dtor == 2);
    assert(ctor + cpctor == dtor);
}

/**************************************/
// 12193

void test12193()
{
    struct Foo
    {
        bool bar;
        alias bar this;
        void opOpAssign(string op)(size_t x)
        {
            bar = false;
        }
    }

    Foo foo;
    foo <<= 1;
}

/**************************************/
// 14057

struct W14057
{
    int[] subType;
    alias subType this;

    W14057 opSlice(size_t, size_t)
    {
        return this;
    }
}

void test14057()
{
    auto w = W14057();
    W14057 w2 = w[0 .. 1337];
}

/**************************************/

struct Tuple20(T...) { T field; alias field this; }

void test20a()
{
    // ae1save in in AssignExp::semantic
    int a, b;

    struct A1
    {
        void opIndexAssign(int v, Tuple20!(int, int) ) { a = v; }
        Tuple20!(int, int) opSlice(size_t dim)(int, int) { return typeof(return).init; }
    }
    struct A2
    {
        A1 a1;
        alias a1 this;
        int opIndexAssign(int) { return b; }
    }

    stompStack();
    A2 foo() { return A2(); }
    foo()[1..2] = 1;
    // ref A1 __tmp = foo().a1; __tmp.opIndexAssign(1, __tmp.opSlice!0(1, 2));
    assert(a == 1);     // should work
    assert(b == 0);
}

void test20b()
{
    // ae1save in UnaExp::op_overload()
    int a, b;

    struct A1
    {
        void opIndexUnary(string op)(Tuple20!(int, int) ) { a = 1; }
        Tuple20!(int, int) opSlice(size_t dim)(int, int) { return typeof(return).init; }
        void dummy() {} // nessary to make A1 nested struct
    }
    struct A2
    {
        A1 a1;
        alias a1 this;
        int opIndexUnary(string op)(int) { return 0; }
    }

    stompStack();
    A2 foo() { return A2(); }
    +foo()[1..2];
    // ref A1 __tmp = foo().a1; __tmp.opIndexUnary!"+"(__tmp.opSlice!0(1, 2));
    assert(a == 1);     // should pass
    assert(b == 0);
}

void test20c()
{
    // ae1save in ArrayExp::op_overload()
    int a, b;

    struct A1
    {
        void opIndex(Tuple20!(int, int) ) { a = 1; }
        Tuple20!(int, int) opSlice(size_t dim)(int, int) { return typeof(return).init; }
    }
    struct A2
    {
        A1 a1;
        alias a1 this;
        int opIndex(int) { return 0; }
    }

    stompStack();
    A2 foo() { return A2(); }
    foo()[1..2];
    // ref A1 __tmp = foo().a1; __tmp.opIndex(__tmp.opSlice!0(1, 2));
    assert(a == 1);     // should pass
    assert(b == 0);
}

void test20d()
{
    // ae1save in BinAssignExp::op_overload()
    int a, b;

    struct A1
    {
        void opIndexOpAssign(string op)(int v, Tuple20!(int, int) ) { a = v; }
        Tuple20!(int, int) opSlice(size_t dim)(int, int) { return typeof(return).init; }
        void dummy() {} // nessary to make A1 nested struct
    }
    struct A2
    {
        A1 a1;
        alias a1 this;
        ref int opIndexOpAssign(alias op)(int) { return b; }
    }

    stompStack();
    A2 foo() { return A2(); }
    foo()[1..2] += 1;
    // ref A1 __tmp = foo().a1; __tmp.opIndexOpAssign!"+"(1, __tmp.opSlice!0(1, 2));
    assert(a == 1);     // should pass
    assert(b == 0);
}

/**************************************/
// 14624

void test14624()
{
    struct A1
    {
        int x;
        ref int opIndex() return { return x; }
        ref int opSlice() { assert(0); }
    }
    {
        A1 a = A1(1);
        auto x = a[];       // a.opIndex()
        assert(x == a.x);
        auto y = -a[];      // -a.opIndex()        <-- not found: a.opIndexUnary!"-"
        assert(y == -a.x);
        a[] = 1;            // a.opIndex() = 1;    <-- not found: a.opIndexAssign(int)
        assert(a.x == 1);
        a[] += 1;           // a.opIndex() += 1;   <-- not found: a.opIndexOpAssign!"+"(int)
        assert(a.x == 2);
    }

    struct A2
    {
        int x;
        ref int opIndex() return               { x = 10; return x; }
        ref int opSlice() { assert(0); }
        ref int opSliceUnary(alias op)()       { x = 11; return x; }
        ref int opSliceAssign(int) return      { x = 12; return x; }
        ref int opSliceOpAssign(alias op)(int) { x = 13; return x; }
    }
    {
        A2 a = A2(1);
        auto x = a[];       // a.opIndex()
        assert(a.x == 10);
        auto y = -a[];      // a.opSliceUnary!"-"()     is preferred than: -a.opIndex()
        assert(a.x == 11);
        a[] = 1;            // a.opSliceAssign(1)       is preferred than: a.opIndex() = 1;
        assert(a.x == 12);
        a[] += 1;           // a.opSliceOpAssign!"+"(1) is preferred than: a.opIndex() += 1;
        assert(a.x == 13);
    }
}

/**************************************/
// 14625

void test14625()
{
    struct R
    {
        @property bool empty() { return true; }
        @property int front() { return 0; }
        void popFront() {}
    }

    struct C1
    {
        R opIndex() { return R(); }
        R opSlice() { assert(0); }
    }
    C1 c1;
    foreach (e; c1) {}      // OK <- asserts in opSlice()
    foreach (e; c1[]) {}    // OK, opIndex()

    struct C2
    {
        R opIndex() { return R(); }
    }
    C2 c2;
    foreach (e; c2) {}      // OK <- rejected
    foreach (e; c2[]) {}    // OK, opIndex()
}

/**************************************/

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
    test4099();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test3789();
    test10037();
    test6798();
    test12904();
    test7641();
    test8434();
    test18();
    test19();
    test9453();
    test9496();
    test9689();
    test9694();
    test10064();
    test12585();
    test10394();
    test10567();
    test11062();
    test11311();
    test14057();
    test20a();
    test20b();
    test20c();
    test20d();
    test14624();
    test14625();

    printf("Success\n");
    return 0;
}

