/*
TEST_OUTPUT:
---
\	S1	S2a	S2b	S3a	S3b	S4a	S4b
-	true	true	true	true	true	true	true
Xa	true	true	true	true	true	true	true
Xb	true	true	true	true	true	true	true
Xc	true	true	true	true	true	true	true
Xd	true	true	true	true	true	true	true
Xe	true	true	true	true	true	true	true
Xf	true	true	true	true	true	true	true
Xg	true	true	true	true	true	true	true
---

RUN_OUTPUT:
---
Success
---
*/

import core.stdc.stdio;

template TypeTuple(T...){ alias T TypeTuple; }

/***************************************************/
// 2625

struct Pair {
    immutable uint g1;
    uint g2;
}

void test1() {
    Pair[1] stuff;
    static assert(!__traits(compiles, (stuff[0] = Pair(1, 2))));
}

/***************************************************/
// 5327

struct ID
{
    immutable int value;
}

struct Data
{
    ID id;
}
void test2()
{
    Data data = Data(ID(1));
    immutable int* val = &data.id.value;
    static assert(!__traits(compiles, data = Data(ID(2))));
}

/***************************************************/

struct S31A
{
    union
    {
        immutable int field1;
        immutable int field2;
    }

    enum result = false;
}
struct S31B
{
    union
    {
        immutable int field1;
        int field2;
    }

    enum result = true;
}
struct S31C
{
    union
    {
        int field1;
        immutable int field2;
    }

    enum result = true;
}
struct S31D
{
    union
    {
        int field1;
        int field2;
    }

    enum result = true;
}

struct S32A
{
    int dummy0;
    union
    {
        immutable int field1;
        int field2;
    }

    enum result = true;
}
struct S32B
{
    immutable int dummy0;
    union
    {
        immutable int field1;
        int field2;
    }

    enum result = false;
}


struct S32C
{
    union
    {
        immutable int field1;
        int field2;
    }
    int dummy1;

    enum result = true;
}
struct S32D
{
    union
    {
        immutable int field1;
        int field2;
    }
    immutable int dummy1;

    enum result = false;
}

void test3()
{
    foreach (S; TypeTuple!(S31A,S31B,S31C,S31D, S32A,S32B,S32C,S32D))
    {
        S s;
        static assert(__traits(compiles, s = s) == S.result);
    }
}

/***************************************************/
// 3511

struct S4
{
    private int _prop = 42;
    ref int property() { return _prop; }
}

void test4()
{
    S4 s;
    assert(s.property == 42);
    s.property = 23;    // Rewrite to s.property() = 23
    assert(s.property == 23);
}

/***************************************************/

struct S5
{
    int mX;
    string mY;

    ref int x()
    {
        return mX;
    }
    ref string y()
    {
        return mY;
    }

    ref int err(Object)
    {
        static int v;
        return v;
    }
}

void test5()
{
    S5 s;
    s.x += 4;
    assert(s.mX == 4);
    s.x -= 2;
    assert(s.mX == 2);
    s.x *= 4;
    assert(s.mX == 8);
    s.x /= 2;
    assert(s.mX == 4);
    s.x %= 3;
    assert(s.mX == 1);
    s.x <<= 3;
    assert(s.mX == 8);
    s.x >>= 1;
    assert(s.mX == 4);
    s.x >>>= 1;
    assert(s.mX == 2);
    s.x &= 0xF;
    assert(s.mX == 0x2);
    s.x |= 0x8;
    assert(s.mX == 0xA);
    s.x ^= 0xF;
    assert(s.mX == 0x5);

    s.x ^^= 2;
    assert(s.mX == 25);

    s.mY = "ABC";
    s.y ~= "def";
    assert(s.mY == "ABCdef");

    static assert(!__traits(compiles, s.err += 1));
}

/***************************************************/
// 4424

void test4424()
{
    static struct S
    {
        this(this) {}
        void opAssign(T)(T rhs) if (!is(T == S)) {}
    }
}

/***************************************************/
// 6174

struct CtorTest6174(Data)
{
    const Data data;

    const Data[2] sa1;
    const Data[2][1] sa2;
    const Data[][2] sa3;

    const Data[] da1;
    const Data[2][] da2;

    this(Data a)
    {
        auto pdata = &data;

        // If compiler can determine that an assignment really sets the fields
        // which belongs to `this` object, it can bypass const qualifier.
        // For example, sa3, da1, da2, and pdata have indirections.
        // As long as you don't try to rewrite values beyond the indirections,
        // an assignment will always be succeeded inside constructor.

        static assert( is(typeof( data        = a         )));    // OK
      static if (is(Data == struct))
      {
        static assert( is(typeof( data.x      = 1         )));    // OK
        static assert( is(typeof( data.y      = 2         )));    // OK
      }
        static assert(!is(typeof( *pdata      = a         )));    // NG
        static assert( is(typeof( *&data      = a         )));    // OK

        static assert( is(typeof( sa1         = [a,a]     )));    // OK
        static assert( is(typeof( sa1[0]      = a         )));    // OK
        static assert( is(typeof( sa1[]       = a         )));    // OK
        static assert( is(typeof( sa1[][]     = a         )));    // OK

        static assert( is(typeof( sa2         = [[a,a]]   )));    // OK
        static assert( is(typeof( sa2[0][0]   = a         )));    // OK
        static assert( is(typeof( sa2[][0][]  = a         )));    // OK
        static assert( is(typeof( sa2[0][][0] = a         )));    // OK

        static assert( is(typeof( sa3         = [[a],[]]  )));    // OK
        static assert( is(typeof( sa3[0]      = [a,a]     )));    // OK
        static assert(!is(typeof( sa3[0][0]   = a         )));    // NG
        static assert( is(typeof( sa3[]       = [a]       )));    // OK
        static assert( is(typeof( sa3[][0]    = [a]       )));    // OK
        static assert(!is(typeof( sa3[][0][0] = a         )));    // NG

        static assert( is(typeof( da1         = [a,a]     )));    // OK
        static assert(!is(typeof( da1[0]      = a         )));    // NG
        static assert(!is(typeof( da1[]       = a         )));    // NG

        static assert( is(typeof( da2         = [[a,a]]   )));    // OK
        static assert(!is(typeof( da2[0][0]   = a         )));    // NG
        static assert(!is(typeof( da2[]       = [a,a]     )));    // NG
        static assert(!is(typeof( da2[][0]    = a         )));    // NG
        static assert(!is(typeof( da2[0][]    = a         )));    // NG
    }
    void func(Data a)
    {
        auto pdata = &data;

        static assert(!is(typeof( data        = a         )));    // NG
      static if (is(Data == struct))
      {
        static assert(!is(typeof( data.x      = 1         )));    // NG
        static assert(!is(typeof( data.y      = 2         )));    // NG
      }
        static assert(!is(typeof( *pdata      = a         )));    // NG
        static assert(!is(typeof( *&data      = a         )));    // NG

        static assert(!is(typeof( sa1         = [a,a]     )));    // NG
        static assert(!is(typeof( sa1[0]      = a         )));    // NG
        static assert(!is(typeof( sa1[]       = a         )));    // NG
        static assert(!is(typeof( sa1[][]     = a         )));    // NG

        static assert(!is(typeof( sa2         = [[a,a]]   )));    // NG
        static assert(!is(typeof( sa2[0][0]   = a         )));    // NG
        static assert(!is(typeof( sa2[][0][]  = a         )));    // NG
        static assert(!is(typeof( sa2[0][][0] = a         )));    // NG

        static assert(!is(typeof( sa3         = [[a],[]]  )));    // NG
        static assert(!is(typeof( sa3[0]      = [a,a]     )));    // NG
        static assert(!is(typeof( sa3[0][0]   = a         )));    // NG
        static assert(!is(typeof( sa3[]       = [a]       )));    // NG
        static assert(!is(typeof( sa3[][0]    = [a]       )));    // NG
        static assert(!is(typeof( sa3[][0][0] = a         )));    // NG

        static assert(!is(typeof( da1         = [a,a]     )));    // NG
        static assert(!is(typeof( da1[0]      = a         )));    // NG
        static assert(!is(typeof( da1[]       = a         )));    // NG

        static assert(!is(typeof( da2         = [[a,a]]   )));    // NG
        static assert(!is(typeof( da2[0][0]   = a         )));    // NG
        static assert(!is(typeof( da2[]       = [a,a]     )));    // NG
        static assert(!is(typeof( da2[][0]    = a         )));    // NG
        static assert(!is(typeof( da2[0][]    = a         )));    // NG
    }
}

const char gc6174;
const char[1] ga6174;
static this()
{
    gc6174 = 'a';    // OK
    ga6174[0] = 'a'; // line 5, Err
}
struct Foo6174
{
    const char cc;
    const char[1] array;
    const char[1] arr;
    this(char c)
    {
        cc = c;       // OK
        array = [c];  // line 12, Err
        arr[0] = c;   // line 12, Err
    }
}
void test6174a()
{
    static struct Pair
    {
        const int x;
        int y;
    }
    alias CtorTest6174!long CtorTest1;
    alias CtorTest6174!Pair CtorTest2;

    auto foo = Foo6174('c');
}

/***************************************************/

template Select(bool cond, T, F)
{
    static if (cond)
        alias Select = T;
    else
        alias Select = F;
}

void test6174b()
{
    enum { none, unrelated, mutable, constant }

    static struct FieldStruct(bool c, int k)
    {
        enum fieldConst = c;
        enum assignKind = k;

        Select!(fieldConst, const int, int) x;
        int y;

        static if (assignKind == none)      {}
        static if (assignKind == unrelated) void opAssign(int) {}
        static if (assignKind == mutable)   void opAssign(FieldStruct) {}
        static if (assignKind == constant)  void opAssign(FieldStruct) const {}
    }
    static struct TestStruct(F, bool fieldConst)
    {
        int w;
        Select!(fieldConst, const F, F) f;
        Select!(fieldConst, const int, int) z;

        this(int)
        {
            // If F has an identity `opAssign`,it is used even for initializing.
            // Otherwise, initializing  will always succeed, by bypassing const qualifier.
            static assert(is(typeof( f = F() )) == (
                            F.assignKind == none ||
                            F.assignKind == unrelated ||
                            F.assignKind == mutable ||
                            F.assignKind == constant));

            static assert(is(typeof(   w = 1000 )) == true);
            static assert(is(typeof( f.x = 1000 )) == true);
            static assert(is(typeof( f.y = 1000 )) == true);
            static assert(is(typeof(   z = 1000 )) == true);
        }
        void func()
        {
            // In mutable member functions, identity assignment is allowed
            // when all of the fields are identity assignable,
            // or identity `opAssign`, which callable from mutable object, is defined.
            static assert(__traits(compiles, f = F()) == (
                            F.assignKind == none      && !fieldConst && !F.fieldConst ||
                            F.assignKind == unrelated && !fieldConst && !F.fieldConst ||
                            F.assignKind == constant ||
                            F.assignKind == mutable   && !fieldConst));

            static assert(__traits(compiles,   w = 1000) == true);
            static assert(__traits(compiles, f.x = 1000) == (!fieldConst && !F.fieldConst));
            static assert(__traits(compiles, f.y = 1000) == (!fieldConst && true         ));
            static assert(__traits(compiles,   z = 1000) == !fieldConst);
        }
        void func() const
        {
            // In non-mutable member functions, identity assignment is allowed
            // just only user-defined identity `opAssign` is qualified.
            static assert(__traits(compiles, f = F()) == (F.assignKind == constant));

            static assert(__traits(compiles,   w = 1000) == false);
            static assert(__traits(compiles, f.x = 1000) == false);
            static assert(__traits(compiles, f.y = 1000) == false);
            static assert(__traits(compiles,   z = 1000) == false);
        }
    }
    foreach (fieldConst; TypeTuple!(false, true))
    foreach (  hasConst; TypeTuple!(false, true))
    foreach (assignKind; TypeTuple!(none, unrelated, mutable, constant))
    {
        alias TestStruct!(FieldStruct!(hasConst, assignKind), fieldConst) TestX;
    }
}

void test6174c()
{
    static assert(!is(typeof({
        int func1a(int n)
        in{ n = 10; }
        body { return n; }
    })));
    static assert(!is(typeof({
        int func1b(int n)
        out(r){ r = 20; }
        body{ return n; }
    })));

    struct DataX
    {
        int x;
    }
    static assert(!is(typeof({
        DataX func2a(DataX n)
        in{ n.x = 10; }
        body { return n; }
    })));
    static assert(!is(typeof({
        DataX func2b(DataX n)
        in{}
        out(r){ r.x = 20; }
        body{ return n; }
    })));
}

/***************************************************/
// 6216

void test6216a()
{
    static class C{}

    static struct Xa{ int n; }
    static struct Xb{ int[] a; }
    static struct Xc{ C c; }
    static struct Xd{ void opAssign(typeof(this) rhs){} }
    static struct Xe{ void opAssign(T)(T rhs){} }
    static struct Xf{ void opAssign(int rhs){} }
    static struct Xg{ void opAssign(T)(T rhs)if(!is(T==typeof(this))){} }

    // has value type as member
    static struct S1 (X){ static if (!is(X==void)) X x; int n; }

    // has reference type as member
    static struct S2a(X){ static if (!is(X==void)) X x; int[] a; }
    static struct S2b(X){ static if (!is(X==void)) X x; C c; }

    // has identity opAssign
    static struct S3a(X){ static if (!is(X==void)) X x; void opAssign(typeof(this) rhs){} }
    static struct S3b(X){ static if (!is(X==void)) X x; void opAssign(T)(T rhs){} }

    // has non identity opAssign
    static struct S4a(X){ static if (!is(X==void)) X x; void opAssign(int rhs){} }
    static struct S4b(X){ static if (!is(X==void)) X x; void opAssign(T)(T rhs)if(!is(T==typeof(this))){} }

    enum result = [
        /*S1,   S2a,    S2b,    S3a,    S3b,    S4a,    S4b*/
/*- */  [true,  true,   true,   true,   true,   true,   true],
/*Xa*/  [true,  true,   true,   true,   true,   true,   true],
/*Xb*/  [true,  true,   true,   true,   true,   true,   true],
/*Xc*/  [true,  true,   true,   true,   true,   true,   true],
/*Xd*/  [true,  true,   true,   true,   true,   true,   true],
/*Xe*/  [true,  true,   true,   true,   true,   true,   true],
/*Xf*/  [true,  true,   true,   true,   true,   true,   true],
/*Xg*/  [true,  true,   true,   true,   true,   true,   true],
    ];

    pragma(msg, "\\\tS1\tS2a\tS2b\tS3a\tS3b\tS4a\tS4b");
    foreach (i, X; TypeTuple!(void,Xa,Xb,Xc,Xd,Xe,Xf,Xg))
    {
        S1!X  s1;
        S2a!X s2a;
        S2b!X s2b;
        S3a!X s3a;
        S3b!X s3b;
        S4a!X s4a;
        S4b!X s4b;

        pragma(msg,
                is(X==void) ? "-" : X.stringof,
                "\t", __traits(compiles, (s1  = s1)),
                "\t", __traits(compiles, (s2a = s2a)),
                "\t", __traits(compiles, (s2b = s2b)),
                "\t", __traits(compiles, (s3a = s3a)),
                "\t", __traits(compiles, (s3b = s3b)),
                "\t", __traits(compiles, (s4a = s4a)),
                "\t", __traits(compiles, (s4b = s4b))  );

        static assert(result[i] ==
            [   __traits(compiles, (s1  = s1)),
                __traits(compiles, (s2a = s2a)),
                __traits(compiles, (s2b = s2b)),
                __traits(compiles, (s3a = s3a)),
                __traits(compiles, (s3b = s3b)),
                __traits(compiles, (s4a = s4a)),
                __traits(compiles, (s4b = s4b))  ]);
    }
}

void test6216b()
{
    static int cnt = 0;

    static struct X
    {
        int n;
        void opAssign(X rhs){ cnt = 1; }
    }
    static struct S
    {
        int n;
        X x;
    }

    S s;
    s = s;
    assert(cnt == 1);
    // Built-in opAssign runs member's opAssign
}

void test6216c()
{
    static int cnt = 0;

    static struct X
    {
        int n;
        void opAssign(const X rhs) const { cnt = 2; }
    }
    static struct S
    {
        int n;
        const(X) x;
    }

    S s;
    const(S) cs;
    s = s;
    s = cs;     // cs is copied as mutable and assigned into s
    assert(cnt == 2);
    static assert(!__traits(compiles, cs = cs));
                // built-in opAssin is only allowed with mutable object
}

void test6216d()
{
    static int cnt = 0;

    static struct X
    {
        int[] arr;  // X has mutable indirection
        void opAssign(const X rhs) const { ++cnt; }
    }
    static struct S
    {
        int n;
        const(X) x;
    }

    X mx;
    const X cx;
    mx = mx;    // copying mx to const X is possible
    assert(cnt == 1);
    mx = cx;
    assert(cnt == 2);
    cx = mx;    // copying mx to const X is possible
    assert(cnt == 3);

    S s;
    const(S) cs;
    s = s;
    s = cs;
    //assert(cnt == 4);
    static assert(!__traits(compiles, cs = cs));
                // built-in opAssin is only allowed with mutable object
}

void test6216e()
{
    static struct X
    {
        int x;
        @disable void opAssign(X);
    }
    static struct S
    {
        X x;
    }
    S s;
    static assert(!__traits(compiles, s = s));
                // built-in generated opAssin is marked as @disable.
}

/***************************************************/
// 6286

void test6286()
{
    const(int)[4] src = [1, 2, 3, 4];
    int[4] dst;
    dst = src;
    dst[] = src[];
    dst = 4;
    int[4][4] x;
    x = dst;
}

/***************************************************/
// 6336

void test6336()
{
    // structs aren't identity assignable
    static struct S1
    {
        immutable int n;
    }
    static struct S2
    {
        void opAssign(int n){ assert(0); }
    }

    S1 s1;
    S2 s2;

    void f(S)(out S s){}
    static assert(!__traits(compiles, f(s1)));
    f(s2);
    // Out parameters refuse only S1 because it isn't blit assignable

    ref S g(S)(ref S s){ return s; }
    g(s1);
    g(s2);
    // Allow return by ref both S1 and S2
}

/***************************************************/
// 8783

struct Foo8783
{
    int[1] bar;
}

const Foo8783[1] foos8783;

static this()
{
    foreach (i; 0 .. foos8783.length)
        foos8783[i].bar[i] = 1; // OK
    foreach (i, ref f; foos8783)
        f.bar[i] = 1; // line 9, Error
}

/***************************************************/
// 9077

struct S9077a
{
    void opAssign(int n) {}
    void test() { typeof(this) s; s = this; }
    this(this) {}
}
struct S9077b
{
    void opAssign()(int n) {}
    void test() { typeof(this) s; s = this; }
    this(this) {}
}

/***************************************************/
// 9140

immutable(int)[] bar9140()
out(result) {
    foreach (ref r; result) {}
} body {
    return null;
}

/***************************************************/
// 9154

struct S9154a
{
    int x;
    void opAssign(ref S9154a s) { }
}
struct S9154b
{
    int x;
    void opAssign(X)(ref X s) { }
}
struct T9154
{
    S9154a member1;
    S9154b member2;
}

void test9154()
{
    T9154 t1, t2;
    t1 = t2;
}

/***************************************************/
// 9258

class A9258 {}
class B9258 : A9258 // Error: class test.B9258 identity assignment operator overload is illegal
{
    void opAssign(A9258 b) {}
}

class C9258
{
    int n;
    alias n this;
    void opAssign(int n) {}
}
class D9258
{
    int n;
    alias n this;
    void opAssign(int n, int y = 0) {}
}
class E9258 : A9258
{
    void set(A9258 a) {}
    alias set opAssign;
}

/***************************************************/
// 9416

struct S9416
{
    void opAssign()(S9416)
    {
        static assert(0);
    }
}
struct U9416
{
    S9416 s;
}
void test9416()
{
    U9416 u;
    static assert(__traits(allMembers, U9416)[$-1] == "opAssign");
    static assert(!__traits(compiles, u = u));
}

/***************************************************/
// 9658

struct S9658
{
    private bool _isNull = true;
    this(int v) const
    {
        _isNull = false;    // cannot modify const expression this._isNull
    }
}

/***************************************************/
// 11187

void test11187()
{
    static struct X
    {
        int[] arr;
    }
    static struct S
    {
        const(X) cx;
    }
    static assert(is(typeof((const S).init.cx.arr) == const(int[])));
    static assert(is(typeof((      S).init.cx.arr) == const(int[])));
    const S sc;
    S sm = sc;
    static assert(is(const S : S));
}

/***************************************************/
// 12131

struct X12131
{
    void opAssign()(X12131 y) pure {}
}

struct Y12131
{
    X12131 a;
}

void test12131() pure
{
    X12131 x;
    x = X12131();   // OK

    Y12131 y;
    y = Y12131();   // OK <- Error
}

/***************************************************/
// 12211

void test12211()
{
    int a = 0;
    void foo(ref int x)
    {
        assert(x == 10);
        assert(&x == &a);
        x = 3;
    }
    foo(a = 10);
    assert(a == 3);
    foo(a += 7);
    assert(a == 3);

    // array ops should make rvalue
    int[3] sa, sb;
    void bar(ref int[]) {}
    static assert(!__traits(compiles, bar(sa[]  = sb[])));
    static assert(!__traits(compiles, bar(sa[] += sb[])));
}

/***************************************************/
// 4791 (dup of 12212)

void test4791()
{
    int[2] na;
    na = na;

    static struct S
    {
        static string res;
        int n;
        this(this) { ++n; res ~= "p" ~ cast(char)('0' + n); }
        ~this()    {      res ~= "d" ~ cast(char)('0' + n); }
    }
    {
        S[3] sa;
        sa[0].n = 1, sa[1].n = 2, sa[2].n = 3;

        S.res = null;
        sa = sa;
        assert(S.res == "p2d1p3d2p4d3");
        assert(sa[0].n == 2 && sa[1].n == 3 && sa[2].n == 4);

        S.res = null;
    }
    assert(S.res == "d4d3d2");
}

/***************************************************/
// 12212

void test12212()
{
    struct S
    {
        int x, y;
        static int cpctor;
        this(this) { cpctor++; }
    }

    void funcVal(E)(E[3] x) {}
    auto funcRef(E)(ref E[3] x) { return &x; }
    ref get(E)(ref E[3] a){ return a; }

    {
        int[3] a, b;
        funcVal(a = b);

        auto p = funcRef(a = b);
        assert(p == &a);
    }

    {
        S.cpctor = 0;

        S[3] a, b;
        assert(S.cpctor == 0);

        S[3] c = a;
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 3);
        S.cpctor = 0;

        c = a;
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 3);
        S.cpctor = 0;

        c = (a = b);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 6);
        S.cpctor = 0;

        c = (get(a) = b);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 6);
        S.cpctor = 0;
    }
    {
        S.cpctor = 0;

        S[3] a, b;
        assert(S.cpctor == 0);

        funcVal(a = b);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 6);
        S.cpctor = 0;

        funcVal(get(a) = b);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 6);
        S.cpctor = 0;
    }
    {
        S.cpctor = 0;

        S[3] a, b;
        assert(S.cpctor == 0);

        S[3]* p;

        p = funcRef(a = b);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(p == &a);
        assert(S.cpctor == 3);
        S.cpctor = 0;

        p = funcRef(get(a) = b);
        assert(p == &a);
        //printf("cpctpr = %d\n", S.cpctor);
        assert(S.cpctor == 3);
        S.cpctor = 0;
    }
}

/***************************************************/
// 12650

void test12650()
{
    // AssignExp::toElem should make an lvalue of e1.
    static class A1
    {
        struct S { int a; }

        static foo(ref const(S) s)
        {
            assert(s.a == 2);
            return &s;
        }

        S s;

        this()
        {
            const v = S(2);

            // (this.s = v) will become ConstructExp
            auto p = foo(s = v);
            assert(p == &s);
        }
    }
    assert(new A1().s.a == 2);

    static class A2
    {
        static foo(ref int[2] sa)
        {
            assert(sa[1] == 2);
            return &sa;
        }

        int[2] sa;

        this()
        {
            // (this.sa = [1,2]) will become ConstructExp
            auto p = foo(sa = [1,2]);
            assert(p == &sa);
        }
    }
    assert(new A2().sa[1] == 2);

    static class A3
    {
        static foo(ref int n)
        {
            assert(n == 2);
            return &n;
        }

        int n;

        this()
        {
            const v = 2;

            // (this.n = v) will become ConstructExp
            auto p = foo(n = v);
            assert(p == &n);
        }
    }
    assert(new A3().n == 2);
}

/***************************************************/
// 13044

void test13044()
{
    static struct Good
    {
        const int i;
    }

    static struct Bad
    {
        const int i;
        ~this() {}
    }

    Good good1, good2;
    static assert(!__traits(compiles, { good1 = good2; })); // OK

    Bad bad1, bad2;
    static assert(!__traits(compiles, { bad1 = bad2; }));   // OK <- fails
}

/***************************************************/
// 12500

void test12500()
{
    size_t foo;
    ++foo *= 1.5;   // Rewrite to: (foo += 1) *= 1.5;
}

/***************************************************/
// 14672

void test14672()
{
    interface I {}

    class B {}
    class D : B, I {}

    D d = new D();
    D[] da = [d];
    B[] ba = [null];
    I[] ia = [null];

    // ba and da points different payloads,
    // so element-wise assignment should work.
    ba[] = da[];    // OK <- e2ir ICE
    assert(ba[0] is d);

    // Today element-wise assignment is implemented as memcpy, For that reason
    // the conversion from derived classes to base interfaces is disallowed
    // because it requries offset adjustments.
    static assert(!__traits(compiles, { ia[] = da[]; }));

    // after the assignment, ba will wongly point the payload of da,
    // that's typed as D[]. To aboid type system breaking, it's disallowed.
    static assert(!__traits(compiles, { ba = da; }));

    // the assigned array literal is a new payload,
    // so rebinding ba should work.
    ba = [d];       // OK
    assert(ba[0] is d);
}

/***************************************************/
// 15044

void destroy15044(T)(ref T obj)
{
    static if (__traits(hasMember, T, "__xdtor"))
        obj.__xdtor();
    else
        static assert(0, T.stringof);
}

struct V15044
{
    ~this()
    {
    }

    RC15044!V15044 dup()
    {
        return RC15044!V15044(&this);
    }
}

struct RC15044(T)
{
    ~this()
    {
        destroy15044(*t);
        static assert(__traits(hasMember, T, "__xdtor"));
    }
    T* t;
}

/***************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test4424();
    test6174a();
    test6174b();
    test6174c();
    test6216a();
    test6216b();
    test6216c();
    test6216d();
    test6216e();
    test6286();
    test6336();
    test9154();
    test9416();
    test11187();
    test12131();
    test12211();
    test4791();
    test12212();
    test12650();
    test13044();
    test12500();
    test14672();

    printf("Success\n");
    return 0;
}
