/*
REQUIRED_ARGS: -w -de
PERMUTE_ARGS:
RUN_OUTPUT:
---
Success
---
*/

extern(C) int printf(const char*, ...);

/***************************************************/
// mutable field

struct S1A
{
    int v;
    this(int)
    {
        v = 1;
        v = 2;  // multiple initialization
    }
}

struct S1B
{
    int v;
    this(int)
    {
        if (true) v = 1; else v = 2;
        v = 3;  // multiple initialization
    }
    this(long)
    {
        if (true) v = 1;
        v = 3;  // multiple initialization
    }
    this(string)
    {
        if (true) {} else v = 2;
        v = 3;  // multiple initialization
    }
}

struct S1C
{
    int v;
    this(int)
    {
        true ? (v = 1) : (v = 2);
        v = 3;  // multiple initialization
    }
    this(long)
    {
        auto x = true ? (v = 1) : 2;
        v = 3;  // multiple initialization
    }
    this(string)
    {
        auto x = true ? 1 : (v = 2);
        v = 3;  // multiple initialization
    }
}

/***************************************************/
// with control flow

struct S2
{
    immutable int v;
    immutable int w;
    int x;
    this(int)
    {
        if (true) v = 1;
        else      v = 2;

        true ? (w = 1) : (w = 2);

        x = 1;  // initialization
    L:  x = 2;  // assignment after labels
    }
    this(long n)
    {
        if (n > 0)
            return;
        v = 1;  // skipped initialization

        // w skipped initialization

        x = 1;  // initialization
        foreach (i; 0..1) x = 2;  // assignment in loops
    }
}

/***************************************************/
// with immutable constructor

struct S3
{
    int v;
    int w;
    this(int) immutable
    {
        if (true) v = 1;
        else      v = 2;

        true ? (w = 1) : (w = 2);
    }
}

/***************************************************/
// in typeof

struct S4
{
    immutable int v;
    this(int)
    {
        static assert(is(typeof(v = 1)));
        v = 1;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8117

struct S8117
{
    @disable this();
    this(int) {}
}

class C8117
{
    S8117 s = S8117(1);
}

void test8117()
{
    auto t = new C8117();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9665

struct X9665
{
    static uint count;
    ulong payload;
    this(int n) { payload = n; count += 1; }
    this(string s) immutable { payload = s.length; count += 10; }
    void opAssign(X9665 x) { payload = 100; count += 100; }
}

struct S9665
{
              X9665 mval;
    immutable X9665 ival;
    this(int n)
    {
        X9665.count = 0;
        mval = X9665(n);                // 1st, initializing
        ival = immutable X9665("hi");   // 1st, initializing
        mval = X9665(1);                // 2nd, assignment
        static assert(!__traits(compiles, ival = immutable X9665(1)));  // 2nd, assignment
        //printf("X9665.count = %d\n", X9665.count);
        assert(X9665.count == 112);
    }
    this(int[])
    {
        X9665.count = 0;
        mval = 1;       // 1st, initializing (implicit constructor call)
        ival = "hoo";   // ditto
        assert(X9665.count == 11);
    }
}

void test9665()
{
    S9665 s1 = S9665(1);
    assert(s1.mval.payload == 100);
    assert(s1.ival.payload == 2);

    S9665 s2 = S9665([]);
    assert(s2.mval.payload == 1);
    assert(s2.ival.payload == 3);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11246

struct Foo11246
{
    static int ctor = 0;
    static int dtor = 0;
    this(int i)
    {
        ++ctor;
    }

    ~this()
    {
        ++dtor;
    }
}

struct Bar11246
{
    Foo11246 foo;

    this(int)
    {
        foo = Foo11246(5);
        assert(Foo11246.ctor == 1);
        assert(Foo11246.dtor == 0);
    }
}

void test11246()
{
    {
        auto bar = Bar11246(1);
        assert(Foo11246.ctor == 1);
        assert(Foo11246.dtor == 0);
    }
    assert(Foo11246.ctor == 1);
    assert(Foo11246.dtor == 1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13515

Object[string][100] aa13515;

static this()
{
    aa13515[5]["foo"] = null;
}

struct S13515
{
    Object[string][100] aa;

    this(int n)
    {
        aa[5]["foo"] = null;
    }
}

void test13515()
{
    assert(aa13515[5].length == 1);
    assert(aa13515[5]["foo"] is null);

    auto s = S13515(1);
    assert(s.aa[5].length == 1);
    assert(s.aa[5]["foo"] is null);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14409

class B14409 { this(int) {} }
class C14409 : B14409
{
    this(int n)
    {
        if (true)
            super(n);
        else
            assert(0);
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14376

auto map14376()
{
    return MapResult14376!(e => 0)();
}

struct MapResult14376(alias pred)
{
    @property int front() { return pred(0); }
}

struct S14376
{
    typeof(map14376()) x;

    this(int dummy)
    {
        if (true)
            this.x = map14376();
        else
            assert(0);
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14351

class B14351
{
    this(inout int[]) inout { }
}

class D14351a : B14351
{
    this(int[] arr) { super(arr); }
}

class D14351b : B14351
{
    this(const int[] arr) const { super(arr); }
}

class D14351c : B14351
{
    this(inout int[] arr) inout { super(arr); }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14450

struct S14450a      // non-template struct + non template ctors - OK
{
    int x;
    this(int) { x = 1; }
    this(int) immutable { x = 2; }
}
struct S14450b      // non-template struct + template ctors - OK
{
    int x;
    this()(int) { x = 1; }
    this()(int) immutable { x = 2; }
}
struct S14450c()    // template struct + non-template ctors - Error -> OK
{
    int x;
    this(int) { x = 1; }
    this(int) immutable { x = 2; }
}
struct S14450d()    // template struct + template ctors - OK
{
    int x;
    this()(int) { x = 1; }
    this()(int) immutable { x = 2; }
}
struct S14450e() // template struct + pure template ctors - Error -> OK
{
    int x;
    this()(int) pure { x = 1; }
    this()(int) pure immutable { x = 2; }
}

void test14450()
{
    { auto m = S14450a(1);    assert(m.x == 1); }
    { auto m = S14450b(1);    assert(m.x == 1); }
    { auto m = S14450c!()(1); assert(m.x == 1); }
    { auto m = S14450d!()(1); assert(m.x == 1); }
    { auto m = S14450e!()(1); assert(m.x == 1); }

    { auto i = immutable S14450a(1);    assert(i.x == 2); }
    { auto i = immutable S14450b(1);    assert(i.x == 2); }
    { auto i = immutable S14450c!()(1); assert(i.x == 2); }
    { auto i = immutable S14450d!()(1); assert(i.x == 2); }
    { auto i = immutable S14450e!()(1); assert(i.x == 2); }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14944

static int[2] tbl14944;

static this()
{
    foreach (ref v; tbl14944)
    {
        // This is an initialization of referenced memory
        // rather than the initialization of the reference.
        v = 1;
    }
}

void test14944()
{
    assert(tbl14944[0] == 1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15258
// a field initialization affects other overlapped fields

class C15258
{
    this(const char* result)
    {
        this.result = result;
    }

    union
    {
        const char** results;
        const char* result;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15869

struct Set {
    @disable this(this);
    int value = 0;
}

Set clobber(ref Set a) {
    Set ret; // <- This overwrites *a, i.e. &ret is the same as a
    ret.value = a.value; // <- Now a.value is 0
    return ret;
}

struct XX {
    Set a = Set(1);
    this(int n) {
        a = clobber(a); // fix is to make this an assignment, not a construction
    }
}
void test15869()
{
    Set a = Set(1);
    a = clobber(a);
    assert(a.value == 1);

    XX xx = XX(0);
    assert(xx.a.value == 1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=19389

struct Foo19389 {
    int x;

    this(int dummy) { x = dummy; }
}

struct Bar19389 {
    Foo19389 a;
    Foo19389 b;

    this(int dummy) {
        a = (b = Foo19389(dummy));
    }
}


void test19389()
{
    Bar19389 bar = Bar19389(7);
    assert(bar.a.x == 7);
    assert(bar.b.x == 7); // fails
}


/***************************************************/

int main()
{
    test8117();
    test9665();
    test11246();
    test13515();
    test14450();
    test14944();
    test15869();
    test19389();

    printf("Success\n");
    return 0;
}
