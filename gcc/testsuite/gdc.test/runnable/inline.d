
import core.stdc.stdio;

// Test function inlining

debug = NRVO;

/************************************/

int foo(int i)
{
    return i;
}

int bar()
{
    return foo(3) + 4;
}

void test1()
{
    printf("%d\n", bar());
    assert(bar() == 7);
}


/************************************/

struct Foo2
{
    int a,b,c,e,f,g;
}


int foo2(Foo2 f)
{
    f.b += 73;
    return f.b;
}

int bar2()
{
    Foo2 gg;

    gg.b = 6;
    return foo2(gg) + 4;
}

void test2()
{
    printf("%d\n", bar2());
    assert(bar2() == 83);
}


/************************************/

struct Foo3
{
    int bar() { return y + 3; }
    int y = 4;
}

void test3()
{
    Foo3 f;

    assert(f.bar() == 7);
}


/************************************/

void func(void function () v)
{
}

void test4()
{
   static void f1() { }

   func(&f1);
   //func(f1);
}


/************************************/

void foo5(ubyte[16] array)
{
    bar5(array.ptr);
}

void bar5(ubyte *array)
{
}

void abc5(ubyte[16] array)
{
    foo5(array);
}

void test5()
{
}

/************************************/

struct Struct
{
    real foo()
    {
        return 0;
    }

    void bar(out Struct Q)
    {
        if (foo() < 0)
            Q = this;
    }
}

void test6()
{
}

/************************************/

struct S7(T)
{
    immutable(T)[] s;
}

T foo7(T)(T t)
{
    enum S7!(T)[] i = [{"hello"},{"world"}];
    auto x = i[0].s;
    return t;
}

void test7()
{
    auto x = foo7('c');
}

/************************************/

// 10833
string fun10833(T...)()
{
    foreach (v ; T)
        return v;
    assert(0);
}

void test10833()
{
    auto a = fun10833!("bar")();
}

/************************************/
// Bugzilla 4825

int a8() {
    int r;
    return r;
}

int b8() {
    return a8();
}

void test8() {
    void d() {
        auto e = b8();
    }
    static const int f = b8();
}

/************************************/
// 4841

auto fun4841a()
{
    int i = 42;
    struct Result
    {
        this(int u) {}
        auto bar()
        {
            // refer context of fun4841a
            return i;
        }
    }
    return Result();
}
void test4841a()
{
    auto t = fun4841a();
    auto x = t.bar();
    assert(x == 42);
}

auto fun4841b()
{
    int i = 40;
    auto foo()  // hasNestedFrameRefs() == false
    {
        //
        struct Result
        {
            this(int u) {}
            auto bar()
            {
                // refer context of fun4841b
                return i + 2;
            }
        }
        return Result();
    }
    return foo();
}
void test4841b()
{
    auto t = fun4841b();
    assert(cast(void*)t.tupleof[$-1] !is null);     // Result to fun4841b
    auto x = t.bar();
    assert(x == 42);
}

auto fun4841c()
{
    int i = 40;
    auto foo()  // hasNestedFrameRefs() == true
    {
        int g = 2;
        struct Result
        {
            this(int u) {}
            auto bar()
            {
                // refer context of fun4841c and foo
                return i + g;
            }
        }
        return Result();
    }
    return foo();
}
void test4841c()
{
    auto t = fun4841c();
    assert(  cast(void*)t.tupleof[$-1] !is null);   // Result to foo
    assert(*cast(void**)t.tupleof[$-1] !is null);   // foo to fun4841c
    auto x = t.bar();
    assert(x == 42);
}

void test4841()
{
    test4841a();
    test4841b();
    test4841c();
}

/************************************/
// 7261

struct AbstractTask
{
    ubyte taskStatus;
}

struct Task
{
    AbstractTask base;
    alias base this;

    void opAssign(Task rhs)
    {
    }

    ~this()
    {
        if (taskStatus != 3) { }
    }
}

/************************************/
// 9356

void test9356()
{
    static inout(char)[] bar (inout(char)[] a)
    {
        return a;
    }

    string result;
    result ~= bar("abc");
    assert(result == "abc");
}

/************************************/
// 12079

void test12079()
{
    string[string][string] foo;

    foo.get("bar", null).get("baz", null);
}

/************************************/
// 12243

char f12243() { return 'a'; }

void test12243()
{
    string s;
    s ~= f12243();
}

/************************************/
// 11201

struct Foo11201
{
    int a;
    float b;

    Foo11201 func()() const { return this; }
}

auto f11201()(Foo11201 a) { return a; }

void test11201()
{
    auto a = Foo11201(0, 1);

    assert(f11201(a.func!()()) == a);
}

/************************************/
// 11223

struct Tuple11223(T...)
{
    T values;

    void opAssign(Tuple11223 rhs)
    {
        if (0)
            values = rhs.values;
        else
            assert(1);
    }
}

void test11223()
{
    Tuple11223!string tmp;
    tmp = Tuple11223!string();
}

/************************************/


void foo3918()
{
    import core.stdc.stdlib : alloca;
    void[] mem = alloca(1024)[0..1024];
}

void test3918()
{
    foreach(i; 0 .. 10_000_000)
    {
        foo3918();
    }
}

/************************************/
// 11314

struct Tuple11314(T...)
{
    T values;

    void opAssign(typeof(this) rhs)
    {
        if (0)
            values[] = rhs.values[];
        else
            assert(1);
    }
}

struct S11314 {}

void test11314()
{
    Tuple11314!S11314 t;
    t = Tuple11314!S11314(S11314.init);
}

/************************************/
// 11224

S11224* ptr11224;

struct S11224
{
    this(int)
    {
        ptr11224 = &this;
        /*printf("ctor &this = %p\n", &this);*/
    }
    this(this)
    {
        /*printf("cpctor &this = %p\n", &this);*/
    }
    int num;
}
S11224 foo11224()
{
    S11224 s = S11224(1);
    //printf("foo  &this = %p\n", &s);
    assert(ptr11224 is &s);
    return s;
}
void test11224()
{
    auto s = foo11224();
    //printf("main &this = %p\n", &s);
    assert(ptr11224 is &s);
}

/************************************/
// 11322

bool b11322;
uint n11322;

ref uint fun11322()
{
    if (b11322)
        return n11322;
    else
        return n11322;
}

void test11322()
{
    fun11322()++;
    assert(n11322 == 1);
    fun11322() *= 5;
    assert(n11322 == 5);
}

/************************************/
// 11394

debug(NRVO) static void* p11394a, p11394b, p11394c;

static int[5] make11394(in int x) pure
{
    typeof(return) a;
    a[0] = x;
    a[1] = x + 1;
    a[2] = x + 2;
    a[3] = x + 3;
    a[4] = x + 4;
    debug(NRVO) p11394a = cast(void*)a.ptr;
    return a;
}

struct Bar11394
{
    immutable int[5] arr;

    this(int x)
    {
        this.arr = make11394(x);    // NRVO should work
        debug(NRVO) p11394b = cast(void*)this.arr.ptr;
    }
}

void test11394()
{
    auto b = Bar11394(5);
    debug(NRVO) p11394c = cast(void*)b.arr.ptr;
  //debug(NRVO) printf("p1 = %p\np2 = %p\np3 = %p\n", p11394a, p11394b, p11394c);
    debug(NRVO) assert(p11394a == p11394b);
    debug(NRVO) assert(p11394b == p11394c);
}

/**********************************/
// 12080

class TZ12080 {}

struct ST12080
{
    ST12080 opBinary()() const pure nothrow
    {
        auto retval = ST12080();
        return retval;  // NRVO
    }

    long  _stdTime;
    immutable TZ12080 _timezone;
}

class Foo12080
{

    ST12080 bar;
    bool quux;

    public ST12080 sysTime()
    out {}
    body
    {
        if (quux)
            return ST12080();

        return bar.opBinary();
        // returned value is set to __result
        // --> Inliner wrongly created the second DeclarationExp for __result.
    }
}

/**********************************/
// 13503

void f13503a(string[] s...)
{
    assert(s[0] == "Cheese");
}

auto f13503b(string arg)
{
    string result = arg;
    return result;
}

string f13503c(string arg)
{
    string result = arg;
    return result;
}

void test13503()
{
    f13503a(f13503b("Cheese"));
    f13503a(f13503c("Cheese"));
}

/**********************************/
// 14267

// EXTRA_SOURCES: imports/a14267.d
import imports.a14267;

void test14267()
{
    foreach (m; __traits(allMembers, SysTime14267))
    {
        static if (is(typeof(__traits(getMember, SysTime14267, m))))
        {
            foreach (func; __traits(getOverloads, SysTime14267, m))
            {
                auto prot = __traits(getProtection, func);
                static if (__traits(isStaticFunction, func))
                {
                    static assert(func.stringof == "min()");
                    auto result = func;
                }
            }
        }
    }
}

/**********************************/
// 13244

struct MapResult13244(alias fun)
{
    int[] input;
    @property front() { return fun(input[0]); }
}

int[] array13244(R)(R r)
{
    int[] a;
    a ~= r.front;
    return a;
}

void test13244()
{
    auto arr = [[cast(ubyte)1]];
    foreach (ref x; arr)
    {
        auto m = MapResult13244!(c => x[c])([0]);
        array13244(m);
    }
}

/**********************************/
// 14306

struct MapResult(alias fun)
{
    void front()
    {
//  while (1) { break; }
        fun(1);
    }
}

void bar(R)(R r)
{
    foreach (i; 0..100)
    {
        r.front();
    }
}

struct S
{
    int x;
    int bump()
    {
        while (1) { break; }
        ++x;
        return x;
    }
}

void fun(ref S s)
{
    MapResult!(y => s.bump())().bar;
//  MapResult!((int x) => s.bump())().bar;

    if (s.x != 100)
        assert(0);
}

void test14306()
{
    S t;
    fun(t);
}

/**********************************/
// 14754

auto aafunc14754(string k)
{
    enum aa = [ "K": "V" ];
    auto p = k in aa;
    return null;
}

struct MapResult14754(alias fun, R)
{
    R _input;

    @property auto ref front()
    {
        return fun(_input[0]);
    }
}

auto array14754(R)(R r)
{
    alias E = typeof(r.front);
    E[] result;
    result ~= r.front;
    return result;
}

auto mapfun14754(R)(R words, string k)
{
    return array14754(MapResult14754!(s => aafunc14754(k), R)(words));
}

void test14754()
{
    auto r = mapfun14754([""], "");
}

/**********************************/
// 14606

struct S14606
{
    this(long stdTime)
    {
        _stdTime = stdTime;
    }

    long _stdTime;
}

S14606 getS14606()
{
    S14606 sysTime = S14606(0);
    return sysTime;
}

struct T14606
{
    this(string)
    {
        uint[3] arr;
        s = getS14606();
    }

    S14606 s;
}

void test14606()
{
    auto t = T14606(null);
}

/**********************************/
// 14753

pragma(inline)
void test14753(string) { }

/**********************************/

struct S14975
{
    int bar;

    pragma(inline, true) this(int bar)
    {
        this.bar = bar;
    }
}

void test14975()
{
    S14975 baz = 1;
    if (baz.bar != 1)
        assert(0);
}

/**********************************/
// 15210

struct BigInt15210 {}

struct Tuple15210(Types...)
{
    Types field;

    void opAssign(R)(R rhs)
    {
        field = rhs.field;
    }
}

void test15210()
{
    alias X = Tuple15210!BigInt15210;

    X[BigInt15210] cache;

    auto x = X();

    cache[BigInt15210()] = x;
}

/**********************************/

int foo7625(int v)
{
    return bar7625(2 * v);
}

int bar7625(int a)
{
    ++a;
    if (a > 0)
        return 1;
    return baz(a);
}

int baz(int a)
{
    if (a > 0)
        throw new Exception("a > 0");
    return a - 1;
}

void test7625()
{
    int x = foo7625(1);
    if (x != 1)
        assert(0);
}

/**********************************/
// 9785 partial fix

void test9785()
{
        int j = 3;

        void loop(scope const void function(int x) dg) {
            pragma(inline, true);
            dg(++j);
        }

        loop((x) {
                pragma(inline, true);
                printf("%d\n", x);
                assert(x == 4);
        });
}


/**********************************/
// 9785 partial fix

void test9785_2() {
        int j = 3;

        void loop(scope const void function(int x) dg) {
            pragma(inline, true);
            dg(++j);
        }

        static void func(int x) {
                pragma(inline, true);
                printf("%d\n", x);
                assert(x == 4);
        }

        loop(&func);
}

/**********************************/
// 9785 partial fix

void test9785_3() @nogc
{
    int j = 3;

    void loop(scope const void delegate(int x) @nogc dg) @nogc {
        pragma(inline, true);
        dg(++j);
    }

    loop((x) @nogc {
            pragma(inline, true);
            //printf("%d\n", x + j * 2);
            assert(x == 4);
            assert(j == 4);
    });

    j = 3;
    void func(int x) @nogc {
            pragma(inline, true);
            //printf("%d\n", x + j * 2);
            assert(x == 4);
            assert(j == 4);
    }

    loop(&func);
}

/**********************************/
// 15207

struct Vec15207
{
    float x, y, z;

    this(float x_, float y_, float z_)
    {
        x = x_;
        y = y_;
        z = z_;
    }

    Vec15207 clone()
    {
        // When the variable 'res' is replaced with a STCref temporary,
        // this line was accidentally changed to reference initialization.
        Vec15207 res = this;

        return res;
    }
}

class C15207
{
    Vec15207 a;

    this()
    {
        a = Vec15207(1, 2, 3).clone();

        assert(a.x == 1);
        assert(a.y == 2);
        assert(a.z == 3);
        printf("%f %f %f\n", a.x, a.y, a.z);
    }
}

void test15207()
{
    auto c = new C15207();
}

/**********************************/
// 15253

struct MessageType15253
{
    MessageType15253[] messageTypes;

    const void toString1(scope void delegate(const(char)[]) sink)
    {
        messageTypes[0].toString1(sink);
    }
}

struct ProtoPackage15253
{
    MessageType15253[] messageTypes;

    const void toString1(scope void delegate(const(char)[]) sink)
    {
        messageTypes[0].toString1(sink);
    }
}

/**********************************/
// 15296

static int x15296;

struct S15296
{
    // Can be expanded only as statements.
    pragma(inline, true)
    void bar(size_t , size_t )
    {
        for (size_t w = 0; w < 2; w++) { ++x15296; }
    }

    pragma(inline, true)
    void foo(size_t a, size_t b)
    {
        bar(a, b);
    }
}

pragma(inline, true)
static void voidCall15296()
{
    for (size_t w = 0; w < 3; w++) { ++x15296; }
}

void test15296()
{
    bool cond = true;

    S15296 s;

    // CallExp at the top of ExpStatement
    x15296 = 0;
    s.foo(0, 0);
    assert(x15296 == 2);

    // CondExp at the top of ExpStatement
    x15296 = 0;
    (cond ? s.foo(0, 0) : voidCall15296());
    assert(x15296 == 2);
    (cond ? voidCall15296() : s.foo(0, 0));
    assert(x15296 == 2 + 3);

    // CommaExp at the top of ExpStatement
    x15296 = 0;
    (s.foo(0, 0), voidCall15296());
    assert(x15296 == 3 + 2);
}

// ----

struct File15296
{
    struct Impl {}
    Impl* _p;

    pragma(inline, true)
    ~this() { _p = null; }

    struct LockingTextWriter
    {
        pragma(inline, true)
        this(ref File15296 f)
        {
            assert(f._p, "Attempting to write to closed File");
        }
    }

    pragma(inline, true)
    auto lockingTextWriter() { return LockingTextWriter(this); }

    pragma(inline, true)
    void write() { auto w = lockingTextWriter(); }

    //pragma(inline, true)
    static uint formattedWrite(Writer)(Writer w) { return 0; }

    pragma(inline, true)
    void writef() { formattedWrite(lockingTextWriter()); }
}

__gshared File15296 stdout15296 = {new File15296.Impl()};

pragma(inline, true)
@property File15296 trustedStdout15296() { return stdout15296; }

// ----
// reduced case from runnable/test34.d test34()

void test15296b()
{
    // trustedStdout() returns a temporary File object. Its dtor call
    // should be deferred till the end of expanded writef body statements.
    trustedStdout15296().writef();
}

// ----
// reduced case from runnable/xtest46.d test136()

struct Perm15296c
{
    this(byte[] input)
    {
        foreach (elem; input)
        {
            // if vthis.isDataseg() is true in expandInline,
            // its edtor should not be called.
            stdout15296.write();
        }
    }
}

void test15296c()
{
    auto perm2 = Perm15296c([0, 1, 2]);
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=17676
__gshared bool bgEnable = 1;

void test17676() nothrow
{
    fullcollect();
}

size_t fullcollect() nothrow
{
    if(bgEnable)
       return fullcollectTrigger();

    return fullcollectNow();
}

size_t fullcollectNow() nothrow
{
    if (bgEnable)
        assert(0);
    pragma(inline, false);
    return 1;
}

size_t fullcollectTrigger() nothrow
{
    pragma(inline, false);
    return 0;
}

/**********************************/

int main()
{
    test1();
    test2();
    test3();
    test3918();
    test4();
    test5();
    test9356();
    test6();
    test7();
    test8();
    test4841();
    test11201();
    test11223();
    test11314();
    test11224();
    test11322();
    test11394();
    test13503();
    test13244();
    test14306();
    test14754();
    test14606();
    test14975();
    test15210();
    test7625();
    test9785();
    test9785_2();
    test9785_3();
    test15207();
    test15296();
    test15296b();
    test15296c();
    test17676();
    
    printf("Success\n");
    return 0;
}
