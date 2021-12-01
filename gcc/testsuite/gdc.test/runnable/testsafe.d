// PERMUTE_ARGS:

//http://d.puremagic.com/issues/show_bug.cgi?id=5415

@safe
void pointercast()
{
    int* a;
    void* b;

    static assert( __traits(compiles, cast(void*)a));
    static assert(!__traits(compiles, cast(int*)b));
    static assert(!__traits(compiles, cast(int*)b));
    static assert(!__traits(compiles, cast(short*)b));
    static assert(!__traits(compiles, cast(byte*)b));
    static assert( __traits(compiles, cast(short*)a));
    static assert( __traits(compiles, cast(byte*)a));
}

@safe
void pointercast2()
{
    size_t a;
    int b;
    Object c;

    static assert(!__traits(compiles, cast(void*)a));
    static assert(!__traits(compiles, cast(void*)b));
    static assert(!__traits(compiles, cast(void*)c));
}

@safe
void pointerarithmetic()
{//http://d.puremagic.com/issues/show_bug.cgi?id=4132
    void* a;
    int b;

    static assert(!__traits(compiles, a + b));
    static assert(!__traits(compiles, a - b));
    static assert(!__traits(compiles, a += b));
    static assert(!__traits(compiles, a -= b));
    static assert(!__traits(compiles, a++));
    static assert(!__traits(compiles, a--));
    static assert(!__traits(compiles, ++a));
    static assert(!__traits(compiles, --a));
    static assert( __traits(compiles, a + 0));
    static assert( __traits(compiles, a - 0));
    static assert( __traits(compiles, 0 + a));
    static assert(!__traits(compiles, a + 1));
    static assert(!__traits(compiles, a - 1));
    static assert(!__traits(compiles, 1 + a));
    static assert( __traits(compiles, a += 0));
    static assert( __traits(compiles, a -= 0));
    static assert(!__traits(compiles, a += 1));
    static assert(!__traits(compiles, a -= 1));
}



union SafeUnion1
{
    int a;
    struct
    {
        int b;
        int* c;
    }
}
union SafeUnion2
{
    int a;
    struct
    {
        int b;
        int c;
    }
}
union UnsafeUnion1
{
    int a;
    int* c;
}
union UnsafeUnion2
{
    int a;
    align(1)
    struct
    {
        byte b;
        int* c;
    }
}
union UnsafeUnion3
{
    int a;
    Object c;
}
union UnsafeUnion4
{
    int a;
    align(1)
    struct
    {
        byte b;
        Object c;
    }
}
struct pwrapper
{
    int* a;
}
union UnsafeUnion5
{
    SafeUnion2 x;
    pwrapper b;
}

union uA
{
    struct
    {
        int* a;
        void* b;
    }
}
struct uB
{
    uA a;
}
struct uC
{
    uB a;
}
struct uD
{
    uC a;
}

@safe
void safeunions()   // improved for https://issues.dlang.org/show_bug.cgi?id=11510
{
    SafeUnion1 su1;
    SafeUnion2 su2;
    UnsafeUnion1 uu1;
    UnsafeUnion2 uu2;
    UnsafeUnion3 uu3;
    UnsafeUnion4 uu4;
    UnsafeUnion5 uu5;
    uD uud;

    int n;
    void* p;
    Object o;

    // Writing field is always allowed, even if it is overlapped.
    su1.a = 7, su1.b = 8, su1.c = null;
    su2.a = 7, su2.b = 8, su2.c = 9;
    uu1.a = 7,            //uu1.c = null;
    uu2.a = 7; uu2.b = 8, //uu2.c = null;
    uu3.a = 7;            //uu3.c = null;
    uu4.a = 7; uu4.b = 8, //uu4.c = null;
    uu5.x.a = 7; uu5.x.b = 8, uu5.x.c = 9;
    uud.a.a.a.a = null, uud.a.a.a.b = null;

    // Reading field is allowed, if it is not overlapped or has no pointers.
    n = su1.a, n = su1.b, p = su1.c;
    n = su2.a, n = su2.b, n = su2.c;
    n = uu1.a;
    n = uu2.a, n = uu2.b;
    n = uu3.a;
    n = uu4.a, n = uu4.b;
    n = uu5.x.a, n = uu5.x.b, n = uu5.x.c;
    p = uud.a.a.a.a, p = uud.a.a.a.b;

    // Reading overlapped pointer field is not allowed.
    static assert(!__traits(compiles, () @safe { auto p = uu1.c; }));
    static assert(!__traits(compiles, () @safe { auto p = uu2.c; }));
    static assert(!__traits(compiles, () @safe { auto c = uu3.c; }));
    static assert(!__traits(compiles, () @safe { auto c = uu4.c; }));
    static assert(!__traits(compiles, () @safe { auto p = uu5.b.a; }));
}



@safe
void safeexception()
{
    try {}
    catch(Exception e) {}

    static assert(!__traits(compiles, () @safe {
        try {}
        catch(Error e) {}
    }));

    static assert(!__traits(compiles, () @safe {
        try {}
        catch(Throwable e) {}
    }));
}

@safe
void inlineasm()
{
    version (D_InlineAsm_X86)
        static assert(!__traits(compiles, { asm { int 3; } }() ));
    else version (D_InlineAsm_X86_64)
        static assert(!__traits(compiles, { asm { int 3; } }() ));
}

@safe
void multablecast()
{
    Object m;
    const(Object) c;
    immutable(Object) i;

    static assert( __traits(compiles, cast(const(Object))m));
    static assert( __traits(compiles, cast(const(Object))i));

    static assert(!__traits(compiles, cast(immutable(Object))m));
    static assert(!__traits(compiles, cast(immutable(Object))c));

    static assert(!__traits(compiles, cast(Object)c));
    static assert(!__traits(compiles, cast(Object)i));

    void* mp;
    const(void)* cp;
    immutable(void)* ip;

    static assert( __traits(compiles, cast(const(void)*)mp));
    static assert( __traits(compiles, cast(const(void)*)ip));

    static assert(!__traits(compiles, cast(immutable(void)*)mp));
    static assert(!__traits(compiles, cast(immutable(void)*)cp));

    static assert(!__traits(compiles, cast(void*)cp));
    static assert(!__traits(compiles, cast(void*)ip));
}

@safe
void sharedcast()
{
    Object local;
    shared(Object) xshared;
    immutable(Object) ishared;

    static assert(!__traits(compiles, cast()xshared));
    static assert(!__traits(compiles, cast(shared)local));

    static assert(!__traits(compiles, cast(immutable)xshared));
    static assert(!__traits(compiles, cast(shared)ishared));
}

int threadlocalvar;

@safe
void takeaddr()
{
    static assert(!__traits(compiles, (int x) @safe { auto y = &x; } ));
    static assert(!__traits(compiles, () @safe { int x; auto y = &x; } ));
    static assert( __traits(compiles, () @safe { static int x; auto y = &x; } ));
    static assert( __traits(compiles, () @safe { auto y = &threadlocalvar; } ));
}

__gshared int gsharedvar;

@safe
void use__gshared()
{
    static assert(!__traits(compiles, () @safe { int x = gsharedvar; } ));
}

@safe
void voidinitializers()
{//http://d.puremagic.com/issues/show_bug.cgi?id=4885
    static assert(!__traits(compiles, () @safe { uint* ptr = void; } ));
    static assert( __traits(compiles, () @safe { uint i = void; } ));
    static assert( __traits(compiles, () @safe { uint[2] a = void; } ));

    struct ValueStruct { int a; }
    struct NonValueStruct { int* a; }
    static assert( __traits(compiles, () @safe { ValueStruct a = void; } ));
    static assert(!__traits(compiles, () @safe { NonValueStruct a = void; } ));

    static assert(!__traits(compiles, () @safe { uint[] a = void; } ));
    static assert(!__traits(compiles, () @safe { int** a = void; } ));
    static assert(!__traits(compiles, () @safe { int[int] a = void; } ));
}

@safe
void pointerindex()
{//http://d.puremagic.com/issues/show_bug.cgi?id=9195
    static assert(!__traits(compiles, () @safe { int* p; auto a = p[30]; }));
    static assert( __traits(compiles, () @safe{ int* p; auto a = p[0]; }));
}

@safe
void basiccast()
{//http://d.puremagic.com/issues/show_bug.cgi?id=5088
    auto a = cast(int)cast(const int)1;
    auto b = cast(real)cast(const int)1;
    auto c = cast(real)cast(const real)2.0;
}

@safe
void arraycast()
{
    int[] x;
    void[] y = x;
    static assert( __traits(compiles, cast(void[])x));
    static assert(!__traits(compiles, cast(int[])y));
    static assert(!__traits(compiles, cast(int*[])y));
    static assert(!__traits(compiles, cast(void[][])y));

    int[3] a;
    int[] b = cast(int[])a;
    uint[3] c = cast(uint[3])a;

    const char[] cc;
    static assert( __traits(compiles, cast(const(ubyte)[])cc));
    static assert( __traits(compiles, cast(const(ubyte[]))cc));
    static assert(!__traits(compiles, cast(shared(ubyte)[])cc));

    shared char[] sc;
    static assert( __traits(compiles, cast(shared(ubyte)[])sc));
    static assert( __traits(compiles, cast(shared(ubyte[]))sc));
    static assert(!__traits(compiles, cast(const(ubyte)[])sc));
}

@safe
void structcast()
{
    struct A { ptrdiff_t x; }
    struct B { size_t x; }
    struct C { void* x; }
    A a;
    B b;
    C c;

    static assert( __traits(compiles, a = cast(A)b));
    static assert( __traits(compiles, a = cast(A)c));
    static assert( __traits(compiles, b = cast(B)a));
    static assert( __traits(compiles, b = cast(B)c));
    static assert(!__traits(compiles, c = cast(C)a));
    static assert(!__traits(compiles, c = cast(C)b));
}

@safe void test6497()
{
    int n;
    (0 ? n : n) = 3;
}

@safe
void varargs()
{
    static void fun(string[] val...) {}
    fun("a");
}

extern(C++) interface E {}
extern(C++) interface F : E {}

@safe
void classcast()
{
    class A {}
    class B : A {}

    A a;
    B b;

    static assert( __traits(compiles, cast(A)a));
    static assert( __traits(compiles, cast(B)a));
    static assert( __traits(compiles, cast(A)b));
    static assert( __traits(compiles, cast(B)b));

    const A ca;
    const B cb;

    static assert( __traits(compiles, cast(const(A))ca));
    static assert( __traits(compiles, cast(const(B))ca));
    static assert( __traits(compiles, cast(const(A))cb));
    static assert( __traits(compiles, cast(const(B))cb));

    static assert(!__traits(compiles, cast(A)ca));
    static assert(!__traits(compiles, cast(B)ca));
    static assert(!__traits(compiles, cast(A)cb));
    static assert(!__traits(compiles, cast(B)cb));

    interface C {}
    interface D : C {}

    C c;
    D d;

    static assert( __traits(compiles, cast(C)c));
    static assert( __traits(compiles, cast(D)c));
    static assert( __traits(compiles, cast(C)d));
    static assert( __traits(compiles, cast(D)d));

    E e;
    F f;

    static assert( __traits(compiles, cast(E)e));
    static assert(!__traits(compiles, cast(F)e));
    static assert( __traits(compiles, cast(E)f));
    static assert( __traits(compiles, cast(F)f));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6278

@safe
{

class A6278 {
    int test()
    in { assert(0); }
    do { return 1; }
}
class B6278 : A6278 {
    override int test()
    in { assert(0); }
    do { return 1; }
}

}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7803

@safe int f7803() {
    scope(success) {/* ... */}
    return 3;
}

nothrow int g7803() {
    scope(success) {/* ... */}
    return 3;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6405

void foo6405(int[][] args...) @trusted { }
void test6405() @safe { foo6405([1,2,3], [1,2,3]); }

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12502

void test12502() @safe
{
    const char[1] arr;
    auto ax = cast(const(char) []) arr[]; // ok
    auto a1 = cast(const(ubyte)[]) arr[]; // ok
    auto a2 = cast(const(char) []) arr;   // ok
    auto a3 = cast(const(ubyte)[]) arr;   // ok <- error
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14162

@trusted auto trusted(alias fun)() { return fun(); }

@safe void func1()()
{
    char[3] s = "abc";
    string t = trusted!(() => cast(string)(s[]));
    assert(t == "abc");
}

@safe void func2()()
{
    char[3] s = "abc";
    string t = trusted!(() => cast(string)(s[]));
    assert(t == "abc");
}

@safe void test14162()
{
    func1();
    func2();
}

/***************************************************/

void main()
{
    test14162();
}
