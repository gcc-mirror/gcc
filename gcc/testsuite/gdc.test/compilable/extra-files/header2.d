// for D 2.0 only

class C { }

void foo(const C c, const(char)[] s, const int* q, const (int*) p)
{
}

void bar(in void *p)
{
}

void f(void function() f2);

class C2;
void foo2(const C2 c);

struct Foo3
{
   int k;
   ~this() @trusted @disable @nogc @live { k = 1; }
   this(this) { k = 2; }
}


class C3 { @property int get() { return 0; } }

T foo3(T)() {}

struct S4A(T)
{
   T x;
   ~this() @safe {}
}

struct S4B(T) if (1)
{
   T x;
}

union U4A(T)
{
    T x;
}

union U4B(T) if (2*4 == 8)
{
   T x;
}

class C4A(T)
{
  T x;
}

class C4B(T) if (true) { T x; }

class C4C(T) : C4A!int if (!false)
{
  T x;
}

class C4D(T) if (!false) : C4B!long, C4C!(int[])
{
    T x;
}

interface I4(T) if ((int[1]).length == 1) { T x; }

// eponymous template case
template MyClass4(T)
    if (is(typeof(T.subtype)))
{
    alias HelperSymbol = T.subtype;
    class MyClass4 {}
}

enum isInt(T) = is(T == int);
enum bool isString(T) = is(T == string);
static immutable typeName(T) = T.stringof;
int storageFor(T) = 0;

template templateVariableFoo(T)
{
    enum int templateVariableFoo = T.stringof.length;
}
template templateVariableBar(T) if (is(T == int))
{
    enum int templateVariableBar = T.stringof.length;
}

auto flit = 3 / 2.0;

// https://issues.dlang.org/show_bug.cgi?id=11217
void foo11217()(    const int[] arr) {}
void foo11217()(immutable int[] arr) {}
void foo11217()(      ref int[] arr) {}
void foo11217()(     lazy int[] arr) {}
void foo11217()( auto ref int[] arr) {}
void foo11217()(    scope int[] arr) {}
void foo11217()(       in int[] arr) {}
void foo11217()(    inout int[] arr) {}

// https://issues.dlang.org/show_bug.cgi?id=13275
void test13275()
{
    if (        auto n = 1) {}
    if (       const n = 1) {}
    if (   immutable n = 1) {}
    if (shared       n = 1) {}
    if (shared const n = 1) {}

    if (             int  n = 1) {}

    if (       const int  n = 1) {}
    if (   immutable int  n = 1) {}
    if (shared       int  n = 1) {}
    if (shared const int  n = 1) {}

    if (       const(int) n = 1) {}
    if (   immutable(int) n = 1) {}
    if (shared      (int) n = 1) {}
    if (shared const(int) n = 1) {}

    foreach (             e; [1,2]) {}
    foreach (       const e; [1,2]) {}
    foreach (   immutable e; [1,2]) {}
    foreach (shared       e; [1,2]) {}
    foreach (shared const e; [1,2]) {}

    foreach (             int e; [1,2]) {}
    foreach (       const int e; [1,2]) {}
    foreach (   immutable int e; [1,2]) {}
    foreach (shared       int e; [1,2]) {}
    foreach (shared const int e; [1,2]) {}

    foreach (             int e; [1,2]) {}
    foreach (       const(int) e; [1,2]) {}
    foreach (   immutable(int) e; [1,2]) {}
    foreach (shared      (int) e; [1,2]) {}
    foreach (shared const(int) e; [1,2]) {}
}

// https://issues.dlang.org/show_bug.cgi?id=9766
align (1) struct S9766
{
align {}
align (true ? 2 : 3):
    int var1;

align:
    int var2;
}

align(2) struct S12200_1
{
align:
}

align(2) struct S12200_2
{
align(1):
}

// https://issues.dlang.org/show_bug.cgi?id=14694
inout(T)[] overlap(T)(inout(T)[] r1, inout(T)[] r2) @trusted pure nothrow
{
    alias U = inout(T);
    static U* max(U* a, U* b) nothrow { return a > b ? a : b; }
    static U* min(U* a, U* b) nothrow { return a < b ? a : b; }

    auto b = max(r1.ptr, r2.ptr);
    auto e = min(r1.ptr + r1.length, r2.ptr + r2.length);
    return b < e ? b[0 .. e - b] : null;
}

// https://issues.dlang.org/show_bug.cgi?id=16140
void gun()()
{
    int[] res;
    while (auto va = fun()) {}  // expression expected, not 'auto'

    while (true)
        if (auto va = fun()) {}
        else break;
}

// https://issues.dlang.org/show_bug.cgi?id=14690
pragma(inline, true)
int fun(int a, int b)
{
    return 3;
}

// https://issues.dlang.org/show_bug.cgi?id=16649
void leFoo()()
{
    sign = a == 2 ? false : (y < 0) ^ sign;
    sign = a == 2 ? false : sign ^ (y < 0);
    sign = 2 + 3 | 7 + 5;
}

// https://issues.dlang.org/show_bug.cgi?id=17371
interface LeInterface
{}
class LeClass
{
    this()
    {
        auto foo = new class () LeInterface {};
    }
}
const levar = new class LeClass, LeInterface {};

// https://issues.dlang.org/show_bug.cgi?id=20074
class CC
{
    void fun()() @safe
    {
        () @trusted pure
        {
        } ();
    }
}

// https://issues.dlang.org/show_bug.cgi?id=17663
private:
public struct Export {}
