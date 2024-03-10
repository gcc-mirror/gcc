/* REQUIRED_ARGS: -preview=nosharedaccess
TEST_OUTPUT:
---
pure nothrow @nogc ref @safe shared(C1)(return ref shared(C1) c)
pure nothrow @nogc ref @safe shared(int)(return ref shared(C3) c)
---
*/
ref shared(int) f(return shared ref int y)
{
    return y;
}

// https://issues.dlang.org/show_bug.cgi?id=20908
struct S
{
    int i = 2;
}

union U
{
    int i = 1;
    bool b;
}

void test20908()
{
    // shared locals (or struct members) should be able to be initialised:
    shared int x;

    ref shared(int) fun()
    {
        static shared(int) val;

        // return by reference
        return val;
    }

    ref shared(int) fun2()
    {
        static shared(int)* val;

        // transfer pointer to reference
        return *val;
    }

    ref shared(int) fun3()
    {
        static shared(int)*** val;

        // Multiple indirections
        return ***val;
    }

    shared S s;
    shared U u;
}

// Simple tests for `DotVarExp`
// A `DotVarExp` is `a.b`. If `a` is a `shared ref`,
// it is of type `shared(T)*` (as opposed to `shared(T*)`).
// We should allow arbitrarily nested `DotVarExp` as long
// as no shared memory is read, as in the case above
// (we're just offsetting a pointer).
struct C1
{
    int value;
}

struct C2
{
    C1 c1;
}

struct C3
{
    C2 c1;
    C2 c2;
}

ref shared(int) test_dotvarexp_1(return ref shared C1 c)
{
    return c.value;
}

shared(int)* test_dotvarexp_2(return ref shared C1 c)
{
    return &c.value;
}

shared(C2)* test_dotvarexp_3(return ref shared C3 c)
{
    return &c.c1;
}

shared(C2)* test_dotvarexp_4(return ref shared C3 c)
{
    return &c.c2;
}

ref shared(int) test_dotvarexp_5(return shared ref C3 c)
{
    return c.c1.c1.value;
}

ref shared(int) test_dotvarexp_5(return ref shared(C3)[] c)
{
    return c[0].c1.c1.value;
}

// Test `auto` inference
auto ref test_inference_1(return ref shared C1 c)
{
    return c;
}

pragma(msg, typeof(test_inference_1));

auto ref test_inference_2(return ref shared C3 c)
{
    return c.c2.c1.value;
}

pragma(msg, typeof(test_inference_2));

// https://issues.dlang.org/show_bug.cgi?id=21793

struct Child
{
    this(int) shared {}
}

struct Parent
{
    shared Child ch;
    this(int i) shared
    {
        ch = shared Child(i);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=23732
class Class {}
void main()
{
    auto b = new shared Class();
}

// https://issues.dlang.org/show_bug.cgi?id=23790
bool cas(shared bool*, bool, bool) { return true; }

struct Argh
{
    bool locked;
    void lock() shared
    {
        while(!cas(&locked, false, true)) {}
    }
}
