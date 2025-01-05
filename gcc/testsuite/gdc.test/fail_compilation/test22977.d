/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22977.d(16): Error: escaping local variable through nested function `scfunc`
fail_compilation/test22977.d(22): Error: escaping reference to stack allocated value returned by `scfunc2()`
---
*/

// Issue 22977 - [dip1000] can escape scope pointer returned by nested function
// https://issues.dlang.org/show_bug.cgi?id=22977

auto p0(scope string s) @safe
{
    string scfunc() { return s; }
    return scfunc();
}

auto p1(scope string s) @safe
{
    ref string scfunc2() { return s; }
    return scfunc2();
}

// Reduced from Mir
struct Tuple(T...)
{
    T expand;
}

auto autoExpandAndForward(alias value)()
{
    return value.expand[0];
}

struct MapIterator
{
    int* p;
    int* foo() scope
    {
        auto t = Tuple!(int*)(p);
        return autoExpandAndForward!t;
    }
}

// Reduced from Phobos
float partial(alias fun)()
{
    return fun();
}

auto partialFunction() @safe
{
    int function() f = () => 0;
    return &partial!(f);
}
