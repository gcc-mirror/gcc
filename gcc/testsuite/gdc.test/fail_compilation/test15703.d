/*
REQUIRED_ARGS: -m32
TEST_OUTPUT:
---
fail_compilation/test15703.d(23): Error: cast from `Object[]` to `uint[]` is not allowed in a `@safe` function
fail_compilation/test15703.d(23):        Target element type is mutable and source element type contains a pointer
fail_compilation/test15703.d(25): Error: cast from `object.Object` to `const(uint)*` is not allowed in a `@safe` function
fail_compilation/test15703.d(25):        Source type is incompatible with target type containing a pointer
fail_compilation/test15703.d(28): Error: cast from `uint[]` to `Object[]` is not allowed in a `@safe` function
fail_compilation/test15703.d(28):        Target element type contains a pointer
fail_compilation/test15703.d(44): Error: cast from `int[]` to `S[]` is not allowed in a `@safe` function
fail_compilation/test15703.d(44):        Target element type is opaque
fail_compilation/test15703.d(45): Error: cast from `S[]` to `int[]` is not allowed in a `@safe` function
fail_compilation/test15703.d(45):        Source element type is opaque
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15703

void test() @safe
{
     auto objs = [ new Object() ];
     auto longs = cast(size_t[]) objs;          // error
     auto longc = cast(const(size_t)[]) objs;   // ok
     auto longp = cast(const(size_t)*) objs[0]; // error

     size_t[] al;
     objs = cast(Object[]) al;                  // error

     auto am = cast(int[])[];
}

void test2() @safe
{
    const(ubyte)[] a;
    auto b = cast(const(uint[])) a;
}

struct S;

void opaque() @safe
{
    auto a = [1, 2];
    S[] b = cast(S[]) a;
    a = cast(int[]) b;
}
