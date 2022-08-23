/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test18644.d(15): Error: storing reference to stack allocated value returned by `foo()` into allocated memory causes it to escape
fail_compilation/test18644.d(16): Error: escaping reference to stack allocated value returned by `foo()`
fail_compilation/test18644.d(22): Error: escaping reference to stack allocated value returned by `foo()`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18644

@safe int* test1() {
    int i;
    int* foo() { return &i; }
    int*[] b = [foo()];
    return foo();
}

@safe ref int test2() {
    int i;
    ref int foo() { return i; }
    return foo();
}
