/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test18644.d(15): Error: escaping a `scope` value returned from nested function `foo` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18644.d(16): Error: escaping local variable through nested function `foo` is not allowed in a `@safe` function
fail_compilation/test18644.d(22): Error: escaping local variable through nested function `foo` is not allowed in a `@safe` function
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
