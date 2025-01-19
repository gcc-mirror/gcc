/*
 * TEST_OUTPUT:
---
fail_compilation/test15704.d(17): Error: copying `void[]` to `void[]` is not allowed in a `@safe` function
fail_compilation/test15704.d(18): Error: copying `const(void)[]` to `void[]` is not allowed in a `@safe` function
fail_compilation/test15704.d(19): Deprecation: copying `int[]` to `void[]` will become `@system` in a future release
---
 */

// https://issues.dlang.org/show_bug.cgi?id=15704

void main() @safe {
    Object[] objs = [ new Object() ];
    void[] arr1 = objs;
    void[] arr2 = [ 123, 345, 567 ];

    arr1[] = arr2[];  // overwrites pointers with arbitrary ints
    arr1[] = new const(void)[3];
    arr1[] = [5];
}
