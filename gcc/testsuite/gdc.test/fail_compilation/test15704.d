/*
 * TEST_OUTPUT:
---
fail_compilation/test15704.d(19): Error: copying `void[]` to `void[]` is not allowed in a `@safe` function
fail_compilation/test15704.d(20): Error: copying `const(void)[]` to `void[]` is not allowed in a `@safe` function
fail_compilation/test15704.d(21): Deprecation: copying `int[]` to `void[]` will become `@system` in a future release
fail_compilation/test15704.d(22): Error: cannot implicitly convert expression `cast(byte)0` of type `byte` to `void[]`
fail_compilation/test15704.d(22): Error: cannot copy `byte` to `void[]`
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
    arr1[] = byte(); // filling not allowed
}
