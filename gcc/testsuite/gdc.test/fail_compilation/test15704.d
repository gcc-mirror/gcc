/*
 * TEST_OUTPUT:
---
fail_compilation/test15704.d(15): Error: cannot copy void[] to void[] in @safe code
---
 */

// https://issues.dlang.org/show_bug.cgi?id=15704

void main() @safe {
    Object[] objs = [ new Object() ];
    void[] arr1 = objs;
    void[] arr2 = [ 123, 345, 567 ];

    arr1[] = arr2[];  // overwrites pointers with arbitrary ints
}

