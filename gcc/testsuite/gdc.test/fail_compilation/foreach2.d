/*
TEST_OUTPUT:
---
fail_compilation/foreach2.d(15): Error: argument type mismatch, `int` to `ref immutable(int)`
fail_compilation/foreach2.d(16): Error: argument type mismatch, `int` to `ref immutable(int)`
fail_compilation/foreach2.d(19): Error: argument type mismatch, `int` to `ref double`
fail_compilation/foreach2.d(20): Error: argument type mismatch, `int` to `ref const(double)`
fail_compilation/foreach2.d(21): Error: argument type mismatch, `int` to `ref immutable(double)`
---
*/
void test4090 ()
{
    // From https://issues.dlang.org/show_bug.cgi?id=4090
    int[] arr = [1,2,3];
    foreach (immutable ref x; arr) {}
    foreach (immutable ref int x; arr) {}

    // convertible type + qualifier + ref
    foreach (          ref double x; arr) {}
    foreach (    const ref double x; arr) {}
    foreach (immutable ref double x; arr) {}
}
