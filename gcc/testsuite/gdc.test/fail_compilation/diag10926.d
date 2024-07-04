/*
TEST_OUTPUT:
---
fail_compilation/diag10926.d(11): Error: cannot modify expression `cast(const(int)[])c` because it is not an lvalue
---
*/

void main() {
    const(int)[] a, b;
    int[] c, d;
    (true ? a : c) ~= 20; // line 6, Error: a is not an lvalue
}
