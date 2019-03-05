/*
TEST_OUTPUT:
---
fail_compilation/diag10926.d(11): Error: cast(const(int)[])c is not an lvalue
---
*/

void main() {
    const(int)[] a, b;
    int[] c, d;
    (true ? a : c) ~= 20; // line 6, Error: a is not an lvalue
}
