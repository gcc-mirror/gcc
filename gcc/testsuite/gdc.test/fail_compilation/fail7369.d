/*
TEST_OUTPUT:
---
fail_compilation/fail7369.d(9): Error: cannot modify `this.a` in `const` function
---
*/
struct S7369 {
    int a;
    invariant() { a += 5; }
}
