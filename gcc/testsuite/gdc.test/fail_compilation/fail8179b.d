// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail8179b.d(10): Error: cannot cast expression `[1, 2]` of type `int[]` to `int[2][1]`
---
*/
void foo(int[2][1]) {}
void main() {
    foo(cast(int[2][1])[1, 2]);
}

