// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail3150.d(10): Error: cannot cast expression `[1, 2]` of type `int[]` to `ulong`
---
*/

void main() {
    ulong u = cast(ulong)[1,2];
}
