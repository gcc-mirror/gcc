/*
TEST_OUTPUT:
---
fail_compilation/diag3438b.d(9): Error: default argument expected for `y`
---
*/

// Make sure the deprecation doesn't interfere w/ the check for default arguments
struct S { this(int x = 1, int y) { } }
