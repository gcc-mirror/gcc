/*
TEST_OUTPUT:
---
fail_compilation/ice12836.d(9): Error: undefined identifier `C`
fail_compilation/ice12836.d(9): Error: undefined identifier `K`
---
*/

immutable C L = 1 << K;
