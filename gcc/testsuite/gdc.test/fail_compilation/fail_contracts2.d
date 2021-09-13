/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts2.d(8): Error: missing `do { ... }` after `in` or `out`
---
*/

void foo()in{}{}
