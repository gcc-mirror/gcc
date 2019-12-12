// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/fail19955.d(8): Error: `switch` statement without a `default`; use `final switch` or add `default: assert(0);` or add `default: break;`
---
*/
void f() { switch(1) static assert(1); }
