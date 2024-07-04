// https://issues.dlang.org/show_bug.cgi?id=24110

/*
TEST_OUTPUT:
---
fail_compilation/test24110.d(12): Error: static assert:  `__traits(compiles, __error)` is false
---
*/

struct S { int x; }
alias T = shared S;
static assert(__traits(compiles, (T[] a, T[] b) => a < b));
