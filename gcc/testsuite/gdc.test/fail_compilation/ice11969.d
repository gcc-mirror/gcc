/*
TEST_OUTPUT:
---
fail_compilation/ice11969.d(9): Error: undefined identifier `index`
fail_compilation/ice11969.d(10): Error: undefined identifier `cond`
fail_compilation/ice11969.d(11): Error: undefined identifier `msg`
---
*/
void test1() { mixin ([index]); }
void test2() { mixin (assert(cond)); }
void test3() { mixin (assert(0, msg)); }
