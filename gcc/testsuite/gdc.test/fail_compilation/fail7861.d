/*
TEST_OUTPUT:
---
fail_compilation/fail7861.d(17): Error: no property `nonexistent` for type `test.B`
---
*/
module test;

mixin template A() {
import test;
}

struct B {
mixin A!();
}

enum C = B.nonexistent;
