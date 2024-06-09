/*
TEST_OUTPUT:
---
fail_compilation/fail7861.d(18): Error: no property `nonexistent` for type `test.B`
fail_compilation/fail7861.d(14):        struct `B` defined here
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
