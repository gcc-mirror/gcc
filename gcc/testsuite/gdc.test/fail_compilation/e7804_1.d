/*
TEST_OUTPUT:
---
fail_compilation/e7804_1.d(14): Error: undefined identifier `Aggr`
fail_compilation/e7804_1.d(15): Error: unrecognized trait `farfelu`
fail_compilation/e7804_1.d(17): Error: undefined identifier `Aggr`
fail_compilation/e7804_1.d(18): Error: unrecognized trait `farfelu`
---
*/
module e7804_1;

struct S {}

__traits(farfelu, Aggr, "member") a;
__traits(farfelu, S, "member") a2;

alias foo = __traits(farfelu, Aggr, "member");
alias foo2 = __traits(farfelu, S, "member");
