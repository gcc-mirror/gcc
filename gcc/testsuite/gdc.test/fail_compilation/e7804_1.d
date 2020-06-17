/*
TEST_OUTPUT:
---
fail_compilation/e7804_1.d(10): Error: trait `farfelu` is either invalid or not supported as type
fail_compilation/e7804_1.d(11): Error: trait `farfelu` is either invalid or not supported in alias
---
*/
module e7804_1;

__traits(farfelu, Aggr, "member") a;
alias foo = __traits(farfelu, Aggr, "member");
