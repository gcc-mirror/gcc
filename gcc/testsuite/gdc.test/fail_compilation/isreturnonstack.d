/*
TEST_OUTPUT:
---
fail_compilation/isreturnonstack.d(11): Error: argument to `__traits(isReturnOnStack, int)` is not a function
fail_compilation/isreturnonstack.d(12): Error: expected 1 arguments for `isReturnOnStack` but had 2
---
*/

int test() { return 0; }

enum b = __traits(isReturnOnStack, int);
enum c = __traits(isReturnOnStack, test, int);
