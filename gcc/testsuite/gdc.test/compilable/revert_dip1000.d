/*
REQUIRED_ARGS: -revert=dip1000
TEST_OUTPUT:
---
---
*/
int* oops(scope int* p) @safe { return p; }
