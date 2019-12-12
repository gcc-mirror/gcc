/*
TEST_OUTPUT:
---
fail_compilation/fail9063.d(9): Error: static assert  "msg"
---
*/

@property string bar() { return "msg"; }
static assert(false, bar);
