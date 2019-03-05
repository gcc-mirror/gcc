/*
TEST_OUTPUT:
---
fail_compilation/diag7998.d(10): Error: static assert  "abcxe"
---
*/

module diag7998;

static assert(false, "abc" ~['x'] ~ "e");
