// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/diag12480.d(12): Error: static assert:  `2u == 3u` is false
---
*/

module diag12480;

static immutable arr = ["a", "b"];
static assert(arr.length == 3);
