/*
TEST_OUTPUT:
---
fail_compilation/staticassertargs.d(9): Error: static assert:  abcxe3!!
---
*/

enum e = "!!";
static assert(false, "abc", ['x', 'e'], 3, e);
