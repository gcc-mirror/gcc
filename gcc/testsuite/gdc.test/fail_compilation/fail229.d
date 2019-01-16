/*
TEST_OUTPUT:
---
fail_compilation/fail229.d(11): Error: array index 18446744073709551615 overflow
fail_compilation/fail229.d(11): Error: array dimension overflow
---
*/

// Issue 1936 - Error with no line number (array dimension overflow)

static int[] x = [-1: 1];
