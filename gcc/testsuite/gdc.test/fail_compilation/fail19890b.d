// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/fail19890b.d(8): Error: `void[/^[0-9]+(LU)?$/]` size 1 * /^[0-9]+$/ exceeds 0x7fffffff size limit for static array
---
*/
void[] f = cast(void[-2]) "a";
