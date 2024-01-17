/* REQUIRED_ARGS: -m32
TEST_OUTPUT:
---
fail_compilation/fail19890a.d(8): Error: `void[cast(size_t)4294967295]` size 1 * 4294967295 exceeds 0x7fffffff size limit for static array
---
*/

void[] f = cast(void[-1]) "a";
