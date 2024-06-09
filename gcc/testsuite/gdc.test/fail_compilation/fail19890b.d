/* REQUIRED_ARGS: -m32
TEST_OUTPUT:
---
fail_compilation/fail19890b.d(8): Error: `void[cast(size_t)4294967294]` size 1 * 4294967294 exceeds 0x7fffffff size limit for static array
---
*/

void[] f = cast(void[-2]) "a";
