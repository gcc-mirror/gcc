/*
TEST_OUTPUT:
---
fail_compilation/pragmainline.d(8): Error: one boolean expression expected for `pragma(inline)`, not 3
---
*/

pragma(inline, 1,2,3) void bar();
pragma(inline, "string") void baz(); // works now
