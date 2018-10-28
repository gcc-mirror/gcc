/*
TEST_OUTPUT:
---
fail_compilation/pragmainline.d(9): Error: pragma inline one boolean expression expected for `pragma(inline)`, not 3
fail_compilation/pragmainline.d(10): Error: pragma inline pragma(`inline`, `true` or `false`) expected, not `"string"`
---
*/

pragma(inline, 1,2,3) void bar();
pragma(inline, "string") void baz();

