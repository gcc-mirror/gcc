/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/traits_child.d(100): Error: expected 2 arguments for `child` but had 1
fail_compilation/traits_child.d(101): Error: symbol or expression expected as first argument of __traits `child` instead of `long`
fail_compilation/traits_child.d(102): Error: symbol expected as second argument of __traits `child` instead of `3`
---
*/

#line 100
enum a = __traits(child, long);
enum b = __traits(child, long, 3);
enum c = __traits(child, "hi", 3);
