/*
TEST_OUTPUT:
---
fail_compilation/mangle2.d(9): Error: pragma `mangle` null character not allowed in mangled name
fail_compilation/mangle2.d(10): Error: pragma `mangle` null character not allowed in mangled name
---
*/
//\0 chars
__gshared pragma(mangle, "test\09") ubyte test9_5;
__gshared extern pragma(mangle, "test\09") ubyte test9_5_e;
