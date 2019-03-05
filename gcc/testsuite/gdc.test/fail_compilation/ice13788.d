/*
TEST_OUTPUT:
---
fail_compilation/ice13788.d(11): Error: pragma mangle string expected for mangled name
fail_compilation/ice13788.d(12): Error: string expected for mangled name, not (1) of type int
fail_compilation/ice13788.d(13): Error: pragma mangle zero-length string not allowed for mangled name
fail_compilation/ice13788.d(14): Error: pragma mangle mangled name characters can only be of type char
---
*/

pragma(mangle) void f1();
pragma(mangle, 1) void f2();
pragma(mangle, "") void f3();
pragma(mangle, "a"w) void f4();
