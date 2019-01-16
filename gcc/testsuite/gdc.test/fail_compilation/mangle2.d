/*
TEST_OUTPUT:
---
fail_compilation/mangle2.d(20): Error: pragma mangle char 0x20 not allowed in mangled name
fail_compilation/mangle2.d(21): Error: pragma mangle char 0x20 not allowed in mangled name
fail_compilation/mangle2.d(24): Error: pragma mangle char 0x0a not allowed in mangled name
fail_compilation/mangle2.d(25): Error: pragma mangle char 0x0a not allowed in mangled name
fail_compilation/mangle2.d(28): Error: pragma mangle char 0x07 not allowed in mangled name
fail_compilation/mangle2.d(29): Error: pragma mangle char 0x07 not allowed in mangled name
fail_compilation/mangle2.d(32): Error: pragma mangle char 0x01 not allowed in mangled name
fail_compilation/mangle2.d(33): Error: pragma mangle char 0x01 not allowed in mangled name
fail_compilation/mangle2.d(36): Error: pragma mangle char 0x00 not allowed in mangled name
fail_compilation/mangle2.d(37): Error: pragma mangle char 0x00 not allowed in mangled name
fail_compilation/mangle2.d(40): Error: pragma mangle Outside Unicode code space
fail_compilation/mangle2.d(41): Error: pragma mangle Outside Unicode code space
---
*/

//spaces
__gshared pragma(mangle, "test 9") ubyte test9_1;
__gshared extern pragma(mangle, "test 9") ubyte test9_1_e;

//\n chars
__gshared pragma(mangle, "test\n9") ubyte test9_2;
__gshared extern pragma(mangle, "test\n9") ubyte test9_2_e;

//\a chars
__gshared pragma(mangle, "test\a9") ubyte test9_3;
__gshared extern pragma(mangle, "test\a9") ubyte test9_3_e;

//\x01 chars
__gshared pragma(mangle, "test\x019") ubyte test9_4;
__gshared extern pragma(mangle, "test\x019") ubyte test9_4_e;

//\0 chars
__gshared pragma(mangle, "test\09") ubyte test9_5;
__gshared extern pragma(mangle, "test\09") ubyte test9_5_e;

//\xff chars
__gshared pragma(mangle, "test\xff9") ubyte test9_6;
__gshared extern pragma(mangle, "test\xff9") ubyte test9_6_e;

