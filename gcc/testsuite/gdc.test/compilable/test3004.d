// https://issues.dlang.org/show_bug.cgi?id=3004
/*
REQUIRED_ARGS: -ignore -v
TRANSFORM_OUTPUT: remove_lines("^(predefs|binary|version|config|DFLAG|parse|import|semantic|entry|library|function  object|\s*$)")
TEST_OUTPUT:
---
pragma    GNU_attribute (__error)
pragma    GNU_attribute (__error)
code      test3004
function  test3004.test
function  core.internal.array.appending._d_arrayappendcTXImpl!(char[], char)._d_arrayappendcTX
function  core.internal.array.utils._d_HookTraceImpl!(char[], _d_arrayappendcTX, "Cannot append to array if compiling without support for runtime type information!")._d_HookTraceImpl
---
*/

extern(C) int printf(char*, ...);

pragma(GNU_attribute, flatten)
void test() { printf("Hello GNU world!\n".dup.ptr); }

pragma(GNU_attribute, flatten);
