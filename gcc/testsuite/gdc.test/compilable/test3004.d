// https://issues.dlang.org/show_bug.cgi?id=3004
/*
REQUIRED_ARGS: -ignore -v
TRANSFORM_OUTPUT: remove_lines("^(predefs|binary|version|config|DFLAG|parse|import|semantic|entry|library|function  object|function  core|\s*$)")
TEST_OUTPUT:
---
pragma    GNU_attribute (__error)
pragma    GNU_attribute (__error)
code      test3004
function  test3004.test
---
*/

extern(C) int printf(char*, ...);

pragma(GNU_attribute, flatten)
void test() { printf("Hello GNU world!\n".dup.ptr); }

pragma(GNU_attribute, flatten);
