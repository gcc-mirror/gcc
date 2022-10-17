// EXTRA_OBJC_SOURCES:
/*
TEST_OUTPUT:
---
fail_compilation/objc_class2.d(14): Error: function `objc_class2.A.test` number of colons in Objective-C selector must match number of parameters
---
*/

import core.attribute : selector;

extern (Objective-C)
extern class A
{
    void test(int a, int b, int c) @selector("test:"); // non-matching number of colon
}
