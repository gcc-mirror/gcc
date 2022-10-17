/* TEST_OUTPUT:
---
fail_compilation/test20565.d(107): Error: function `test20565.test.box` the same declaration cannot be in multiple scopes with non-D linkage
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20565

#line 100

void test()
{
    {
        extern (C++) int box() { return 3; }
    }
    {
        extern (C++) int box() { return 4; }
    }
}
