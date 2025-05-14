/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/test22818.d(102): Deprecation: typesafe variadic parameters with a `class` type (`C c...`) are deprecated
fail_compilation/test22818.d(104): Error: scope parameter `c` may not be returned
---
*/

// issues.dlang.org/show_bug.cgi?id=22818

#line 100

@safe:
ref int g(C c ...)
{
    return c.x;
}

class C
{
    int x;
}
