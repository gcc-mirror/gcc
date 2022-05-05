/* TEST_OUTPUT:
---
fail_compilation/test3818.d(104): Error: missing `; expression` before `)` of `foreach`
fail_compilation/test3818.d(104):        perhaps the `;` goes before `a`
fail_compilation/test3818.d(109): Error: missing `; expression` before `)` of `foreach`
fail_compilation/test3818.d(109):        perhaps the `;` goes before `c`
fail_compilation/test3818.d(110): Error: declaration expected, not `{`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=3818

#line 100

void test1()
{
    int[10] a;
    foreach (i, x, a)
    {
    }
}

static foreach (a, b, c)
{
}
