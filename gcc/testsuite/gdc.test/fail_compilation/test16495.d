/* TEST_OUTPUT:
---
fail_compilation/test16495.d(12): Error: undefined identifier `q`
fail_compilation/test16495.d(17): Error: expected 1 arguments for `fullyQualifiedName` but had 0
---
 */

// https://issues.dlang.org/show_bug.cgi?id=16495

void test1()
{
    auto m = __traits(fullyQualifiedName, q);
}

void test2()
{
    auto n = __traits(fullyQualifiedName);
}
