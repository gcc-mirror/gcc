/*
TEST_OUTPUT:
---
fail_compilation/parse14745.d(11): Error: function literal cannot be `immutable`
fail_compilation/parse14745.d(12): Error: function literal cannot be `const`
---
*/

void test14745()
{
    auto fp1 = function () pure immutable { return 0; };
    auto fp2 = function () pure const { return 0; };
}
