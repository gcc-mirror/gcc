/*
TEST_OUTPUT:
---
fail_compilation/fail126.d(8): Error: forward reference to 'test'
---
*/

void test(typeof(test) p)
{
}
