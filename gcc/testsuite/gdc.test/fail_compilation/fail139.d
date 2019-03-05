/*
TEST_OUTPUT:
---
fail_compilation/fail139.d(8): Error: forward reference to 'test'
---
*/

void test(typeof(&test) p)
{
}

void main()
{
    test(null);
}
