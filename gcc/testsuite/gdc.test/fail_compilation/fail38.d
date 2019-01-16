/*
TEST_OUTPUT:
---
fail_compilation/fail38.d(12): Error: 'super' is only allowed in non-static class member functions
---
*/

int x;

void test()
{
    super.x = 2;
}

int main()
{
    test();
    return 0;
}
