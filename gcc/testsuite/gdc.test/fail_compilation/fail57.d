/*
TEST_OUTPUT:
---
fail_compilation/fail57.d(11): Error: divide by 0
fail_compilation/fail57.d(11): Error: divide by 0
---
*/

int main()
{
    int x = 1 / 0;
    return 0;
}
