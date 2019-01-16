/*
TEST_OUTPUT:
---
fail_compilation/faildeleteaa.d(11): Error: cannot delete type int
---
*/

void main()
{
    int[int] aa = [1 : 2];
    delete aa[1];
}
