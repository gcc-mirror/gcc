/*
TEST_OUTPUT:
---
fail_compilation/diag11840.d(12): Error: undefined identifier `i`
fail_compilation/diag11840.d(12): Error: undefined identifier `j`
---
*/

void main()
{
    int[10] data;
    data[i .. j] = 0;
}
