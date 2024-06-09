/*
TEST_OUTPUT:
---
fail_compilation/diag11425.d(14): Error: variable `x` is shadowing variable `diag11425.main.x`
fail_compilation/diag11425.d(11):        declared here
---
*/

void main()
{
    int x;

    {
        int x = 1;
    }
}
