/*
TEST_OUTPUT:
---
fail_compilation/diag11425.d(13): Error: variable x is shadowing variable diag11425.main.x
---
*/

void main()
{
    int x;

    {
        int x = 1;
    }
}
