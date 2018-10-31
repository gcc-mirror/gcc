/*
TEST_OUTPUT:
---
fail_compilation/diag4540.d(11): Error: `x` must be of integral or string type, it is a `float`
---
*/

void main()
{
    float x;
    switch (x)
    {
        default:
    }
}
