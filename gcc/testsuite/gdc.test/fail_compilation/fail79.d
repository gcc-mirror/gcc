/*
TEST_OUTPUT:
---
fail_compilation/fail79.d(13): Error: incompatible types for ((& a) + (& b)): 'int*' and 'int*'
---
*/

void main()
{
    int a, b;
    int* p;

    p = &a + &b;
}

