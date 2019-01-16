/*
TEST_OUTPUT:
---
fail_compilation/fail74.d(13): Error: cannot append type C[1] to type C[1]
---
*/

class C
{
    C[1] c;
    this()
    {
        c ~= c;
    }
}
