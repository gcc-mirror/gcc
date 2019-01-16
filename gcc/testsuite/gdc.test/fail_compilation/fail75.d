/*
TEST_OUTPUT:
---
fail_compilation/fail75.d(13): Error: cannot append type fail75.C to type C[1]
---
*/

class C
{
    C[1] c;
    this()
    {
        c ~= this;
    }
}
