/*
TEST_OUTPUT:
---
fail_compilation/fail307.d(11): Error: cannot implicitly convert expression `cast(int)(cast(double)cast(int)b + 6.1)` of type `int` to `short`
---
*/

void main()
{
    ubyte b = 6;
    short c5 = cast(int)(b + 6.1);
}
