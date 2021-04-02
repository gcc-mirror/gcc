/*
TEST_OUTPUT:
---
fail_compilation/ice9013.d(9): Error: undefined identifier `missing`
---
*/
void main()
{
    foreach (i; 0 .. missing)
        int[] foo = cast(int[])[i];
}
