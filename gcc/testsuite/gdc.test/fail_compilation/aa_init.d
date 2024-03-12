/*
REQUIRED_ARGS: -vcolumns
TEST_OUTPUT:
---
fail_compilation/aa_init.d(13,18): Error: invalid associative array initializer `[]`, use `null` instead
fail_compilation/aa_init.d(14,24): Error: missing key for value `4` in initializer
fail_compilation/aa_init.d(15,9): Error: cannot implicitly convert expression `[]` of type `void[]` to `int[int]`
---
*/

void main()
{
    int[int] a = [];
    int[int] b = [2:3, 4];
    a = [];
}
