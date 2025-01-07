// https://issues.dlang.org/show_bug.cgi?id=24745

/*
TEST_OUTPUT:
---
fail_compilation/test24745.d(12): Error: incorrect syntax for associative array, expected `[]`, found `{}`
---
*/

void main()
{
    int[int] f = {1: 1, 2: 2};
}
