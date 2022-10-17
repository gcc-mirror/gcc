// https://issues.dlang.org/show_bug.cgi?id=22529

/*
TEST_OUTPUT:
---
fail_compilation/fail22529.d(13): Error: found `return` when expecting `;` following statement
---
*/

void main()
{
    foo()
    return;
}
