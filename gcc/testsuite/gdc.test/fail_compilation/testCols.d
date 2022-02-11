// REQUIRED_ARGS: -vcolumns

/*
TEST_OUTPUT:
---
fail_compilation/testCols.d(12,5): Error: undefined identifier `nonexistent`
---
*/

void test()
{
    nonexistent();
}
