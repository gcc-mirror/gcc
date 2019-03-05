// REQUIRED_ARGS: -vcolumns
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/testCols.d(13,5): Error: undefined identifier `nonexistent`
---
*/

void test()
{
    nonexistent();
}
