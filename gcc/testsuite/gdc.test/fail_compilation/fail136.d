/*
TEST_OUTPUT:
---
fail_compilation/fail136.d(10): Error: found `"EF BB BF"` when expecting `;` following statement
---
*/

void main()
{
    x"EF BB BF";
}
