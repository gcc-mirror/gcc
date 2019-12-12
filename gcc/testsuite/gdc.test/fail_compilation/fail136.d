/*
TEST_OUTPUT:
---
fail_compilation/fail136.d(10): Error: `string` has no effect in expression `"\xef\xbb\xbf"`
---
*/

void main()
{
    x"EF BB BF";
}
