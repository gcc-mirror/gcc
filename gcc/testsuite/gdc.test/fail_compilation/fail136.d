/*
TEST_OUTPUT:
---
fail_compilation\fail136.d(10): Error: `"\xef\xbb\xbf"` has no effect
---
*/

void main()
{
    x"EF BB BF";
}
