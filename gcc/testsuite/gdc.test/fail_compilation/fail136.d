/*
TEST_OUTPUT:
---
fail_compilation/fail136.d(10): Error: Built-in hex string literals are obsolete, use `std.conv.hexString!"EF BB BF"` instead.
---
*/

void main()
{
    x"EF BB BF";
}
