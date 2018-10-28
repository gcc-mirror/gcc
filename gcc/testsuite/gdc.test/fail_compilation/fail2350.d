/*
TEST_OUTPUT:
---
fail_compilation/fail2350.d(8): Error: function fail2350.test2350 naked assembly functions with contracts are not supported
---
*/

void test2350()
in
{
}
body
{
	asm { naked; }
}
