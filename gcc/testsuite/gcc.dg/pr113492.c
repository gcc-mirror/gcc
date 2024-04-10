/* PR 113492
 * { dg-do compile }
 * { dg-options "-std=c23 -funsigned-bitfields" } */

struct foo {
	int i : 3;
} i;

void test()
{
	struct foo {
		unsigned i : 3;
	} u;

	1 ? i : u;
	1 ? u : i;
}
	
struct bar {
	unsigned i : 3;
} u;

void test2()
{
	struct bar {
		int i : 3;
	} i;

	1 ? i : u;
	1 ? u : i;
}
	
void test3()
{
	typedef int myint;
	struct bar {
		myint i : 3;
	} i;

	1 ? i : u;
	1 ? u : i;
}

