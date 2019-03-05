/*
TEST_OUTPUT:
---
fail_compilation/diag16499.d(22): Error: incompatible types for ((2) in (foo)): 'int' and 'A'
fail_compilation/diag16499.d(24): Error: incompatible types for ((1.00000) in (bar)): 'double' and 'B'
---
*/

struct A {}
struct B {
	void* opBinaryRight(string op)(int b) if (op == "in")
	{
		return null;
	}
}

void main()
{
	A foo;
	B bar;

	2 in foo;
	2 in bar; // OK
	1.0 in bar;
}
