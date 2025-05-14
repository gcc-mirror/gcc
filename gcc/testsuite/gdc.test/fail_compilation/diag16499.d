/*
TEST_OUTPUT:
---
fail_compilation/diag16499.d(24): Error: operator `in` is not defined for type `A`
fail_compilation/diag16499.d(11):        perhaps overload the operator with `auto opBinaryRight(string op : "in")(int rhs) {}`
fail_compilation/diag16499.d(26): Error: operator `in` is not defined for type `B`
fail_compilation/diag16499.d(12):        perhaps overload the operator with `auto opBinaryRight(string op : "in")(double rhs) {}`
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
