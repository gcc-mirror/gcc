/*
TEST_OUTPUT:
---
fail_compilation/fix21042.d(14): Error: template `gun` is not callable using argument types `!()(int)`
fail_compilation/fix21042.d(14):        no parameter named `x`
fail_compilation/fix21042.d(10):        Candidate is: `gun(T)(T a)`
---
*/

void gun(T)(T a) {}

void main()
{
	gun(x: 1); // (no explanation) --> (no parameter named `x`)
}
