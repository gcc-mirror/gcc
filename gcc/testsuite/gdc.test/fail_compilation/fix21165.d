/*
TEST_OUTPUT:
---
fail_compilation/fix21165.d(12): Error: undefined identifier `foo`
fail_compilation/fix21165.d(16): Error: undefined identifier `foo`
fail_compilation/fix21165.d(21): Error: undefined identifier `foo`
fail_compilation/fix21165.d(30): Error: undefined identifier `foo`
---
*/

// Test case from Issue #21165
foo
f() {}

// Test case from Issue #21171
foo
bar;

// Test case from Issue #21169
void fun(
	foo x
) {}

// Test case from Issue #21168
enum plusOne(int x) = x + 1;

alias fooPlusOne =
	plusOne
	!
	foo;
