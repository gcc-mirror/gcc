/*
TEST_OUTPUT:
---
fail_compilation/b19717.d(16): Error: undefined identifier `Foo`, did you mean function `foo`?
fail_compilation/b19717.d(13): Error: forward reference to template `foo`
fail_compilation/b19717.d(13): Error: forward reference to inferred return type of function call `foo()`
---
*/

enum bar = __traits(getMember, mixin(__MODULE__), "foo");

auto foo() {
	return foo();
}

void foo(Foo) {}
