/*
TEST_OUTPUT:
---
fail_compilation/b19523.d(12): Error: undefined identifier `SomeStruct`
fail_compilation/b19523.d(13): Error: function `b19523.foo(int delegate() arg)` is not callable using argument types `(_error_)`
fail_compilation/b19523.d(13):        cannot pass argument `__lambda2` of type `_error_` to parameter `int delegate() arg`
---
*/
module b19523;

void bar () {
	SomeStruct s;
	foo({
		return s;
	});
}

void foo (int delegate() arg) {}
