void f(int x, Object y);

void g()
{
	Object o;
	f(o, o, 404);
	f(5, 6, 404);
}

/*
TEST_OUTPUT:
---
fail_compilation/bug16165.d(6): Error: function `f` is not callable using argument types `(Object, Object, int)`
fail_compilation/bug16165.d(6):        cannot pass argument `o` of type `object.Object` to parameter `int x`
fail_compilation/bug16165.d(1):        `bug16165.f(int x, Object y)` declared here
fail_compilation/bug16165.d(7): Error: function `f` is not callable using argument types `(int, int, int)`
fail_compilation/bug16165.d(7):        cannot pass argument `6` of type `int` to parameter `Object y`
fail_compilation/bug16165.d(1):        `bug16165.f(int x, Object y)` declared here
---
 */
