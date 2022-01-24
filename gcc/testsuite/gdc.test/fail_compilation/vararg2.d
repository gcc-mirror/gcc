/* TEST_OUTPUT:
---
fail_compilation/vararg2.d(106): Error: function `vararg2.foo(int x, const return ...)` is not callable using argument types `(double)`
fail_compilation/vararg2.d(106):        cannot pass argument `1.0` of type `double` to parameter `int x`
fail_compilation/vararg2.d(111): Error: function `vararg2.bar(int x, scope shared ...)` is not callable using argument types `(double)`
fail_compilation/vararg2.d(111):        cannot pass argument `1.0` of type `double` to parameter `int x`
---
*/

#line 100

int* foo(int x, return const ...);
int* bar(int x, scope shared ...);

void test1()
{
    foo(1.0);
}

void test2()
{
    bar(1.0);
}
