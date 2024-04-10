/* TEST_OUTPUT:
----
fail_compilation/vararg2.d(106): Error: function `foo` is not callable using argument types `(double)`
fail_compilation/vararg2.d(106):        cannot pass argument `1.0` of type `double` to parameter `int x`
fail_compilation/vararg2.d(101):        `vararg2.foo(int x, const return ...)` declared here
fail_compilation/vararg2.d(111): Error: function `bar` is not callable using argument types `(double)`
fail_compilation/vararg2.d(111):        cannot pass argument `1.0` of type `double` to parameter `int x`
fail_compilation/vararg2.d(102):        `vararg2.bar(int x, scope shared ...)` declared here
----
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
