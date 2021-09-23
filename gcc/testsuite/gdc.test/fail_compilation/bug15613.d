/*
TEST_OUTPUT:
---
fail_compilation/bug15613.d(16): Error: function `bug15613.f(int...)` is not callable using argument types `(typeof(null))`
fail_compilation/bug15613.d(16):        cannot pass argument `null` of type `typeof(null)` to parameter `int...`
fail_compilation/bug15613.d(17): Error: function `bug15613.g(Object, ...)` is not callable using argument types `(int)`
fail_compilation/bug15613.d(17):        cannot pass argument `8` of type `int` to parameter `Object`
---
*/

void f(int...);
void g(Object, ...);

void main()
{
    f(null);
    g(8);
}
