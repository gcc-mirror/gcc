/*
TEST_OUTPUT:
---
fail_compilation/bug15613.d(21): Error: function `f` is not callable using argument types `(typeof(null))`
fail_compilation/bug15613.d(21):        cannot pass argument `null` of type `typeof(null)` to parameter `int...`
fail_compilation/bug15613.d(16):        `bug15613.f(int...)` declared here
fail_compilation/bug15613.d(22): Error: function `f` is not callable using argument types `()`
fail_compilation/bug15613.d(22):        expected an argument for parameter `int...`
fail_compilation/bug15613.d(16):        `bug15613.f(int...)` declared here
fail_compilation/bug15613.d(23): Error: function `g` is not callable using argument types `(int)`
fail_compilation/bug15613.d(23):        cannot pass argument `8` of type `int` to parameter `Object`
fail_compilation/bug15613.d(17):        `bug15613.g(Object, ...)` declared here
---
*/

void f(int...);
void g(Object, ...);

void main()
{
    f(null);
    f();
    g(8);
}

#line 22
/*
TEST_OUTPUT:
---
fail_compilation/bug15613.d(35): Error: function `h` is not callable using argument types `(int, void function(int[]...))`
fail_compilation/bug15613.d(35):        cannot pass argument `& h` of type `void function(int[]...)` to parameter `int[]...`
fail_compilation/bug15613.d(31):        `bug15613.h(int[]...)` declared here
---
*/

void h(int[]...);

void test()
{
    h(7, &h);
}
