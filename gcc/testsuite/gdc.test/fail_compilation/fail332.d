/*
TEST_OUTPUT:
---
fail_compilation/fail332.d(26): Error: function `foo` is not callable using argument types `()`
fail_compilation/fail332.d(26):        missing argument for parameter #1: `int __param_0`
fail_compilation/fail332.d(21):        `fail332.foo(int __param_0, ...)` declared here
fail_compilation/fail332.d(27): Error: function `foo` is not callable using argument types `(typeof(null))`
fail_compilation/fail332.d(27):        cannot pass argument `null` of type `typeof(null)` to parameter `int __param_0`
fail_compilation/fail332.d(21):        `fail332.foo(int __param_0, ...)` declared here
fail_compilation/fail332.d(29): Error: function `baz` is not callable using argument types `(string)`
fail_compilation/fail332.d(29):        cannot pass argument `""` of type `string` to parameter `int[] __param_0...`
fail_compilation/fail332.d(22):        `fail332.baz(int[] __param_0...)` declared here
fail_compilation/fail332.d(30): Error: function `baz` is not callable using argument types `(int, typeof(null))`
fail_compilation/fail332.d(30):        cannot pass argument `null` of type `typeof(null)` to parameter `int[] __param_0...`
fail_compilation/fail332.d(22):        `fail332.baz(int[] __param_0...)` declared here
---
*/

import core.vararg;

void foo(int, ...) {}
void baz(int[]...) {}

void test()
{
    foo();
    foo(null);

    baz("");
    baz(3, null);
}

/*
TEST_OUTPUT:
---
fail_compilation/fail332.d(60): Error: function `bar` is not callable using argument types `()`
fail_compilation/fail332.d(60):        missing argument for parameter #1: `Object`
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
fail_compilation/fail332.d(61): Error: function `bar` is not callable using argument types `(int)`
fail_compilation/fail332.d(61):        cannot pass argument `4` of type `int` to parameter `Object`
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
fail_compilation/fail332.d(62): Error: function `bar` is not callable using argument types `(typeof(null))`
fail_compilation/fail332.d(62):        expected 2 variadic argument(s), not 0
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
fail_compilation/fail332.d(63): Error: function `bar` is not callable using argument types `(typeof(null), int)`
fail_compilation/fail332.d(63):        expected 2 variadic argument(s), not 1
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
fail_compilation/fail332.d(64): Error: function `bar` is not callable using argument types `(typeof(null), int, string)`
fail_compilation/fail332.d(64):        cannot pass argument `""` of type `string` to parameter `int[2]...`
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
fail_compilation/fail332.d(65): Error: function `bar` is not callable using argument types `(typeof(null), int, int, int)`
fail_compilation/fail332.d(65):        expected 2 variadic argument(s), not 3
fail_compilation/fail332.d(56):        `fail332.bar(Object, int[2]...)` declared here
---
*/
void bar(Object, int[2]...);

void test2()
{
    bar();
    bar(4);
    bar(null);
    bar(null, 2);
    bar(null, 2, "");
    bar(null, 2,3,4);
}
