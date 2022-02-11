/*
TEST_OUTPUT:
---
fail_compilation/fail318_b.d(8): Error: function `D main` must return `int`, `void` or `noreturn`, not `string`
---
*/

auto main()
{
    return "";
}
