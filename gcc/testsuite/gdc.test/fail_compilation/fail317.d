/*
TEST_OUTPUT:
---
fail_compilation/fail317.d(10): Error: function `fail317.I.f` has no function body with return type inference
---
*/

interface I
{
    auto f()
    in {}
    out {}
}
