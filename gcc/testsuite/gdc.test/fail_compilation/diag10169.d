/*
EXTRA_FILES: imports/a10169.d
TEST_OUTPUT:
---
fail_compilation/diag10169.d(13): Error: no property `x` for `B(0)` of type `imports.a10169.B`
fail_compilation/imports/a10169.d(3):        struct `B` defined here
---
*/
import imports.a10169;

void main()
{
    auto a = B.init.x;
}
