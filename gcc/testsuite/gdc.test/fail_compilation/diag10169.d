/*
TEST_OUTPUT:
---
fail_compilation/diag10169.d(12): Deprecation: imports.a10169.B.x is not visible from module diag10169
fail_compilation/diag10169.d(12): Error: struct imports.a10169.B member `x` is not accessible
---
*/
import imports.a10169;

void main()
{
    auto a = B.init.x;
}
