/*
TEST_OUTPUT:
---
fail_compilation/diag10169.d(11): Error: no property `x` for type `B`, did you mean `imports.a10169.B.x`?
---
*/
import imports.a10169;

void main()
{
    auto a = B.init.x;
}
