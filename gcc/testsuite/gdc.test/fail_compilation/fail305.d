/*
TEST_OUTPUT:
---
fail_compilation/fail305.d(10): Error: cannot return non-void from `void` function
---
*/

void main()
{
    return "a";
}
