/*
TEST_OUTPUT:
---
fail_compilation/fail265.d-mixin-10(10): Error: found `End of File` instead of statement
---
*/

void main()
{
    mixin(`for(;;)`);
}
