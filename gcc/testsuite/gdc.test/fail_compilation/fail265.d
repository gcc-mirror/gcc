/*
TEST_OUTPUT:
---
fail_compilation/fail265.d-mixin-11(11): Error: found `End of File` instead of statement
fail_compilation/fail265.d-mixin-11(11):        while parsing string mixin statement
---
*/

void main()
{
    mixin(`for(;;)`);
}
