/*
TEST_OUTPUT:
---
fail_compilation/ice13027.d(9): Error: template instance `b!"c"` template `b` is not defined
---
*/
void main()
{
    scope a = b!"c";
}
