/*
TEST_OUTPUT:
---
fail_compilation/fail7702.d(14): Error: `s.opDispatch!"x"` isn't a template
---
*/
struct S
{
   template opDispatch (string name) {}
}
void main()
{
    S s;
    s.x!int;
}
