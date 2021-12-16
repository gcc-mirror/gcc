//https://issues.dlang.org/show_bug.cgi?id=22574
/*
TEST_OUTPUT:
---
fail_compilation/test22574.d(100): Error: undefined identifier `x`
---
*/
#line 100
template test(x* x)
{

}
