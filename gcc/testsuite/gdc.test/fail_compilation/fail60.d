/*
TEST_OUTPUT:
---
fail_compilation/fail60.d(14): Error: `this` is only defined in non-static member functions, not `A`
---
*/
class A
{
 class B
 {

 }

 B b=new B;
}

