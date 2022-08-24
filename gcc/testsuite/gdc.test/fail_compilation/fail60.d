/*
TEST_OUTPUT:
---
fail_compilation/fail60.d(14): Error: cannot construct nested class `B` because no implicit `this` reference to outer class `A` is available
---
*/
class A
{
 class B
 {

 }

 B b=new B;
}
