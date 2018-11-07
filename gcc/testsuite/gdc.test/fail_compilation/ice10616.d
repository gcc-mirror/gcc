/*
TEST_OUTPUT:
---
fail_compilation/ice10616.d(8): Error: class ice10616.A is forward referenced when looking for 'B'
---
*/

class A : A.B
{
    interface B {}
}
