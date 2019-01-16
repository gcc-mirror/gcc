/*
TEST_OUTPUT:
---
fail_compilation/class2.d(11): Error: class class2.C identity assignment operator overload is illegal
---
*/

class C
{
    // Templated identity opAssign
    void opAssign(T)(T rhs){}
}
