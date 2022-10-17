/*
TEST_OUTPUT:
---
fail_compilation/class1.d(11): Error: class `class1.C` identity assignment operator overload is illegal
---
*/

class C
{
    // Non-templated identity opAssign
    void opAssign(C rhs){}
}
