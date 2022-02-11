/*
TEST_OUTPUT:
---
fail_compilation/fail132.d(19): Error: outer class `A` `this` needed to `new` nested class `B`
---
*/

//import std.stdio;

class A
{
    class B
    {
    }
}

void main()
{
    A.B c = new A.B;
}
