/*
TEST_OUTPUT:
---
fail_compilation/fail142.d(20): Error: cannot create instance of abstract class `B`
fail_compilation/fail142.d(20):        function `void test()` is not implemented
---
*/

class A
{
    abstract void test() {}
}

class B : A
{
}

void main()
{
    B b = new B();
}
