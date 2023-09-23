/*
TEST_OUTPUT:
---
fail_compilation/fail142.d(21): Error: cannot create instance of abstract class `B`
fail_compilation/fail142.d(15):        class `B` is declared here
fail_compilation/fail142.d(12):        function `void test()` is not implemented
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
