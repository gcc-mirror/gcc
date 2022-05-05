/*
TEST_OUTPUT:
---
fail_compilation/fail4269c.d(11): Error: undefined identifier `B`
fail_compilation/fail4269c.d(12): Error: undefined identifier `B`
---
*/
enum bool WWW = is(typeof(A.x));

class A {
    B blah;
    void foo(B b){}
}
