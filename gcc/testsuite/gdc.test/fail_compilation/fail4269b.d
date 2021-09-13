/*
TEST_OUTPUT:
---
fail_compilation/fail4269b.d(11): Error: undefined identifier `B`
fail_compilation/fail4269b.d(12): Error: undefined identifier `B`
---
*/
enum bool WWW = is(typeof(A.x));

struct A {
    B blah;
    void foo(B b){} 
}

