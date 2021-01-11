/*
TEST_OUTPUT:
---
fail_compilation/fail4269a.d(12): Error: undefined identifier `B`
fail_compilation/fail4269a.d(12): Error: variable `fail4269a.A.blah` field not allowed in interface
fail_compilation/fail4269a.d(13): Error: undefined identifier `B`
---
*/
enum bool WWW = is(typeof(A.x));

interface A {
    B blah;
    void foo(B b){} 
}

