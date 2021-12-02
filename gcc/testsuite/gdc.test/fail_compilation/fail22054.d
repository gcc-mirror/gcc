// https://issues.dlang.org/show_bug.cgi?id=22054

/*
TEST_OUTPUT:
---
fail_compilation/fail22054.d(21): Error: no property `what` for type `fail22054.exception`
fail_compilation/fail22054.d(16):        `class fail22054.exception` is opaque and has no members.
fail_compilation/fail22054.d(22): Error: no property `what` for type `fail22054.exception2`
fail_compilation/fail22054.d(17):        `struct fail22054.exception2` is opaque and has no members.
---
*/




class exception;
struct exception2;

void main ()
{
    assert(exception.what() == "Hello");
    assert(exception2.what() == "Hello");
}
