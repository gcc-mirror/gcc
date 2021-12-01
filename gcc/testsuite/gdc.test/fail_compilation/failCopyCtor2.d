/*
TEST_OUTPUT:
---
fail_compilation/failCopyCtor2.d(15): Error: `struct B` may not define a rvalue constructor and have fields with copy constructors
fail_compilation/failCopyCtor2.d(18):        rvalue constructor defined here
fail_compilation/failCopyCtor2.d(17):        field with copy constructor defined here
---
*/

struct A
{
    this (ref shared A a) immutable {}
}

struct B
{
    A a;
    this(immutable B b) shared {}
}
