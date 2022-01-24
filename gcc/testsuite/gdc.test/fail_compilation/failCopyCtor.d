/*
TEST_OUTPUT:
---
fail_compilation/failCopyCtor.d(10): Error: `struct A` may not define both a rvalue constructor and a copy constructor
fail_compilation/failCopyCtor.d(12):        rvalue constructor defined here
fail_compilation/failCopyCtor.d(13):        copy constructor defined here
---
*/

struct A
{
    this(immutable A a) {}
    this(ref shared A a) immutable {}
    this(ref A a) {}
}
