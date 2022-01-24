/*
TEST_OUTPUT:
---
fail_compilation/fail18228.d(12): Error: Using `this` as a type is obsolete. Use `typeof(this)` instead
fail_compilation/fail18228.d(13): Error: Using `this` as a type is obsolete. Use `typeof(this)` instead
fail_compilation/fail18228.d(14): Error: Using `super` as a type is obsolete. Use `typeof(super)` instead
---
*/

class C
{
    this(this a) {}
    this(int a, this b) {}
    this(super a) {}
}
