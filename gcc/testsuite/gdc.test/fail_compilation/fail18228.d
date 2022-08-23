/*
TEST_OUTPUT:
---
fail_compilation/fail18228.d(12): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail18228.d(13): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/fail18228.d(14): Error: undefined identifier `super`, did you mean `typeof(super)`?
---
*/

class C
{
    this(this a) {}
    this(int a, this b) {}
    this(super a) {}
}
