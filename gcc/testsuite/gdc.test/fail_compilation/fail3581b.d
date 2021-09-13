/*
TEST_OUTPUT:
---
fail_compilation/fail3581b.d(9): Error: function `fail3581b.B.f` `private` method is not virtual and cannot override
---
*/

class A { void f() {} }
class B : A { private override void f() {}; }

void main() {}
