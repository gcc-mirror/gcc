/*
TEST_OUTPUT:
---
fail_compilation/fail3581a.d(9): Error: function `fail3581a.B.f` cannot override a non-virtual function
---
*/

class A { void f() {} }
class B : A { static override void f() {}; }

void main() {}
