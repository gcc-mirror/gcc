/*
TEST_OUTPUT:
---
fail_compilation/fail52.d(10): Error: class fail52.C circular inheritance
---
*/

class A : B { void f(); }
class B : C { override void g(); }
class C : A { void g(); }
