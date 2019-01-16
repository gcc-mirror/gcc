/*
TEST_OUTPUT:
---
fail_compilation/fail51.d(11): Error: interface fail51.B circular inheritance of interface
---
*/

// interface A { void f(); }

interface A : B { void f(); }
interface B : A { void g(); }
