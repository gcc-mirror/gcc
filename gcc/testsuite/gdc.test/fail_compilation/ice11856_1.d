/*
TEST_OUTPUT:
---
fail_compilation/ice11856_1.d(13): Error: none of the overloads of template `ice11856_1.g` are callable using argument types `!()(A)`
fail_compilation/ice11856_1.d(11):        Candidate is: `g(T)(T x)`
---
*/
struct A {}

void f(T)(T x) if (is(typeof(x.g()))) {}
void g(T)(T x) if (is(typeof(x.f()))) {}

void main() { A().g(); }
