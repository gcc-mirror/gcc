/*
TEST_OUTPUT:
---
fail_compilation/ice11856_1.d(16): Error: none of the overloads of template `ice11856_1.g` are callable using argument types `!()(A)`
fail_compilation/ice11856_1.d(14):        Candidate is: `g(T)(T x)`
  with `T = A`
  must satisfy the following constraint:
`       is(typeof(x.f()))`
---
*/
struct A {}

void f(T)(T x) if (is(typeof(x.g()))) {}
void g(T)(T x) if (is(typeof(x.f()))) {}

void main() { A().g(); }
