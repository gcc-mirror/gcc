/*
TEST_OUTPUT:
----
fail_compilation/ice11856_1.d(18): Error: no property `g` for `A()` of type `A`
fail_compilation/ice11856_1.d(18):        the following error occured while looking for a UFCS match
fail_compilation/ice11856_1.d(18): Error: template `g` is not callable using argument types `!()(A)`
fail_compilation/ice11856_1.d(16):        Candidate is: `g(T)(T x)`
  with `T = A`
  must satisfy the following constraint:
`       is(typeof(x.f()))`
----
*/
struct A {}

void f(T)(T x) if (is(typeof(x.g()))) {}
void g(T)(T x) if (is(typeof(x.f()))) {}

void main() { A().g(); }
