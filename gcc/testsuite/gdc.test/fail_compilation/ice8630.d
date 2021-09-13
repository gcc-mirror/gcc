/*
TEST_OUTPUT:
---
fail_compilation/ice8630.d(9): Error: undefined identifier `v`
fail_compilation/ice8630.d(10): Error: template instance `ice8630.foo!(int[])` error instantiating
---
*/
auto map(alias func, R)(R r) { return r; }
typeof(v) foo(R)(R v) { return map!(p=>p)(v); }
void main() { foo([1]); }
