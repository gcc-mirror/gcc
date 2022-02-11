/*
TEST_OUTPUT:
---
fail_compilation/fail134.d(14): Error: template instance `foo!(f)` does not match template declaration `foo(T)`
fail_compilation/fail134.d(14):        `f` is not a type
fail_compilation/fail134.d(15): Error: template instance `fail134.bar!(f)` error instantiating
---
*/

// https://issues.dlang.org/show_bug.cgi?id=651
// Assertion failure: 'global.errors' on line 2622 in file 'template.c'
void f() {}
template foo(T) {}
template bar(T...) { alias foo!(T) buz; }
alias bar!(f) a;
