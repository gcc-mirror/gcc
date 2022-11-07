/*
TEST_OUTPUT:
---
fail_compilation/diag23355.d(1): Error: undefined identifier `n`
fail_compilation/diag23355.d(4): Error: none of the overloads of template `diag23355.ffi1` are callable using argument types `!()(int[4])`
fail_compilation/diag23355.d(1):        Candidate is: `ffi1(T)(T[n] s)`
fail_compilation/diag23355.d(2): Error: undefined identifier `n`
fail_compilation/diag23355.d(4): Error: none of the overloads of template `diag23355.ffi2` are callable using argument types `!()(int[4])`
fail_compilation/diag23355.d(2):        Candidate is: `ffi2()(T[n] s)`
---
*/
#line 1
void ffi1(T)(T[n] s) { }
void ffi2()(T[n] s) { }

void main() { int[4] x; ffi1(x); ffi2(x); }
