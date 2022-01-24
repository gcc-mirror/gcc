/*
TEST_OUTPUT:
---
fail_compilation/diag6699.d(8): Error: no property `x` for type `int`
---
*/
alias int b6699;
alias b6699.x b6699a;

/*
TEST_OUTPUT:
---
fail_compilation/diag6699.d(18): Error: undefined identifier `junk1`
fail_compilation/diag6699.d(18): Error: undefined identifier `junk2`
fail_compilation/diag6699.d(19): Error: undefined identifier `junk3`
---
*/
class X : junk1, junk2 {}
interface X2 : junk3 {}
