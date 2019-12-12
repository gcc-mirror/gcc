/*
TEST_OUTPUT:
---
fail_compilation/diag3673.d(9): Error: template constraints appear both before and after BaseClassList, put them before
---
*/

class A {}
class B(T) if(false) : A if (true) { }
