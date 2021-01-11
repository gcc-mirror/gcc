/*
TEST_OUTPUT:
---
fail_compilation/fail3673a.d(8): Error: template constraints only allowed for templates
---
*/
class A {}
class B : A if(false) { }
