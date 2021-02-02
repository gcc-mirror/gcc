/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts3.d(13): Error: function `fail_contracts3.D.foo` cannot have an in contract when overridden function `fail_contracts3.C.foo` does not have an in contract
---
*/

class C {
       void foo(){}
}

class D : C {
       override void foo()in{}do{}
}
