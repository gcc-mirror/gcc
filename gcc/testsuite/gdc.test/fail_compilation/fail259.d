/*
TEST_OUTPUT:
---
fail_compilation/fail259.d(11): Error: function fail259.C.foo does not override any function
---
*/

class C
{
    final
        override void foo(){}
}

