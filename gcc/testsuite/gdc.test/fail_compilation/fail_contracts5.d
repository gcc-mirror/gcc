
/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts5.d(13): Error: constructor `fail_contracts5.S.this` `in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract
---
*/

// bugzilla 12901
struct S
{
    int a;
    this(int n)
    in { a = n; }
    // no body
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts5.d(33): Error: function `fail_contracts5.C2.foo` `in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract
fail_compilation/fail_contracts5.d(32): Error: constructor `fail_contracts5.C2.this` `in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract
---
*/
// https://github.com/dlang/dmd/issues/21403
abstract class C1 {}

class C2 : C1
{
    bool member;

    this() out (member) {}
    final void foo() out (member) {}
}
