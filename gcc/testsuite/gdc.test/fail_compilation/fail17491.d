/* TEST_OUTPUT:
---
fail_compilation/fail17491.d(24): Error: (S17491).init is not an lvalue
fail_compilation/fail17491.d(25): Error: S17491(0) is not an lvalue
fail_compilation/fail17491.d(27): Error: constant S17491(0).field is not an lvalue
fail_compilation/fail17491.d(28): Error: constant *&S17491(0).field is not an lvalue
fail_compilation/fail17491.d(33): Error: S17491(0) is not an lvalue
fail_compilation/fail17491.d(34): Error: S17491(0) is not an lvalue
fail_compilation/fail17491.d(36): Error: constant S17491(0).field is not an lvalue
fail_compilation/fail17491.d(37): Error: constant *&S17491(0).field is not an lvalue
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17491

struct S17491
{
    int field;
    static int var;
}

void test17491()
{
    S17491.init = S17491(42);       // NG
    *&S17491.init = S17491(42);     // NG

    S17491.init.field = 42;         // NG
    *&S17491.init.field = 42;       // Should be NG

    S17491.init.var = 42;           // OK
    *&S17491.init.var = 42;         // OK

    S17491(0) = S17491(42);         // NG
    *&S17491(0) = S17491(42);       // NG

    S17491(0).field = 42;           // NG
    *&S17491(0).field = 42;         // Should be NG

    S17491(0).var = 42;             // OK
    *&S17491(0).var = 42;           // OK
}
