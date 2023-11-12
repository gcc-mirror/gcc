/* TEST_OUTPUT:
---
fail_compilation/fail17491.d(22): Error: cannot modify expression `(S17491).init` because it is not an lvalue
fail_compilation/fail17491.d(23): Error: cannot take address of expression `S17491(0)` because it is not an lvalue
fail_compilation/fail17491.d(25): Error: cannot modify expression `S17491(0).field` because it is not an lvalue
fail_compilation/fail17491.d(26): Error: cannot take address of expression `S17491(0).field` because it is not an lvalue
fail_compilation/fail17491.d(31): Error: cannot modify expression `S17491(0)` because it is not an lvalue
fail_compilation/fail17491.d(32): Error: cannot take address of expression `S17491(0)` because it is not an lvalue
fail_compilation/fail17491.d(34): Error: cannot modify expression `S17491(0).field` because it is not an lvalue
fail_compilation/fail17491.d(35): Error: cannot take address of expression `S17491(0).field` because it is not an lvalue
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
    *&S17491.init.field = 42;       // NG

    S17491.init.var = 42;           // OK
    *&S17491.init.var = 42;         // OK

    S17491(0) = S17491(42);         // NG
    *&S17491(0) = S17491(42);       // NG

    S17491(0).field = 42;           // NG
    *&S17491(0).field = 42;         // NG

    S17491(0).var = 42;             // OK
    *&S17491(0).var = 42;           // OK
}
