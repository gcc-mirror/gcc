/*
TEST_OUTPUT:
---
fail_compilation/fail_circular2.d(10): Error: circular initialization of variable 'fail_circular2.S.d1'
fail_compilation/fail_circular2.d(12): Error: circular initialization of variable 'fail_circular2.S.e1'
---
*/
struct S
{
    static const int d1 = S.d1;     // CTFE error (expression type is determined to int)

    enum int e1 = S.e1;             // CTFE error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_circular2.d(24): Error: circular initialization of variable 'fail_circular2.C.d1'
fail_compilation/fail_circular2.d(26): Error: circular initialization of variable 'fail_circular2.C.e1'
---
*/
class C
{
    static const int d1 = C.d1;     // CTFE error

    enum int e1 = C.e1;             // CTFE error
}
