/*
TEST_OUTPUT:
---
fail_compilation/diag_ptr_conversion.d(15): Error: cannot implicitly convert `const(int)*` to `int*`
fail_compilation/diag_ptr_conversion.d(15):        Note: Converting const to mutable requires an explicit cast (`cast(int*)`).
fail_compilation/diag_ptr_conversion.d(16): Error: cannot implicitly convert `int*` to `float*`
fail_compilation/diag_ptr_conversion.d(16):        Note: Pointer types point to different base types (`int` vs `float`)
---
*/

void testPointerConversions()
{
    int* p;
    const(int)* cp = p;
    p = cp;
    float* f = p;
}
