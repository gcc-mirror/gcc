/*
TEST_OUTPUT:
---
fail_compilation/fail232.d(15): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail232.d(15): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail232.d(16): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail232.d(16): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail232.d(17): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail232.d(17): Error: shift by 33 is outside the range `0..31`
---
*/
void bug1601() {
    int i;

    i = i >> 33;
    i = i << 33;
    i = i >>> 33;
}
