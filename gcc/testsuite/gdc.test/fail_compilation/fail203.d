/*
TEST_OUTPUT:
---
fail_compilation/fail203.d(10): Error: shift by 33 is outside the range `0..31`
fail_compilation/fail203.d(10): Error: shift by 33 is outside the range `0..31`
---
*/
void main() {
        int c;
        c = c << 33;
}
