/*
TEST_OUTPUT:
---
fail_compilation/fail204.d(9): Error: shift assign by 65 is outside the range `0..63`
---
*/
void main() {
        long c;
        c >>= 65;
}

