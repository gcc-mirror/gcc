/*
REQUIRED_ARGS: -m64
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test16116.d(15): Error: incompatible types for ((v) * (i)): '__vector(short[8])' and 'int'
---
*/

// https://issues.dlang.org/show_bug.cgi?id=16116

void foo() {
    __vector(short[8]) v;
    int i;
    v = v * i;
}
