/*
TEST_OUTPUT
---
fail_compilation/diag16271.d(10): Error: found `x` when expecting function literal following `ref`
---
*/

void main()
{
    auto fun = ref x;
}

