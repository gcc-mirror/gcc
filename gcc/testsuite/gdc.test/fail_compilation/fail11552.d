/*
REQUIRED_ARGS: -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/fail11552.d(12): Error: label `label` is undefined
---
*/

void main()
{
    goto label;
}
