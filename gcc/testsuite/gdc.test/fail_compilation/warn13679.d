// REQUIRED_ARGS: -w

/*
TEST_OUTPUT:
---
fail_compilation/warn13679.d(15): Warning: cannot use `foreach_reverse` with an associative array
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

void main()
{
    int[int] aa;
    foreach_reverse(k, v; aa) {}
}
