// REQUIRED_ARGS: -w
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/warn13679.d(14): Warning: cannot use foreach_reverse with an associative array
---
*/

void main()
{
    int[int] aa;
    foreach_reverse(k, v; aa) {}
}
