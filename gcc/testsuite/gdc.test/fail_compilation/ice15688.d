// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/ice15688.d(12): Error: undefined identifier `mappings`
fail_compilation/ice15688.d(12): Error: function expected before `()`, not `0` of type `int`
---
*/

void main()
{
    (mappings, 0)();
}
