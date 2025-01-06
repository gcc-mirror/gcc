/*
TEST_OUTPUT:
---
fail_compilation/test22999.d(17): Error: switch case fallthrough - use 'goto default;' if intended
fail_compilation/test22999.d(24): Error: switch case fallthrough - use 'goto case;' if intended
---
*/

// no switch fallthrough error with multi-valued case
// https://issues.dlang.org/show_bug.cgi?id=22999
void main()
{
    int i;
    switch (0)
    {
        case 0, 1: i = 20;
        default: assert(0);
    }

    switch (0)
    {
        default:
        case 0, 1: i = 20;
        case 2, 3: i = 30;
    }
}
