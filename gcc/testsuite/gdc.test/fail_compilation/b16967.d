/*
 * TEST_OUTPUT:
---
fail_compilation/b16967.d(15): Error: switch case fallthrough - use 'goto default;' if intended
fail_compilation/b16967.d(25): Error: switch case fallthrough - use 'goto default;' if intended
---
*/
int foo(int x)
in
{
    switch (x)
    {
        case 1:
            assert(x != 0);
        default:
            break;
    }
}
out(v)
{
    switch(v)
    {
        case 42:
            assert(x != 0);
        default:
            break;
    }
}
do
{
    return 42;
}
