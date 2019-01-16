/* 
 * REQUIRED_ARGS: -c
 * TEST_OUTPUT:
---
compilable/b16967.d(16): Deprecation: switch case fallthrough - use 'goto default;' if intended
compilable/b16967.d(26): Deprecation: switch case fallthrough - use 'goto default;' if intended
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
body
{
    return 42;
}
