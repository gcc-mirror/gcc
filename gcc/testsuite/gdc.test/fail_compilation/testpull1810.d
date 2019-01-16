// REQUIRED_ARGS: -c -w
/*
TEST_OUTPUT:
---
fail_compilation/testpull1810.d(19): Warning: statement is not reachable
---
*/

uint foo(uint i)
{
    try
    {
        ++i;
        return 3;
    }
    catch (Exception e)
    {
    }
    return 4;
}

