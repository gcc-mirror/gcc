/*
TEST_OUTPUT:
---
fail_compilation/fail15535.d(17): Error: goto default not allowed in final switch statement
---
*/

void test()
{
    int i;
    switch (i)
    {
    case 0:
        final switch (i)
        {
        case 1:
            goto default;
        }
    default:
        break;
    }
}
