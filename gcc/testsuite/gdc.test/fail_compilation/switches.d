/*
TEST_OUTPUT:
---
fail_compilation/switches.d(14): Error: `case 2` not found
fail_compilation/switches.d(25): Error: no `case` statement following `goto case;`
---
*/

void test1(int i)
{
    switch (i)
    {
        case 1:
            goto case 2;
        defaut:
            break;
    }
}

void test2(int i)
{
    switch (i)
    {
        case 1:
            goto case;
        defaut:
            break;
    }
}
