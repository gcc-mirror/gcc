/*
TEST_OUTPUT:
---
fail_compilation/ice17831.d(23): Error: `case` variables have to be `const` or `immutable`
fail_compilation/ice17831.d(23): Error: `case` variable `i` declared at fail_compilation/ice17831.d(21) cannot be declared in `switch` body
fail_compilation/ice17831.d(37): Error: `case` variables have to be `const` or `immutable`
fail_compilation/ice17831.d(37): Error: `case` variable `i` declared at fail_compilation/ice17831.d(35) cannot be declared in `switch` body
fail_compilation/ice17831.d(52): Error: `case` variables have to be `const` or `immutable`
fail_compilation/ice17831.d(52): Error: `case` variable `i` declared at fail_compilation/ice17831.d(49) cannot be declared in `switch` body
fail_compilation/ice17831.d(65): Error: `case` variables have to be `const` or `immutable`
fail_compilation/ice17831.d(65): Error: `case` variable `i` declared at fail_compilation/ice17831.d(64) cannot be declared in `switch` body
fail_compilation/ice17831.d(77): Error: `case` variables have to be `const` or `immutable`
fail_compilation/ice17831.d(77): Error: `case` variable `i` declared at fail_compilation/ice17831.d(76) cannot be declared in `switch` body
---
 */

void test17831a()
{
    switch (0)
    {
        foreach (i; 0 .. 5)
        {
            case i:
                break;
        }
        default:
            break;
    }
}

void test17831b()
{
    switch (0)
    {
        for (int i = 0; i < 5; i++)
        {
            case i:
                break;
        }
        default:
            break;
    }
}

void test17831c()
{
    switch (0)
    {
        int i = 0;
        while (i++ < 5)
        {
            case i:
                break;
        }
        default:
            break;
    }
}

void test17831d()
{
    switch (0)
    {
        int i = 0;
        case i:
            break;
        default:
            break;
    }
}

void test17831e()
{
    switch (0)
    {
        static int i = 0;
        case i:
            break;
        default:
            break;
    }
}
