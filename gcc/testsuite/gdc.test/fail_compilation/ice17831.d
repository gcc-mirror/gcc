// REQUIRED_ARGS: -d
/*
TEST_OUTPUT:
---
fail_compilation/ice17831.d(19): Error: case variable `i` declared at fail_compilation/ice17831.d(17) cannot be declared in switch body
fail_compilation/ice17831.d(33): Error: case variable `i` declared at fail_compilation/ice17831.d(31) cannot be declared in switch body
fail_compilation/ice17831.d(48): Error: case variable `i` declared at fail_compilation/ice17831.d(45) cannot be declared in switch body
fail_compilation/ice17831.d(61): Error: case variable `i` declared at fail_compilation/ice17831.d(60) cannot be declared in switch body
fail_compilation/ice17831.d(73): Error: case variable `i` declared at fail_compilation/ice17831.d(72) cannot be declared in switch body
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
