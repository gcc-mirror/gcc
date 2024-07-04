/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/switch_skip.d(22): Error: `switch` skips declaration of variable `switch_skip.test3.j`
    switch (i)
    ^
fail_compilation/switch_skip.d(26):        declared here
            int j;
                ^
fail_compilation/switch_skip.d(39): Error: `switch` skips declaration of variable `switch_skip.test.z`
    final switch(n)
    ^
fail_compilation/switch_skip.d(41):        declared here
        int z = 5;
            ^
---
*/

void test3(int i)
{
    switch (i)
    {
        case 1:
        {
            int j;
        case 2:
            ++j;
            break;
        }
        default:
            break;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=18858
int test(int n)
{
    final switch(n)
    {
        int z = 5;
        enum e = 6;

        case 1:
            int y = 2;
            return y;
    }
}
