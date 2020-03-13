// REQUIRED_ARGS: -c
/*
TEST_OUTPUT:
---
gigi
fail_compilation/fail19182.d(12): Error: `pragma(msg)` is missing a terminating `;`
---
*/

void foo()
{
    pragma(msg, "gigi") // Here
    static foreach (e; [])
    {
        pragma(msg, "lili");
    }

}
