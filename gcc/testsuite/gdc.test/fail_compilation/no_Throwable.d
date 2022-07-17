/*
DFLAGS:
REQUIRED_ARGS: -c
EXTRA_SOURCES: extra-files/minimal/object.d
TEST_OUTPUT:
---
fail_compilation/no_Throwable.d(14): Error: cannot use `throw` statements because `object.Throwable` was not declared
fail_compilation/no_Throwable.d(19): Error: cannot use try-catch statements because `object.Throwable` was not declared
---
*/

void test()
{
    throw new Exception("msg");
}

void test2()
{
    try
    {
        test();
    }
    catch (Exception e)
    {
    }
}
