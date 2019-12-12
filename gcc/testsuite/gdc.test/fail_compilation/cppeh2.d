// DISABLED: win32 win64
/*
TEST_OUTPUT:
---
fail_compilation/cppeh2.d(21): Error: cannot mix catching D and C++ exceptions in the same try-catch
---
*/

version(Windows) static assert(0, "This test should not run on this platform");

extern (C++, std)
{
    class exception { }
}

void bar();
void abc();

void foo()
{
    try
    {
        bar();
    }
    catch (std.exception e)
    {
        abc();
    }
    catch (Exception e)
    {
        abc();
    }
}
