// DISABLED: win32 win64
/*
TEST_OUTPUT:
---
fail_compilation/cppeh1.d(26): Error: catching C++ class objects is not allowed in a `@safe` function
---
*/

version (Windows) static assert(0, "This test should not run on this platform");

extern (C++, std)
{
    class exception { }
}

@safe:
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
}
