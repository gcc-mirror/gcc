/*
REQUIRED_ARGS: -unittest
TEST_OUTPUT:
---
fail_compilation/test19176.d(13): Error: argument `foo()` to __traits(getUnitTests) must be a module or aggregate, not a template
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19176

void main()
{
    __traits(getUnitTests, foo);
}

template foo()
{
    static if(true)
    {
        enum bar;
    }
    else
    {
        enum bar;
    }
}
