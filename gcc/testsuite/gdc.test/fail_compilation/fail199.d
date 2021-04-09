// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail199.d(20): Deprecation: class `fail199.DepClass` is deprecated
fail_compilation/fail199.d(20): Deprecation: class `fail199.DepClass` is deprecated
---
*/

//import std.stdio;

deprecated class DepClass
{
    void test()
    {
        //writefln("Accessing what's deprecated!");
    }
}

class Derived : DepClass {}
