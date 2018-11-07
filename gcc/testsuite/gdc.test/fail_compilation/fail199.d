// REQUIRED_ARGS: -de

//import std.stdio;

deprecated class DepClass
{
    void test()
    {
        //writefln("Accessing what's deprecated!");
    }
}

class Derived : DepClass {}
