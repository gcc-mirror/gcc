// REQUIRED_ARGS: -de

//import std.stdio;

deprecated interface DepClass
{
    void test();
}

class Derived : DepClass
{
    void test()
    {
        //writefln("Accessing what's deprecated!");
    }
}
