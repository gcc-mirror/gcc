/*
TEST_OUTPUT:
---
fail_compilation/fail124.d(15): Error: class fail124.CC inherits from duplicate interface C
---
*/

//import std.stdio;

interface C
{
    void f();
}

class CC : C, C
{
    void f() { /*writefln("hello");*/ }
}

void main()
{
    CC cc = new CC();
    cc.f();
}
