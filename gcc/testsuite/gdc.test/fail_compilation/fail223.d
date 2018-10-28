/*
TEST_OUTPUT:
---
fail_compilation/fail223.d(14): Error: cannot modify this.x in const function
---
*/

//import std.stdio;

class A
{
public:
    int x = 0;
    void setX(int nx) const { x = nx; }
}

void foo(const A a) { a.setX(1); }

int main(char[][] args)
{
    A a = new A;
    foo(a);
    //writefln(a.x);
    return 0;
}
