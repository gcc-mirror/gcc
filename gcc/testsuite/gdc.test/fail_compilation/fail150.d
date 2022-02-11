/*
TEST_OUTPUT:
---
fail_compilation/fail150.d(22): Error: `.new` is only for allocating nested classes
---
*/

//import std.stdio;

class Class1
{
}

class Foo
{
}

int main(char[][] argv)
{
    Class1 myclass = new Class1;

    myclass.new Foo();
    return 0;
}
