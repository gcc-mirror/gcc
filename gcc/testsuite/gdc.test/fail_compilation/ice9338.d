/*
TEST_OUTPUT:
---
fail_compilation/ice9338.d(13): Error: value of 'this' is not known at compile time
fail_compilation/ice9338.d(14): Error: value of 'this' is not known at compile time
---
*/

class Foo
{
    void test()
    {
        enum members1 = makeArray();
        enum members2 = this.makeArray();
    }

    string[] makeArray()
    {
        return ["a"];
    }
}
