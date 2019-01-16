/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(11): Error: constructor fail66.C1.this missing initializer for const field y
---
*/

class C1
{
    const int y;
    this() {}
}

/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(28): Error: cannot modify const expression c.y
---
*/
class C2
{
    const int y;
    this() { y = 7; }
}
void test2()
{
    C2 c = new C2();
    c.y = 3;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(43): Error: cannot modify const expression this.y
---
*/
class C3
{
    const int y;
    this() { y = 7; }
    void foo()
    {
        y = 6;
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(59): Error: cannot modify const expression x
---
*/
class C4
{
    static const int x;
    static this() { x = 5; }
    void foo()
    {
        x = 4;
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(73): Error: cannot modify const expression z5
---
*/
const int z5;
static this() { z5 = 3; }
void test5()
{
    z5 = 4;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail66.d(89): Error: cannot modify const expression c.y
---
*/
class C6
{
    const int y;
    this()
    {
        C6 c = this;
        y = 7;
        c.y = 8;
    }
}
