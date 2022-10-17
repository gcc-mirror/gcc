/*
TEST_OUTPUT:
---
fail_compilation/fail329.d(28): Error: variable `fail329.A.foo.__ensure.result` cannot modify result `result` in contract
---
*/

//import core.stdc.stdio;

/*******************************************/

class A
{
    int x = 7;

    int foo(int i)
    in
    {
        //printf("A.foo.in %d\n", i);
        assert(i == 2);
        assert(x == 7);
        //printf("A.foo.in pass\n");
    }
    out (result)
    {
        assert(result & 1);
        assert(x == 7);
        result++;
    }
    do
    {
        return i;
    }
}

class B : A
{
    override int foo(int i)
    in
    {
        float f;
        //printf("B.foo.in %d\n", i);
        assert(i == 4);
        assert(x == 7);
        f = f + i;
    }
    out (result)
    {
        assert(result < 8);
        assert(x == 7);
    }
    do
    {
        return i - 1;
    }
}

void test1()
{
    auto b = new B();
    b.foo(2);
    b.foo(4);
}
