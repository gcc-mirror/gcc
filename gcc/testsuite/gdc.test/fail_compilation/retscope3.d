/*
REQUIRED_ARGS: -preview=dip1000
*/

/*
TEST_OUTPUT:
---
---
*/

#line 2000

// https://issues.dlang.org/show_bug.cgi?id=17790

@safe:

int* bar1()
{
    int i;
    int*[] arr = [ &i ];
    return arr[0];
}

struct S2000 { int* p; }

S2000 bar2()
{
    int i;
    S2000[] arr = [ S2000(&i) ];
    return arr[0];
}

void bar3(string[] u...) @safe pure nothrow @nogc
{
    foreach (str; u)
    {
    }
}

void bar4()
{
    static struct S { int* p; }
    S[2][10] pairs;
    foreach (ref pair; pairs)
    {
    }
}

/**********************************************/

/*
TEST_OUTPUT:
---
fail_compilation/retscope3.d(2008): Error: escaping a reference to local variable `i` by copying `& i` into allocated memory is not allowed in a `@safe` function
fail_compilation/retscope3.d(2017): Error: escaping a reference to local variable `i` by copying `S2000(& i)` into allocated memory is not allowed in a `@safe` function
fail_compilation/retscope3.d(4003): Error: escaping a reference to parameter `u` by copying `u[]` into allocated memory is not allowed in a `@safe` function
fail_compilation/retscope3.d(4016): Error: storing reference to outer local variable `i` into allocated memory causes it to escape
fail_compilation/retscope3.d(4025): Error: escaping reference to stack allocated value returned by `makeSA()` into allocated memory
---
*/

#line 4000

void bar4000(int[1] u...) @safe
{
    int[][] n = [u[]];
}

void bar4001() @safe
{
    static int i;
    int*[] n = [&i];
}

ref int bar4002(return ref int i) @safe
{
    void nested()
    {
        int*[] n = [&i];
    }
    return i;
}

int[3] makeSA() @safe;

void bar4003() @safe
{
    int[][] a = [makeSA()[]];
}
