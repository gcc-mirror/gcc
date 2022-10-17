/*
RUN_OUTPUT:
---
Success
---
*/

extern(C) int printf(const char*, ...);

/***********************/

@nogc int test1()
{
    return 3;
}

/***********************/
// https://issues.dlang.org/show_bug.cgi?id=3032

void test3032() @nogc
{
    scope o1 = new Object();        // on stack
    scope o2 = new class Object {}; // on stack

    int n = 1;
    scope fp = (){ n = 10; };       // no closure
    fp();
    assert(n == 10);
}

/***********************/
// https://issues.dlang.org/show_bug.cgi?id=12642

__gshared int[1] data12642;

int[1] foo12642() @nogc
{
    int x;
    return [x];
}

void test12642() @nogc
{
    int x;
    data12642 = [x];
    int[1] data2;
    data2 = [x];

    data2 = foo12642();
}

/***********************/
// https://issues.dlang.org/show_bug.cgi?id=12936

void test12936() @nogc
{
    foreach (int[1] a; [[1]])
    {
        assert(a == [1]);
    }
    foreach (i, int[1] a; [[1], [2]])
    {
             if (i == 0) assert(a == [1]);
        else if (i == 1) assert(a == [2]);
        else             assert(0);
    }
}

/***********************/

int main()
{
    test1();
    test3032();
    test12642();
    test12936();

    printf("Success\n");
    return 0;
}
