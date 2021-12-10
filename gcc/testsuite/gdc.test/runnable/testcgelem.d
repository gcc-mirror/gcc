/*
REQUIRED_ARGS: -mcpu=native
PERMUTE_ARGS: -O -inline -release
*/

/***
 * Do coverage testing of cgelem.d
 * Check coverage here:
 * https://codecov.io/gh/dlang/dmd/src/master/src/dmd/backend/cgelem.d
 */

import core.stdc.stdio;

template tuple(A...) { alias tuple = A; }

/*************************************************/

void test_eladdr()
{
    //  & (*p1 = e) => ((*(t = p1) = e), t)
    int i = 4;
    int* p1 = &i;
    int e = 5;
    auto x = &(*p1 = e);
    assert(i == 5);
    assert(x == p1);
}

/*************************************************/

void test_elneg()
{
    static int i = 3;
    int j = - -i;
    assert(i == j);
}

/*************************************************/

int main()
{
    test_eladdr();
    test_elneg();

    printf("Success\n");
    return 0;
}
