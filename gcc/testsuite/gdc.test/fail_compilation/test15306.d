/*
REQUIRED_ARGS:
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test15306.d(17): Error: immutable delegate 'test15306.main.__dgliteral1' cannot access mutable data 'i'
fail_compilation/test15306.d(21): Error: shared delegate 'test15306.main.__dgliteral2' cannot access non-shared data 'p'
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15306

void main()
{
    // immutable cannot access mutable
    int i = 42;
    auto dg1 = delegate void() immutable { auto inner = i; };

    // shared cannot access unshared
    int* p = &i;
    auto dg2 = delegate int() shared { return *p; };
    assert(dg2() == i);

    // unshared can access shared
    shared j = 43;
    shared int* q = &j;
    auto dg3 = delegate int() { return *q; };
    assert(dg2() == j);
}
