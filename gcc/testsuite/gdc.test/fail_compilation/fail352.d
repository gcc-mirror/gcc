/*
TEST_OUTPUT:
---
fail_compilation/fail352.d(18): Error: cannot infer argument types, expected 1 argument, not 2
---
*/

struct Range
{
    bool empty;
    int front() { return 0; }
    void popFront() { empty = true; }
}

void main()
{
    // no index for range foreach
    foreach(i, v; Range()) {}
}
