// https://issues.dlang.org/show_bug.cgi?id=13577

/*
TEST_OUTPUT:
---
fail_compilation/fail13577.d(27): Error: cannot implicilty convert range element of type `int[]` to variable `x` of type `immutable(int[])`
---
*/

struct Tuple(Types...)
{
    Types items;
    alias items this;
}

struct Range(T)
{
    T[] arr;
    alias ElemType = Tuple!(int, T);
    ElemType front() { return typeof(return)(0, arr[0]); }
    bool empty() { return false; }
    void popFront() {}
}

void main()
{
    foreach (immutable i, immutable x; Range!(int[])()) {} // Error
}
