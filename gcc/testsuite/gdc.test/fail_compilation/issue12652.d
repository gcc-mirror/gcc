/*
TEST_OUTPUT:
----
fail_compilation/issue12652.d(18): Error: static initializations of associative arrays is not allowed.
fail_compilation/issue12652.d(18):        associative arrays must be initialized at runtime: https://dlang.org/spec/hash-map.html#runtime_initialization
---
*/

enum A
{
    x,
    y,
    z
}

struct S
{
    string[A] t = [A.x : "aaa", A.y : "bbb"];
}

void main ()
{
    S s;
}
