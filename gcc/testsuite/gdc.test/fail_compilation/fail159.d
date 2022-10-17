/*
TEST_OUTPUT:
---
fail_compilation/fail159.d(24): Error: static assert:  `foo(S(1, 5), S(1, 4)) == 0` is false
---
*/

struct S
{
    int i;
    int j = 3;

    int opEquals(S e2) { return 1; }
}

int foo(S s1, S s2)
{
    return s1 == s2;
}

void main()
{
    static assert(foo( S(1,5), S(1,5) ) == 1);
    static assert(foo( S(1,5), S(1,4) ) == 0);
}
