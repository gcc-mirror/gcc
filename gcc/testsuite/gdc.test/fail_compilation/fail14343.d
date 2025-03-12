// https://issues.dlang.org/show_bug.cgi?id=14343
/* TEST_OUTPUT:
---
fail_compilation/fail14343.d(21): Error: cannot modify struct instance `s` of type `S14343b` because it contains `const` or `immutable` members
fail_compilation/fail14343.d(23): Error: cannot modify struct instance `s` of type `S14343b` because it contains `const` or `immutable` members
---
*/

struct S14343b
{
    int i;
    immutable(Object) o;

    void opAddAssign(int j) { i += j; }
    void opAssign(S14343b other) {}
}

void test14343()
{
    S14343b s;
    ++s;
    assert(s.i == 1);
    s++;
    assert(s.i == 2);
}
