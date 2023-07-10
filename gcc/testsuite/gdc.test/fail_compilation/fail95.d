/*
TEST_OUTPUT:
---
fail_compilation/fail95.d(19): Error: template `fail95.A` is not callable using argument types `!()(int)`
fail_compilation/fail95.d(11):        Candidate is: `A(alias T)(T)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=142
// Assertion failure: '0' on line 610 in file 'template.c'
template A(alias T)
{
    void A(T) { T = 2; }
}

void main()
{
    int i;
    A(i);
    assert(i == 2);
}
