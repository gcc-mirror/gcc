/*
TEST_OUTPUT:
---
fail_compilation/diag11078.d(19): Error: none of the overloads of 'value' are callable using argument types (double), candidates are:
fail_compilation/diag11078.d(12):        diag11078.S1.value()
fail_compilation/diag11078.d(13):        diag11078.S1.value(int n)
---
*/

struct S1
{
    @property int value() { return 1; }
    @property void value(int n) { }
}

void main()
{
    S1 s1;
    s1.value = 1.0;
}
