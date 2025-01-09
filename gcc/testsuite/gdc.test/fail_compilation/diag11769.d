/*
TEST_OUTPUT:
---
fail_compilation/diag11769.d(18): Error: `diag11769.foo!string.bar` called with argument types `(string)` matches multiple overloads after implicit conversions:
fail_compilation/diag11769.d(13):     `diag11769.foo!string.bar(wstring __param_0)`
and:
fail_compilation/diag11769.d(14):     `diag11769.foo!string.bar(dstring __param_0)`
---
*/

template foo(T)
{
    void bar(wstring) {}
    void bar(dstring) {}
}
void main()
{
    foo!string.bar("abc");
}
