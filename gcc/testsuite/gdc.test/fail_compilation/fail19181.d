/*
TEST_OUTPUT:
---
fail_compilation/fail19181.d(15): Error: undefined identifier `LanguageError`
---
*/
struct S
{
    void opDispatch(string name, T)(T arg) { }
}

void main()
{
    S s;
    s.foo(LanguageError);
}
