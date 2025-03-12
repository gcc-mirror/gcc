/*
TEST_OUTPUT:
---
fail_compilation/ice12902.d(20): Error: variable `ice12902.main.$` - type `void` is inferred from initializer `s.opDollar()`, and variables cannot be of type `void`
fail_compilation/ice12902.d(20): Error: expression `s.opDollar()` is `void` and has no value
---
*/

struct S
{
    void opDollar() { }
    void opIndex() { }
    void opIndexAssign() { }
    void opSliceAssign() { }
}

void main()
{
    S s;
    s[] = s[$];
}
