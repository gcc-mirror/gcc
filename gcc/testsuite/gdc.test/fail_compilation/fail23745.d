// https://issues.dlang.org/show_bug.cgi?id=2374

/*
TEST_OUTPUT:
---
fail_compilation/fail23745.d(21): Error: undefined identifier `UndefinedType`
fail_compilation/fail23745.d(14): Error: function `fun` does not override any function, did you mean to override `fail23745.A.fun`?
fail_compilation/fail23745.d(21):        Function `fail23745.A.fun` contains errors in its declaration, therefore it cannot be correctly overridden
---
*/

class B : A
{
    override void fun()
    {
    }
}

class A
{
    void fun(UndefinedType);
}
