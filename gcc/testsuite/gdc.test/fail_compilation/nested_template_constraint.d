/*
TEST_OUTPUT:
---
fail_compilation/nested_template_constraint.d(17): Error: template `foo` is not callable using argument types `!()(string, int)`
fail_compilation/nested_template_constraint.d(10):        Candidate is: `foo(int x = 0)`
fail_compilation/nested_template_constraint.d(11):          - Containing: `foo(T, U)(T t, U u)`
---
*/

template foo(int x = 0) {
    void foo(T, U)(T t, U u)
        if (is(T == int) && is(U == int)) {}
}

void main()
{
    foo("hello", 4);
}
