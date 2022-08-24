/*
TEST_OUTPUT:
---
fail_compilation/fail236.d(14): Error: undefined identifier `x`
fail_compilation/fail236.d(22): Error: none of the overloads of template `fail236.Templ2` are callable using argument types `!()(int)`
fail_compilation/fail236.d(12):        Candidate is: `Templ2(alias a)(x)`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=870
// contradictory error messages for templates
template Templ2(alias a)
{
    void Templ2(x)
    {
    }
}

void main()
{
    int i;
    Templ2(i);
}
