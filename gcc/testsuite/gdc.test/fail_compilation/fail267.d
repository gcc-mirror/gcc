/*
TEST_OUTPUT:
---
fail_compilation/fail267.d(15): Error: template `Bar()` does not have property `foo`
---
*/

class C
{
    template Bar()
    {
    }
}

typeof(C.Bar.foo) quux;
