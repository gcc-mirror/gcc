/*
TEST_OUTPUT:
---
fail_compilation/ice8499.d(18): Error: undefined identifier `i`
---
*/

struct Variant
{
    @property T get(T)()
    {
        struct X {}   // necessary
    }
}

void main()
{
    (Variant()).get!(typeof(() => i));
}
