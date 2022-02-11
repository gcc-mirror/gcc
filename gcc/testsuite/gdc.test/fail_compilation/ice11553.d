/*
TEST_OUTPUT:
---
fail_compilation/ice11553.d(22): Error: recursive template expansion while looking for `A!().A()`
fail_compilation/ice11553.d(22): Error: expression `A()` of type `void` does not have a boolean value
---
*/

template A(alias T)
{
    template A()
    {
        alias A = T!();
    }
}

template B()
{
    alias B = A!(.B);
}

static if (A!B) {}
