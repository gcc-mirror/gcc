/*
TEST_OUTPUT:
---
fail_compilation/ice11553.d(22): Error: recursive template expansion while looking for A!().A()
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
