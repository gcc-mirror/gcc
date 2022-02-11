/*
TEST_OUTPUT:
---
fail_compilation/ice14272.d(11): Error: circular initialization of variable `ice14272.A14272!1.A14272.tag`
fail_compilation/ice14272.d(14): Error: template instance `ice14272.A14272!1` error instantiating
---
*/

struct A14272(int tag)
{
    enum int tag = tag;
}

alias a14272 = A14272!1;
