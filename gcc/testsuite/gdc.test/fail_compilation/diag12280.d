/*
TEST_OUTPUT:
---
fail_compilation/diag12280.d(15): Error: undefined identifier `nonexistent`
fail_compilation/diag12280.d(13): Error: template instance diag12280.f!10 error instantiating
fail_compilation/diag12280.d(18):        11 recursive instantiations from here: f!0
---
*/

void f(int i)()
{
    static if (i < 10)
        f!(i + 1);
    else
        nonexistent();
}

alias f0 = f!0;
