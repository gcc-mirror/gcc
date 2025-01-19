/**
Test language editions (currently experimental)

TEST_OUTPUT:
---
fail_compilation/editions.d(15): Error: scope parameter `x` may not be returned
---
*/
@__edition_latest_do_not_use
module editions;

@safe:
int* f(scope int* x)
{
    return x;
}
