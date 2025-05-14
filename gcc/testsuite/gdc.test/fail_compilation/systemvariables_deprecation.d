/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/systemvariables_deprecation.d(15): Deprecation: `@safe` function `main` calling `middle`
fail_compilation/systemvariables_deprecation.d(20):        which calls `inferred`
fail_compilation/systemvariables_deprecation.d(26):        and access `@system` variable `x0` makes it fail to infer `@safe`
---
*/

// test deprecation messages before -preview=systemVariables becomes default

void main() @safe
{
    middle(); // nested deprecation
}

auto middle()
{
    return inferred(); // no deprecation, inferredC is not explicit `@safe`
}

auto inferred()
{
    @system int* x0;
    x0 = null;
}
