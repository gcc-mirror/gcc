/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/systemvariables_deprecation.d(16): Deprecation: `@safe` function `main` calling `middle`
fail_compilation/systemvariables_deprecation.d(21):        which calls `systemvariables_deprecation.inferred`
fail_compilation/systemvariables_deprecation.d(27):        which wouldn't be `@safe` because of:
fail_compilation/systemvariables_deprecation.d(27):        cannot access `@system` variable `x0` in @safe code
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
