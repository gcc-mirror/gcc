/*
TEST_OUTPUT:
---
fail_compilation/fail235.d(12): Error: template instance `Tuple!(typeid(char))` expression `typeid(char)` is not a valid template value argument
---
*/
template Tuple(TPL...)
{
    alias TPL Tuple;
}

auto K = Tuple!(typeid(char));

/*
TEST_OUTPUT:
---
fail_compilation/fail235.d(24): Error: template instance `Alias!(typeid(char))` expression `typeid(char)` is not a valid template value argument
---
*/
template Alias(alias A)
{
    alias A Alias;
}
auto A = Alias!(typeid(char));
