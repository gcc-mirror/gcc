/*
TEST_OUTPUT:
---
fail_compilation/fail235.d(12): Error: expression typeid(char) is not a valid template value argument
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
fail_compilation/fail235.d(24): Error: expression typeid(char) is not a valid template value argument
---
*/
template Alias(alias A)
{
    alias A Alias;
}
auto A = Alias!(typeid(char));
