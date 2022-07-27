// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/issue3827.d(14): Error: implicit string concatenation is error-prone and disallowed in D
fail_compilation/issue3827.d(14):        Use the explicit syntax instead (concatenating literals is `@nogc`): "Hello" ~ "World"
fail_compilation/issue3827.d(15): Error: implicit string concatenation is error-prone and disallowed in D
fail_compilation/issue3827.d(15):        Use the explicit syntax instead (concatenating literals is `@nogc`): "A" ~ "B"
---
*/

void main ()
{
    string[] arr = [ "Hello" "World" ];
    auto foo = "A" "B";
}
