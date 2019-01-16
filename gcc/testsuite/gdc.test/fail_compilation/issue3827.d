// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/issue3827.d(12): Deprecation: Implicit string concatenation is deprecated, use "Hello" ~ "World" instead
fail_compilation/issue3827.d(13): Deprecation: Implicit string concatenation is deprecated, use "A" ~ "B" instead
---
*/

void main ()
{
    string[] arr = [ "Hello" "World" ];
    auto foo = "A" "B";
}
