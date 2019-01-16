/*
TEST_OUTPUT:
---
fail_compilation/parse13361.d(11): Error: empty attribute list is not allowed
fail_compilation/parse13361.d(14): Error: empty attribute list is not allowed
fail_compilation/parse13361.d(14): Error: use `@(attributes)` instead of `[attributes]`
---
*/
struct A
{
  @()
    int b;

  []    // deprecated style
    int c;
}
