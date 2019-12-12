// PERMUTE_ARGS:
/*
TEST_OUTPUT
---
fail_compilation/fail19897.d(10): Error: cannot implicitly convert expression `[]` of type `const(char[0])` to `const(char)`
---
*/
struct S
{
    char[0] x;
}
const a = S('a');
const char c = a.x;
