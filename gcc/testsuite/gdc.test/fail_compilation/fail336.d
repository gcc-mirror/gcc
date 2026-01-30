/*
TEST_OUTPUT:
---
fail_compilation/fail336.d(17): Error: Cannot use struct initializer syntax for struct `S` because it has a constructor
fail_compilation/fail336.d(17):        Use `S( arguments )` instead of `{ initializers }`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=3476
// C-style initializer for structs must be disallowed for structs with a constructor
struct S
{
    int a;
    this(int) {}
}

S s = { 1 };
