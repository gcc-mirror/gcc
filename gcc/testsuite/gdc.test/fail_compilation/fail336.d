/*
TEST_OUTPUT:
---
fail_compilation/fail336.d(16): Error: struct S has constructors, cannot use { initializers }, use S( initializers ) instead
---
*/

// Issue 3476 - C-style initializer for structs must be disallowed for structs with a constructor

struct S
{
    int a;
    this(int) {}
}

S s = { 1 };
