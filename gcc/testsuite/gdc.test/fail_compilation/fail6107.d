/*
TEST_OUTPUT:
---
fail_compilation/fail6107.d(12): Error: variable name `__ctor` is not allowed
fail_compilation/fail6107.d(12):        identifiers starting with `__` are reserved for internal use
fail_compilation/fail6107.d(16): Error: variable name `__ctor` is not allowed
fail_compilation/fail6107.d(16):        identifiers starting with `__` are reserved for internal use
---
*/
struct Foo
{
    enum __ctor = 4;
}
class Bar
{
    int __ctor = 4;
}
