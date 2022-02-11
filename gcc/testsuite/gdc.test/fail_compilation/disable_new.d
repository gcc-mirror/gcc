/*
TEST_OUTPUT:
---
fail_compilation/disable_new.d(23): Error: cannot allocate `class C` with `new` because it is annotated with `@disable new()`
fail_compilation/disable_new.d(24): Error: cannot allocate `struct S` with `new` because it is annotated with `@disable new()`
---
*/

class C
{
    // force user of a type to use an external allocation strategy
    @disable new();
}

struct S
{
    // force user of a type to use an external allocation strategy
    @disable new();
}

void main()
{
    auto c = new C();
    auto s = new S();
}
