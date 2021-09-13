/*
TEST_OUTPUT:
---
fail_compilation/fail341.d(26): Error: struct fail341.S is not copyable because it is annotated with `@disable`
fail_compilation/fail341.d(27): Error: function `fail341.foo` cannot be used because it is annotated with `@disable`
---
*/

struct T
{
    @disable this(this)
    {
    }
}

struct S
{
    T t;
}

@disable void foo() { }

void main()
{
    S s;
    auto t = s;
    foo();
}
