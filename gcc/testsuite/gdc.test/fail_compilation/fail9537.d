/*
TEST_OUTPUT:
---
fail_compilation/fail9537.d(26): Error: cannot take address of expression `foo(tuple(1, 2))` because it is not an lvalue
---
*/

struct Tuple(T...)
{
    T field;
    alias field this;
}

Tuple!T tuple(T...)(T args)
{
    return Tuple!T(args);
}

auto ref foo(T)(auto ref T t)
{
    return t[0];    // t[0] is deduced to non-ref
}

void main()
{
    int* p = &foo(tuple(1, 2));
}
