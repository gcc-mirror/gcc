// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail15292.d(27): Error: cannot compare S15292 because its auto generated member-wise equality has recursive definition
---
*/

struct NullableRef15292(T)
{
    inout(T) get() inout
    {
        assert(false);
    }

    alias get this;
}

struct S15292
{
    NullableRef15292!S15292 n;
}

void main()
{
    S15292 s;
    assert(s == s);
}
