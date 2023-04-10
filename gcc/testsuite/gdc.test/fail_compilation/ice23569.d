// https://issues.dlang.org/show_bug.cgi?id=23569
/*
TEST_OUTPUT:
---
fail_compilation/ice23569.d(24): Error: cannot compare classes for equality because `object.Object` was not declared
---
*/
module object;

T _d_newclassT(T)()
if (is(T == class))
{
    return null;
}

@safe unittest1()
{
    class F
    {
        this(int )
        {
        }
    }
    auto ice23569 = new F(0) == new F(0);
}
