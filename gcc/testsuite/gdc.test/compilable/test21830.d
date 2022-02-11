// REQUIRED_ARGS: -de -unittest

deprecated struct OldS21830 { }

struct NewS21830 { }

static if (1)
{
    auto test21830(T)(T t)
    if (is(T == NewS21830))
    {
        return T.init;
    }
}

deprecated auto test21830(T)(T t)
if (is(T == OldS21830))
{
    return T.init;
}

unittest
{
    auto b = test21830(NewS21830()); // error here about using test21830!OldS21830
}
