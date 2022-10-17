// REQUIRED_ARGS: -de -unittest

deprecated struct S21831 { }

auto test21831(T)(T t)    // error: struct `S21831` is deprecated
if (!__traits(isDeprecated, T))
{
    return T.init;
}

deprecated auto test21831(T)(T t)
if (__traits(isDeprecated, T))
{
    return T.init;
}

deprecated unittest
{
    auto b = test21831(S21831()); // instantiated from here
}
