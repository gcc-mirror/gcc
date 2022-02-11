// REQUIRED_ARGS: -unittest -de

auto test18251(T)(T t)
if (!__traits(isDeprecated, T))
{
    return T.init;
}

unittest
{
    auto b = test18251(2);
}

deprecated auto test18251(T)(T t)  // deprecated storage class got lost when expanding.
if (__traits(isDeprecated, T))
{
    return T.init;
}

deprecated unittest
{
    auto b = test18251(2 + 2i);
}
