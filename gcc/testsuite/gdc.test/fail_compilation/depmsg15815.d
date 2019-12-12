// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/depmsg15815.d(23): Deprecation: alias depmsg15815.Alias!(const(Foo)).Alias is deprecated - message
Foo
---
*/

template Unqual(T)
{
    static if (is(T U == const U)) alias Unqual = U;
    else alias Unqual = T;
}

deprecated("message")
template Alias(T)
{
    alias Alias = Unqual!T;
}

struct Foo {}
pragma(msg, Alias!(const(Foo)));
