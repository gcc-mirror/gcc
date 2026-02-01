module imports.pragmainline_a;

pragma(inline, true)
int foo()
{
    return 1;
}

pragma(inline, true)
auto bar()
{
    return &foo;
}

mixin template LengthField(alias sym)
{
    pragma(inline, true)
    size_t length() const
    {
        return sym.length;
    }
}

struct Data
{
    string data;
    mixin LengthField!data;

    pragma(inline, true)
    int opApply(scope int delegate(const Data) dg) {
        return dg(this);
    }
}

pragma(inline, true)
int value()
{
    return 10;
}
