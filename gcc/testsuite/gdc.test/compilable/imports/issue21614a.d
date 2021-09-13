module imports.issue21614a;

struct FormatSpec(Char)
{
    import imports.issue21614a;
}

template Tuple(Specs...)
{
    struct Tuple
    {
        alias spec = FormatSpec!char();
        this(Specs)
        {
        }
    }
}

auto findRoot(T)(T)
{
    return Tuple!(T)();
}
