module imports.gdc239;

class C239a
{
    auto bar()
    {
        chainPath();
    }
}

auto chainPath()
{
    struct OnlyResult { }
    chain([], OnlyResult(), []);
}

auto chain(Ranges...)(Ranges rs)
{
    static struct Result
    {
        Ranges source;
        this(Ranges)
        {
        }
    }
    Result(rs);
}
