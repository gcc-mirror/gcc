module imports.link10920a;

struct FormatSpec(C)
{
    void func() {}
}

struct BitArray
{
    auto toString()
    {
        // An auto function may runs semantic3 to calculate return type,
        // even if it's non-root symbol.
        // But inside the function body, all instantiations should be treated
        // as speculative.
        FormatSpec!char fs;
        fs.func();
    }
}
