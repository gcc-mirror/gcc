// EXTRA_FILES: imports/imp22122.d
module imports.test22122;

struct S22122
{
    import imports.imp22122;
    Variant!(Imp22122)[] array;
}

void test22122_catch(S22122 s)
{
    try
    {
        foreach(elem; s.array)
        {
            import imports.imp22122;
            with(elem.get!Imp22122)
            {
            }
        }
    }
    catch (Exception)
    {
    }
}

void test22122_finally(S22122 s)
{
    try
    {
        foreach(elem; s.array)
        {
            import imports.imp22122;
            with(elem.get!Imp22122)
            {
            }
        }
    }
    finally
    {
    }
}

private struct Variant(T)
{
    union Impl
    {
    }
    auto get(E)()
    {
        return Impl();
    }
}
