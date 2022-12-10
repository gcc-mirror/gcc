module imports.pr108055conv;

T toStr(T, S)(S src)
{
    static if (is(typeof(T.init[0]) E))
    {
        struct Appender
        {
            inout(E)[] data;
        }

        import imports.pr108055spec;
        import imports.pr108055write;

        auto w = Appender();
        FormatSpec!E f;
        formatValue(w, src, f);
        return w.data;
    }
}

T to(T, A)(A args)
{
    return toStr!T(args);
}

