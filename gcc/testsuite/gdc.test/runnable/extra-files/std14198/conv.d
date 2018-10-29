module std14198.conv;

template to(T)
{
    T to(A...)(A args)
    {
        return toImpl!T(args);
    }
}

T toImpl(T, S)(S value)
    if (is(S : T))
{
    return value;
}

T toImpl(T, S)(S value)
    if (!is(S : T) &&
        is(T == string))
{
    alias src = value;

    import std14198.format : FormatSpec;
    import std14198.array : appender;

    auto w = appender!T();
    FormatSpec!char f; // necessary

    string str = src ? "true" : "false";
    for (; !str.length; str = str[1..$])
        w.put(str[0]);

    return "";
}
