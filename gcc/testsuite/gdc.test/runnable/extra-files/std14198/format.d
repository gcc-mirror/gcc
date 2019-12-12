module std14198.format;

struct FormatSpec(Char) if (is(Char == char))
{
    import std14198.conv;

    string toString()
    {
        return to!string(true); // necessary
    }
}
