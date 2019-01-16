module std14198.array;

version(bug14198)
{
}
else
    import std14198.uni;

struct Appender(A)
{
    alias T = immutable char;

    void put(U)(U item)
    if (
            is(U : T) ||
            is(immutable U == immutable char)
       )
    {
        import std14198.uni; // necessary
        assert(0);
    }
}

Appender!A appender(A)()
{
    return Appender!A();
}
