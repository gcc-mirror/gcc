module imports.a15079;

Vector!string parseAlgorithmName()
{
    assert(0);
}

struct Vector(ALLOC)
{
    @disable this(this);

    RefCounted!(Vector, ALLOC) dupr()
    {
        assert(0);
    }
}

struct RefCounted(T, ALLOC)
{
    ~this()
    {
        T* objc;
        .destroy(*objc);
    }
}

// ----

void _destructRecurse(S)(ref S s)
    if (is(S == struct))
{
    static if (__traits(hasMember, S, "__xdtor") &&
               __traits(isSame, S, __traits(parent, s.__xdtor)))
    {
        s.__xdtor();
    }
}

void destroy(T)(ref T obj) if (is(T == struct))
{
    _destructRecurse(obj);
    () @trusted {
        auto buf = (cast(ubyte*) &obj)[0 .. T.sizeof];
        const init = cast(ubyte[]) __traits(initSymbol, T);
        if (init.ptr is null) // null ptr means initialize to 0s
            buf[] = 0;
        else
            buf[] = init[];
    } ();
}
