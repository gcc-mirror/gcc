// PERMUTE_ARGS:

public struct CirBuff(T)
{
    private T[] data;
    private size_t head = 0;
    private size_t size = 0;
    public size_t length() const { return size; }

    public bool opEquals(CirBuff!T d) @trusted
    {
        if (length != d.length)
            return false;
        for (size_t i=0; i!=size; ++i)
        {
            if (this.data[(this.head+i) % this.data.length] !=
                d.data[(d.head + i) % d.data.length])
            {
                return false;
            }
        }
        return true;
    }
}

class Once
{
    Foo!Bar _bar;
}

class Bar
{
    static Once _once;
    mixin(sync!(Once, "_once"));
}

class Foo(T = int)
{
    CirBuff!T _buff;
}

template sync(T, string U = "this", size_t ITER = 0)
{
    static if (ITER == __traits(derivedMembers, T).length)
        enum sync = "";
    else
    {
        enum string mem = __traits(derivedMembers, T)[ITER];
        enum string sync =
            "static if(! __traits(isVirtualMethod, " ~ U ~ "." ~ mem ~ ")) { }"
            ~ sync!(T, U, ITER+1);
    }
}
