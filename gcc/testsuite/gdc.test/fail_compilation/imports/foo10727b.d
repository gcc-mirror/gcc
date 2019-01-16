struct CirBuff(T)
{
    import imports.stdtraits10727 : isArray;
    CirBuff!T opAssign(R)(R) if (isArray!R)
    {}

    T[] toArray()
    {
        T[] ret = new T[this.length];
        return ret;
    }
    alias toArray this;
}

class Bar(T = int)
{
    CirBuff!T _bar;
}

class Once
{
    Bar!Foo _foobar;
}

class Foo : Frop {} // Frop is not defined
