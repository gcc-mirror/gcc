void main()
{
    alias Zoo = Foo!(1);
}

struct Foo(size_t N)
{
    string bar()
    {
        Appender!(string) w;
        char[] buf; put(w, buf);
        return "";
    }

    public bool opEquals(T)(T other) const
    // Add const, different from bug 10056
    {
        alias Foo!(typeof(this), T, "CMP") P;
        return false;
    }
}

template Foo(T, U, string OP)
{
    static if (T.ISEMPTY && U.ISEMPTY)
        enum bool S = false;
    else
        enum bool S = false;

    alias Foo = Foo!(0);
}

/**********************************************/

void put(R, E)(ref R r, E e)
{
    static if (is(typeof(r.put(e))))
    {
        r.put(e);
    }
    else
    {
        static assert(false, "Cannot put a "~E.stringof~" into a "~R.stringof);
    }
}

struct Appender(A : T[], T)
{
    private template canPutItem(U)
    {
        enum bool canPutItem = is(U : T);
    }
    private template canPutRange(R)
    {
        enum bool canPutRange = is(typeof(Appender.init.put(R.init[0])));
    }

    void put(U)(U item) if (canPutItem!U)
    {
        char[T.sizeof == 1 ? 4 : 2] encoded;
        put(encoded[]);
    }
    void put(R)(R items) if (canPutRange!R)
    {
    }
}
