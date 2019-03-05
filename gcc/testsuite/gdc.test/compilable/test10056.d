void main()
{
    alias Zoo = Foo10056!(false, false, 1);
}

struct Foo10056(bool S, bool L, size_t N)
{
    string bar()
    {
        Appender10056!(string) w;
        char[] buf; put10056(w, buf);
        return "";
    }

    public bool opEquals(T)(T other) //const
    //If you add const, also fails to compile with 2.062.
    {
        alias Foo10056!(typeof(this), T, "CMP") P;
        return false;
    }
}

template Foo10056(T, U, string OP)
{
    static if (T.ISEMPTY && U.ISEMPTY)
        enum bool S = false;
    else
        enum bool S = false;

    alias Foo10056 = Foo10056!(false, false, 0);
}

/**********************************************/

void put10056(R, E)(ref R r, E e)
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

struct Appender10056(A : T[], T)
{
    private template canPutItem(U)
    {
        enum bool canPutItem = is(U : T);
    }
    private template canPutRange(R)
    {
        enum bool canPutRange = is(typeof(Appender10056.init.put(R.init[0])));
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
