module imports.diag10141b;

template isSomeChar(T) {
    enum isSomeChar =
        is(immutable T == immutable  char) ||
        is(immutable T == immutable wchar) ||
        is(immutable T == immutable dchar);
}

struct Appender(A : T[], T)
{
    private template canPutItem(U)
    {
        enum bool canPutItem =
            //isImplicitlyConvertible!(U, T) ||
            isSomeChar!T && isSomeChar!U;
    }
    private template canPutRange(Range)
    {
        enum bool canPutRange =
            //isInputRange!Range &&
            is(typeof(Appender.init.put("a"d[0])));
    }

    /**
     * Appends one item to the managed array.
     */
    void put(U)(U item) if (canPutItem!U)
    {
        char[T.sizeof == 1 ? 4 : 2] encoded;
        auto len = 1;
        put(encoded[0 .. len]); // !
    }

    /**
     * Appends an entire range to the managed array.
     */
    void put(Range)(Range items) if (canPutRange!Range)
    {
    }
}

void put(R, E)(ref R r, E e)
{
    static if (is(typeof(r.put(e))))
    {
        r.put(e);
    }
    else
    {
        static assert(false,
                "Cannot put a "~E.stringof~" into a "~R.stringof);
    }
}
