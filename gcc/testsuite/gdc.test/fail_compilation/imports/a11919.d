void doBar(T)(T t)
{
    static if (t.tupleof.length)
        if (zoo!t.length == 0) {}
    static if (is(T B == super)
            && is(B[0] == class)
            && is(B[])
            )
    {
        B[0] b = t;
        doBar(b);
    }
}
template zoo(alias t)
{
    enum zoo = __traits(getAttributes, t.tupleof);
}
