// REQUIRED_ARGS: -mcpu=native

static if (__traits(compiles, __vector(ubyte[16])))
{
    alias ubyte16 = __vector(ubyte[16]);

    ubyte16 bug(ubyte val)
    {
        immutable ubyte16 a = 0, b = val;
        return b;
    }

    void main()
    {
        bug(0);
    }
}
else
{
    void main()
    {
    }
}
