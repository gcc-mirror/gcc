
static if (__traits(compiles, __vector(long[2])))
{
    __vector(long[2]) f()
    {
        __vector(long[2]) q;
        return q;
    }

    enum __vector(long[2]) v = f();
}
