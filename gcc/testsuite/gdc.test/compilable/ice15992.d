// PERMUTE_ARGS:

struct Appender()
{
    bool canExtend = false;
}

struct CustomFloat()
{
    union ToBinary
    {
        CustomFloat!() get;
    }

    void opAssign(F)(F input)
        if (__traits(compiles, cast(real)input))
    {
    }

    real get()()
    {
        Appender!() app;
        assert(false);
    }

    T opCast(T)() { return get!(); }

    alias g = get!();
}

void f()
{
    alias FPTypes = CustomFloat!();
}
