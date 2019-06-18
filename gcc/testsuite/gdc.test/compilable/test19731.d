// https://issues.dlang.org/show_bug.cgi?id=19731

class Out19731
{
    static struct State
    {
        int flags_;
    }
    Object obj_;

    invariant (obj_ !is null);

    auto obj7(out State state)
    {
        return this.obj_;
    }

    enum compiles = __traits(compiles, &Out19731.init.obj7);
}

class Arguments19731
{
    Object obj_;

    invariant (obj_ !is null);

    import core.stdc.stdarg;
    auto obj7(...)
    {
        return this.obj_;
    }

    enum compiles = __traits(compiles, &Arguments19731.init.obj7);
}

class Require19731
{
    Object obj_;

    invariant (obj_ !is null);

    auto obj7(int a)
        in(a != 0)
    {
        return this.obj_;
    }

    enum compiles = __traits(compiles, &Require19731.init.obj7);
}

class Ensure19731
{
    Object obj_;

    invariant (obj_ !is null);

    auto obj7(int a)
        out(result; result is obj_)
    {
        return this.obj_;
    }

    enum compiles = __traits(compiles, &Ensure19731.init.obj7);
}

class Sync19731
{
    Object obj_;

    invariant (obj_ !is null);

    synchronized auto obj7()
    {
        return this.obj_;
    }

    enum compiles = __traits(compiles, &Sync19731.init.obj7);
}
