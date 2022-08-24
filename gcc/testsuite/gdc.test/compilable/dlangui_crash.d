// https://issues.dlang.org/show_bug.cgi?id=22365

class DrawableCache
{
    Ref _nullDrawable;

    this()
    {
        debug Log;
    }
}

class DrawableCacheEmpty
{
    Ref _nullDrawable;

    this() {}
}

struct Ref
{

    ~this()
    {
    }
}

void foo()
{
    try
        debug Log;
    catch (Exception)
        assert(false);
}
