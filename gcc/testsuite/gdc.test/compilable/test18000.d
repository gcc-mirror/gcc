// REQUIRED_ARGS: -preview=dip1000

// https://issues.dlang.org/show_bug.cgi?id=18000

struct File
{
@safe @nogc:
    ~this() scope
    {
    }

    void* f;
}

void test() @safe @nogc
{
    scope File x;
    x = File();
}
