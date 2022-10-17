// https://issues.dlang.org/show_bug.cgi?id=20661

class Context
{
    size_t[const(Key)] aa; /* Error: AA key type Key does not have bool opEquals(ref const Key) const */
    bool* checkAll;
}

struct Key
{
    Context context;
    bool opEquals(ref const Key other) @safe const
    {
        auto c = context.checkAll;
        return true;
    }
}
