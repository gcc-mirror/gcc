module imports.a14267;

struct SysTime14267
{
    // semantic() is called twice, and its scope is wrongly set to NULL
    // at the second call.
    this(long stdTime) {}
    this(this) {}
    ~this() {}

    static SysTime14267 min()
    {
        // inlining this function will call the semantic3() of SysTime14267 constructor.
        // but its 'scope' field is NULL so unintentionally semantic3() call fails.
        auto st = SysTime14267(long.min);
        auto st2 = st;
        return st2;
    }
}
