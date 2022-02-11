
/* REQUIRED_ARGS: -betterC
 */

// https://issues.dlang.org/show_bug.cgi?id=18099

struct D
{
    static struct V
    {
        ~this() { }
    }

    V get()
    {
        V v;
        return v;
    }
}
