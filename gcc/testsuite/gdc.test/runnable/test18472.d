/* REQUIRED_ARGS: -betterC
*/

/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=18472
// https://github.com/dlang/dmd/pull/14676

@nogc nothrow pure:
immutable(Char)[] format(Char, Args...)(in Char[] fmt, Args args)
{

    if (__ctfe)
    {
        auto data2 = new char[5];
        auto data = new Data2;
        {
            auto data3 = new Data2;
        }
        data2 = cast(char[]) "test2";
        return data2;
    }
    else
    {
        return "test";
    }
}

extern(C) void main()
{
    static assert(getData() == "test");
    static assert("%s %s".format("test", "test") == "test2", "Not working");
    assert("%s %s".format("test", "test") == "test", "%s %s".format("test", "test"));
    assert(getData() == "test2", getData());
}

string getData()
{
    if (__ctfe)
    {
        auto data2 = new ubyte[5];
        auto data = new Data2;
        return "test";
    }
    else
    {
        return "test2";
    }
}

private struct Data2
{
    size_t capacity;
}
