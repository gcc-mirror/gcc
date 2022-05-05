//https://issues.dlang.org/show_bug.cgi?id=20777
struct FooInt
{
    int i;
    auto opBinary(string op : "+")(int j)
    {
        return typeof(this)(i + j);
    }

    static @property FooInt max()
    {
        return typeof(this)(int.max);
    }
}

enum foolist
{
    hi  = FooInt(0),
    bye
}
