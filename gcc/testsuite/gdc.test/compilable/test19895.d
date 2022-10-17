// https://issues.dlang.org/show_bug.cgi?id=19895

void fn()
{
    void[] a;
    auto b = cast(byte[0][]) a;
}
