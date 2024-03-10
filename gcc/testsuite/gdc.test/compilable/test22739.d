// https://issues.dlang.org/show_bug.cgi?id=22739

extern(C++) auto f(T)()
{
    return T.init;
}
void main()
{
    f!int;
}
