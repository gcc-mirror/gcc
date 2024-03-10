// https://issues.dlang.org/show_bug.cgi?id=22760

extern(C++) void f(T)(T)
{
}
struct S1(T)
{
    struct S2
    {
    }
}
void fun()
{
    f(S1!int.S2());
}
