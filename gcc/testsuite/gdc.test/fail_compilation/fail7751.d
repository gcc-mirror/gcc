class Foo(T)
{
    T x;
    Foo y;
}
auto foo(T)(T x, Foo!T y=null)
{
    return new Foo!T(x, y);
}
void bar(U)(U foo, U[] spam=[])
{
    spam ~= [];
}
void main()
{
    bar(foo(0));
}
