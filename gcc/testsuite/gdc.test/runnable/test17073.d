struct S0
{
    int x = void;
}
struct S1
{
    S0  x = S0(42);
}
void main()
{
    S1  x;
    assert(x.x.x == 42);
}
