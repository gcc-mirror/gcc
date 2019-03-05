
void main()
{
    class C {}
    class D : C {}
    auto x = new immutable(D);
    C y = x;
}
