//PR c++/8222
// Origin: giovannibajo@libero.it and setzersn@gmx.de

// { dg-do run }

struct Foo
{
    template <class>
    void func() {}
};
template <class>
void Bar(Foo* p)
{
    p->func<int>();
}
 
int main()
{
    Foo c;
    Bar<int>(&c);
}
