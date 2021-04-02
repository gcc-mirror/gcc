// PR c++/68451
// { dg-do compile { target c++11 } }

struct A {};

struct B
{
    A a;
    friend decltype(a);
};

template <typename T>
struct C
{
    A a;
    friend decltype(a);
};

int main()
{
    B b;
    C<int> c;
}
