// PR c++/10926

struct Foo
{
    template <int i>
    ~Foo(); // { dg-error "" }
};
