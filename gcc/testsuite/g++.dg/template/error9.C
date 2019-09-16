// PR c++/10926

struct Foo
{
    template <int i>
    ~Foo(); // { dg-error "5:destructor .Foo::~Foo\\\(\\\)." }
};
