// PR c++/49604
// { dg-do compile { target c++11 } }

struct Foo
{
private:
    int val;
    enum impl_t : int;
public:
    Foo(impl_t v) : val(v) {}
};
enum Foo::impl_t : int { X };

Foo test = Foo::X;  // { dg-error "private" }
