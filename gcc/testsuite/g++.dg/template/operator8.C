//PR c++/27494

struct A
{
    template<operator+> void foo() {}   // { dg-error "identifier|parameter|template arguments" }
};
