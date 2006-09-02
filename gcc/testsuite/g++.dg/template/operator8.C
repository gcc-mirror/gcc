//PR c++/27494

struct A
{
    template<operator+> void foo() {}   // { dg-error "identifier|non-function|template arguments" }
};
 
struct B
{
    template<operator> void foo() {}    // { dg-error "identifier|non-function|'void'" }
    template<int> void bar() {}         // { dg-error "template arguments" }
};
