// { dg-do assemble  }
// Bug: instantiation of D() corrupts declaration of basis[].

struct B { };
template <int t>
struct D : public B 
{
    D() : B () { }
};

B const * basis[] =
{
    new D<0>,
    new D<1>,
};
