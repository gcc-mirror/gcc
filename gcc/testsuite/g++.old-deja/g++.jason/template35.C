// Bug: instantiation of D() corrupts declaration of basis[].
// Build don't link:

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
