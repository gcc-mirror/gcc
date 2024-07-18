// PR c++/100659

template <typename> struct A
{
  struct E { static int V; };
  A::E::V;		       // { dg-warning "access decl" }
  enum { V };		       // { dg-error "conflicts with a previous decl" }
};
