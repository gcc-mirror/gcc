namespace N
{
  struct A; // { dg-error "previous declaration" "" }
}

template<int I>
struct N::A {}; // { dg-error "redeclared" "" }
