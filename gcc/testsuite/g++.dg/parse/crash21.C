namespace N
{
  struct A; // { dg-message "previous declaration" }
}

template<int I>
struct N::A {}; // { dg-error "redeclared" }
