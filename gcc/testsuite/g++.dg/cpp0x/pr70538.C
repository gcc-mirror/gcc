// { dg-do compile { target c++11 } }

struct A;
template <typename> class C;
using PathComponentPiece = C<int>;
class B {
  B(int);
  template <typename T> B(T);
  B(C<A>);
};
template <typename> class C : B {
  using base_type = B;
  base_type::base_type;  // { dg-warning "access declarations" }
  PathComponentPiece m_fn1() { return PathComponentPiece(); }
};
