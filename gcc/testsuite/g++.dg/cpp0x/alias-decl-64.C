// PR c++/85976
// { dg-do compile { target c++11 } }

template <int> class A;
template <typename> class B;
template <typename> struct C;
template <typename P_expr> class D {
  using B<typename P_expr::T_numtype>::rank_;
  void operator()(typename C<A<rank_>>::i);
};

template <typename P_expr> class F {
  using B<typename P_expr::T_numtype>::rank_;
  void operator()(typename C<A<rank_>>::i);
};
