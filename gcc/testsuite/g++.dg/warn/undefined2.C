// PR c++/119303

template <class> struct c {
  enum { d = 4 };
};
template <bool> struct e {
  typedef void g;
};
template <class _Tp>
inline typename e<!c<_Tp>::d>::g bar(_Tp); // { dg-warning "used but never defined" }

int x;

void foo() { bar(x); }
