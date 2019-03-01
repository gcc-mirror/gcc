// PR c++/89537
// { dg-do compile { target c++11 } }

template <typename> class A {};
template <typename, typename, typename, typename> class B;
class C {
  using mapped_type = int;

public:
  template <typename _Compare>
  C(B<mapped_type, _Compare, A<int>, A<int>> *p1, unsigned)
      : keys(p1->keys), // { dg-error "18: invalid use of non-static member function" }
        values(p1->values) {} // { dg-error "20: invalid use of non-static member function" }
  A<int> keys;
  A<int> values;
};
class D {
public:
  using key_compare = int;
  template <typename _Alloc> D(key_compare, _Alloc);
};
template <typename _Tp, typename, typename, typename = A<_Tp>> class B {
  using _Impl = D;
  _Impl _M_impl;

public:
  using key_compare = int;
  using iterator = C;
  template <typename _Alloc> B(key_compare p1, _Alloc p2) : _M_impl(p1, p2) {}
  template <typename _Alloc> B(_Alloc p1) : B(key_compare(), p1) {}
  iterator begin() { return {this, 0}; }
  void keys();
  void values();
};
void fn1() {
  B<int, int, A<int>> m(fn1);
  m.begin();
}
