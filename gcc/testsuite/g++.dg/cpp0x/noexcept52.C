// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

template <typename _Alloc> class A {
  typedef _Alloc _Alloc_traits;
  A &operator=(A &&) noexcept(_Alloc_traits::_S_nothrow_move);
  A &m_fn1(A &&) noexcept(_Alloc_traits::_S_nothrow_move);
  void m_fn2(A<char>) {}
};
