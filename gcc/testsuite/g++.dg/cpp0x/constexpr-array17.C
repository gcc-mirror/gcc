// PR c++/71092
// { dg-do compile { target c++11 } }

template <typename _Default> struct A { using type = _Default; };
template <typename _Default, template <typename> class>
using __detected_or = A<_Default>;
template <typename _Default, template <typename> class _Op>
using __detected_or_t = typename __detected_or<_Default, _Op>::type;
template <typename _Tp> struct B { typedef _Tp value_type; };
struct C {
  template <typename _Tp> using __pointer = typename _Tp::pointer;
};
template <typename _Alloc> struct J : C {
  using pointer = __detected_or_t<typename _Alloc::value_type *, __pointer>;
};
template <typename _T1> void _Construct(_T1 *) { new _T1; }
struct D {
  template <typename _ForwardIterator, typename _Size>
  static _ForwardIterator __uninit_default_n(_ForwardIterator p1, _Size) {
    _Construct(p1);
  }
};
template <typename _ForwardIterator, typename _Size>
void __uninitialized_default_n(_ForwardIterator p1, _Size) {
  D::__uninit_default_n(p1, 0);
}
template <typename _ForwardIterator, typename _Size, typename _Tp>
void __uninitialized_default_n_a(_ForwardIterator p1, _Size, _Tp) {
  __uninitialized_default_n(p1, 0);
}
template <typename> struct __shared_ptr {
  constexpr __shared_ptr() : _M_ptr(), _M_refcount() {}
  int _M_ptr;
  int _M_refcount;
};
template <typename _Alloc> struct F {
  typedef _Alloc _Tp_alloc_type;
  struct G {
    typename J<_Tp_alloc_type>::pointer _M_start;
    G(_Tp_alloc_type);
  };
  F(int, _Alloc p2) : _M_impl(p2) {}
  G _M_impl;
};
template <typename _Tp, typename _Alloc = B<_Tp>> struct K : F<_Alloc> {
  typedef _Alloc allocator_type;
  K(int, allocator_type p2 = allocator_type()) : F<_Alloc>(0, p2) {
    __uninitialized_default_n_a(this->_M_impl._M_start, 0, 0);
  }
};
struct H {
  H();
  struct I {
    __shared_ptr<int> trigger[1];
  };
  __shared_ptr<int> resetTrigger_;
  K<I> states_;
  __shared_ptr<int> triggerManager_;
};
__shared_ptr<int> a;
H::H() : states_(0), triggerManager_(a) {}
