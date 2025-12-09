/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -mrvv-max-lmul=m2 -fpermissive -Wno-return-type" } */

namespace std {
template <typename _Iterator> _Iterator __miter_base(_Iterator);
template <typename _Default, typename, template <typename> class>
struct __detector {
  using type = _Default;
};
template <typename _Default, template <typename> class _Op>
using __detected_or = __detector<_Default, void, _Op>;
template <typename _Default, template <typename> class _Op>
using __detected_or_t = typename __detected_or<_Default, _Op>::type;
template <typename _Tp> class allocator {
public:
  typedef _Tp value_type;
};
template <typename> struct pointer_traits {
  template <typename _Up> using rebind = _Up *;
};
} // namespace std
namespace __gnu_cxx {
template <typename _Iterator, typename> class __normal_iterator {
public:
  _Iterator base();
};
} // namespace __gnu_cxx
namespace std {
template <bool, typename _OutIter, typename _InIter>
void __assign_one(_OutIter __out, _InIter __in) {
  *__out = *__in;
}
template <bool _IsMove, typename _BI1, typename _BI2>
__copy_move_backward_a2(_BI1 __first, _BI1 __last, _BI2 __result) { /* { dg-warning "with no type" "" } */
  while (__first != __last) {
    --__last;
    --__result;
    __assign_one<_IsMove>(__result, __last);
  }
}
template <bool _IsMove, typename _BI1, typename _BI2>
__copy_move_backward_a1(_BI1 __first, _BI1 __last, _BI2 __result) { /* { dg-warning "with no type" "" } */
  __copy_move_backward_a2<_IsMove>(__first, __last, __result);
}
template <bool _IsMove, typename _II, typename _OI>
__copy_move_backward_a(_II __first, _II __last, _OI __result) { /* { dg-warning "with no type" "" } */
  __copy_move_backward_a1<_IsMove>(__first, __last, __result);
}
template <typename _BI1, typename _BI2>
move_backward(_BI1 __first, _BI1 __last, _BI2 __result) { /* { dg-warning "with no type" "" } */
  __copy_move_backward_a<true>(__first, __miter_base(__last), __result);
}
struct __allocator_traits_base {
  template <typename _Tp> using __pointer = typename _Tp::pointer;
  template <typename _Tp> using __c_pointer = typename _Tp::const_pointer;
};
template <typename _Alloc> struct allocator_traits : __allocator_traits_base {
  typedef typename _Alloc::value_type value_type;
  using pointer = __detected_or_t<value_type *, __pointer>;
  template <template <typename> class, typename _Tp> struct _Ptr {
    using type = typename pointer_traits<pointer>::rebind<_Tp>;
  };
  using const_pointer = typename _Ptr<__c_pointer, value_type>::type;
};
} // namespace std
namespace __gnu_cxx {
template <typename _Alloc>
struct __alloc_traits : std::allocator_traits<_Alloc> {};
} // namespace __gnu_cxx
namespace std {
template <typename, typename _Alloc> struct _Vector_base {
  typedef __gnu_cxx::__alloc_traits<_Alloc> _Tp_alloc_type;
  typedef typename __gnu_cxx::__alloc_traits<_Tp_alloc_type>::pointer pointer;
  struct {
    pointer _M_finish;
  } _M_impl;
};
template <typename _Tp, typename _Alloc = allocator<_Tp>>
class vector : _Vector_base<_Tp, _Alloc> {
  typedef _Vector_base<_Tp, _Alloc> _Base;
  typedef typename _Base::_Tp_alloc_type _Alloc_traits;

public:
  typedef _Tp value_type;
  typedef typename _Base::pointer pointer;
  typedef typename _Alloc_traits::const_pointer const_pointer;
  typedef __gnu_cxx::__normal_iterator<pointer, vector> iterator;
  typedef __gnu_cxx::__normal_iterator<const_pointer, vector> const_iterator;
  iterator begin();
  iterator insert(const_iterator, const value_type &);
  struct _Temporary_value {};
  template <typename _Arg> void _M_insert_aux(iterator, _Arg &&);
};
template <typename _Tp, typename _Alloc>
typename vector<_Tp, _Alloc>::iterator
vector<_Tp, _Alloc>::insert(const_iterator, const value_type &) {
  auto __pos = begin();
  _Temporary_value __x_copy;
  _M_insert_aux(__pos, __x_copy);
}
template <typename _Tp, typename _Alloc>
template <typename _Arg>
void vector<_Tp, _Alloc>::_M_insert_aux(iterator __position, _Arg &&) {
  move_backward(__position.base(), this->_M_impl._M_finish,
                this->_M_impl._M_finish);
}
namespace internals {
struct distributing {
  distributing &operator=(const distributing &);
  int global_row;
  *constraints; /* { dg-warning "with no type" "" } */
};
distributing &distributing::operator=(const distributing &in) {
  global_row = in.global_row;
  return; /* { dg-warning "return-statement with no value" "" } */
}
insert_index(vector<distributing> my_indices) { /* { dg-warning "with no type" "" } */
  typedef vector<distributing>::iterator index_iterator;
  index_iterator pos;
  distributing row_value;
  my_indices.insert(pos, row_value);
}
} // namespace internals
} // namespace std
