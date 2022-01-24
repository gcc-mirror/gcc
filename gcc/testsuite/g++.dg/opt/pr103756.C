// PR rtl-optimization/103756
// { dg-do compile }
// { dg-options "-std=c++17 -O -fcompare-debug -fconserve-stack -frename-registers -fno-tree-ch -fira-algorithm=priority" }

char __copy_move_b___trans_tmp_9;
template <typename> struct __iterator_traits;
template <typename _Tp> struct __iterator_traits<_Tp *> {
  typedef _Tp &reference;
};
template <typename _Iterator> struct reverse_iterator {
  _Iterator current;
  reverse_iterator();
  reverse_iterator(reverse_iterator &__x) : current(__x.current) {}
  _Iterator base() { return current; }
  typename __iterator_traits<_Iterator>::reference operator*() {
    return *current;
  }
  reverse_iterator operator--() {
    ++current;
    return *this;
  }
};
template <typename _IteratorL, typename _IteratorR>
auto operator-(_IteratorL __x, _IteratorR __y) {
  return __y - __x.base();
}
struct __copy_move_backward {
  template <typename _BI1, typename _BI2>
  static _BI2 __copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result) {
    typename __n = __last - __first;
    for (; __n > 0; --__n) {
      reverse_iterator __trans_tmp_8 = --__result;
      *__trans_tmp_8 = __copy_move_b___trans_tmp_9;
    }
    return __result;
  }
};
template <int, typename _BI1, typename _BI2>
inline _BI2 __copy_move_backward_a2(_BI1 __first, _BI1 __last, _BI2 __result) {
  return __copy_move_backward::__copy_move_b(__first, __last, __result);
}
template <int _IsMove, typename _BI1, typename _BI2>
_BI2 __copy_move_backward_a1(_BI1 __last, _BI2 __result) {
  _BI1 __first;
  return __copy_move_backward_a2<_IsMove>(__first, __last, __result);
}
template <int _IsMove, typename _II, typename _OI>
void __copy_move_backward_a(_II, _OI __result) {
  reverse_iterator<unsigned char *> __trans_tmp_7 =
      __copy_move_backward_a1<_IsMove>(__trans_tmp_7, __result);
}
template <typename _BI1, typename _BI2>
void move_backward(_BI1 __first, _BI2 __result) {
  __copy_move_backward_a<true>(__first, __result);
}
reverse_iterator<unsigned char *> __rotate___first;
void __rotate() { move_backward(__rotate___first, __rotate___first); }
