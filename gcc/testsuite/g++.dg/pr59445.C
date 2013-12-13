/* { dg-do compile } */
/* { dg-options "-O2" } */

template <typename _Iterator> struct A;
template <typename _Tp> struct A<_Tp *> {
  typedef _Tp value_type;
  typedef int difference_type;
};
template <typename _Compare> struct B {};
template <typename _Compare> struct C {
  _Compare _M_comp;
  template <typename _Value, typename _Iterator>
  int operator()(_Value &p1, _Iterator p2) {
    return _M_comp(p1, *p2);
  }
};
template <typename _Compare> C<_Compare> __val_comp_iter(B<_Compare>);
template <typename _RandomAccessIterator, typename _Compare>
void __unguarded_linear_insert(_RandomAccessIterator p1, _Compare p2) {
  typename A<_RandomAccessIterator>::value_type a;
  _RandomAccessIterator b = p1;
  --b;
  while (p2(a, b)) {
    *p1 = 0;
    p1 = b;
    --b;
  }
}
template <typename _RandomAccessIterator, typename _Compare>
void __insertion_sort(_RandomAccessIterator, _Compare p2) {
  for (_RandomAccessIterator c;; ++c)
    __unguarded_linear_insert(c, __val_comp_iter(p2));
}
template <typename _RandomAccessIterator, typename _Distance, typename _Compare>
void __chunk_insertion_sort(_RandomAccessIterator, _Distance, _Compare p3) {
  _RandomAccessIterator d;
  __insertion_sort(d, p3);
}
template <typename _RandomAccessIterator, typename _Pointer, typename _Compare>
void __merge_sort_with_buffer(_RandomAccessIterator p1, _Pointer, _Compare p3) {
  __chunk_insertion_sort(p1, 0, p3);
}
template <typename _RandomAccessIterator, typename _Pointer, typename _Distance,
          typename _Compare>
void __stable_sort_adaptive(_RandomAccessIterator, _Pointer, _Distance,
                            _Compare p4) {
  _RandomAccessIterator e;
  __merge_sort_with_buffer(e, 0, p4);
}
template <typename _RandomAccessIterator, typename _Compare>
void __stable_sort(_RandomAccessIterator p1, _Compare p2) {
  __stable_sort_adaptive(
      p1, 0, typename A<_RandomAccessIterator>::difference_type(), p2);
}
template <typename _RandomAccessIterator, typename _Compare>
void stable_sort(_RandomAccessIterator, _RandomAccessIterator p2, _Compare) {
  B<_Compare> f;
  __stable_sort(p2, f);
}
class D {
public:
  void m_fn1();
};
class F {
  struct G {
    D MFI;
    int operator()(int p1, int p2) {
      if (p1)
        return 0;
      if (p2)
        return 1;
      MFI.m_fn1();
    }
  };
  void m_fn1(int &p1) const;
};
void F::m_fn1(int &p1) const {
  int *g, *h;
  stable_sort(h, g, G());
}

