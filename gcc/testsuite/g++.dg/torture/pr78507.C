// PR middle-end/78507
// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

struct A {
  template <typename _Iterator1, typename _Iterator2>
  int operator()(_Iterator1, _Iterator2);
};
struct B {
  template <typename _BI1, typename _BI2>
  static _BI2 __copy_move_b(_BI1 p1, _BI2 p2) {
    _BI1 a;
    long b = p1 - a;
    for (; b > 0; --b)
      *--p2 = *--p1;
  }
};
template <int, typename _BI1, typename _BI2>
void __copy_move_backward_a(_BI1 p1, _BI2 p2) {
  B::__copy_move_b(p1, p2);
}
template <int, typename _BI1, typename _BI2>
void __copy_move_backward_a2(_BI1 p1, _BI2 p2) {
  __copy_move_backward_a<0>(p1, p2);
}
template <typename _BI1, typename _BI2> void move_backward(_BI1 p1, _BI2 p2) {
  __copy_move_backward_a2<0>(p1, p2);
}
template <typename _RandomAccessIterator, typename _Compare>
void __insertion_sort(_RandomAccessIterator, _Compare p2) {
  for (_RandomAccessIterator c;; ++c)
    if (p2(0, 0))
      move_backward(c, c + 1);
}
template <typename _RandomAccessIterator, typename _Compare>
void __final_insertion_sort(_RandomAccessIterator, _Compare p2) {
  _RandomAccessIterator d;
  __insertion_sort(d, p2);
}
template <typename _RandomAccessIterator, typename _Compare>
void __sort(_RandomAccessIterator p1, _Compare p2) {
  __final_insertion_sort(p1, p2);
}
template <typename _RandomAccessIterator, typename _Compare>
void sort(_RandomAccessIterator, _RandomAccessIterator p2, _Compare) {
  A e;
  __sort(p2, e);
}
struct C {
  struct D {
    int DwarfRegNum;
  };
  int parseRegisterLiveOutMask() const;
};
int C::parseRegisterLiveOutMask() const {
  D f, g;
  sort(&f, &g, [] {});
}

