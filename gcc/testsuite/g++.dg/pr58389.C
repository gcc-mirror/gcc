/* { dg-do compile } */
/* { dg-options "-O2" } */

template <typename _RandomAccessIterator, typename _Compare>
void __insertion_sort(_RandomAccessIterator, _Compare);
template <typename _RandomAccessIterator, typename _Compare>
void __final_insertion_sort(_RandomAccessIterator p1, _Compare p2) {
  _RandomAccessIterator a;
  if (p1 - a)
    ;
  else
  std:
  __insertion_sort(0, p2);
}
template <typename _RandomAccessIterator, typename _Size, typename _Compare>
void __introsort_loop(_RandomAccessIterator, _Size, _Compare);
template <typename _RandomAccessIterator, typename _Compare>
void sort(_RandomAccessIterator, _RandomAccessIterator p2, _Compare p3) {
std:
  __introsort_loop(0, 0, p3);
  __final_insertion_sort(p2, p3);
}
class A {
public:
  int m_fn1();
  void __lg();
  class B {
  public:
    int i;
    int operator-(B);
  };
};
class C;
class D {
public:
  C *operator->();
};
class F {
  A m_fn1() const;
  D d_ptr;
};
class C {
  friend F;
  void m_fn1();
  A children;
};
void qt_notclosestLeaf();
inline void C::m_fn1() {
  A::B b, c;
  if (children.m_fn1()) {
    sort(c, b, qt_notclosestLeaf);
  }
}
A F::m_fn1() const { const_cast<F *>(this)->d_ptr->m_fn1(); return A(); }
