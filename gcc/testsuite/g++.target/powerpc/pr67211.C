/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -mdejagnu-tune=power8 -O3 -w" } */

/* target/67211, compiler got a 'insn does not satisfy its constraints' error.  */

template <typename _InputIterator, typename _ForwardIterator>
void find_first_of(_InputIterator, _InputIterator, _ForwardIterator p3,
                   _ForwardIterator p4) {
  for (; p3 != p4; ++p3)
    ;
}

template <typename, typename, typename> struct A {
  int _S_buffer_size;
  int *_M_cur;
  int *_M_first;
  int *_M_last;
  int **_M_node;
  void operator++() {
    if (_M_cur == _M_last)
      m_fn1(_M_node + 1);
  }
  void m_fn1(int **p1) {
    _M_node = p1;
    _M_first = *p1;
    _M_last = _M_first + _S_buffer_size;
  }
};

template <typename _Tp, typename _Ref, typename _Ptr>
bool operator==(A<_Tp, _Ref, _Ptr>, A<_Tp, _Ref, _Ptr>);
template <typename _Tp, typename _Ref, typename _Ptr>
bool operator!=(A<_Tp, _Ref, _Ptr> p1, A<_Tp, _Ref, _Ptr> p2) {
  return p1 == p2;
}

class B {
public:
  A<int, int, int> m_fn2();
};
struct {
  B j;
} a;
void Linked() {
  A<int, int, int> b, c, d;
  find_first_of(d, c, b, a.j.m_fn2());
}
