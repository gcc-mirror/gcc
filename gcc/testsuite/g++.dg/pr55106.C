/* { dg-do compile } */
/* { dg-options "-c -O3" } */
template<typename _Tp> struct A {
  typedef _Tp *pointer;
  typedef _Tp& reference;
  typedef _Tp& const_reference;
  template<typename>struct rebind
  {
    typedef A other;
  };
};

template<typename _Alloc>struct __alloc_traits
{
  typedef typename _Alloc::pointer         pointer;
  typedef typename _Alloc::reference       reference;
  typedef typename _Alloc::const_reference const_reference;
  template<typename _Tp>struct rebind
  {
    typedef typename _Alloc::template rebind<_Tp>::other other;
  };
};
template<typename _Tp, typename _Alloc>struct B
{
  typedef typename __alloc_traits<_Alloc>::template rebind<
      _Tp>::other _Tp_alloc_type;
  typedef typename __alloc_traits<_Tp_alloc_type>::pointer pointer;
  struct F
  {
    pointer _M_start;
  };
  F _M_impl;
};
template<typename _Tp, typename _Alloc = A<_Tp> >class vec : B<_Tp, _Alloc>{
  typedef B<_Tp, _Alloc>                 _Base;
  typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  typedef __alloc_traits<_Tp_alloc_type> _Alloc_traits;

public:
  typedef _Tp                                     value_type;
  typedef typename _Alloc_traits::reference       reference;
  typedef typename _Alloc_traits::const_reference const_reference;
  reference operator[](int p1)
  {
    return *(this->_M_impl._M_start + p1);
  }

  const_reference operator[](long) const;
};

int a[17];
class C {
  vec<int> m_value;
  void opModDivGuts(const C&);
  int mostSetBitP1() const;
};
void C::opModDivGuts(const C& p1)
{
  int b = p1.mostSetBitP1(), c = b + 1;
  int d[16];

  for (int i = c; i; i--)
    a[i] = p1.m_value[i] << b;

  for (int i = 0; i < c; i++)
    m_value[i] = d[i] >> b << -b;
}
