// PR middle-end/71281
// { dg-do compile }
// { dg-additional-options "-std=c++11 -Ofast" }


template <typename> struct A;
template <typename _Tp> struct A<_Tp *> { typedef _Tp reference; };

template <typename _Iterator> class B {
public:
  typename A<_Iterator>::reference operator*();
};

template <typename> class C;
template <typename> struct D;

template <typename _Tp> struct D<C<_Tp>> {
    using value_type = _Tp;
    using const_pointer = _Tp *;
    template <typename _Up> using rebind_alloc = C<_Up>;
};

template <typename _Alloc> struct __alloc_traits : D<_Alloc> {
    typedef D<_Alloc> _Base_type;
    typedef typename _Base_type::value_type &reference;
    template <typename _Tp> struct F {
	typedef typename _Base_type::template rebind_alloc<_Tp> other;
    };
};

template <typename _Tp, typename _Alloc> struct G {
    typedef typename __alloc_traits<_Alloc>::template F<_Tp>::other
      _Tp_alloc_type;
};

int a, b;
long d[1][1][1];
void fn1() __attribute__((__noreturn__));
template <typename _Tp, typename _Alloc = C<_Tp>> class H {
    typedef __alloc_traits<typename G<_Tp, _Alloc>::_Tp_alloc_type> _Alloc_traits;
    typedef typename _Alloc_traits::reference reference;

public:
    B<typename _Alloc_traits::const_pointer> m_fn1();
    long m_fn2();
    reference operator[](unsigned);
    reference m_fn3(unsigned){
	if (m_fn2())
	  fn1();
    }	// { dg-warning "control reaches end of non-void function" }
};

H<H<H<unsigned>>> c;
void fn2() {
    H<unsigned, C<int>> e;
    for (int f = 1;;)
      for (int g = 0;;)
	for (int h = 0;;)
	  {
	    *d[0][h] =
	      c.m_fn3(f)[0][g] * a + -*(e).m_fn1() * b + (*c[f].m_fn1()).m_fn3(g);
	  }
}
