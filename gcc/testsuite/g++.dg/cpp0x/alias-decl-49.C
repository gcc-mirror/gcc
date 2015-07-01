// PR c++/66647
// { dg-do compile { target c++11 } }

template <typename _Tp> struct A
{
  static constexpr _Tp value = 1;
};
template <typename> class B
{
public:
  template <typename> struct rebind
  {
  };
};

template <typename _Alloc, typename _Tp> class C
{
  template <typename _Alloc2, typename _Tp2>
  static A<int> _S_chk (typename _Alloc2::template rebind<_Tp2> *);

public:
  using __type = decltype (_S_chk<_Alloc, _Tp> (0));
};

template <typename _Alloc, typename _Tp, int = C<_Alloc, _Tp>::__type::value>
struct D;
template <typename _Alloc, typename _Tp> struct D<_Alloc, _Tp, 1>
{
  typedef typename _Alloc::template rebind<_Tp> __type;
};
template <typename _Alloc> struct F
{
  template <typename _Tp> using rebind_alloc = typename D<_Alloc, _Tp>::__type;
};
template <typename _Alloc> struct __alloc_traits
{
  template <typename> struct rebind
  {
    typedef typename F<_Alloc>::template rebind_alloc<int> other;
  };
};
template <typename _Alloc> struct G
{
  typename __alloc_traits<_Alloc>::template rebind<int>::other _Tp_alloc_type;
};
template <typename _Tp, typename _Alloc = B<_Tp> > class vector : G<_Alloc>
{
};

template <int> using tfuncptr = void();
template <int d> struct H
{
  vector<tfuncptr<d> > funcs;
};
