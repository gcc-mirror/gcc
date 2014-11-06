// PR ipa/63587
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-strict-aliasing" }

template <class> struct A
{
};
template <typename> struct B
{
  template <typename> struct C;
};
class D;
template <typename> class F;
struct G
{
  void operator()(const D &, D);
};
class D
{
public:
  D (int);
};
struct H
{
  H (int);
};
template <typename _Key, typename, typename, typename _Compare, typename>
class I
{
  typedef _Key key_type;
  template <typename _Key_compare> struct J
  {
    _Key_compare _M_key_compare;
  };
  J<_Compare> _M_impl;

public:
  A<int> _M_get_insert_unique_pos (const key_type &);
  A<int> _M_get_insert_hint_unique_pos (H &);
  template <typename... _Args> int _M_emplace_hint_unique (H, _Args &&...);
};
template <typename _Key, typename _Tp, typename _Compare = G,
	  typename _Alloc = F<A<_Tp> > >
class K
{
  typedef _Key key_type;
  typedef _Key value_type;
  typedef typename B<_Alloc>::template C<value_type> _Pair_alloc_type;
  I<key_type, value_type, int, _Compare, _Pair_alloc_type> _M_t;

public:
  void operator[](key_type)
  {
    _M_t._M_emplace_hint_unique (0);
  }
};
template <typename _Key, typename _Val, typename _KeyOfValue,
	  typename _Compare, typename _Alloc>
A<int>
I<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::_M_get_insert_unique_pos (
  const key_type &p1)
{
  _M_impl._M_key_compare (p1, 0);
}
template <typename _Key, typename _Val, typename _KeyOfValue,
	  typename _Compare, typename _Alloc>
A<int>
I<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::_M_get_insert_hint_unique_pos (
  H &)
{
  _M_get_insert_unique_pos (0);
}
template <typename _Key, typename _Val, typename _KeyOfValue,
	  typename _Compare, typename _Alloc>
template <typename... _Args>
int
I<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::_M_emplace_hint_unique (
  H p1, _Args &&...)
{
  _M_get_insert_hint_unique_pos (p1);
}
namespace {
struct L;
}
void
fn1 ()
{
  K<D, L> a;
  a[0];
  K<D, int> b;
  b[0];
}
