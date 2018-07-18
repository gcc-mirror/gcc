// PR c++/41727

struct tag0;

template < class Tag > struct outer
{
  template < typename Arg0, typename Arg1 > struct inner;
};

template < int Value > struct value_wrap { };

template </* class Tag */>
template < typename Arg0, int Arg1 >
struct outer <tag0 >::inner < Arg0, value_wrap < Arg1 > >
{
  typedef Arg0 type;
};

typedef outer < tag0 >
::inner < tag0, value_wrap < 999 > >
::type				// { dg-bogus "incomplete" "" { xfail *-*-* } }
  outer_inner_type;
