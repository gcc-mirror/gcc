// { dg-do compile }
// { dg-options "-std=c++11 -O2 -fnon-call-exceptions" }

template < typename > class allocator;

template < class _CharT > struct char_traits;
template < typename _CharT, typename _Traits = char_traits < _CharT >,
  typename _Alloc = allocator < _CharT > >class basic_string;
typedef basic_string < char >string;

template < typename _Tp > class new_allocator
{
  template < typename _Tp1 > struct rebind
  {
    typedef new_allocator < _Tp1 > other;
  };
};

template < typename _Tp > using __allocator_base = new_allocator < _Tp >;
template < typename _Tp > class allocator:public __allocator_base < _Tp >
{
};

template < typename _CharT, typename _Traits, typename _Alloc >
  class basic_string
{
public:
  basic_string (const _CharT * __s, const _Alloc & __a = _Alloc ());
  ~basic_string ()noexcept;
};

template < typename T > struct add_reference
{
  typedef T & type;
};

template < typename ... Values > class tuple;
template <> class tuple <>
{
};

template < typename Head, typename ... Tail > class tuple < Head, Tail ... >:private tuple <
  Tail ...
  >
{
  typedef tuple < Tail ... >inherited;
public:
  template < typename ... VValues >
    tuple (const tuple < VValues ... >&other):inherited (other.tail ()),
    m_head (other.head ())
  {
  }
  typename add_reference < const Head >::type head () const
  {
    return m_head;
  }
  const inherited & tail () const
  {
    return *this;
  }
  Head m_head;
};

template < typename T > struct make_tuple_result
{
  typedef T type;
};

template < typename ... Values >
  tuple < typename make_tuple_result <
  Values >::type ... >make_tuple (const Values & ... values);

int
main ()
{
  tuple < int, float, string > t3c =
    make_tuple (17, 2.718281828, string ("Fun"));
}
