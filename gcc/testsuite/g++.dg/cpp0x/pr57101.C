// { dg-options "-std=c++11 -fcompare-debug" }

typedef long unsigned size_t;
namespace
{
  template < typename _Tp, _Tp __v > struct integral_constant
  {
    static const _Tp value = __v;
  };
  typedef integral_constant < bool, false > false_type;
  template < typename > struct remove_cv;
  template < typename > struct __is_void_helper:false_type
  {
  };
  template
    <
    typename
    _Tp
    >
    struct
    is_void:integral_constant
    < bool, (__is_void_helper < typename remove_cv < _Tp >::type >::value) >
  {
  };
  template < typename > struct is_function:false_type
  {
  };
  template < typename _Tp > struct remove_const
  {
    typedef _Tp type;
  };
  template < typename _Tp > struct remove_volatile
  {
    typedef _Tp type;
  };
  template < typename _Tp > struct remove_cv
  {
    typedef
      typename
      remove_const < typename remove_volatile < _Tp >::type >::type type;
  };
  template < typename > struct is_lvalue_reference:false_type
  {
  };
  template < typename _Tp, bool = is_void < _Tp >::value > struct __add_rvalue_reference_helper
  {
    typedef _Tp type;
  };
  template
    <
    typename
    _Tp > struct add_rvalue_reference:__add_rvalue_reference_helper < _Tp >
  {
  };
  template
    < typename _Tp > typename add_rvalue_reference < _Tp >::type declval ();
  template
    <
    typename,
    typename
    _To, bool = (is_function < _To >::value) > struct __is_convertible_helper;
  template
    <
    typename
    _From, typename _To > struct __is_convertible_helper <_From, _To, false >
  {
    static const bool __value = sizeof ((declval < _From > ()));
  };
  template
    <
    typename
    _From,
    typename
    _To
    >
    struct
    is_convertible:integral_constant
    < bool, __is_convertible_helper < _From, _To >::__value >
  {
  };
  template < bool, typename _Tp = void >struct enable_if
  {
    typedef _Tp type;
  };
  template < typename _Tp > struct identity
  {
    typedef _Tp type;
  };
  template
    <
    typename
    _Tp
    >
    typename
    enable_if
    <
    is_lvalue_reference
    < _Tp >::value, _Tp >::type forward (typename identity < _Tp >::type)
  {
    return 0;

  }
  template < class _T1, class > struct pair
  {
    _T1 first;
    template < class _U1, class = typename enable_if < is_convertible < _U1, _T1 >::value >::type > pair (_U1 __x):
    first
      (forward < _U1 > (__x))
    {
    }
  };
}

namespace __gnu_cxx
{
  template < typename > class new_allocator
  {
  };
}

namespace std
{
  template < typename _Tp > class allocator:__gnu_cxx::new_allocator < _Tp >
  {
  public:
    template < typename > struct rebind
    {
      typedef allocator other;
    };
  };
  template < typename, typename > struct unary_function;
  template < typename, typename, typename > struct binary_function
  {
  };
  template < typename _Tp > struct less:binary_function < _Tp, _Tp, bool >
  {
  };
  template
    <
    typename
    _Pair
    > struct _Select1st:unary_function < _Pair, typename _Pair::first_type >
  {
  };
  template < typename > struct _Rb_tree_node;
  template
    <
    typename,
    typename
    _Val,
    typename,
    typename _Compare, typename _Alloc = allocator < _Val > >class _Rb_tree
  {
    typedef
      typename
      _Alloc::template
      rebind < _Rb_tree_node < _Val > >::other _Node_allocator;
  public:
    typedef _Alloc allocator_type;
    template < typename _Key_compare > struct _Rb_tree_impl
    {
      _Rb_tree_impl (_Key_compare, _Node_allocator);
    };
    _Rb_tree_impl < _Compare > _M_impl;
  _Rb_tree (_Compare __comp, allocator_type __a):
    _M_impl (__comp, __a)
    {
    }
  };
  template < class _E > class initializer_list
  {
    typedef size_t size_type;
    typedef _E *iterator;
    iterator _M_array;
    size_type _M_len;
  };
  template
    <
    typename
    _Key,
    typename
    _Tp,
    typename
    _Compare
    =
    less
    <
    _Key >, typename _Alloc = allocator < pair < _Key, _Tp > > >class multimap
  {
    typedef _Key key_type;
    typedef pair < _Key, _Tp > value_type;
    typedef _Compare key_compare;
    typedef _Alloc allocator_type;
    typedef
      _Rb_tree
      <
      key_type,
      value_type, _Select1st < value_type >, key_compare > _Rep_type;
    _Rep_type _M_t;
  public:
  multimap (initializer_list < value_type >, _Compare __comp = _Compare (), allocator_type __a = allocator_type ()):
    _M_t
      (__comp, __a)
    {
    }
  };
}

using namespace std;
void
test01 ()
{
  typedef multimap < int, double >Container;
  Container (
	      {
	      {
	      1}
	      }
  );
}
