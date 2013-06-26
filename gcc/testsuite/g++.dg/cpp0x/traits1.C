// PR c++/57016
// { dg-require-effective-target c++11 }

template < typename _Tp, _Tp __v > struct integral_constant
{
  static constexpr _Tp value = __v;
};
template < bool, typename, typename > struct conditional;
template < typename ... >struct __and_;
template
  <
  typename
  _B1,
  typename
  _B2 > struct __and_ <_B1, _B2 >:conditional < _B1::value, _B2, _B1 >::type
{};
template < typename _Pp > struct __not_:integral_constant < bool, _Pp::value >
{};
template < typename > struct add_rvalue_reference;
template
  < typename _Tp > typename add_rvalue_reference < _Tp >::type declval ();
template < bool, typename _Iftrue, typename > struct conditional
{
  typedef _Iftrue type;
};
template < class, class > struct pair;
template < typename > class allocator;
template < typename, typename, typename > struct binary_function;
template < typename _Tp > struct equal_to:binary_function < _Tp, _Tp, bool >
{};
template < typename > struct hash;
template < >struct hash <int >
{};
template
  <
  typename,
  typename,
  typename,
  typename, typename, typename, typename, typename > struct _Hashtable_base;
template
  <
  typename,
  typename
  > struct __is_noexcept_hash:integral_constant < bool, noexcept ((declval)) >
{}
;
struct _Identity;
template < bool, bool _Constant_iterators, bool > struct _Hashtable_traits
 ;
struct _Mod_range_hashing;
struct _Default_ranged_hash;
struct _Prime_rehash_policy;
template
  <
  typename
  _Tp,
  typename
  _Hash
  >
  using
  __cache_default
  =
  __not_
  <
  __and_
  <
  integral_constant
  < bool, __is_final (_Hash) >, __is_noexcept_hash < _Tp, _Hash > >>;
template < typename _Key, typename _Value, typename, typename _ExtractKey, typename _Equal, typename _H1, typename _H2, typename, typename _RehashPolicy, typename _Traits > class _Hashtable:
_Hashtable_base
  < _Key, _Value, _ExtractKey, _Equal, _H1, _H2, _RehashPolicy, _Traits >
{}
;
template
  <
  bool
  _Cache > using __uset_traits = _Hashtable_traits < _Cache, true, true >;
template
  <
  typename
  _Value,
  typename
  _Hash
  =
  hash
  <
  _Value
  >,
  typename
  _Pred
  =
  equal_to
  <
  _Value
  >,
  typename
  _Alloc
  =
  allocator
  <
  _Value
  >,
  typename
  _Tr
  =
  __uset_traits
  <
  __cache_default
  <
  _Value,
  _Hash
  >::value
  >
  >
  using
  __uset_hashtable
  =
  _Hashtable
  <
  _Value,
  _Value,
  _Alloc,
  _Identity,
  _Pred,
  _Hash,
  _Mod_range_hashing, _Default_ranged_hash, _Prime_rehash_policy, _Tr >;
template < class _Value, class = hash < _Value > >class unordered_set
{
  typedef __uset_hashtable < _Value > iterator;
  template < typename > pair < iterator, bool > emplace ();
}
;
template class unordered_set < int >;
