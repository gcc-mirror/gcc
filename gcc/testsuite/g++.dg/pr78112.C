/* { dg-do compile } */
/* { dg-options "-g -dA -std=gnu++11" } */
namespace std
{
template <typename _Tp> struct integral_constant
{
  static constexpr _Tp value = 0;
};
template <int> using __bool_constant = integral_constant<int>;
struct __not_ : integral_constant<int>
{
};
template <typename, typename, template <typename...> class _Op,
          typename... _Args>
struct __detector
{
  using type = _Op<_Args...>;
};
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or = __detector<_Default, void, _Op, _Args...>;
template <typename _Default, template <typename...> class _Op,
          typename... _Args>
using __detected_or_t = typename __detected_or<_Default, _Op, _Args...>::type;
template <template <typename...> class _Default,
          template <typename...> class _Op, typename... _Args>
using __detected_or_t_ = __detected_or_t<_Default<_Args...>, _Op, _Args...>;
template <typename, typename> struct pair;
template <typename, typename> using __replace_first_arg_t = int;
}
class new_allocator
{
};
namespace std
{
template <typename> using __allocator_base = new_allocator;
template <typename> class allocator : __allocator_base<int>
{
};
template <typename> struct equal_to;
struct __allocator_traits_base
{
  template <typename _Alloc, typename _Up>
  using __rebind = typename _Alloc::template rebind<_Up>::other;
};
template <typename _Alloc, typename _Up>
using __alloc_rebind
    = __detected_or_t_<__replace_first_arg_t,
                       __allocator_traits_base::__rebind, _Alloc, _Up>;
}
struct __alloc_traits
{
  static int _S_select_on_copy;
};
namespace std
{
template <typename> struct hash;
namespace __detail
{
struct _Select1st;
struct _Hashtable_traits
{
  using __hash_cached = __bool_constant<0>;
};
template <typename, int> struct _Hash_node;
template <typename _Value> struct _Hash_node<_Value, 0>
{
  int &_M_v ();
};
struct _Mod_range_hashing;
struct _Default_ranged_hash;
struct _Prime_rehash_policy
{
  using __has_load_factor = integral_constant<int>;
};
struct _Map_base
{
};
struct _Insert
{
};
template <typename _Policy>
using __has_load_factor = typename _Policy::__has_load_factor;
template <typename _RehashPolicy, typename,
          typename = __detected_or_t<integral_constant<bool>,
                                     __has_load_factor, _RehashPolicy> >
struct _Rehash_base;
template <typename _RehashPolicy, typename _Traits>
struct _Rehash_base<_RehashPolicy, _Traits, integral_constant<int> > {};
struct _Hashtable_ebo_helper { template <typename _OtherTp> _Hashtable_ebo_helper (_OtherTp); };
struct _Hashtable_base {};
struct _Equality {};
template <typename _NodeAlloc> struct _Hashtable_alloc : _Hashtable_ebo_helper
{
  using __ebo_node_alloc = _Hashtable_ebo_helper;
  using __node_type = typename _NodeAlloc::value_type;
  using __node_alloc_traits = __alloc_traits;
  template <typename _Alloc> _Hashtable_alloc (_Alloc) : __ebo_node_alloc (0) {}
  template <typename... _Args> __node_type *_M_allocate_node (...);
};
}
template <typename, typename> using __cache_default = __not_;
template <typename _Key, typename _Value, typename _Alloc, typename _ExtractKey, typename _Equal, typename _H1, typename _H2,
          typename _Hash, typename _RehashPolicy, typename _Traits>
struct _Hashtable : __detail::_Hashtable_base, __detail::_Map_base, __detail::_Insert, __detail::_Rehash_base<_RehashPolicy, int>,
  __detail::_Equality, __detail:: _Hashtable_alloc<__alloc_rebind<_Alloc, __detail::_Hash_node<_Value, _Traits::__hash_cached::value> > >
{
  using __traits_type = _Traits;
  using __hash_cached = typename __traits_type::__hash_cached;
  using __node_type = __detail::_Hash_node<_Value, __hash_cached::value>;
  using __node_alloc_type = __alloc_rebind<_Alloc, __node_type>;
  using __hashtable_alloc = __detail::_Hashtable_alloc<__node_alloc_type>;
  using typename __hashtable_alloc::__node_alloc_traits;
  _Key key_type;
  _RehashPolicy _M_rehash_policy;
  template <typename _NodeGenerator>
  void _M_assign (const _Hashtable &, const _NodeGenerator &);
  _Hashtable ();
  _Hashtable (const _Hashtable &);
};
template <typename _Key, typename _Value, typename _Alloc, typename _ExtractKey, typename _Equal, typename _H1, typename _H2,
          typename _Hash, typename _RehashPolicy, typename _Traits>
template <typename _NodeGenerator>
void _Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal, _H1, _H2, _Hash, _RehashPolicy,
                _Traits>::_M_assign (const _Hashtable &__ht, const _NodeGenerator &__node_gen) try {
__node_type *__this_n = __node_gen (0); } catch (...) {}
template <typename _Key, typename _Value, typename _Alloc, typename _ExtractKey, typename _Equal, typename _H1, typename _H2,
          typename _Hash, typename _RehashPolicy, typename _Traits>
_Hashtable<_Key, _Value, _Alloc, _ExtractKey, _Equal, _H1, _H2, _Hash, _RehashPolicy, _Traits>::_Hashtable (const _Hashtable &__ht)
    : __hashtable_alloc (__node_alloc_traits::_S_select_on_copy), _M_rehash_policy ()
{
  _M_assign (__ht, [this](__node_type *__n) { return this->_M_allocate_node (__n->_M_v ()); });
}
template <bool _Cache> using __umap_traits = __detail::_Hashtable_traits;
template <typename _Key, typename _Tp, typename _Hash = hash<_Key>, typename _Pred = std::equal_to<_Key>,
          typename _Alloc = std::allocator<std::pair<const _Key, _Tp> >,
          typename _Tr = __umap_traits<__cache_default<_Key, _Hash>::value> >
using __umap_hashtable = _Hashtable<_Key, std::pair<const _Key, _Tp>, _Alloc, __detail::_Select1st, _Pred, _Hash,
                                    __detail::_Mod_range_hashing, __detail::_Default_ranged_hash, __detail::_Prime_rehash_policy, _Tr>;
template <class _Key, class _Tp, class _Hash = hash<_Key>, class _Pred = std::equal_to<_Key>,
          class _Alloc = std::allocator<std::pair<const _Key, _Tp> > >
struct unordered_map
{
  typedef __umap_hashtable<_Key, _Tp, _Hash, _Pred, _Alloc> _Hashtable;
  _Hashtable _M_h;
  unordered_map (const int &__a) : _M_h () {}
};
}
struct uneq_allocator_base {};
template <typename Tp> struct uneq_allocator : uneq_allocator_base { typedef Tp value_type; };
template <typename Tp, typename Alloc = std::allocator<Tp> >
struct Trans_NS___gnu_test_propagating_allocator : public uneq_allocator<Tp>
{
  template <typename Up> struct rebind { typedef Trans_NS___gnu_test_propagating_allocator<Up, int> other; };
};
void fn1 ()
{
  std::unordered_map<int, int, int, int, Trans_NS___gnu_test_propagating_allocator<int> > v1 (0);
  std::unordered_map<int, int, int, int, Trans_NS___gnu_test_propagating_allocator<int> > v2 (v1);
}
