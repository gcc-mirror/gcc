// PR c++/47416
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-return-type" }

namespace std
{
  template < typename _Tp, _Tp __v > struct integral_constant
  {
    static const _Tp value = __v;
  };
  typedef integral_constant < bool, false > false_type;
    template < typename > struct is_array:false_type
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
    typedef typename remove_const < typename remove_volatile <
      _Tp >::type >::type type;
  };
    template < typename > struct remove_reference
  {
  };
    template < typename _Tp > struct remove_reference <_Tp & >
  {
    typedef _Tp type;
  };
    template < typename _Up, bool = is_array < _Up >::value, bool =
    is_function < _Up >::value > struct __decay_selector;
    template < typename _Up > struct __decay_selector <_Up, false, false >
  {
    typedef typename remove_cv < _Up >::type __type;
  };
    template < typename _Tp > class decay
  {
    typedef typename remove_reference < _Tp >::type __remove_type;
  public:typedef typename __decay_selector <
      __remove_type >::__type type;
  };
  template < typename _Tp > struct __strip_reference_wrapper
  {
    typedef _Tp __type;
  };
  template < typename _Tp > struct __decay_and_strip
  {
    typedef typename __strip_reference_wrapper < typename decay <
      _Tp >::type >::__type __type;
  };
  template < typename _Tp > _Tp forward (typename remove_reference <
					 _Tp >::type &)
  {
  }
  template < class _T1, class _T2 > struct pair
  {
    _T1 first;
    _T2 second;
    constexpr pair (_T1, _T2 &):first (), second (__b)	// { dg-error "was not declared in this scope" }
    {
    }
  };
  template < class _T1,
    class _T2 > pair < typename __decay_and_strip < _T1 >::__type,
    typename __decay_and_strip < _T2 >::__type > make_pair (_T1 && __x, _T2
							    && __y)
  {
    typedef typename __decay_and_strip < _T1 >::__type __ds_type1;
    typedef typename __decay_and_strip < _T2 >::__type __ds_type2;
    typedef pair < __ds_type1, __ds_type2 > __pair_type;
    __pair_type (forward < _T1 > (__x), std::forward < _T2 > (__y));
  }
}

typedef long size_t;
namespace std
{
  template < typename > class allocator;
  template < class > struct char_traits;
    template < typename _CharT, typename = char_traits < _CharT >, typename =
    allocator < _CharT > >class basic_string;
  typedef basic_string < char >string;
}
namespace __gnu_cxx
{
  template < bool > class __pool;
  template < template < bool > class, bool > struct __common_pool
  {
  };
    template < template < bool > class, bool > struct __common_pool_base;
    template < template < bool > class _PoolTp >
    struct __common_pool_base <_PoolTp, true >:__common_pool < _PoolTp, true >
  {
  };
    template < template < bool > class _PoolTp,
    bool _Thread > struct __common_pool_policy:__common_pool_base < _PoolTp,
    _Thread >
  {
    template < typename, template < bool > class _PoolTp1 =
      _PoolTp, bool _Thread1 = _Thread > struct _M_rebind
    {
      typedef __common_pool_policy < _PoolTp1, _Thread1 > other;
    };
  };
    template < typename _Tp > class __mt_alloc_base
  {
  };
template < typename _Tp, typename _Poolp = __common_pool_policy < __pool, true > >class __mt_alloc:public __mt_alloc_base <
    _Tp
    >
  {
  public:size_t size_type;
    typedef _Tp value_type;
    template < typename _Tp1, typename _Poolp1 = _Poolp > struct rebind
    {
      typedef typename _Poolp1::template _M_rebind < _Tp1 >::other pol_type;
      typedef __mt_alloc < _Tp1, pol_type > other;
    };
  };
}

namespace std
{
  template < typename _Tp > class allocator:public __gnu_cxx::__mt_alloc <
    _Tp >
  {
  };
  template < typename, typename > struct unary_function
  {
  };
  template < typename, typename, typename > struct binary_function
  {
  };
  template < typename _Tp > struct equal_to:binary_function < _Tp, _Tp, bool >
  {
  };
}

namespace boost
{
  template < class > struct hash;
    template < class K, class T, class = hash < K >, class =
    std::equal_to < K >, class =
    std::allocator < std::pair < const K, T > >>class unordered_map;
    template < >struct hash <std::string >:std::unary_function < std::string,
    size_t >
  {
  };
  namespace unordered_detail
  {
    template < class Alloc, class T > struct rebind_wrap
    {
      typedef typename Alloc::template rebind < T >::other type;
    };
  }
  namespace unordered_detail
  {
    size_t default_bucket_count;
      template < class, class > struct map_extractor;
    struct ungrouped
    {
    };
      template < class T > class hash_table:T::buckets, T::buffered_functions
    {
    };
      template < class, class, class H, class P, class A, class, class G > struct types
    {
      typedef H hasher;
      typedef P key_equal;
      typedef A value_allocator;
    };
      template < class T > class hash_unique_table:T
    {
    public:typedef typename T::hasher hasher;
      typedef typename T::key_equal key_equal;
      typedef typename T::value_allocator value_allocator;
      typedef typename T::table table;
        hash_unique_table (size_t n, hasher, key_equal,
			   value_allocator & a):table (n, a)	// { dg-error "is not a direct base" }
      {
      }
    };
    template < class K, class H, class P, class A > struct map:types < K,
      typename A::value_type, H, P, A, map_extractor < K,
      typename A::value_type >, ungrouped >
    {
      typedef hash_unique_table < map < K, H, P, A > >impl;
      typedef hash_table < map < K, H, P, A > >table;
    };
  }
  template < class K, class T, class H, class P, class A > class unordered_map
  {
    typedef std::pair < const K, T > value_type;
    typedef H hasher;
    typedef P key_equal;
    typedef A allocator_type;
    typedef typename unordered_detail::rebind_wrap < allocator_type,
      value_type >::type value_allocator;
    typedef boost::unordered_detail::map < K, H, P, value_allocator > types;
    typedef typename types::impl table;
    typedef size_t size_type;
  private:table table_;
  public: unordered_map (size_type n = boost::unordered_detail::default_bucket_count,
			 hasher hf = hasher (), key_equal eql = key_equal (),
			 allocator_type a = allocator_type ()):table_ (n, hf, eql, a)	// { dg-message "required" }
    {
    }
  };
}

void
foo (const int &a)
{
  typedef boost::unordered_map < std::string, int >Name2Port;
  Name2Port b;			// { dg-message "required" }
  std::make_pair (a, b);
}
