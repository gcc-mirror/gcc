// { dg-do compile { target c++11 } }

typedef long unsigned int size_t;

template<typename _Tp>
struct remove_reference {};

template<typename _Tp>
constexpr _Tp&&
forward(typename remove_reference<_Tp>::type& __t) noexcept
{
}

struct __allocator_traits_base {
  template<typename _Tp, typename _Up, typename = void>
  struct __rebind
  {
    using type = typename _Tp::template rebind<_Up>::other;
  };
};

template<typename _Alloc, typename _Up>
using __alloc_rebind = typename __allocator_traits_base::template __rebind<_Alloc, _Up>::type;

template<typename _Alloc>  struct allocator_traits {
  template<typename _Tp>  using rebind_alloc = __alloc_rebind<_Alloc, _Tp>;
  template<typename _Tp, typename... _Args>
  static auto construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
    -> decltype(_S_construct(__a, __p, forward<_Args>(__args)...))  {   }
};

template<typename _Alloc, typename = typename _Alloc::value_type>
struct __alloc_traits    : allocator_traits<_Alloc>    {
  typedef allocator_traits<_Alloc> _Base_type;
  template<typename _Tp>       struct rebind       {   typedef typename _Base_type::template rebind_alloc<_Tp> other;   };
};

template<typename _Tp>     class allocator {
  typedef _Tp value_type;
  template<typename _Tp1>  struct rebind  {   typedef allocator<_Tp1> other;   };
};

template<typename _CharT, typename _Alloc>
class basic_string {
  typedef typename __alloc_traits<_Alloc>::template rebind<_CharT>::other _Char_alloc_type;
};

template<size_t _Nw>  struct _Base_bitset {
  static void foo (basic_string<char, allocator<char> >) {}
};
