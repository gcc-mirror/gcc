// PR c++/49181
// { dg-do compile { target c++11 } }

namespace std
{
  typedef __SIZE_TYPE__ size_t;

  template<typename _Tp, _Tp>
    struct integral_constant;

  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp value = __v;
      typedef _Tp value_type;
      typedef integral_constant<_Tp, __v> type;
      constexpr operator value_type() { return value; }
    };

  typedef integral_constant<bool, true> true_type;

  typedef integral_constant<bool, false> false_type;

  template<typename _Tp, _Tp __v>
    constexpr _Tp integral_constant<_Tp, __v>::value;

  template<bool, typename _Tp = void>
    struct enable_if
    { };

  template<typename _Tp>
    struct enable_if<true, _Tp>
    { typedef _Tp type; };

  template<typename _Tp>
    inline _Tp
    declval();

struct bad_alloc { };
}

void* operator new(std::size_t)
#if __cplusplus <= 201402L
throw (std::bad_alloc)			// { dg-warning "deprecated" "" { target { ! c++17 } } }
#endif
;

namespace std
{

  template<typename _Tp>
    class allocator
    {
    public:
      typedef _Tp* pointer;
      typedef _Tp value_type;

      pointer
      allocate(size_t, const void* = 0);
    };

  template<typename _Alloc>
    struct allocator_traits
    {
      typedef typename _Alloc::value_type value_type;

      template<typename _Tp> static typename _Tp::pointer
_S_pointer_helper(_Tp*);
      static value_type* _S_pointer_helper(...);
      typedef decltype(_S_pointer_helper((_Alloc*)0)) __pointer;

      typedef __pointer pointer;

      typedef const void* const_void_pointer;

      private:
      template<typename _Alloc2>
    struct __allocate_helper
    {
      template<typename _Alloc3,
        typename = decltype(std::declval<_Alloc3*>()->allocate(
          std::declval<size_t>(),
          std::declval<const_void_pointer>()))>
          static true_type __test(int);

      template<typename>
        static false_type __test(...);

      typedef decltype(__test<_Alloc>(0)) type;
      static const bool value = type::value;
    };

      template<typename _Alloc2>
    static typename
    enable_if<__allocate_helper<_Alloc2>::value, pointer>::type
    _S_allocate(_Alloc2& __a, size_t __n, const_void_pointer __hint)
    { return __a.allocate(__n, __hint); }

      public:
      static pointer
    allocate(_Alloc& __a, size_t __n, const_void_pointer __hint)
    { return _S_allocate(__a, __n, __hint); }
    };

}

namespace std
{
  typedef short test_type;
  template struct allocator_traits<allocator<test_type>>;
}
