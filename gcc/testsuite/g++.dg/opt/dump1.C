// PR c++/57102
// { dg-options "-std=gnu++0x -O2 -fno-inline -fdump-final-insns" }
// { dg-final cleanup-saved-temps }

namespace std
{
  typedef __SIZE_TYPE__ size_t;
  typedef __PTRDIFF_TYPE__ ptrdiff_t;
}
extern "C++" {
  void* operator new(std::size_t, void* __p) noexcept;
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp value = __v;
      typedef integral_constant<_Tp, __v> type;
    };
  typedef integral_constant<bool, true> true_type;
  typedef integral_constant<bool, false> false_type;
  template<bool, typename, typename>
    struct conditional;
  template<typename...>
    struct __or_;
  template<typename _B1, typename _B2>
    struct __or_<_B1, _B2>
    : public conditional<_B1::value, _B1, _B2>::type
    {};
  template<typename _B1, typename _B2, typename _B3, typename... _Bn>
    struct __or_<_B1, _B2, _B3, _Bn...>
    : public conditional<_B1::value, _B1, __or_<_B2, _B3, _Bn...>>::type
    {};
  template<typename...>
    struct __and_;
  template<typename _B1, typename _B2>
    struct __and_<_B1, _B2>
    : public conditional<_B1::value, _B2, _B1>::type
    {};
  template<typename _Pp>
    struct __not_
    : public integral_constant<bool, !_Pp::value>
    {};
  template<typename _Tp>
    struct __success_type
    { typedef _Tp type; };
  template<typename>
    struct remove_cv;
  template<typename>
    struct __is_void_helper
    : public false_type {};
  template<typename _Tp>
    struct is_void
    : public __is_void_helper<typename remove_cv<_Tp>::type>::type
    {};
  template<typename>
    struct __is_integral_helper
    : public true_type {};
  template<typename _Tp>
    struct is_integral
    : public __is_integral_helper<typename remove_cv<_Tp>::type>::type
    {};
  template<typename>
    struct is_array
    : public false_type {};
  template<typename>
    struct is_lvalue_reference
    : public false_type {};
  template<typename>
    struct is_rvalue_reference
    : public false_type {};
  template<typename>
    struct __is_member_object_pointer_helper
    : public false_type {};
  template<typename _Tp>
    struct is_member_object_pointer
    : public __is_member_object_pointer_helper<
    typename remove_cv<_Tp>::type>::type
    {};
  template<typename>
    struct __is_member_function_pointer_helper
    : public false_type {};
  template<typename _Tp>
    struct is_member_function_pointer
    : public __is_member_function_pointer_helper<
    typename remove_cv<_Tp>::type>::type
    {};
  template<typename _Tp>
    struct is_enum
    : public integral_constant<bool, __is_enum(_Tp)>
    {};
  template<typename>
    struct is_function
    : public false_type {};
  template<typename _Tp>
    struct is_reference
    : public __or_<is_lvalue_reference<_Tp>,
                   is_rvalue_reference<_Tp>>::type
    {};
  template<typename _Tp>
    struct __is_member_pointer_helper
    : public false_type {};
  template<typename _Tp>
    struct is_member_pointer
    : public __is_member_pointer_helper<typename remove_cv<_Tp>::type>::type
    {};
  template<typename>
    struct is_const
    : public false_type {};
  template<typename>
    struct is_volatile
    : public false_type {};
  template<typename>
    struct add_rvalue_reference;
  template<typename _Tp>
    typename add_rvalue_reference<_Tp>::type declval() noexcept;
  struct __do_is_nary_constructible_impl
  {
    template<typename _Tp, typename... _Args, typename
             = decltype(_Tp(declval<_Args>()...))>
      static true_type __test(int);
  };
  template<typename _Tp, typename... _Args>
    struct __is_nary_constructible_impl
    : public __do_is_nary_constructible_impl
    {
      typedef decltype(__test<_Tp, _Args...>(0)) type;
    };
  template<typename _Tp, typename... _Args>
    struct __is_nary_constructible
    : public __is_nary_constructible_impl<_Tp, _Args...>::type
    {};
  template<typename _Tp, typename... _Args>
    struct __is_constructible_impl
    : public __is_nary_constructible<_Tp, _Args...>
    {};
  template<typename _Tp, typename... _Args>
    struct is_constructible
    : public __is_constructible_impl<_Tp, _Args...>::type
    {};
  template<typename, typename>
    struct is_same
    : public true_type {};
  template<typename _From, typename _To,
           bool = __or_<is_void<_From>, is_function<_To>,
                        is_array<_To>>::value>
    struct __is_convertible_helper
    {
       template<typename _To1>
 static void __test_aux(_To1);
      template<typename _From1, typename _To1,
        typename = decltype(__test_aux<_To1>(std::declval<_From1>()))>
 static true_type
 __test(int);
      typedef decltype(__test<_From, _To>(0)) type;
    };
  template<typename _From, typename _To>
    struct is_convertible
    : public __is_convertible_helper<_From, _To>::type
    {};
  template<typename _Tp>
    struct remove_const
    { typedef _Tp type; };
  template<typename _Tp>
    struct remove_volatile
    { typedef _Tp type; };
  template<typename _Tp>
    struct remove_cv
    {
      typedef typename
      remove_const<typename remove_volatile<_Tp>::type>::type type;
    };
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp type; };
  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp type; };
  template<typename _Tp,
           bool = __and_<__not_<is_reference<_Tp>>,
                         __not_<is_void<_Tp>>>::value>
    struct __add_rvalue_reference_helper
    { typedef _Tp type; };
  template<typename _Tp>
    struct add_rvalue_reference
    : public __add_rvalue_reference_helper<_Tp>
    {};
  template<typename _Unqualified, bool _IsConst, bool _IsVol>
    struct __cv_selector;
  template<typename _Unqualified>
    struct __cv_selector<_Unqualified, false, false>
    { typedef _Unqualified __type; };
  template<typename _Qualified, typename _Unqualified,
    bool _IsConst = is_const<_Qualified>::value,
    bool _IsVol = is_volatile<_Qualified>::value>
    class __match_cv_qualifiers
    {
      typedef __cv_selector<_Unqualified, _IsConst, _IsVol> __match;
    public:
      typedef typename __match::__type __type;
    };
  template<typename _Tp>
    struct __make_unsigned
    { typedef _Tp __type; };
  template<typename _Tp,
    bool _IsInt = is_integral<_Tp>::value,
    bool _IsEnum = is_enum<_Tp>::value>
    class __make_unsigned_selector;
  template<typename _Tp>
    class __make_unsigned_selector<_Tp, true, false>
    {
      typedef __make_unsigned<typename remove_cv<_Tp>::type> __unsignedt;
      typedef typename __unsignedt::__type __unsigned_type;
      typedef __match_cv_qualifiers<_Tp, __unsigned_type> __cv_unsigned;
    public:
      typedef typename __cv_unsigned::__type __type;
    };
  template<typename _Tp>
    struct make_unsigned
    { typedef typename __make_unsigned_selector<_Tp>::__type type; };
  template<typename _Tp, typename>
    struct __remove_pointer_helper
    { typedef _Tp type; };
  template<typename _Tp>
    struct remove_pointer
    : public __remove_pointer_helper<_Tp, typename remove_cv<_Tp>::type>
    {};
  template<typename _Up,
    bool _IsArray = is_array<_Up>::value,
    bool _IsFunction = is_function<_Up>::value>
    struct __decay_selector;
  template<typename _Up>
    struct __decay_selector<_Up, false, false>
    { typedef typename remove_cv<_Up>::type __type; };
  template<typename _Tp>
    class decay
    {
      typedef typename remove_reference<_Tp>::type __remove_type;
    public:
      typedef typename __decay_selector<__remove_type>::__type type;
    };
  template<bool, typename _Tp = void>
    struct enable_if
    { typedef _Tp type; };
  template<typename... _Cond>
    using _Require = typename enable_if<__and_<_Cond...>::value>::type;
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct conditional
    { typedef _Iftrue type; };
  template<typename _Signature>
    class result_of;
  template<bool, bool, typename _Functor, typename... _ArgTypes>
    struct __result_of_impl
    ;
  struct __result_of_other_impl
  {
    template<typename _Fn, typename... _Args>
      static __success_type<decltype(
      std::declval<_Fn>()(std::declval<_Args>()...)
      )> _S_test(int);
  };
  template<typename _Functor, typename... _ArgTypes>
    struct __result_of_impl<false, false, _Functor, _ArgTypes...>
    : private __result_of_other_impl
    {
      typedef decltype(_S_test<_Functor, _ArgTypes...>(0)) type;
    };
  template<typename _Functor, typename... _ArgTypes>
    struct result_of<_Functor(_ArgTypes...)>
    : public __result_of_impl<
        is_member_object_pointer<
          typename remove_reference<_Functor>::type
        >::value,
        is_member_function_pointer<
          typename remove_reference<_Functor>::type
        >::value,
     _Functor, _ArgTypes...
      >::type
    {};
  template<typename _Tp>
    constexpr _Tp&&
    forward(typename std::remove_reference<_Tp>::type& __t) noexcept
    { return static_cast<_Tp&&>(__t); }
  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
  template<std::size_t _Int, class _Tp>
    class tuple_element;
  struct allocator_arg_t {};
  constexpr allocator_arg_t allocator_arg = allocator_arg_t();
  template<typename _Tp>
    struct __add_ref
    { typedef _Tp& type; };
  template<std::size_t _Idx, typename... _Elements>
    struct _Tuple_impl;
  template<std::size_t _Idx, typename _Head, typename... _Tail>
    struct _Tuple_impl<_Idx, _Head, _Tail...>
    {};
  template<typename... _Elements>
    class tuple : public _Tuple_impl<0, _Elements...>
    {};
  template<typename _Head, typename... _Tail>
    struct tuple_element<0, tuple<_Head, _Tail...> >
    {
      typedef _Head type;
    };
  template<std::size_t __i, typename... _Elements>
    typename __add_ref<
                      typename tuple_element<__i, tuple<_Elements...>>::type
                    >::type
    get(tuple<_Elements...>& __t) noexcept;
  template<std::size_t... _Indexes>
    struct _Index_tuple
    {};
  template<std::size_t _Num>
    struct _Build_index_tuple
    {
      typedef _Index_tuple<> __type;
    };
  template<typename _Functor, typename... _Args>
    typename enable_if<
      (!is_member_pointer<_Functor>::value
       && !is_function<typename remove_pointer<_Functor>::type>::value),
      typename result_of<_Functor(_Args&&...)>::type
    >::type
    __invoke(_Functor& __f, _Args&&... __args)
    {
      return __f(std::forward<_Args>(__args)...);
    }

  template<typename _Tp>
    class reference_wrapper
    {
    public:
      _Tp&
      get() const noexcept
      {}
      template<typename... _Args>
 typename result_of<_Tp&(_Args&&...)>::type
 operator()(_Args&&... __args) const
 {
   return __invoke(get(), std::forward<_Args>(__args)...);
 }
    };
  template<typename _Tp>
    inline reference_wrapper<_Tp>
    ref(_Tp& __t) noexcept
    {}
  template<typename _Tp>
    struct _Maybe_wrap_member_pointer
    {
      typedef _Tp type;
    };
  template<typename _Signature>
    struct _Bind_simple;
  template<typename _Callable, typename... _Args>
    struct _Bind_simple<_Callable(_Args...)>
    {
      typedef typename result_of<_Callable(_Args...)>::type result_type;
      result_type
      operator()()
      {
        typedef typename _Build_index_tuple<sizeof...(_Args)>::__type _Indices;
        return _M_invoke(_Indices());
      }
      template<std::size_t... _Indices>
        typename result_of<_Callable(_Args...)>::type
        _M_invoke(_Index_tuple<_Indices...>)
        {
          return std::forward<_Callable>(std::get<0>(_M_bound))(
              std::forward<_Args>(std::get<_Indices+1>(_M_bound))...);
        }
      std::tuple<_Callable, _Args...> _M_bound;
    };
  template<typename _Func, typename... _BoundArgs>
    struct _Bind_simple_helper
    {
      typedef _Maybe_wrap_member_pointer<typename decay<_Func>::type>
        __maybe_type;
      typedef typename __maybe_type::type __func_type;
      typedef _Bind_simple<__func_type(typename decay<_BoundArgs>::type...)>
        __type;
    };
  template<typename _Callable, typename... _Args>
    typename _Bind_simple_helper<_Callable, _Args...>::__type
    __bind_simple(_Callable&& __callable, _Args&&... __args)
  ;
  union _Any_data
  ;
  template<typename _Functor>
    inline _Functor&
    __callable_functor(_Functor& __f)
    ;
  template<typename _Signature>
    class function;
  class _Function_base
  {
    template<typename _Functor>
      class _Base_manager
      {
      protected:
 static _Functor*
 _M_get_pointer(const _Any_data& __source)
 ;
      };
  };
  template<typename _Signature, typename _Functor>
    class _Function_handler;
  template<typename _Res, typename _Functor, typename... _ArgTypes>
    class _Function_handler<_Res(_ArgTypes...), _Functor>
    : public _Function_base::_Base_manager<_Functor>
    {
      typedef _Function_base::_Base_manager<_Functor> _Base;
    public:
      static _Res
      _M_invoke(const _Any_data& __functor, _ArgTypes... __args)
      {
 return (*_Base::_M_get_pointer(__functor))(
     std::forward<_ArgTypes>(__args)...);
      }
    };
  template<typename _Res, typename... _ArgTypes>
    class function<_Res(_ArgTypes...)>
    {
      typedef _Res _Signature_type(_ArgTypes...);
      template<typename _Functor>
 using _Invoke = decltype(__callable_functor(std::declval<_Functor&>())
     (std::declval<_ArgTypes>()...) );
      template<typename _CallRes, typename _Res1>
 struct _CheckResult
 : is_convertible<_CallRes, _Res1> {};
      template<typename _Functor>
 using _Callable = _CheckResult<_Invoke<_Functor>, _Res>;
      template<typename _Cond, typename _Tp>
 using _Requires = typename enable_if<_Cond::value, _Tp>::type;
    public:
      template<typename _Functor,
        typename = _Requires<_Callable<_Functor>, void>>
 function(_Functor);
      typedef _Res (*_Invoker_type)(const _Any_data&, _ArgTypes...);
      _Invoker_type _M_invoker;
  };
  template<typename _Res, typename... _ArgTypes>
    template<typename _Functor, typename>
      function<_Res(_ArgTypes...)>::
      function(_Functor __f)
      {
 typedef _Function_handler<_Signature_type, _Functor> _My_handler;
{
     _M_invoker = &_My_handler::_M_invoke;
   }
      }
  template<typename _Ptr>
    class __ptrtr_pointer_to
    ;
  template<typename _Ptr>
    struct pointer_traits : __ptrtr_pointer_to<_Ptr>
    {};
  template<typename _Tp>
    struct pointer_traits<_Tp*>
    {
      typedef ptrdiff_t difference_type;
    };
  template<typename _Alloc, typename _Tp>
    class __alloctr_rebind_helper
    {
      template<typename, typename>
        static constexpr bool
        _S_chk(...)
        { return false; }
    public:
      static const bool __value = _S_chk<_Alloc, _Tp>(nullptr);
    };
  template<typename _Alloc, typename _Tp,
           bool = __alloctr_rebind_helper<_Alloc, _Tp>::__value>
    struct __alloctr_rebind;
  template<template<typename, typename...> class _Alloc, typename _Tp,
            typename _Up, typename... _Args>
    struct __alloctr_rebind<_Alloc<_Up, _Args...>, _Tp, false>
    {
      typedef _Alloc<_Tp, _Args...> __type;
    };
  template<typename _Alloc>
    struct allocator_traits
    {
      typedef _Alloc allocator_type;
      typedef typename _Alloc::value_type value_type; static value_type* _S_pointer_helper(...); typedef decltype(_S_pointer_helper((_Alloc*)0)) __pointer; public:
      typedef __pointer pointer; static typename pointer_traits<pointer>::difference_type _S_difference_type_helper(...); typedef decltype(_S_difference_type_helper((_Alloc*)0)) __difference_type; public:
      typedef __difference_type difference_type; static typename make_unsigned<difference_type>::type _S_size_type_helper(...); typedef decltype(_S_size_type_helper((_Alloc*)0)) __size_type; public:
      typedef __size_type size_type; public:
      template<typename _Tp>
        using rebind_alloc = typename __alloctr_rebind<_Alloc, _Tp>::__type;
      template<typename _Tp>
        using rebind_traits = allocator_traits<rebind_alloc<_Tp>>;
      template<typename _Tp, typename... _Args>
 struct __construct_helper
 {
   template<typename>
     static false_type __test(...);
   typedef decltype(__test<_Alloc>(0)) type;
   static const bool value = type::value;
 };
      template<typename _Tp, typename... _Args>
 static typename
 enable_if<__and_<__not_<__construct_helper<_Tp, _Args...>>,
    is_constructible<_Tp, _Args...>>::value, void>::type
        _S_construct(_Alloc&, _Tp* __p, _Args&&... __args)
 { ::new((void*)__p) _Tp(std::forward<_Args>(__args)...); }
      static pointer
      allocate(_Alloc& __a, size_type __n)
      ;
      template<typename _Tp, typename... _Args>
 static auto construct(_Alloc& __a, _Tp* __p, _Args&&... __args)
 -> decltype(_S_construct(__a, __p, std::forward<_Args>(__args)...))
 { _S_construct(__a, __p, std::forward<_Args>(__args)...); }
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  enum _Lock_policy { _S_single, _S_mutex, _S_atomic };
  static const _Lock_policy __default_lock_policy =
  _S_atomic;
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template<typename _Tp>
    struct default_delete
    ;
  template <typename _Tp, typename _Dp = default_delete<_Tp> >
    class unique_ptr
    {
      class _Pointer
      {
 template<typename _Up>
   static _Tp* __test(...);
 typedef typename remove_reference<_Dp>::type _Del;
      public:
 typedef decltype(__test<_Del>(0)) type;
      };
    public:
      typedef typename _Pointer::type pointer;
      typedef _Tp element_type;
      template<typename _Up, typename _Ep, typename = _Require<
        is_convertible<typename unique_ptr<_Up, _Ep>::pointer, pointer>,
        typename conditional<is_reference<_Dp>::value,
        is_same<_Ep, _Dp>,
        is_convertible<_Ep, _Dp>>::type>>
 unique_ptr(unique_ptr<_Up, _Ep>&& __u) noexcept
      ;
  };
}
namespace __gnu_cxx
{
  template<typename _Tp>
    struct __aligned_buffer
    {};
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  using __gnu_cxx::_Lock_policy;
  using __gnu_cxx::__default_lock_policy;
  template<_Lock_policy _Lp = __default_lock_policy>
    class _Sp_counted_base
    {};
  template<typename _Tp, _Lock_policy _Lp = __default_lock_policy>
    class __shared_ptr;
  struct _Sp_make_shared_tag {};
  template<typename _Tp, typename _Alloc, _Lock_policy _Lp>
    class _Sp_counted_ptr_inplace final : public _Sp_counted_base<_Lp>
    {
      struct _Impl
      : public _Alloc
      {
 _Impl(_Alloc __a) : _Alloc(__a), _M_ptr() {}
 _Tp* _M_ptr;
      };
    public:
      template<typename... _Args>
 _Sp_counted_ptr_inplace(_Alloc __a, _Args&&... __args)
 : _M_impl(__a), _M_storage()
 {
   allocator_traits<_Alloc>::construct(__a, _M_impl._M_ptr,
       std::forward<_Args>(__args)...);
 }
      _Impl _M_impl;
      __gnu_cxx::__aligned_buffer<_Tp> _M_storage;
    };
  template<_Lock_policy _Lp>
    class __shared_count
    {
    public:
      template<typename _Tp, typename _Alloc, typename... _Args>
 __shared_count(_Sp_make_shared_tag, _Tp*, const _Alloc& __a,
         _Args&&... __args)
 {
   typedef _Sp_counted_ptr_inplace<_Tp, _Alloc, _Lp> _Sp_cp_type;
   typedef typename allocator_traits<_Alloc>::template
     rebind_traits<_Sp_cp_type> _Alloc_traits;
   typename _Alloc_traits::allocator_type __a2(__a);
   _Sp_cp_type* __mem = _Alloc_traits::allocate(__a2, 1);
   try
     {
       _Alloc_traits::construct(__a2, __mem, std::move(__a),
      std::forward<_Args>(__args)...);
     }
   catch(...)
     {}
 }
    };
  template<typename _Tp, _Lock_policy _Lp>
    class __shared_ptr
    {
    public:
      template<typename _Tp1, typename = typename
        std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
 __shared_ptr(const __shared_ptr<_Tp1, _Lp>& __r) noexcept
 : _M_ptr(__r._M_ptr), _M_refcount(__r._M_refcount)
 {}
      template<typename _Alloc, typename... _Args>
 __shared_ptr(_Sp_make_shared_tag __tag, const _Alloc& __a,
       _Args&&... __args)
 : _M_ptr(), _M_refcount(__tag, (_Tp*)0, __a,
    std::forward<_Args>(__args)...)
 {}
      _Tp* _M_ptr;
      __shared_count<_Lp> _M_refcount;
    };
  template<typename _Tp>
    class shared_ptr : public __shared_ptr<_Tp>
    {
    public:
      template<typename _Tp1, typename = typename
        std::enable_if<std::is_convertible<_Tp1*, _Tp*>::value>::type>
 shared_ptr(const shared_ptr<_Tp1>& __r) noexcept
        : __shared_ptr<_Tp>(__r) {}
      template<typename _Alloc, typename... _Args>
 shared_ptr(_Sp_make_shared_tag __tag, const _Alloc& __a,
     _Args&&... __args)
 : __shared_ptr<_Tp>(__tag, __a, std::forward<_Args>(__args)...)
 {}
    };
  template<typename _Tp, typename _Alloc, typename... _Args>
    inline shared_ptr<_Tp>
    allocate_shared(const _Alloc& __a, _Args&&... __args)
    {
      return shared_ptr<_Tp>(_Sp_make_shared_tag(), __a,
        std::forward<_Args>(__args)...);
    }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template<typename _Signature>
    class packaged_task;
  struct __future_base
  {
    struct _Result_base
    {
      struct _Deleter
      ;
    };
    template<typename _Res>
      struct _Result : _Result_base
      {
 typedef _Res result_type;
    };
    template<typename _Res>
      using _Ptr = unique_ptr<_Res, _Result_base::_Deleter>;
    template<typename _Res, typename _Alloc>
      struct _Result_alloc final : _Result<_Res>, _Alloc
      {};
    template<typename _Res, typename _Allocator>
      static _Ptr<_Result_alloc<_Res, _Allocator>>
      _S_allocate_result(const _Allocator& __a)
    ;
    template<typename _Signature>
      class _Task_state_base;
    template<typename _Fn, typename _Alloc, typename _Signature>
      class _Task_state;
    template<typename _Res_ptr,
      typename _Res = typename _Res_ptr::element_type::result_type>
      struct _Task_setter;
    template<typename _Res_ptr, typename _BoundFn>
      static _Task_setter<_Res_ptr>
      _S_task_setter(_Res_ptr& __ptr, _BoundFn&& __call)
      {
 return _Task_setter<_Res_ptr>{ __ptr, std::ref(__call) };
      }
  };
  template<typename _Ptr_type, typename _Res>
    struct __future_base::_Task_setter
    {
      _Ptr_type& _M_result;
      std::function<_Res()> _M_fn;
    };
  template<typename _Res, typename... _Args>
    struct __future_base::_Task_state_base<_Res(_Args...)>
    {
      template<typename _Alloc>
 _Task_state_base(const _Alloc& __a)
 : _M_result(_S_allocate_result<_Res>(__a))
 {}
      typedef __future_base::_Ptr<_Result<_Res>> _Ptr_type;
      _Ptr_type _M_result;
    };
  template<typename _Fn, typename _Alloc, typename _Res, typename... _Args>
    struct __future_base::_Task_state<_Fn, _Alloc, _Res(_Args...)> final
    : __future_base::_Task_state_base<_Res(_Args...)>
    {
      _Task_state(_Fn&& __fn, const _Alloc& __a)
      : _Task_state_base<_Res(_Args...)>(__a), _M_impl(std::move(__fn), __a)
      {}
      virtual void
      _M_run(_Args... __args)
      {
 auto __boundfn = std::__bind_simple(std::ref(_M_impl._M_fn),
     _S_maybe_wrap_ref(std::forward<_Args>(__args))...);
 auto __setter = _S_task_setter(this->_M_result, std::move(__boundfn));
      }
      struct _Impl : _Alloc
      {
 _Impl(_Fn&& __fn, const _Alloc& __a)
   : _Alloc(__a), _M_fn(std::move(__fn)) {}
 _Fn _M_fn;
      } _M_impl;
    };
    template<typename _Signature, typename _Fn, typename _Alloc>
      static shared_ptr<__future_base::_Task_state_base<_Signature>>
      __create_task_state(_Fn&& __fn, const _Alloc& __a)
      {
 typedef __future_base::_Task_state<_Fn, _Alloc, _Signature> _State;
 return std::allocate_shared<_State>(__a, std::move(__fn), __a);
      }
  template<typename _Task, typename _Fn, bool
    = is_same<_Task, typename decay<_Fn>::type>::value>
    struct __constrain_pkgdtask
    { typedef void __type; };
  template<typename _Res, typename... _ArgTypes>
    class packaged_task<_Res(_ArgTypes...)>
    {
      typedef __future_base::_Task_state_base<_Res(_ArgTypes...)> _State_type;
      shared_ptr<_State_type> _M_state;
    public:
      template<typename _Fn, typename _Alloc, typename = typename
        __constrain_pkgdtask<packaged_task, _Fn>::__type>
 packaged_task(allocator_arg_t, const _Alloc& __a, _Fn&& __fn)
 : _M_state(__create_task_state<_Res(_ArgTypes...)>(
      std::forward<_Fn>(__fn), __a))
 {}
    };
}
namespace __gnu_test
{
  template <class Tp>
    struct SimpleAllocator
    {
      typedef Tp value_type;
      SimpleAllocator() ;
      template <class T>
        SimpleAllocator(const SimpleAllocator<T>& other) ;
    };
}
using std::packaged_task;
using std::allocator_arg;
__gnu_test::SimpleAllocator<int> a;
packaged_task<int()> p(allocator_arg, a, []() { return 1; });
