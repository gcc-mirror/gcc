// PR middle-end/89303
// { dg-do run }
// { dg-additional-options "-std=c++14" }

namespace my
{
  typedef __SIZE_TYPE__ size_t;
  typedef decltype(nullptr) nullptr_t;

  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp value = __v;
      typedef _Tp value_type;
      typedef integral_constant<_Tp, __v> type;
      constexpr operator value_type() const noexcept { return value; }
      constexpr value_type operator()() const noexcept { return value; }
    };

  template<typename _Tp, _Tp __v>
    constexpr _Tp integral_constant<_Tp, __v>::value;

  typedef integral_constant<bool, true> true_type;
  typedef integral_constant<bool, false> false_type;

  template<bool __v>
    using __bool_constant = integral_constant<bool, __v>;

  template<bool, typename, typename>
    struct conditional;

  template<typename...>
    struct __and_;

  template<>
    struct __and_<>
    : public true_type
    { };

  template<typename _B1>
    struct __and_<_B1>
    : public _B1
    { };

  template<typename _B1, typename _B2>
    struct __and_<_B1, _B2>
    : public conditional<_B1::value, _B2, _B1>::type
    { };

  template<typename _B1, typename _B2, typename _B3, typename... _Bn>
    struct __and_<_B1, _B2, _B3, _Bn...>
    : public conditional<_B1::value, __and_<_B2, _B3, _Bn...>, _B1>::type
    { };

  template<typename>
    struct remove_cv;

  template<typename>
    struct __is_void_helper
    : public false_type { };

  template<>
    struct __is_void_helper<void>
    : public true_type { };

  template<typename _Tp>
    struct is_void
    : public __is_void_helper<typename remove_cv<_Tp>::type>::type
    { };

  template<typename _Tp, typename _Up = _Tp&&>
    _Up
    __declval(int);

  template<typename _Tp>
    _Tp
    __declval(long);

  template<typename _Tp>
    auto declval() noexcept -> decltype(__declval<_Tp>(0));

  template<typename, typename>
    struct is_same
    : public false_type { };

  template<typename _Tp>
    struct is_same<_Tp, _Tp>
    : public true_type { };

  template<typename _Tp>
    struct remove_const
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_const<_Tp const>
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_volatile
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_volatile<_Tp volatile>
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

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp type; };

  template<bool, typename _Tp = void>
    struct enable_if
    { };

  template<typename _Tp>
    struct enable_if<true, _Tp>
    { typedef _Tp type; };

  template<typename... _Cond>
    using _Require = typename enable_if<__and_<_Cond...>::value>::type;

  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct conditional
    { typedef _Iftrue type; };

  template<typename _Iftrue, typename _Iffalse>
    struct conditional<false, _Iftrue, _Iffalse>
    { typedef _Iffalse type; };

  template<typename _Tp>
    struct __declval_protector
    {
      static const bool __stop = false;
    };

  template<typename _Tp>
    auto declval() noexcept -> decltype(__declval<_Tp>(0))
    {
      static_assert(__declval_protector<_Tp>::__stop,
      "declval() must not be used!");
      return __declval<_Tp>(0);
    }

  namespace void_details {
    template <class... >
    struct make_void { using type = void; };
}

template <class... T> using __void_t = typename void_details ::make_void<T...>::type;

  template<typename _Tp>
    inline constexpr _Tp*
    __addressof(_Tp& __r) noexcept
    {
      return reinterpret_cast<_Tp*>
	(&const_cast<char&>(reinterpret_cast<const volatile char&>(__r)));
    }

  template<typename _Tp>
    constexpr _Tp&&
    forward(typename my::remove_reference<_Tp>::type& __t) noexcept
    { return static_cast<_Tp&&>(__t); }

  template<typename _Tp>
    constexpr _Tp&&
    forward(typename my::remove_reference<_Tp>::type&& __t) noexcept
    {
      return static_cast<_Tp&&>(__t);
    }

  template<typename _Tp>
    constexpr typename my::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename my::remove_reference<_Tp>::type&&>(__t); }
}
       
inline void* operator new(my::size_t, void* p) { return p; }

extern "C" void* malloc(my::size_t);
extern "C" void free(void*);

namespace my
{
  template<typename T>
    class allocator
    {
    public:
      using value_type = T;

      allocator() { }

      template<typename U>
        allocator(const allocator<U>&) { }

      T* allocate(size_t n) { return (T*)malloc(n*sizeof(T)); }
      void deallocate(T* p, size_t) { free(p); }

      template<typename U, typename... Args>
        void construct(U* p, Args&&... args)
        { ::new((void*)p) U(args...); }

      template<typename U>
        void destroy(U* p)
        { p->~U(); }
    };

  class __undefined;

  template<typename _Tp, typename _Up>
    struct __replace_first_arg
    { };

  template<template<typename, typename...> class _Template, typename _Up,
           typename _Tp, typename... _Types>
    struct __replace_first_arg<_Template<_Tp, _Types...>, _Up>
    { using type = _Template<_Up, _Types...>; };

  struct __allocator_traits_base
  {
    template<typename _Tp, typename _Up, typename = void>
      struct __rebind : __replace_first_arg<_Tp, _Up> { };

    template<typename _Tp, typename _Up>
      struct __rebind<_Tp, _Up,
        __void_t<typename _Tp::template rebind<_Up>::other>>
      { using type = typename _Tp::template rebind<_Up>::other; };
  };

  template<typename _Alloc, typename _Up>
    using __alloc_rebind
      = typename __allocator_traits_base::template __rebind<_Alloc, _Up>::type;

  template<typename _Alloc>
    struct allocator_traits;

  template<typename _Tp>
    struct allocator_traits<allocator<_Tp>>
    {
      using allocator_type = allocator<_Tp>;
      using value_type = _Tp;
      using pointer = _Tp*;
      using const_pointer = const _Tp*;
      using size_type = my::size_t;

      static pointer
      allocate(allocator_type& __a, size_type __n)
      { return __a.allocate(__n); }

      static void
      deallocate(allocator_type& __a, pointer __p, size_type __n)
      { __a.deallocate(__p, __n); }

      template<typename _Up, typename... _Args>
        static void
        construct(allocator_type& __a, _Up* __p, _Args&&... __args)
        { __a.construct(__p, my::forward<_Args>(__args)...); }

      template<typename _Up>
        static void
        destroy(allocator_type& __a, _Up* __p)
        { __a.destroy(__p); }
    };

  template<typename _Alloc>
    struct __allocated_ptr
    {
      using pointer = typename allocator_traits<_Alloc>::pointer;
      using value_type = typename allocator_traits<_Alloc>::value_type;

      __allocated_ptr(_Alloc& __a, pointer __ptr) noexcept
      : _M_alloc(my::__addressof(__a)), _M_ptr(__ptr)
      { }

      template<typename _Ptr,
        typename _Req = _Require<is_same<_Ptr, value_type*>>>
      __allocated_ptr(_Alloc& __a, _Ptr __ptr)
      : _M_alloc(my::__addressof(__a)),
      _M_ptr(__ptr)
      { }

      __allocated_ptr(__allocated_ptr&& __gd) noexcept
      : _M_alloc(__gd._M_alloc), _M_ptr(__gd._M_ptr)
      { __gd._M_ptr = nullptr; }

      ~__allocated_ptr()
      {
        if (_M_ptr != nullptr)
          my::allocator_traits<_Alloc>::deallocate(*_M_alloc, _M_ptr, 1);
      }

      __allocated_ptr&
      operator=(my::nullptr_t) noexcept
      {
        _M_ptr = nullptr;
        return *this;
      }

      value_type* get() { return _M_ptr; }

    private:
      _Alloc* _M_alloc;
      pointer _M_ptr;
    };

  template<typename _Alloc>
    __allocated_ptr<_Alloc>
    __allocate_guarded(_Alloc& __a)
    {
      return { __a, my::allocator_traits<_Alloc>::allocate(__a, 1) };
    }

  template<typename _Tp>
    struct __aligned_buffer
    {
      alignas(__alignof__(_Tp)) unsigned char _M_storage[sizeof(_Tp)];
      __aligned_buffer() = default;

      void*
      _M_addr() noexcept
      {
        return static_cast<void*>(&_M_storage);
      }

      const void*
      _M_addr() const noexcept
      {
        return static_cast<const void*>(&_M_storage);
      }

      _Tp*
      _M_ptr() noexcept
      { return static_cast<_Tp*>(_M_addr()); }

      const _Tp*
      _M_ptr() const noexcept
      { return static_cast<const _Tp*>(_M_addr()); }
    };

  class bad_weak_ptr { };

  inline void
  __throw_bad_weak_ptr()
  { (throw (bad_weak_ptr())); }

    class _Sp_counted_base
    {
    public:
      _Sp_counted_base() noexcept
      : _M_use_count(1), _M_weak_count(1) { }

      virtual
      ~_Sp_counted_base() noexcept
      { }

      virtual void
      _M_dispose() noexcept = 0;

      virtual void
      _M_destroy() noexcept
      { delete this; }

      void
      _M_add_ref_copy()
      { ++_M_use_count; }

      void
      _M_add_ref_lock()
      {
        if (_M_use_count == 0)
          __throw_bad_weak_ptr();
        ++_M_use_count;
      }

      void
      _M_release() noexcept
      {
        if (--_M_use_count == 0)
        {
          _M_dispose();
          if (--_M_weak_count == 0)
            _M_destroy();
        }
      }

      void
      _M_weak_add_ref() noexcept
      { ++_M_weak_count; }

      void
      _M_weak_release() noexcept
      {
        if (--_M_weak_count == 0)
          _M_destroy();
      }

      long
      _M_get_use_count() const noexcept
      {
        return _M_use_count;
      }

    private:
      _Sp_counted_base(_Sp_counted_base const&) = delete;
      _Sp_counted_base& operator=(_Sp_counted_base const&) = delete;

      int _M_use_count;
      int _M_weak_count;
    };

  template<typename _Tp>
    class shared_ptr;

  template<typename _Tp>
    class weak_ptr;

  template<typename _Tp>
    class enable_shared_from_this;

  class __weak_count;

  class __shared_count;

  template<typename _Alloc>
    struct _Sp_alloc_shared_tag
    {
      const _Alloc& _M_a;
    };

  template<typename _Tp, typename _Alloc>
    class _Sp_counted_ptr_inplace final : public _Sp_counted_base
    {
      class _Impl : _Alloc
      {
      public:
        explicit _Impl(_Alloc __a) noexcept : _Alloc(__a) { }

        _Alloc& _M_alloc() noexcept { return *this; }

        __aligned_buffer<_Tp> _M_storage;
      };

    public:
      using __allocator_type = __alloc_rebind<_Alloc, _Sp_counted_ptr_inplace>;

      template<typename... _Args>
        _Sp_counted_ptr_inplace(_Alloc __a, _Args&&... __args)
        : _M_impl(__a)
        {
          allocator_traits<_Alloc>::construct(__a, _M_ptr(),
              my::forward<_Args>(__args)...);
        }

      ~_Sp_counted_ptr_inplace() noexcept { }

      virtual void
      _M_dispose() noexcept
      {
        allocator_traits<_Alloc>::destroy(_M_impl._M_alloc(), _M_ptr());
      }

      virtual void
      _M_destroy() noexcept
      {
        __allocator_type __a(_M_impl._M_alloc());
        __allocated_ptr<__allocator_type> __guard_ptr{ __a, this };
        this->~_Sp_counted_ptr_inplace();
      }

    private:
      friend class __shared_count;

      _Tp* _M_ptr() noexcept { return _M_impl._M_storage._M_ptr(); }

      _Impl _M_impl;
    };

  class __shared_count
  {
  public:
    constexpr __shared_count() noexcept : _M_pi(0)
    { }

    template<typename _Tp, typename _Alloc, typename... _Args>
      __shared_count(_Tp*& __p, _Sp_alloc_shared_tag<_Alloc> __a,
          _Args&&... __args)
      {
        typedef _Sp_counted_ptr_inplace<_Tp, _Alloc> _Sp_cp_type;
        typename _Sp_cp_type::__allocator_type __a2(__a._M_a);
        auto __guard = my::__allocate_guarded(__a2);
        _Sp_cp_type* __mem = __guard.get();
        auto __pi = ::new (__mem)
          _Sp_cp_type(__a._M_a, my::forward<_Args>(__args)...);
        __guard = nullptr;
        _M_pi = __pi;
        __p = __pi->_M_ptr();
      }

    ~__shared_count() noexcept
    {
      if (_M_pi != nullptr)
        _M_pi->_M_release();
    }

    __shared_count(const __shared_count& __r) noexcept
    : _M_pi(__r._M_pi)
    {
      if (_M_pi != 0)
        _M_pi->_M_add_ref_copy();
    }

    explicit __shared_count(const __weak_count& __r);

    long
    _M_get_use_count() const noexcept
    { return _M_pi != 0 ? _M_pi->_M_get_use_count() : 0; }

  private:
    friend class __weak_count;

    _Sp_counted_base* _M_pi;
  };

  class __weak_count
  {
  public:
    constexpr __weak_count() noexcept : _M_pi(nullptr)
    { }

    __weak_count(const __shared_count& __r) noexcept
    : _M_pi(__r._M_pi)
    {
      if (_M_pi != nullptr)
        _M_pi->_M_weak_add_ref();
    }

    __weak_count(const __weak_count& __r) noexcept
    : _M_pi(__r._M_pi)
    {
      if (_M_pi != nullptr)
        _M_pi->_M_weak_add_ref();
    }

    __weak_count(__weak_count&& __r) noexcept
    : _M_pi(__r._M_pi)
    { __r._M_pi = nullptr; }

    ~__weak_count() noexcept
    {
      if (_M_pi != nullptr)
      {
        _M_pi->_M_weak_release();
      }
    }

    __weak_count&
    operator=(const __shared_count& __r) noexcept
    {
      _Sp_counted_base* __tmp = __r._M_pi;
      if (__tmp != nullptr)
        __tmp->_M_weak_add_ref();
      if (_M_pi != nullptr)
        _M_pi->_M_weak_release();
      _M_pi = __tmp;
      return *this;
    }

    long
    _M_get_use_count() const noexcept
    { return _M_pi != nullptr ? _M_pi->_M_get_use_count() : 0; }

  private:
    friend class __shared_count;

    _Sp_counted_base* _M_pi;
  };

  inline
  __shared_count::__shared_count(const __weak_count& __r)
  : _M_pi(__r._M_pi)
  {
    if (_M_pi != nullptr)
      _M_pi->_M_add_ref_lock();
    else
      __throw_bad_weak_ptr();
  }

  template<typename _Tp>
    class shared_ptr
    {
    public:
      using element_type = _Tp;

      constexpr shared_ptr() noexcept
        : _M_ptr(0), _M_refcount()
        { }

      shared_ptr(const shared_ptr&) noexcept = default;
      shared_ptr& operator=(const shared_ptr&) noexcept = default;
      ~shared_ptr() = default;

      template<typename _Yp>
	explicit shared_ptr(const weak_ptr<_Yp>& __r)
	: _M_refcount(__r._M_refcount) // may throw
	{
	  // It is now safe to copy __r._M_ptr, as
	  // _M_refcount(__r._M_refcount) did not throw.
	  _M_ptr = __r._M_ptr;
	}

      long
      use_count() const noexcept
      { return _M_refcount._M_get_use_count(); }

      element_type* operator->() const noexcept { return _M_ptr; }

    protected:

      template<typename _Alloc, typename... _Args>
        shared_ptr(_Sp_alloc_shared_tag<_Alloc> __tag, _Args&&... __args)
        : _M_ptr(), _M_refcount(_M_ptr, __tag, my::forward<_Args>(__args)...)
        { _M_enable_shared_from_this_with(_M_ptr); }

      template<typename _Tp1, typename _Alloc,
        typename... _Args>
          friend shared_ptr<_Tp1>
          allocate_shared(const _Alloc& __a, _Args&&... __args);

      friend class weak_ptr<_Tp>;

    private:

      template<typename _Yp>
        using __esft_base_t = decltype(__enable_shared_from_this_base(
              my::declval<const __shared_count&>(),
              my::declval<_Yp*>()));

      template<typename _Yp, typename = void>
        struct __has_esft_base
        : false_type { };

      template<typename _Yp>
        struct __has_esft_base<_Yp, __void_t<__esft_base_t<_Yp>>>
        : true_type { };

      template<typename _Yp, typename _Yp2 = typename remove_cv<_Yp>::type>
        typename enable_if<__has_esft_base<_Yp2>::value>::type
        _M_enable_shared_from_this_with(_Yp* __p) noexcept
        {
          if (auto __base = __enable_shared_from_this_base(_M_refcount, __p))
            __base->_M_weak_assign(const_cast<_Yp2*>(__p), _M_refcount);
        }

      template<typename _Tp1> friend class shared_ptr;
      template<typename _Tp1> friend class weak_ptr;

      element_type* _M_ptr;
      __shared_count _M_refcount;
    };

  template<typename _Tp>
    class weak_ptr
    {
    public:
      using element_type = _Tp;

      constexpr weak_ptr() noexcept
      : _M_ptr(nullptr), _M_refcount()
      { }

      weak_ptr(const weak_ptr&) noexcept = default;

      ~weak_ptr() = default;

      weak_ptr&
      operator=(const weak_ptr& __r) noexcept = default;

      long
      use_count() const noexcept
      { return _M_refcount._M_get_use_count(); }

    private:

      void
      _M_assign(_Tp* __ptr, const __shared_count& __refcount) noexcept
      {
        if (use_count() == 0)
        {
          _M_ptr = __ptr;
          _M_refcount = __refcount;
        }
      }

      template<typename _Tp1> friend class shared_ptr;
      template<typename _Tp1> friend class weak_ptr;
      friend class enable_shared_from_this<_Tp>;

      element_type* _M_ptr;
      __weak_count _M_refcount;
    };

  template<typename _Tp>
    class enable_shared_from_this
    {
    protected:
      constexpr enable_shared_from_this() noexcept { }

      enable_shared_from_this(const enable_shared_from_this&) noexcept { }

      enable_shared_from_this&
      operator=(const enable_shared_from_this&) noexcept
      { return *this; }

      ~enable_shared_from_this() { }

    public:
      shared_ptr<_Tp>
      shared_from_this()
      { return shared_ptr<_Tp>(this->_M_weak_this); }

      shared_ptr<const _Tp>
      shared_from_this() const
      { return shared_ptr<const _Tp>(this->_M_weak_this); }

    private:
      template<typename _Tp1>
        void
        _M_weak_assign(_Tp1* __p, const __shared_count& __n) const noexcept
        { _M_weak_this._M_assign(__p, __n); }

      friend const enable_shared_from_this*
      __enable_shared_from_this_base(const __shared_count&,
         const enable_shared_from_this* __p)
      { return __p; }

      template<typename>
        friend class shared_ptr;

      mutable weak_ptr<_Tp> _M_weak_this;
    };

  template<typename _Tp, typename _Alloc, typename... _Args>
    inline shared_ptr<_Tp>
    allocate_shared(const _Alloc& __a, _Args&&... __args)
    {
      return shared_ptr<_Tp>(_Sp_alloc_shared_tag<_Alloc>{__a},
        my::forward<_Args>(__args)...);
    }

  template<typename _Tp, typename... _Args>
    inline shared_ptr<_Tp>
    make_shared(_Args&&... __args)
    {
      typedef typename my::remove_const<_Tp>::type _Tp_nc;
      return my::allocate_shared<_Tp>(my::allocator<_Tp_nc>(),
           my::forward<_Args>(__args)...);
    }
}

class blob final: public my::enable_shared_from_this<blob>
{
  int* data;

public:
  blob() { data = new int; }
  ~blob() { delete data; }
};

static int
bar(my::shared_ptr<blob>)
{
  return 0;
}

int main()
{
  my::shared_ptr<blob> tg = my::make_shared<blob>();
  return tg->shared_from_this().use_count() - 2;
}
