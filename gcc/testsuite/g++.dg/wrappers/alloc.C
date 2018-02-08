// { dg-do compile { target c++11 } }

template<typename _Default, typename _AlwaysVoid, template<typename...> class _Op, typename... _Args>
struct __detector
{
  using type = _Op<_Args...>;
};
template<typename _Default, template<typename...> class _Op, typename... _Args>
using __detected_or = __detector<_Default, void, _Op, _Args...>;
template<typename _Default, template<typename...> class _Op, typename... _Args>
using __detected_or_t = typename __detected_or<_Default, _Op, _Args...>::type;
template<typename _Alloc>
struct allocator_traits {
  template<typename _Tp>
  using __pointer = typename _Tp::pointer;
  using pointer = __detected_or_t<typename _Alloc::value_type*, __pointer, _Alloc>;
};
template<typename _Storage_policy>
struct _Pointer_adapter {
  typedef typename _Storage_policy::element_type element_type;
  typedef element_type& reference;
};
template<typename _Tp>
inline _Tp* addressof(_Tp& __r); // { dg-warning "used but never defined" }
template<typename _Alloc>
struct __allocated_ptr {
  __allocated_ptr()
  {
    using pointer = typename allocator_traits<_Alloc>::pointer;
    typename _Pointer_adapter<pointer>::reference __r = *(int*)0;
    addressof(__r);
  }
};
template<typename _Tp>
struct _Std_pointer_impl {
  typedef _Tp element_type;
};
template<typename Tp>
struct CustomPointerAlloc {
  typedef Tp value_type;
  typedef _Pointer_adapter<_Std_pointer_impl<Tp>> pointer;
};
__allocated_ptr<CustomPointerAlloc<int>> __guard_ptr;
