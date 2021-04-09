typedef long unsigned int size_t;

template<typename> class allocator;

template<typename _Tp, typename _Up>
struct __replace_first_arg
{ };

template<template<typename, typename...> class _Template, typename _Up,
	 typename _Tp, typename... _Types>
struct __replace_first_arg<_Template<_Tp, _Types...>, _Up>
{
  using type = _Template<_Up, _Types...>;
};

template<typename _Tp, typename _Up>
using __replace_first_arg_t = typename __replace_first_arg<_Tp, _Up>::type;

template<typename _Tp>
class new_allocator
{
public:
  typedef _Tp value_type;
};

template<typename _Tp>
using __allocator_base = new_allocator<_Tp>;

template<typename _Tp>
class allocator : public __allocator_base<_Tp>
{
public:
};

struct __allocator_traits_base
{
  template<typename _Tp, typename _Up, typename = void>
  struct __rebind : __replace_first_arg<_Tp, _Up> { };
};

template<typename _Alloc, typename _Up>
using __alloc_rebind
= typename __allocator_traits_base::template __rebind<_Alloc, _Up>::type;

template<typename _Alloc>
struct allocator_traits : __allocator_traits_base
{
public:
  template<typename _Tp>
  using rebind_alloc = __alloc_rebind<_Alloc, _Tp>;
};

template<typename _Alloc, typename = typename _Alloc::value_type>
struct __alloc_traits

{
  template<typename _Tp>
  struct rebind
  {
    typedef typename allocator_traits<_Alloc>::template rebind_alloc<_Tp> other;
  };
};

typedef typename __alloc_traits<allocator<char>>::template
rebind<char>::other _Char_alloc_type;
