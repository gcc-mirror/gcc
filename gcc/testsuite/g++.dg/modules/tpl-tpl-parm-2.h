


template<typename _Alloc>
struct allocator_traits
{
  template<template<typename> class _Func>
  struct _Ptr {};

  using rebind_alloc = int;
};

inline void frob ()
{
  allocator_traits<int> _M_unpooled;
}
