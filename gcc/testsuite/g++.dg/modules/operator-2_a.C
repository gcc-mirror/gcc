// { dg-additional-options -fmodules }
// { dg-module-cmi M }

module;

#include <new>

export module M;

export template <class T>
inline T* alloc (__SIZE_TYPE__ n)
{
  return (T*) __builtin_operator_new (n * sizeof (T), std::nothrow_t{});
};
