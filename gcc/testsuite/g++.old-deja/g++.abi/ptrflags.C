// Test rtti pointer flags
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Apr 2000 <nathan@nathan@codesourcery.com>

#include <typeinfo>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
#include <cxxabi.h>

struct A {int m;};
struct B;

using namespace abi;

int expect (int flags, type_info const &info)
{
  __pointer_class_type_info const *ptr =
      dynamic_cast <__pointer_class_type_info const *> (&info);
  if (!ptr)
    return 0;
  if (ptr->quals != flags)
    return 0;
  return 1;
}

int main ()
{
  if (! expect (0, typeid (A *)))
    return 1;
  if (! expect (1, typeid (A const *)))
    return 2;
  if (! expect (2, typeid (A volatile *)))
    return 3;
  if (! expect (4, typeid (A __restrict__ *)))
    return 4;
  if (! expect (0, typeid (void A::*))
    return 5;
  if (! expect (0, typeid (void A::**))
    return 6;

  if (! expect (8 | 0, typeid (B *)))
    return 1;
  if (! expect (8 | 1, typeid (B const *)))
    return 2;
  if (! expect (8 | 2, typeid (B volatile *)))
    return 3;
  if (! expect (8 | 4, typeid (B __restrict__ *)))
    return 4;
  if (! expect (8 | 0, typeid (void B::*))
    return 5;
  if (! expect (8 | 0, typeid (void B::**))
    return 6;
  
  return 0;
}

#else
int main ()
{
  return 0;
}
#endif
