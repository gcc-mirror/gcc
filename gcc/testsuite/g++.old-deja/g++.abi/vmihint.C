// Test rtti hint flags
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Apr 2000 <nathan@nathan@codesourcery.com>

#include <typeinfo>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
#include <cxxabi.h>

struct A {int m;};
struct A1vA : virtual A {int m;};
struct A2vA : virtual A {int m;};
struct A1A : A {int m;};
struct A2A : A {int m;};
struct B {int m;};

struct C1 : B, virtual A {int m;};

struct D1 : A1vA, A2vA {int m;};

struct E1 : A1A, A2A {int m;};

struct E2 : A1A, A2vA {int m;};

struct F1 : A1A, A1vA, A2vA {int m;};

struct P1 : protected A {int m;};

struct P2 : B, P1 {int m;};

using namespace abi;

int expect (int flags, std::type_info const &info)
{
  abi::__vmi_class_type_info const *ptr =
      dynamic_cast <abi::__vmi_class_type_info const *> (&info);
  if (!ptr)
    return 0;
  if (ptr->__flags != flags)
    return 0;
  return 1;
}

int main ()
{
  if (! expect (0 | 8, typeid (C1)))
    return 1;
  if (! expect (2 | 8, typeid (D1)))
    return 2;
  if (! expect (1 | 8, typeid (E1)))
    return 3;
  if (! expect (1 | 8, typeid (E2)))
    return 4;
  if (! expect (3 | 8, typeid (F1)))
    return 5;
  
  if (!expect (4, typeid (P1)))
    return 6;
  if (!expect (12, typeid (P2)))
    return 7;
  
  return 0;
}

#else
int main ()
{
  return 0;
}
#endif
