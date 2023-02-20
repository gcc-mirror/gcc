/* { dg-do compile { target c++11 } } */
/* { dg-options "-Wstrict-aliasing=2 -O2 -Wall" } */

#include <type_traits>

struct foo
{
  std::aligned_storage<sizeof(long), alignof(long)>::type raw;	/* { dg-warning "deprecated" "" { target c++23 } } */

  long& cooked()
    {
      return *static_cast<long*>(static_cast<void*>(&raw)); /* { dg-bogus "strict-aliasing" } */
    }
};
