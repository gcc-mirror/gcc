// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Try to invoke a metafunction through a function pointer.

#include <meta>

using namespace std::meta;

consteval size_t
invoke (size_t (*fp)(info))
{
  return fp (^^int);
}

void
g ()
{
  static_assert (invoke (size_of) == sizeof (int));
}
