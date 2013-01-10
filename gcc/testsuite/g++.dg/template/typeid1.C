// PR c++/55878

#include <typeinfo>

struct S;

template <typename T>
static bool fn (S *s)
{
  return typeid (*s) == typeid (T);
}

struct S
{
};

bool x = fn<S> (__null);
