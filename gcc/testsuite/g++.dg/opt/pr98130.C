// PR c++/98130
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

#include <new>

typedef int *T;

static unsigned char storage[sizeof (T)] alignas (T);
static T *p = (T *) storage;

static inline __attribute__((__always_inline__)) void
foo (T value)
{
  new (p) T(value);
}

int
main ()
{
  int a;
  foo (&a);
  if (!*p)
    __builtin_abort ();
}
