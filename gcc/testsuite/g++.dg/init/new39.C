// Testcase for overflow handling in operator new[].
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for stdexcept" { ! hostedlib } }

#include <stdlib.h>
#include <stdexcept>

struct without_new {
  char bar[256];
};

struct with_new {
  char bar[256];
  void *operator new[] (size_t sz)
  {
    if (sz != -1)
      abort ();
    throw std::bad_alloc();
  }
};

template <typename T>
inline void
test (size_t s)
{
  try {
    new T[s];
    abort ();
  } catch (std::bad_alloc &) {
  }
}

template <typename T>
void
test_noopt (size_t s) __attribute__((noinline));

template <typename T>
void
test_noopt (size_t s)
{
  __asm__ ("");
  test<T> (s);
}

template <typename T>
void
all_tests ()
{
  test<T>(-1);
  test<T>(size_t(-1) / sizeof (T) + 1);
  test<T>(size_t(-1) / sizeof (T) + 2);
  test_noopt<T>(-1);
  test_noopt<T>(size_t(-1) / sizeof (T) + 1);
  test_noopt<T>(size_t(-1) / sizeof (T) + 2);
}

int
main ()
{
  try {
    (void) ::operator new(size_t(-1));
    abort ();
  } catch (std::bad_alloc &) {
  }
  all_tests<without_new> ();
  all_tests<with_new> ();
  return 0;
}

