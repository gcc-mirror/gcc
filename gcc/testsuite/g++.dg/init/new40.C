// Testcase for overflow handling in operator new[].
// Optimization of unnecessary overflow checks.
// In C++11 we throw bad_array_new_length instead.
// { dg-options -std=c++03 }
// { dg-do run }

#include <assert.h>
#include <stdlib.h>
#include <stdexcept>

static size_t magic_allocation_size
  = 1 + (size_t (1) << (sizeof (size_t) * 8 - 1));

struct exc : std::bad_alloc {
};

static size_t expected_size;

struct pod_with_new {
  char ch;
  void *operator new[] (size_t sz)
  {
    if (sz != expected_size)
      abort ();
    throw exc ();
  }
};

struct with_new {
  char ch;
  with_new () { }
  ~with_new () { }
  void *operator new[] (size_t sz)
  {
    if (sz != size_t (-1))
      abort ();
    throw exc ();
  }
};

struct non_pod {
  char ch;
  non_pod () { }
  ~non_pod () { }
};

void *
operator new (size_t sz) _GLIBCXX_THROW (std::bad_alloc)
{
  if (sz != expected_size)
    abort ();
  throw exc ();
}

int
main ()
{
  if (sizeof (pod_with_new) == 1)
    expected_size = magic_allocation_size;
  else
    expected_size = -1;

  try {
    new pod_with_new[magic_allocation_size];
    abort ();
  } catch (exc &) {
  }

  if (sizeof (with_new) == 1)
    expected_size = magic_allocation_size;
  else
    expected_size = -1;

  try {
    new with_new[magic_allocation_size];
    abort ();
  } catch (exc &) {
  }

  expected_size = magic_allocation_size;
  try {
    new char[magic_allocation_size];
    abort ();
  } catch (exc &) {
  }

  expected_size = -1;

  try {
    new pod_with_new[magic_allocation_size][2];
    abort ();
  } catch (exc &) {
  }

  try {
    new with_new[magic_allocation_size][2];
    abort ();
  } catch (exc &) {
  }

  try {
    new char[magic_allocation_size][2];
    abort ();
  } catch (exc &) {
  }

  try {
    new non_pod[magic_allocation_size];
    abort ();
  } catch (exc &) {
  }

  return 0;
}
