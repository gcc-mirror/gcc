// { dg-do run }
// { dg-options "-O3" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Make sure that the call to terminate within F2 is not eliminated
// by incorrect MUST_NOT_THROW optimization.  Note that we expect F1
// to be inlined into F2 in order to expose this case.

#include <cstdlib>
#include <exception>

static volatile int zero = 0;

// Note that we need F0 to not be marked nothrow, though we don't actually
// want a throw to happen at runtime here.  The noinline tag is merely to
// make sure the assembly in F0 is not unnecessarily complex.
static void __attribute__((noinline)) f0()
{
  if (zero != 0)
    throw 0;
}

struct S1
{
  S1() { }
  ~S1() { f0(); }
};

static void f1()
{
  S1 s1;
  throw 1;
}

struct S2
{
  S2() { }
  ~S2() { f1(); }
};

static void __attribute__((noinline)) f2()
{
  S2 s2;
  throw 2;
}

static void pass()
{
  exit (0);
}

int main()
{
  std::set_terminate (pass);
  try {
    f2();
  } catch (...) {
  }
  abort ();
}
