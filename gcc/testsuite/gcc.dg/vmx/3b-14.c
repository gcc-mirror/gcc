#include "harness.h"

static vector bool char x(void);
static void g(void);

static vector bool char
f (void) 
{
  vector bool char a = x();
  g();
  return a;
}

static vector bool char
x (void)
{
  static vector bool char zero;
  return zero;
}

static void g ()
{
}

static void test()
{
  static vector bool char zero;
  check(vec_all_eq(f(), zero), "f");
}
