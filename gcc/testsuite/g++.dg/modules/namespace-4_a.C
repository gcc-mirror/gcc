// { dg-additional-options "-fmodules-ts" }

export module frob;
// { dg-module-bmi frob }

namespace
{
  int nope;
}

export int f (int)
{
  return nope;
}

int g (int *a);
