// { dg-additional-options "-fmodules-ts" }

export module frob;
// { dg-module-bmi frob }

namespace 
{
  int nope;
}

export int f (int = nope);
int g (int *a = &nope);
