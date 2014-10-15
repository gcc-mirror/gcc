// { dg-options "-fno-lto" }

#include "pr63270.h"

int v8::internal::G::test()
{
  return 2;
}
