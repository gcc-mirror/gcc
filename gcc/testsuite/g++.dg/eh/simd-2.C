// Test EH when V4SI SIMD registers are involved.
// Contributed by Aldy Hernandez (aldy@quesejoda.com).
// { dg-options "-O -Wno-abi" }
// { dg-options "-O -w -msse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-options "-O -w" { target powerpc*-*-* } }
// { dg-options "-O -w -maltivec" { target { powerpc*-*-* && vmx_hw } } }
// { dg-do run }
// { dg-require-effective-target sse_runtime { target { { i?86-*-* x86_64-*-* } && ilp32 } } }

#include "check-vect.h"

typedef int __attribute__((vector_size (16))) vecint;

vecint vecfunc (vecint beachbum)
{
  return beachbum;
}

void f3 (void)
{
  vecint foobar = (vecint) {0, 0};
  foobar = vecfunc (foobar);

  throw int();
}

void f2 (void)
{
  vecint foobar = (vecint) {0, 0};
  foobar = vecfunc (foobar);

  f3 ();
}

void f1 (void)
{
  int i;
  try
    {
      f2 ();
    }
  catch (int)
    {
      i = 9;
    }
}

int main ()
{
  /* Exit with zero if the hardware does not support AltiVec instructions.  */
  check_vect ();
  f1 ();
  return 0;
}
