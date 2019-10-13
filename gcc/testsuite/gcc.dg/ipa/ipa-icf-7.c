/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

#include <complex.h>

#if (__SIZEOF_INT__ == __SIZEOF_FLOAT__)
typedef int intflt;
#elif (__SIZEOF_LONG__ == __SIZEOF_FLOAT__)
typedef long intflt;
#else
#error Add target support here for type that will union float size
#endif


static double test;

struct struktura
{
  union
  {
    long i;
    float f;
  } u;
};

struct struktura sss;

struct X
{
  int i;
  union
  {
    intflt j;
    intflt k;
    float f;
  } u;
};

__attribute__ ((noinline))
intflt foo(intflt j)
{
  struct X a;

  a.u.j = j;
  a.u.f = a.u.f;
  a.u.f = a.u.f;
  a.u.j = a.u.j;
  a.u.f = a.u.f;
  return a.u.k;
}

__attribute__ ((noinline))
intflt foo2(intflt j)
{
  struct X a;

  a.u.j = j;
  a.u.f = a.u.f;
  a.u.f = a.u.f;
  a.u.j = a.u.j;
  a.u.f = a.u.f;
  return a.u.k;
}

int main()
{
  return 1;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:foo->foo2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
