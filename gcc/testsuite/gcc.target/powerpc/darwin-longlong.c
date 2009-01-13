/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc64 } */
/* { dg-options "-mcpu=G5" } */

#include <stdlib.h>

int  msw(long long in)
{
  union {
    long long ll;
    int  i[2];
  } ud;
  ud.ll = in;
  return ud.i[0];
}

int main()
{
  if (msw(1) != 0)
    abort();
  exit(0);
}
