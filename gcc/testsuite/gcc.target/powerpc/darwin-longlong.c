/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc64 } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=G5" } } */
/* { dg-options "-mcpu=G5" } */

#include <stdlib.h>

int  msw(long long in)
{
  union {
    long long ll;
    int  i[2];
  } ud;
  ud.ll = in;
#ifdef __LITTLE_ENDIAN__
  return ud.i[1];
#else
  return ud.i[0];
#endif
}

int main()
{
  if (msw(1) != 0)
    abort();
  exit(0);
}
