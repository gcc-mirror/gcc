/* Test for cross x86_64<->w64 abi va_list calls.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */
#include "callabi.h"

extern void abort (void);

#define SZ_ARGS	1ll,2ll,3ll,4ll,5ll,6ll,7ll,0ll

static
int fct1 (CROSS_VA_LIST argp, ...)
{
  long long p1,p2;
  int ret = 1;
  va_list argp_2;

    __va_start (argp_2,argp);
  do {
    p1 = __va_arg (argp_2, long long);
    p2 = CROSS_VA_ARG (argp, long long);
    if (p1 != p2)
      ret = 0;
  } while (ret && p1 != 0);
  __va_end (argp_2);
  return ret;
}

static
int CALLABI_CROSS fct2 (int dummy, ...)
{
  CROSS_VA_LIST argp;
  int ret = dummy;

  CROSS_VA_START (argp, dummy);
  ret += fct1 (argp, SZ_ARGS);
  CROSS_VA_END (argp);
  return ret;
}

int main()
{
  if (fct2 (-1, SZ_ARGS) != 0)
    abort ();
  return 0;
}
