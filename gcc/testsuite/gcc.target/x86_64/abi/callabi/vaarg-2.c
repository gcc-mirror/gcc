/* Test for cross x86_64<->w64 abi va_list calls.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -maccumulate-outgoing-args" } */
#include "callabi.h"

extern void abort (void);

#define SZ_ARGS	1ll,2ll,3ll,4ll,5ll,6ll,7ll,0ll

static
int CALLABI_CROSS fct1 (va_list argp, ...)
{
  long long p1,p2;
  int ret = 1;
  CROSS_VA_LIST argp_2;
  CROSS_VA_START (argp_2,argp);

  do {
    p1 = CROSS_VA_ARG (argp_2, long long);
    p2 = __va_arg (argp, long long);
    if (p1 != p2)
      ret = 0;
  } while (ret && p1 != 0);
  CROSS_VA_END (argp_2);
  return ret;
}

static
int fct2 (int dummy, ...)
{
  va_list argp;
  int ret = dummy;

  __va_start (argp, dummy);
  ret += fct1 (argp, SZ_ARGS);
  __va_end (argp);
  return ret;
}

int main()
{
  if (fct2 (-1, SZ_ARGS) != 0)
    abort ();
  return 0;
}
