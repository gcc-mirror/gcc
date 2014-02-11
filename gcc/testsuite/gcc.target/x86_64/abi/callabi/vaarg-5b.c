/* Test for cross x86_64<->w64 abi va_list calls.  */
/* { dg-options "-O2 -mabi=ms -std=gnu99 -fno-builtin" } */

#include <stdarg.h>

#define SZ_ARGS	1ll,2ll,3ll,4ll,5ll,6ll,7ll,0ll

static int __attribute__ ((sysv_abi))
fct1 (va_list argp, ...)
{
  long long p1,p2;
  int ret = 1;
  __builtin_sysv_va_list argp_2;

  __builtin_sysv_va_start (argp_2, argp);
  do {
    p1 = va_arg (argp_2, long long);
    p2 = va_arg (argp, long long);
    if (p1 != p2)
      ret = 0;
  } while (ret && p1 != 0);
  __builtin_sysv_va_end (argp_2);

  return ret;
}

int
fct2 (int dummy, ...)
{
  va_list argp;
  int ret = dummy;

  va_start (argp, dummy);
  ret += fct1 (argp, SZ_ARGS);
  va_end (argp);
  return ret;
}
