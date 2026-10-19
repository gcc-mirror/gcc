/* ix86_gimplify_va_arg must read an unnamed 256-bit vector from the stack
   area of a SysV va_list, also in a function using the MS ABI.  */
/* { dg-do run { target { lp64 || llp64 } } } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx_runtime } */

#include "avx-check.h"

typedef int v8si __attribute__ ((vector_size (32)));

#define READER(name, abi)						\
static __attribute__ ((abi, noipa)) void				\
name (__builtin_sysv_va_list ap)					\
{									\
  int i = __builtin_va_arg (ap, int);					\
  v8si v = __builtin_va_arg (ap, v8si);					\
  int j = __builtin_va_arg (ap, int);					\
  if (i != 1								\
      || v[0] != 2 || v[1] != 3 || v[2] != 4 || v[3] != 5		\
      || v[4] != 6 || v[5] != 7 || v[6] != 8 || v[7] != 9		\
      || j != 10)							\
    abort ();								\
}

READER (sysv_reads_sysv, sysv_abi)
READER (ms_reads_sysv, ms_abi)

static __attribute__ ((sysv_abi, noipa)) void
sysv_varargs (int n, ...)
{
  __builtin_sysv_va_list ap;
  __builtin_sysv_va_start (ap, n);
  sysv_reads_sysv (ap);
  __builtin_sysv_va_end (ap);
  __builtin_sysv_va_start (ap, n);
  ms_reads_sysv (ap);
  __builtin_sysv_va_end (ap);
}

static void
avx_test (void)
{
  v8si v = { 2, 3, 4, 5, 6, 7, 8, 9 };

  sysv_varargs (0, 1, v, 10);
}
