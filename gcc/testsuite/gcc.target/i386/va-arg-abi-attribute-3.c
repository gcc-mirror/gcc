/* ix86_gimplify_va_arg must read an unnamed 512-bit vector from the stack
   area of a SysV va_list, also in a function using the MS ABI.  */
/* { dg-do run { target { lp64 || llp64 } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f_runtime } */

#include "avx512f-check.h"

typedef int v16si __attribute__ ((vector_size (64)));

#define READER(name, abi)						\
static __attribute__ ((abi, noipa)) void				\
name (__builtin_sysv_va_list ap)					\
{									\
  int i = __builtin_va_arg (ap, int);					\
  v16si v = __builtin_va_arg (ap, v16si);				\
  int j = __builtin_va_arg (ap, int);					\
  int k;								\
  if (i != 1 || j != 18)						\
    abort ();								\
  for (k = 0; k < 16; k++)						\
    if (v[k] != k + 2)							\
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
test_512 (void)
{
  v16si v = { 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 };

  sysv_varargs (0, 1, v, 18);
}
