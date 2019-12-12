/* { dg-do run } */
/* { dg-options "-O3 -mzarch --save-temps" } */

#include <stddef.h>
#include <limits.h>

int __attribute__((noinline,noclone))
sadd (int a, int b, int *res)
{
   return __builtin_sadd_overflow(a, b, res);
}

int __attribute__((noinline,noclone))
ssub (int a, int b, int *res)
{
   return __builtin_ssub_overflow(a, b, res);
}


int __attribute__((noinline,noclone))
saddl (long a, long b, long *res)
{
   return __builtin_saddl_overflow(a, b, res);
}

int __attribute__((noinline,noclone))
ssubl (long a, long b, long *res)
{
   return __builtin_ssubl_overflow(a, b, res);
}


int __attribute__((noinline,noclone))
saddll (long long a, long long b, long long *res)
{
   return __builtin_saddll_overflow(a, b, res);
}

int __attribute__((noinline,noclone))
ssubll (long long a, long long b, long long *res)
{
   return __builtin_ssubll_overflow(a, b, res);
}


/* With the attribute at least main always uses the same instructions
   regardless of the -march setting.  This is necessary for the
   scan-assembler-times directive below.  */
int __attribute__ ((target("arch=z10")))
main ()
{
  int ret = 0;
  int result;
  long lresult;
  long long llresult;

  ret += !!sadd (INT_MAX, 1, &result);
  ret += !!ssub (INT_MIN, 1, &result);
  ret += !!saddl (LONG_MAX, 1, &lresult);
  ret += !!ssubl (LONG_MIN, 1, &lresult);
  ret += !!saddll (LLONG_MAX, 1, &llresult);
  ret += !!ssubll (LLONG_MIN, 1, &llresult);

  if (ret != 6)
    __builtin_abort ();

  return 0;
}

/* Check that no compare or bitop instructions are emitted.  */
/* { dg-final { scan-assembler-not "\tcr" } } */
/* { dg-final { scan-assembler-not "\txr" } } */
/* { dg-final { scan-assembler-not "\tnr" } } */
/* { dg-final { scan-assembler-not "\tcgr" } } */
/* { dg-final { scan-assembler-not "\txgr" } } */
/* { dg-final { scan-assembler-not "\tngr" } } */
/* On 31 bit the long long variants use risbgn to merge the 32 bit
   regs into a 64 bit reg.  */
/* { dg-final { scan-assembler-not "\trisbg" { target { lp64 } } } } */
/* Just one for the ret != 6 comparison.  */
/* { dg-final { scan-assembler-times "ci" 1 } } */
/* { dg-final { scan-assembler-times "\tlochio\t" 6 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\tlocghio\t" 6 { target lp64 } } } */
