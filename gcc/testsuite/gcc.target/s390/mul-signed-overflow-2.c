/* { dg-do compile } */
/* { dg-do run { target { s390_z14_hw } } } */
/* z14 only because we need msrkc, msc, msgrkc, msgc  */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */

#include <stddef.h>
#include <limits.h>

int __attribute__((noinline,noclone))
smul (int a, int *res)
{
   return __builtin_smul_overflow(a, -1, res);
}

int __attribute__((noinline,noclone))
smull (long a, long *res)
{
   return __builtin_smull_overflow(a, -1, res);
}

int __attribute__((noinline,noclone))
smulll (long long a, long long *res)
{
   return __builtin_smulll_overflow(a, -1, res);
}


int
main ()
{
  int ret = 0;
  int result;
  long lresult;
  long long llresult;

  ret += !!smul (INT_MIN, &result);
  ret += !!smull (LONG_MIN, &lresult);
  ret += !!smulll (LLONG_MIN, &llresult);

  if (ret != 3)
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
/* Just one for the ret != 3 comparison.  */
/* { dg-final { scan-assembler-times "ci" 1 } } */
/* { dg-final { scan-assembler-times "\tlochio\t" 3 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\tlocghio\t" 3 { target lp64 } } } */
