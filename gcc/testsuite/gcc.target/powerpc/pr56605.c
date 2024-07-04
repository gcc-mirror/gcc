/* PR rtl-optimization/56605 */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mvsx -mdejagnu-cpu=power7 -fno-unroll-loops -fdump-rtl-combine" } */
/* { dg-require-effective-target powerpc_vsx } */

void foo (short* __restrict sb, int* __restrict ia)
{
  int i;
  for (i = 0; i < 4000; i++)
    ia[i] = (int) sb[i];
}

/* { dg-final { scan-rtl-dump-times {\(compare:CC \(and:SI \(subreg:SI \(reg:DI} 1 "combine" } } */
