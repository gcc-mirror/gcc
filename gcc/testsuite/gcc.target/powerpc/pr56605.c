/* PR rtl-optimization/56605 */
/* { dg-do compile { target { powerpc64-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O3 -mvsx -mcpu=power7 -fno-unroll-loops -fdump-rtl-loop2_doloop" } */

void foo (short* __restrict sb, int* __restrict ia)
{
  int i;
  for (i = 0; i < 4000; i++)
    ia[i] = (int) sb[i];
}

/* { dg-final { scan-rtl-dump-times "\\\(compare:CC \\\(subreg:SI \\\(reg:DI" 1 "loop2_doloop" } } */
