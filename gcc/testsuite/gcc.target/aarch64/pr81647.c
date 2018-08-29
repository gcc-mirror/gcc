/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-ssa" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

double x[28], y[28];
int res[28];

int
main (void)
{
  int i;
  for (i = 0; i < 28; ++i)
    {
      x[i] = __builtin_nan ("");
      y[i] = i;
    }
  __asm__ volatile ("" ::: "memory");
  feclearexcept (FE_ALL_EXCEPT);
  for (i = 0; i < 4; ++i)
    res[i] = __builtin_isgreater (x[i], y[i]);
  for (i = 4; i < 8; ++i)
    res[i] = __builtin_isgreaterequal (x[i], y[i]);
  for (i = 8; i < 12; ++i)
    res[i] = __builtin_isless (x[i], y[i]);
  for (i = 12; i < 16; ++i)
    res[i] = __builtin_islessequal (x[i], y[i]);
  for (i = 16; i < 20; ++i)
    res[i] = __builtin_islessgreater (x[i], y[i]);
  for (i = 20; i < 24; ++i)
    res[i] = __builtin_isunordered (x[i], y[i]);
  for (i = 24; i < 28; ++i)
    res[i] = !(__builtin_isunordered (x[i], y[i]));
  __asm__ volatile ("" ::: "memory");
  return fetestexcept (FE_ALL_EXCEPT) != 0;
}

/* { dg-final { scan-tree-dump " u> " "ssa" } } */
/* { dg-final { scan-tree-dump " u>= " "ssa" } } */
/* { dg-final { scan-tree-dump " u< " "ssa" } } */
/* { dg-final { scan-tree-dump " u<= " "ssa" } } */
/* { dg-final { scan-tree-dump " u== " "ssa" } } */
/* { dg-final { scan-tree-dump " unord " "ssa" } } */
/* { dg-final { scan-tree-dump " ord " "ssa" } } */
