/* { dg-do compile { target lp64 } } */
/* { dg-options "-O3 -fno-vect-cost-model -fdump-tree-slp2-details" } */

struct x { double d[2]; };

struct x
pack (double a, double aa)
{
  struct x u;
  u.d[0] = a;
  u.d[1] = aa;
  return u;
}

/* The function should be optimized to just return as arguments and
   result exactly overlap even when previously vectorized.  */

/* { dg-final { scan-tree-dump "basic block vectorized" "slp2" } } */
/* { dg-final { scan-assembler-not "mov" } } */
