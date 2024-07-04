/* { dg-do compile { target lp64 } } */
/* { dg-options "-O3 -mstrict-align" } */

struct SS
{
  int t, t1;
};

int
f (struct SS b)
{
  return b.t + b.t1;
}

/* { dg-final { scan-assembler-not {\mstd\M} } } */
