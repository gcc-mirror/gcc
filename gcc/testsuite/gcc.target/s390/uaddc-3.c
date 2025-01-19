/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch -save-temps -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "\\.UADDC \\(\[^,\]+, \[^,\]+, 0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\.UADDC \\(\[^,\]+, \[^,\]+, _.+\\)" 1 "optimized" } } */
/* { dg-final { scan-assembler-times "alg\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "ag\t" 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "al\t" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "a\t" 2 { target { ! lp64 } } } } */

static unsigned long
uaddc (unsigned long x, unsigned long y, unsigned long carry_in, unsigned long *carry_out)
{
  unsigned long r;
  unsigned long c1 = __builtin_add_overflow (x, y, &r);
  unsigned long c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 + c2;
  return r;
}

void
test (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = uaddc (p[0], q[0], 0, &c);
  p[1] = uaddc (p[1], q[1], c, &c);
}

