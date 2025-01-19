/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch -save-temps -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "\\.USUBC \\(\[^,\]+, \[^,\]+, 0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\.USUBC \\(\[^,\]+, \[^,\]+, _.+\\)" 1 "optimized" } } */
/* { dg-final { scan-assembler-times "slg\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "alcgr\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "slbg\t" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "sl\t" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "alcr\t" 1 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "slb\t" 1 { target { ! lp64 } } } } */

static unsigned long
usubc (unsigned long x, unsigned long y, unsigned long carry_in, unsigned long *carry_out)
{
  unsigned long r;
  unsigned long c1 = __builtin_sub_overflow (x, y, &r);
  unsigned long c2 = __builtin_sub_overflow (r, carry_in, &r);
  *carry_out = c1 + c2;
  return r;
}


void
test (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = usubc (p[0], q[0], 0, &c);
  p[1] = usubc (p[1], q[1], c, &c);
}
