/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbb -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* RV32 version of zbb-andn-xor1-1.c.  The inverse of a comparison is
   computed as a set-on-less-than followed by xori 1.  When that value is
   ANDed with another comparison result, which is known to be 0 or 1, the
   xori is equivalent to a NOT and the pair folds into a single andn.  */

int
f_lt (int a, int b, int c, int d)
{
  return (a < b) & !(c < d);
}

int
f_gt (int a, int b, int c, int d)
{
  return (a < b) & !(c > d);
}

int
f_ltu (unsigned a, unsigned b, unsigned c, unsigned d)
{
  return (a < b) & !(c < d);
}

int
f_gtu (unsigned a, unsigned b, unsigned c, unsigned d)
{
  return (a < b) & !(c > d);
}

/* The inverted comparison as the first operand.  */
int
f_swapped (int a, int b, int c, int d)
{
  return !(c < d) & (a < b);
}

/* The other operand is itself an AND of comparisons.  */
int
f_and (int a, int b, int c, int d, int e, int f)
{
  return ((a < b) & (e < f)) & !(c < d);
}

/* { dg-final { scan-assembler-times "\tandn\t" 6 } } */
/* { dg-final { scan-assembler-not "\txori\t" } } */
