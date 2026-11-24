/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* RISC-V has no sge/sgeu, so the inverse of a comparison is computed as a
   set-on-less-than followed by xori 1.  When that value is ANDed with
   another comparison result, which is known to be 0 or 1, the xori is
   equivalent to a NOT and the pair folds into a single andn.  */

int
f_lt (long a, long b, long c, long d)
{
  return (a < b) & !(c < d);
}

int
f_gt (long a, long b, long c, long d)
{
  return (a < b) & !(c > d);
}

int
f_ltu (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a < b) & !(c < d);
}

int
f_gtu (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return (a < b) & !(c > d);
}

/* The inverted comparison as the first operand.  */
int
f_swapped (long a, long b, long c, long d)
{
  return !(c < d) & (a < b);
}

/* 32-bit operands.  */
int
f_si (int a, int b, int c, int d)
{
  return (a < b) & !(c < d);
}

/* The other operand is itself an AND of comparisons.  */
int
f_and (long a, long b, long c, long d, long e, long f)
{
  return ((a < b) & (e < f)) & !(c < d);
}

/* { dg-final { scan-assembler-times "\tandn\t" 7 } } */
/* { dg-final { scan-assembler-not "\txori\t" } } */
