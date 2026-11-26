/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* RISC-V has no sge/sgeu, so the inverse of a comparison is computed as a
   set-on-less-than followed by xori 1.  When two such inverted values, each
   known to be 0 or 1, are ORed, (!A | !B) is !(A & B).  */

int
f_lt (long a, long b, long c, long d)
{
  return !(a < b) | !(c < d);
}

int
f_gt (long a, long b, long c, long d)
{
  return !(a < b) | !(c > d);
}

int
f_ltu (unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
  return !(a < b) | !(c < d);
}

/* 32-bit operands.  */
int
f_si (int a, int b, int c, int d)
{
  return !(a < b) | !(c < d);
}

/* { dg-final { scan-assembler-times "\tand\t" 4 } } */
/* { dg-final { scan-assembler-times "\txori\t" 4 } } */
/* { dg-final { scan-assembler-not "\tor\t" } } */
