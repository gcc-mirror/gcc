/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbb -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* RV32 version of zbb-minu-leu-1.c.  (A <=u C) | (B <=u C) is
   umin (A, B) <=u C, which takes two instructions instead of the three
   needed to compute each comparison and OR them.  */

int
f_or (unsigned a, unsigned b)
{
  return (a <= 12) | (b <= 12);
}

/* The operands in the other order.  */
int
f_swapped (unsigned a, unsigned b)
{
  return (b <= 12) | (a <= 12);
}

/* The constants differ, so there is no common bound to apply to the
   minimum.  This must not use minu.  */
int
f_diff (unsigned a, unsigned b)
{
  return (a <= 12) | (b <= 13);
}

/* { dg-final { scan-assembler-times "\tminu\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsltiu\t" 4 } } */
/* { dg-final { scan-assembler-times "\tor\t" 1 } } */
