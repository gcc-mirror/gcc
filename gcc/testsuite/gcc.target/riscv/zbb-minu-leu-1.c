/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* (A <=u C) | (B <=u C) is umin (A, B) <=u C, which takes two instructions
   instead of the three needed to compute each comparison and OR them.  */

int
f_or (unsigned long a, unsigned long b)
{
  return (a <= 12) | (b <= 12);
}

/* The operands in the other order.  */
int
f_swapped (unsigned long a, unsigned long b)
{
  return (b <= 12) | (a <= 12);
}

/* The constants differ, so there is no common bound to apply to the
   minimum.  This must not use minu.  */
int
f_diff (unsigned long a, unsigned long b)
{
  return (a <= 12) | (b <= 13);
}

/* { dg-final { scan-assembler-times "\tminu\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsltiu\t" 4 } } */
/* { dg-final { scan-assembler-times "\tor\t" 1 } } */
