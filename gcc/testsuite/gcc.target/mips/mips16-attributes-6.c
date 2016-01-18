/* { dg-do compile } */
/* { dg-options "-mips16 addressing=absolute -mips3d forbid_cpu=octeon.*" } */

static inline NOMIPS16 float
i1 (float f)
{
  return __builtin_mips_recip1_s (f);
}

float f1 (float f) { return i1 (f); }

/* { dg-final { scan-assembler "\trecip1.s\t" } } */
/* { dg-final { scan-assembler "\tjal\ti1" } } */
