/* Test MIPS32 DSP REV 2 instructions */
/* { dg-do compile } */
/* { dg-mips-options "-march=mips32r2 -mdspr2" } */
/* { dg-final { scan-assembler "\tmul.ph\t" } } */

#define NOMIPS16 __attribute__ ((nomips16))

typedef short v2hi __attribute__ ((vector_size(4)));

NOMIPS16 v2hi mul_v2hi (v2hi a, v2hi b)
{
  return a * b;
}

