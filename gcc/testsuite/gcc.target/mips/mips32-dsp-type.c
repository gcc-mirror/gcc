/* Test MIPS32 DSP instructions */
/* { dg-do compile } */
/* { dg-mips-options "-march=mips32 -mdsp" } */
/* { dg-final { scan-assembler "addq.ph" } } */
/* { dg-final { scan-assembler "addu.qb" } } */
/* { dg-final { scan-assembler "subq.ph" } } */
/* { dg-final { scan-assembler "subu.qb" } } */

typedef char v4qi __attribute__ ((vector_size(4)));
typedef short v2hi __attribute__ ((vector_size(4)));

NOMIPS16 v2hi add_v2hi (v2hi a, v2hi b)
{
  return a + b;
}

NOMIPS16 v4qi add_v4qi (v4qi a, v4qi b)
{
  return a + b;
}

NOMIPS16 v2hi sub_v2hi (v2hi a, v2hi b)
{
  return a - b;
}

NOMIPS16 v4qi sub_v4qi (v4qi a, v4qi b)
{
  return a - b;
}
