/* Test MIPS32 DSP LHX instruction */
/* { dg-do compile } */
/* { dg-options "-mgp32 -mdsp -O2" } */

/* { dg-final { scan-assembler "\tlhx\t" } } */

NOMIPS16 signed short test (signed short *a, int index)
{
  return a[index];
}
