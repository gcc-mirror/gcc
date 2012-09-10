/* Test MIPS32 DSP LHX instruction */
/* { dg-do compile } */
/* { dg-options "-mgp32 -mdsp" } */

/* { dg-final { scan-assembler-not "\tlhx\t" } } */

NOMIPS16 unsigned short test (unsigned short *a, int index)
{
  return a[index];
}
