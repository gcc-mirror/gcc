/* { dg-do compile } */
/* { dg-options "-O2 -ftracer -w" } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */

void *foo(int i, int *p)
{
lab:
  if (p) *p = i;
  goto *p;
  return &&lab;
}

