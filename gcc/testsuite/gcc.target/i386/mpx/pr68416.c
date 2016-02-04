/* { dg-do compile } */
/* { dg-options "-O2 -mmpx -fcheck-pointer-bounds" } */
/* { dg-final { scan-assembler-not "bndmov" } } */

int
foo(int **arr, int i)
{
  return (*arr)[i];
}
