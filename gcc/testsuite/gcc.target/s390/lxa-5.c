/* { dg-do compile } */
/* { dg-options "-O1 -march=arch15 -dp" } */
/* { dg-final { scan-assembler-times {\*lxab_displacement_index} 2 } } */
/* { dg-final { scan-assembler-not {\tlgfr\t} } } */

long
lxab_1 (long j)
{
  int i = (int)j + 42;
  return i;
}

char
lxab_2 (char *a, long j)
{
  int i = (int)j + 42;
  return a[i];
}
