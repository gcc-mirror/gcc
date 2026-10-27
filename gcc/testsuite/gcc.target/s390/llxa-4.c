/* { dg-do compile } */
/* { dg-options "-O1 -march=arch15 -dp" } */
/* { dg-final { scan-assembler-times {\*llxab_displacement_index} 2 } } */
/* { dg-final { scan-assembler-not {\tllgfr\t} } } */

unsigned long
llxab_1 (unsigned long j)
{
  unsigned int i = (unsigned int)j + 42;
  return i;
}

unsigned char
llxab_2 (unsigned char *a, unsigned long j)
{
  unsigned int i = (unsigned int)j + 42;
  return a[i];
}
