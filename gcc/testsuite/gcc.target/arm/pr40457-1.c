/* { dg-options "-Os" }  */
/* { dg-do compile } */

int bar(int* p)
{
  int x = p[0] + p[1];
  return x;
}

/* { dg-final { scan-assembler "ldrd|ldm" } } */
