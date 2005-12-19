/* { dg-do compile { target mt-*-* } } */
/* { dg-options "-O2 -march=ms2" } */
/* { dg-final { scan-assembler "\tloopi " } } */

/* Make sure we generate loopi */

void Const (volatile int *ptr)
{
  int i;

  for (i = 0; i != 10; i++)
    *ptr;
}
