/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar ()
{
  int j;
  *(volatile int *)&j = 0;
}

/* { dg-final { scan-assembler-times "mov" 1 } } */
