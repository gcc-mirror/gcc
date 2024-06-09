/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

void bar ()
{
  int j;
  *(volatile int *)&j = 0;
}

/* { dg-final { scan-assembler-times "mov" 1 } } */
