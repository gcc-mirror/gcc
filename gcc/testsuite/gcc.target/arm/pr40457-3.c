/* { dg-options "-Os" }  */
/* { dg-do compile } */

void foo(int* p)
{
  p[0] = 1;
  p[1] = 0;
  p[2] = 2;
}

/* { dg-final { scan-assembler "stm" } } */
