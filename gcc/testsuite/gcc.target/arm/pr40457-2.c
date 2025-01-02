/* { dg-options "-O2" }  */
/* { dg-do compile } */

void foo(int* p)
{
  p[0] = 1;
  p[1] = 0;
}

/* { dg-final { scan-assembler "strd|stm|vst1\\.32" } } */
