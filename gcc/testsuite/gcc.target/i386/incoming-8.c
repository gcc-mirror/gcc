/* PR target/40838 */
/* { dg-do compile { target { { ! *-*-darwin* } && ia32 } } } */
/* { dg-options "-w -mstackrealign -O3 -msse2 -mno-avx -mpreferred-stack-boundary=4" } */

float
foo (float f)
{
  float array[128];
  float x;
  int i;
  for (i = 0; i < sizeof(array) / sizeof(*array); i++)
    array[i] = f;
  for (i = 0; i < sizeof(array) / sizeof(*array); i++)
    x += array[i];
  return x;
}

/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%esp" } } */
