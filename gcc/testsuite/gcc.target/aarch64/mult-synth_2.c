/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

int
foo (int x)
{
  return x * 25;
}

/* { dg-final { scan-assembler-times "mul\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 1 } } */
