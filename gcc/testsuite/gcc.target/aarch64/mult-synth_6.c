/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

int
foo (int x)
{
  return x * 20;
}

/* { dg-final { scan-assembler-not "\tw1" } } */
