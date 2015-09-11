/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

long
foo (int x, int y)
{
   return (long)x * 6L;
}

/* { dg-final { scan-assembler-times "smull\tx\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 1 } } */
