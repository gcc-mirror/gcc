/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

long long
foo (int x, int y)
{
   return (long long)x * 6LL;
}

/* { dg-final { scan-assembler-times "smull\tx\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 1 } } */
