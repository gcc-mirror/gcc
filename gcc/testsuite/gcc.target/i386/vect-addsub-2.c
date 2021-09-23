/* { dg-do compile } */
/* { dg-require-effective-target sse3 } */
/* { dg-options "-O3 -msse3" } */

float a[1024], b[1024];

void foo()
{
  for (int i = 0; i < 256; i++)
    {
      a[4*i+0] = a[4*i+0] - b[4*i+0];
      a[4*i+1] = a[4*i+1] + b[4*i+1];
      a[4*i+2] = a[4*i+2] - b[4*i+2];
      a[4*i+3] = a[4*i+3] + b[4*i+3];
    }
}

/* We should be able to vectorize this with SLP using the addsub
   SLP pattern.  */
/* { dg-final { scan-assembler "addsubps" } } */
/* { dg-final { scan-assembler-not "shuf" } } */
