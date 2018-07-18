/* { dg-do compile } */
/* { dg-options "-O3 -msse2" } */

float A[1024];
float B[1024];
int s;

void foo(void)
{
  int i;
  for (i = 0; i < 128; i++)
    {
      B[i*2+0] = A[i*s+0];
      B[i*2+1] = A[i*s+1];
    }
}

/* { dg-final { scan-assembler-not "\\\(%.sp\\\)" } } */
