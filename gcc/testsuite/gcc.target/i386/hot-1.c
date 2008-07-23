/* Test whether using attribute((hot)) really turns on -O3.  Do this test
   by checking whether we vectorize a simple loop.  */
/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -march=k8" } */
/* { dg-final { scan-assembler "addps" } } */
/* { dg-final { scan-assembler "subss" } } */

#define SIZE 1024
float a[SIZE] __attribute__((__aligned__(32)));
float b[SIZE] __attribute__((__aligned__(32)));
float c[SIZE] __attribute__((__aligned__(32)));

/* This should vectorize.  */
void hot (void) __attribute__((__hot__));

void
hot (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] + c[i];
}

/* This should not vectorize.  */
void
not_hot (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] - c[i];
}
