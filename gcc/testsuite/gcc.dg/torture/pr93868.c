/* { dg-do run } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned a[1024];
unsigned b[1024];

void __attribute__((noipa))
foo (unsigned *q, unsigned *r)
{
  unsigned sum1 = 0, sum2 = 0;
  for (int i = 0; i < 512; ++i)
    {
      sum1 += a[2*i];
      sum2 += a[2*i+1];
      b[2*i] = a[2*i+1];
      b[2*i+1] = a[2*i];
    }
  *q = sum1;
  *r = sum2;
}

int main()
{
  unsigned sum1, sum2;
  a[0] = 0;
  a[1] = 1;
  foo (&sum1, &sum2);
  if (b[0] != 1 || b[1] != 0)
    __builtin_abort ();
  return 0;
}
