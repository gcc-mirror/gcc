/* { dg-do run } */
/* { dg-options "-O3 -march=native" } */

extern void abort (void);

#define N 1024

float a[N], b[N], c[N];
int k[N];

__attribute__((noinline, noclone)) void
f (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      a[i] = b[k[i]];
      b[i] = c[i];
    }
}

int main ()
{
  int i;

  for (i = 0; i < N; i++)
  {
    k[i] = i%2;
    b[i] = i;
    c[i] = 179;
  }

  f ();

  if (a[2] != 179 || a[3] != 179)
    abort ();

  return 0;
}
