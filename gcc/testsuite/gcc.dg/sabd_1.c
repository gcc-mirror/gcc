/* { dg-do run } */
/* { dg-options "-O3 -fwrapv" } */
/* Make sure vectorized absolute difference behaves same as scalar version.  */

#define N 16
signed char a[] = {-100, -100, -100, -100,-100, -100, -100, -100, -100, -100, -100, -100, -100, -100, -100, -100 };
signed char b[] = { 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100 };

signed char out[N];

__attribute__ ((noinline,noipa))
void
foo (void)
{
  for (int i = 0; i < N; i++)
    {
      signed char diff = b[i] - a[i];
      out[i] = diff > 0 ? diff : -diff;
    }
}

signed char out2[N];

__attribute__ ((noinline,noipa))
void
foo_scalar (void)
{
  for (int i = 0; i < N; i++)
    {
      asm volatile ("");
      signed char diff = b[i] - a[i];
      out2[i] = diff > 0 ? diff : -diff;
    }
}

int
main (void)
{
  foo ();
  foo_scalar ();
  for (int i = 0; i < N; i++)
    if (out[i] != out2[i])
      __builtin_abort ();

  return 0;
}

