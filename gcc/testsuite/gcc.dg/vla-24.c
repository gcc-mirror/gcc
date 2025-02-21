/* PR middle-end/65958 */

/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

extern void abort (void);

int foo (int n)
{
  char *p, *q;

  if (1)
    {
      char i[n];
      p = __builtin_alloca (8);
      p[0] = 1;
    }

  q = __builtin_alloca (64);
  __builtin_memset (q, 0, 64);

  return !p[0];
}

int main (void)
{
  if (foo (48) != 0)
    abort ();

  return 0;
}
