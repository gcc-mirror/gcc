/* { dg-do run } */
/* { dg-require-effective-target ultrasparc_vis2_hw } */
/* { dg-options "-mcpu=ultrasparc3 -O" } */

typedef unsigned int Vect __attribute__((vector_size(8)));

extern void abort (void);

Vect a, b, c, d;

__attribute__((noinline, noclone)) void test (void)
{
  Vect mask = { 2, 2 };
  int i;

  c = __builtin_shuffle (a, mask);
  d = __builtin_shuffle (a, b, mask);

  __asm__ ("" : : "r" (&c), "r" (&d) : "memory");

  for (i = 0; i < 2; ++i)
    if (c[i] != a[mask[i] & 1])
      abort ();
    else if (mask[i] & 2)
      {
	if (d[i] != b[mask[i] & 1])
	  abort ();
      }
}

int main (void)
{
  int i;
  for (i = 0; i < 2; ++i)
    {
      a[i] = i + 2;
      b[i] = 2 + i + 2;
    }

  test ();
  return 0;
}
