/* { dg-additional-options "-O3 -fno-vect-cost-model" } */
struct {
    float real;
    float img;
} g[11];

float __attribute__ ((noclone))
foo_11 (void)
{
  float sum = 0.0;
  for (int i = 0; i < 11; ++i)
    sum += g[i].real;
  return sum;
}

float __attribute__ ((noclone))
foo_10 (void)
{
  float sum = 0.0;
  for (int i = 0; i < 10; ++i)
    sum += g[i].real;
  return sum;
}

int main (void)
{
  float check_10 = 0.0;
  float check_11 = 0.0;
  for (int i = 0; i < 11; ++i)
    {
      asm volatile ("" : : : "memory");
      g[i].real = (float) i;
      g[i].img = (float) -i;
      if (i < 10)
	check_10 += (float) i;
      check_11 += (float) i;
    }

  if (foo_10 () != check_10)
    __builtin_abort ();
  if (foo_11 () != check_11)
    __builtin_abort ();

  return 0;
}
