extern void abort (void);

void test(double f, double i)
{
  if (f == __builtin_huge_val())
    abort ();
  if (f == -__builtin_huge_val())
    abort ();
  if (i == -__builtin_huge_val())
    abort ();
  if (i != __builtin_huge_val())
    abort ();

  if (f >= __builtin_huge_val())
    abort ();
  if (f > __builtin_huge_val())
    abort ();
  if (i > __builtin_huge_val())
    abort ();
  if (f <= -__builtin_huge_val())
    abort ();
  if (f < -__builtin_huge_val())
    abort ();
}

void testf(float f, float i)
{
  if (f == __builtin_huge_valf())
    abort ();
  if (f == -__builtin_huge_valf())
    abort ();
  if (i == -__builtin_huge_valf())
    abort ();
  if (i != __builtin_huge_valf())
    abort ();

  if (f >= __builtin_huge_valf())
    abort ();
  if (f > __builtin_huge_valf())
    abort ();
  if (i > __builtin_huge_valf())
    abort ();
  if (f <= -__builtin_huge_valf())
    abort ();
  if (f < -__builtin_huge_valf())
    abort ();
}

void testl(long double f, long double i)
{
  if (f == __builtin_huge_vall())
    abort ();
  if (f == -__builtin_huge_vall())
    abort ();
  if (i == -__builtin_huge_vall())
    abort ();
  if (i != __builtin_huge_vall())
    abort ();

  if (f >= __builtin_huge_vall())
    abort ();
  if (f > __builtin_huge_vall())
    abort ();
  if (i > __builtin_huge_vall())
    abort ();
  if (f <= -__builtin_huge_vall())
    abort ();
  if (f < -__builtin_huge_vall())
    abort ();
}

int main()
{
  test (34.0, __builtin_huge_val());
  testf (34.0f, __builtin_huge_valf());
  testl (34.0l, __builtin_huge_vall());
  return 0;
}

