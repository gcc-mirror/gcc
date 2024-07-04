/* { dg-add-options ieee } */
/* { dg-additional-options "-fno-associative-math -fsigned-zeros" } */

double x[4] = {-0.0, 0.0, -0.0, 0.0};
float y[8] = {-0.0, 0.0, -0.0, 0.0, -0.0, -0.0, 0.0, 0.0};

static __attribute__ ((always_inline)) inline void
test (int factor)
{
  double a[4];
  float b[8];

  asm ("" ::: "memory");

  for (int i = 0; i < 2 * factor; i++)
    a[i] = -x[i];

  for (int i = 0; i < 4 * factor; i++)
    b[i] = -y[i];

#pragma GCC novector
  for (int i = 0; i < 2 * factor; i++)
    if (__builtin_signbit (a[i]) == __builtin_signbit (x[i]))
      __builtin_abort ();

#pragma GCC novector
  for (int i = 0; i < 4 * factor; i++)
    if (__builtin_signbit (b[i]) == __builtin_signbit (y[i]))
      __builtin_abort ();
}

int
main (void)
{
  test (1);
  test (2);
  return 0;
}
