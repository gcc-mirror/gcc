/* { dg-do run } */
/* { dg-options "-O2" } */

int
my_totalorderf (float const *x, float const *y)
{
  int xs = __builtin_signbit (*x);
  int ys = __builtin_signbit (*y);
  if (!xs != !ys)
    return xs;

  int xn = __builtin_isnan (*x);
  int yn = __builtin_isnan (*y);
  if (!xn != !yn)
    return !xn == !xs;
  if (!xn)
    return *x <= *y;

  unsigned int extended_sign = -!!xs;
  union { unsigned int i; float f; } xu = {0}, yu = {0};
  __builtin_memcpy (&xu.f, x, sizeof (float));
  __builtin_memcpy (&yu.f, y, sizeof (float));
  return (xu.i ^ extended_sign) <= (yu.i ^ extended_sign);
}

static float
positive_NaNf ()
{
  float volatile nan = 0.0f / 0.0f;
  return (__builtin_signbit (nan) ? - nan : nan);
}

typedef union { float value; unsigned int word[1]; } memory_float;

static memory_float
construct_memory_SNaNf (float quiet_value)
{
  memory_float m;
  m.value = quiet_value;
  m.word[0] ^= (unsigned int) 1 << 22;
  m.word[0] |= (unsigned int) 1;
  return m;
}

memory_float x[7] =
  {
    { 0 },
    { 1e-5 },
    { 1 },
    { 1e37 },
    { 1.0f / 0.0f },
  };

int
main ()
{
  x[5] = construct_memory_SNaNf (positive_NaNf ());
  x[6] = (memory_float) { positive_NaNf () };
  if (! my_totalorderf (&x[5].value, &x[6].value))
    __builtin_abort ();
  return 0;
}
