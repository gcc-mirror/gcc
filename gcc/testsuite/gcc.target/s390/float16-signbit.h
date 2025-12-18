[[gnu::noipa]] int
signbit_reg (_Float16 x)
{
  return __builtin_signbit (x);
}

[[gnu::noipa]] int
signbit_mem (_Float16 *x)
{
  return __builtin_signbit (*x);
}

int
main (void)
{
  _Float16 x;
  int res = 0;

  x = __builtin_nanf16 ("42");
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = __builtin_inff16 ();
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = 0.f16;
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = 42.42f16;
  res += signbit_reg (x);
  res += signbit_mem (&x);

  if (res != 0)
    __builtin_abort ();

  x = -__builtin_nanf16 ("42");
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = -__builtin_inff16 ();
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = -0.f16;
  res += signbit_reg (x);
  res += signbit_mem (&x);

  x = -42.42f16;
  res += signbit_reg (x);
  res += signbit_mem (&x);

  if (res != 8)
    __builtin_abort ();
}
