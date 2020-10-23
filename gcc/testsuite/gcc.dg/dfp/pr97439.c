// { dg-do run }
// { dg-options "-O1" }

static int
foo(_Decimal128 x, _Decimal128 y)
{
  if (x > y)
    return 1;

  return 0;
}

int __attribute__((noinline))
bar(_Decimal128 x)
{
  return foo (x, -1.0DL * __builtin_infd32());
}

int
main (void)
{
  int res = bar (0.0DL);
  if (res != 1)
    __builtin_abort ();

  return 0;
}
