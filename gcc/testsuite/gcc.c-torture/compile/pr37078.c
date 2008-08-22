int foo (int b)
{
  if (b == (int)0x80000000)
    return __builtin_abs (b);
  return 0;
}
