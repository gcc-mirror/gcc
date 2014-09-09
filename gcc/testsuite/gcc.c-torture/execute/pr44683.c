int __attribute__((noinline,noclone))
copysign_bug (double x)
{
  if (x != 0.0 && (x * 0.5 == x))
    return 1;
  if (__builtin_copysign(1.0, x) < 0.0)
    return 2;
  else
    return 3;
}
int main(void)
{
  double x = -0.0;
  if (copysign_bug (x) != 2)
    __builtin_abort ();
  return 0;
}

