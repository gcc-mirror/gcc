int __attribute__((noinline,noclone)) bar (int (*fn)(int *), int *p)
{
  return fn (p);
}
