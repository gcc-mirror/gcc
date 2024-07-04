volatile int v = 0;

void __attribute__((noinline))
foo (int *p)
{
  *p = 1234;
  if (v)
    *p = 0;
  return;
}
