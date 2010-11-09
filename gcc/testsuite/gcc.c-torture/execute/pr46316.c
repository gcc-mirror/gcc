extern void abort (void);

long long __attribute__((noinline,noclone))
foo (long long t)
{
  while (t > -4)
    t -= 2;

  return t;
}

int main(void)
{
  if (foo (0) != -4)
    abort ();
  return 0;
}
