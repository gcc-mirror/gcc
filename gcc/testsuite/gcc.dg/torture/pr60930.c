/* { dg-do run } */

int x = 1;

__attribute__((noinline, noclone)) void
foo (unsigned long long t)
{
  asm volatile ("" : : "r" (&t));
  if (t == 1)
    __builtin_abort ();
}

int
main ()
{
#if __SIZEOF_LONG_LONG__ >= 8
  unsigned long long t = 0xffffffffffffffffULL * (0xffffffffUL * x);
  if (t != 0xffffffff00000001ULL)
    foo (t);;
#endif
  return 0;
}
