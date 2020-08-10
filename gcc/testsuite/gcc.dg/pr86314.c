// PR target/86314
// { dg-do run { target sync_int_long_stack } }
// { dg-options "-O2" }

__attribute__((noinline, noclone)) unsigned long
foo (unsigned long *p)
{
  unsigned long m = 1UL << ((*p & 1) ? 1 : 0);
  unsigned long n = __atomic_fetch_or (p, m, __ATOMIC_SEQ_CST);
  return (n & m) == 0;
}

int
main ()
{
  unsigned long v = 1;
  if (foo (&v) != 1)
    __builtin_abort ();
  return 0;
}
