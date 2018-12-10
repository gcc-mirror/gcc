/* { dg-require-effective-target indirect_jumps } */

extern void abort (void);

__attribute ((noinline)) void
broken_longjmp (void *p)
{
  void *buf[5];
  __builtin_memcpy (buf, p, 5 * sizeof (void*));
  /* Corrupts stack pointer...  */
  __builtin_longjmp (buf, 1);
}

__attribute ((noipa)) __UINTPTR_TYPE__
foo (void *p)
{
  return (__UINTPTR_TYPE__) p;
}

__attribute ((noipa)) void
bar (void *p)
{
  asm volatile ("" : : "r" (p));
}

volatile int x = 0;
void *volatile p;
void *volatile q;

int
main ()
{
  void *buf[5];
  struct __attribute__((aligned (32))) S { int a[4]; } s;
  bar (&s);
  p = __builtin_alloca (x);
  if (!__builtin_setjmp (buf))
    broken_longjmp (buf);

  /* Fails if stack pointer corrupted.  */
  q = __builtin_alloca (x);
  if (foo (p) < foo (q))
    {
      if (foo (q) - foo (p) >= 1024)
	abort ();
    }
  else if (foo (p) - foo (q) >= 1024)
    abort ();

  return 0;
}
