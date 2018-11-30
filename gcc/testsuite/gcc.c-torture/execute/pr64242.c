/* { dg-require-effective-target indirect_jumps } */

extern void abort (void);

__attribute ((noinline)) void
broken_longjmp(void *p)
{
  void *buf[5];
  __builtin_memcpy (buf, p, 5 * sizeof (void*));
  /* Corrupts stack pointer...  */
  __builtin_longjmp (buf, 1);
}

volatile int x = 0;
volatile void *p;
int
main (void)
{
  void *buf[5];
  p = __builtin_alloca (x);

  if (!__builtin_setjmp (buf))
    broken_longjmp (buf);

  /* Fails if stack pointer corrupted.  */
  if (p != __builtin_alloca (x))
    abort();

  return 0;
}
