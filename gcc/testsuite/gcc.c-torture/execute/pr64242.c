/* { dg-require-effective-target indirect_jumps } */

extern void abort (void);

__attribute ((noinline)) void
broken_longjmp (void *p)
{
  void *buf[32];
  __builtin_memcpy (buf, p, 5 * sizeof (void*));
  __builtin_memset (p, 0, 5 * sizeof (void*));
  /* Corrupts stack pointer...  */
  __builtin_longjmp (buf, 1);
}

volatile int x = 0;
char *volatile p;
char *volatile q;

int
main ()
{
  void *buf[5];
  p = __builtin_alloca (x);
  q = __builtin_alloca (x);
  if (!__builtin_setjmp (buf))
    broken_longjmp (buf);

  /* Compute expected next alloca offset - some targets don't align properly
     and allocate too much.  */
  p = q + (q - p);

  /* Fails if stack pointer corrupted.  */
  if (p != __builtin_alloca (x))
    abort ();

  return 0;
}
