/* { dg-do run } */
/* { dg-options "-O2" } */

static inline void __attribute__((always_inline))
f1 (void)
{
  long unused;
  asm volatile ("" : "=es" (unused) :: "memory");
}

static void __attribute__((noinline))
f2 (long *val)
{
  *val = 0x1234;
}

static long __attribute__((noinline))
test (void)
{
  f1 ();
  {
    long val;
    f2 (&val);
    return val;
  }
}

int
main (void)
{
  return test () != 0x1234;
}
