/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);

/* The alignment also gives this size 32, so will be passed by reference.  */
typedef struct __attribute__ ((__aligned__ (32)))
  {
    long x;
    long y;
  } overaligned;

overaligned a = { 2, 3 };

void
test_pass_by_ref (int x0, overaligned x1, int x2)
{
  if (x0 != 7 || x2 != 9)
    abort ();
  if (memcmp ((void *) &x1, (void *)&a, sizeof (overaligned)))
    abort ();
  long addr = ((long) &x1) & 31;
  if (addr != 0)
    {
      __builtin_printf ("Alignment was %d\n", addr);
      abort ();
    }
}

int
main (int argc, char **argv)
{
  test_pass_by_ref (7, a, 9);
  return 0;
}
