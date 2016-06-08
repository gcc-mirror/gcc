/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);

struct s
  {
    long x;
    long y;
  };

/* This still has size 16, so is still passed by value.  */
typedef __attribute__ ((__aligned__ (32))) struct s overaligned;

/* A few structs, at 32-byte-aligned memory locations.  */
overaligned a = { 2, 3 };
overaligned b = { 5, 8 };
overaligned c = { 13, 21 };

void
test_pass_by_value (int x0, overaligned x1, int x3, int x4, overaligned x5,
		    int x7, int stack, overaligned stack8)
{
  if (x0 != 7 || x3 != 9 || x4 != 11 || x7 != 15 || stack != 10)
    abort ();
  if (memcmp ((void *) &x1, (void *)&a, sizeof (overaligned)))
    abort ();
  if (memcmp ((void *) &x5, (void *)&b, sizeof (overaligned)))
    abort ();
  if (memcmp ((void *)&stack8, (void *)&c, sizeof (overaligned)))
    abort ();
  long addr = ((long) &stack8) & 15;
  if (addr != 0)
    {
      __builtin_printf ("Alignment was %d\n", addr);
      abort ();
    }
}

int
main (int argc, char **argv)
{
  test_pass_by_value (7, a, 9, 11, b, 15, 10, c);
  return 0;
}
