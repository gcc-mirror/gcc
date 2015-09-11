/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -fno-inline" } */

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);

typedef struct __attribute__((aligned (8)))
  {
    int x;
    int y;
  } overaligned;

overaligned a = { 2, 3 };
overaligned b = { 5, 8 };

void
f (int r0, overaligned r1, int r3, int stack, overaligned stack4)
{
  if (r0 != 7 || r3 != 9 || stack != 10)
    abort ();
  if (memcmp ((void *) &r1, (void *)&a, sizeof (overaligned)))
    abort ();
  if (memcmp ((void *)&stack4, (void *)&b, sizeof (overaligned)))
    abort ();
  int addr = ((int) &stack4) & 7;
  if (addr != 0)
    {
      __builtin_printf ("Alignment was %d\n", addr);
      abort ();
    }
}

int
main (int argc, char **argv)
{
  f (7, a, 9, 10, b);
  return 0;
}
