/* Declaration of the frame size doesn't work on ptx.  */
/* { dg-require-effective-target untyped_assembly } */
/* { dg-require-effective-target indirect_calls } */

#ifndef ASIZE
# define ASIZE 0x10000000000UL
#endif

#include <limits.h>

#if LONG_MAX < 8 * ASIZE
# undef ASIZE
# define ASIZE 4096
#endif

extern void abort (void);

int __attribute__((noinline))
foo (const char *s)
{
  if (!s)
    return 1;
  if (s[0] != 'a')
    abort ();
  s += ASIZE - 1;
  if (s[0] != 'b')
    abort ();
  return 0;
}

int (*fn) (const char *) = foo;

int __attribute__((noinline))
bar (void)
{
  char s[ASIZE];
  s[0] = 'a';
  s[ASIZE - 1] = 'b';
  foo (s);
  foo (s);
  return 0;
}

int __attribute__((noinline))
baz (long i)
{
  if (i)
    return fn (0);
  else
    {
      char s[ASIZE];
      s[0] = 'a';
      s[ASIZE - 1] = 'b';
      foo (s);
      foo (s);
      return fn (0);
    }
}

int
main (void)
{
  if (bar ())
    abort ();
  if (baz (0) != 1)
    abort ();
  if (baz (1) != 1)
    abort ();
  return 0;
}
