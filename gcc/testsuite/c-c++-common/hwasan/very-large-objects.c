/* { dg-do compile } */

/* Ensure the sanitizer can handle very large offsets (i.e. that the hooks
   handle offsets too large for the relevant instructions).
   Just want to make sure this compiles without an ICE.  */
#ifndef ASIZE
# define ASIZE 0x10000000000UL
#endif

typedef __UINT64_TYPE__ uint64_t;

#if __LONG_MAX__ < 8 * ASIZE
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

int __attribute__((noinline))
very_large_offset (int *p)
{
  char init_array[(uint64_t)0xfefefef];
  char other_array[(uint64_t)0xfefefef];
  return (int)init_array[p[1]] + (int)other_array[p[0]];
}

