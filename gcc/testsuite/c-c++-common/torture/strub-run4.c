/* { dg-do run } */
/* { dg-options "-fstrub=all" } */
/* { dg-require-effective-target alloca } */
/* { dg-require-effective-target strub } */

/* Check that multi-level, multi-inlined functions still get cleaned up as
   expected, without overwriting temporary stack allocations while they should
   still be available.  */

#ifndef ATTR_STRUB_AT_CALLS
# define ATTR_STRUB_AT_CALLS /* Defined in strub-run4d.c.  */
#endif

const char test_string[] = "\x55\xde\xad\xbe\xef\xc0\x1d\xca\xfe\x55\xaa";

static inline __attribute__ ((__always_inline__))
char *
leak_string (void)
{
  int __attribute__ ((__strub__)) len = 512;
  asm ("" : "+r" (len));
  char s[len];
  __builtin_strcpy (s, test_string);
  __builtin_strcpy (s + len - sizeof (test_string), test_string);
  asm ("" : "+m" (s));
  return (char *) __builtin_stack_address ();
}

static inline __attribute__ ((__always_inline__))
int
look_for_string (char *e)
{
  char *p = (char *) __builtin_stack_address ();

  if (p == e)
    __builtin_abort ();

  if (p > e)
    {
      char *q = p;
      p = e;
      e = q;
    }

  for (char *re = e - sizeof (test_string); p < re; p++)
    for (int i = 0; p[i] == test_string[i]; i++)
      if (i == sizeof (test_string) - 1)
	return i;

  return 0;
}

static inline ATTR_STRUB_AT_CALLS
char *
innermost ()
{
  int __attribute__ ((__strub__)) len = 512;
  asm ("" : "+r" (len));
  char s[len];
  __builtin_strcpy (s, test_string);
  __builtin_strcpy (s + len - sizeof (test_string), test_string);
  asm ("" : "+m" (s));
  char *ret = leak_string ();
  if (__builtin_strcmp (s, test_string) != 0)
    __builtin_abort ();
  if (__builtin_strcmp (s + len - sizeof (test_string), test_string) != 0)
    __builtin_abort ();
  return ret;
}

static inline ATTR_STRUB_AT_CALLS
char *
intermediate ()
{
  int __attribute__ ((__strub__)) len = 512;
  asm ("" : "+r" (len));
  char s[len];
  __builtin_strcpy (s, test_string);
  __builtin_strcpy (s + len - sizeof (test_string), test_string);
  asm ("" : "+m" (s));
  char *ret = innermost ();
  if (__builtin_strcmp (s, test_string) != 0)
    __builtin_abort ();
  if (__builtin_strcmp (s + len - sizeof (test_string), test_string) != 0)
    __builtin_abort ();
  return ret;
}

static inline __attribute__ ((__strub__ ("internal")))
char *
internal ()
{
  return intermediate ();
}

int __attribute__ ((__strub__ ("disabled")))
main ()
{
  /* Since these test check stack contents above the top of the stack, an
     unexpected asynchronous signal or interrupt might overwrite the bits we
     expect to find and cause spurious fails.  Tolerate one such overall
     spurious fail by retrying.  */
  int i = 1;
  while (look_for_string (internal ()))
    if (!i--) __builtin_abort ();
  __builtin_exit (0);
}
