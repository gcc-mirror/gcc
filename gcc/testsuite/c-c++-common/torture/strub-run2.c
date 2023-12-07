/* { dg-do run } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

/* Check that a non-strub function leaves a string behind in the stack, and that
   equivalent strub functions don't.  Allow red zones to be used.  */

const char test_string[] = "\x55\xde\xad\xbe\xef\xc0\x1d\xca\xfe\x55\xaa";

/* Pad before and after the string on the stack, so that it's not overwritten by
   regular stack use.  */
#define PAD 7

static inline __attribute__ ((__always_inline__, __strub__ ("callable")))
char *
leak_string (void)
{
  int len = sizeof (test_string);
  asm ("" : "+rm" (len));
  char s[2 * PAD + 1][len];
  __builtin_strcpy (s[PAD], test_string);
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

static __attribute__ ((__noinline__, __noclone__))
char *
callable ()
{
  return leak_string ();
}

static __attribute__ ((__strub__ ("at-calls")))
char *
at_calls ()
{
  return leak_string ();
}

static __attribute__ ((__strub__ ("internal")))
char *
internal ()
{
  return leak_string ();
}

int main ()
{
  /* Since these test check stack contents above the top of the stack, an
     unexpected asynchronous signal or interrupt might overwrite the bits we
     expect to find and cause spurious fails.  Tolerate one such overall
     spurious fail by retrying.  */
  int i = 1;
  while (!look_for_string (callable ()))
    if (!i--) __builtin_abort ();
  while (look_for_string (at_calls ()))
    if (!i--) __builtin_abort ();
  while (look_for_string (internal ()))
    if (!i--) __builtin_abort ();
  __builtin_exit (0);
}
