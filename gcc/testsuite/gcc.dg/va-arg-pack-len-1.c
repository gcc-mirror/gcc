/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdarg.h>

extern int error_open_missing_mode (void)
  __attribute__((__error__ ("open with O_CREAT needs 3 arguments, only 2 were given")));
extern int warn_open_too_many_arguments (void)
  __attribute__((__warning__ ("open called with more than 3 arguments")));
extern void abort (void);

char expected_char;

__attribute__((noinline)) int
myopen2 (const char *path, int oflag)
{
  if (expected_char++ != path[0] || path[1] != '\0')
    abort ();
  switch (path[0])
    {
    case 'f':
      if (oflag != 0x2)
	abort ();
      break;
    case 'g':
      if (oflag != 0x43)
	abort ();
      /* In real __open_2 this would terminate the program:
	 open with O_CREAT without third argument.  */
      return -6;
    default:
      abort ();
    }
  return 0;
}

__attribute__((noinline)) int
myopenva (const char *path, int oflag, ...)
{
  int mode = 0;
  va_list ap;
  if ((oflag & 0x40) != 0)
    {
      va_start (ap, oflag);
      mode = va_arg (ap, int);
      va_end (ap);
    }
  if (expected_char++ != path[0] || path[1] != '\0')
    abort ();
  switch (path[0])
    {
    case 'a':
      if (oflag != 0x43 || mode != 0644)
	abort ();
      break;
    case 'b':
      if (oflag != 0x3)
	abort ();
      break;
    case 'c':
      if (oflag != 0x2)
	abort ();
      break;
    case 'd':
      if (oflag != 0x43 || mode != 0600)
	abort ();
      break;
    case 'e':
      if (oflag != 0x3)
	abort ();
      break;
    default:
      abort ();
    }
  return 0;
}

extern inline __attribute__((always_inline, gnu_inline)) int
myopen (const char *path, int oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    warn_open_too_many_arguments ();

  if (__builtin_constant_p (oflag))
    {
      if ((oflag & 0x40) != 0 && __builtin_va_arg_pack_len () < 1)
	{
	  error_open_missing_mode ();
	  return myopen2 (path, oflag);
	}
      return myopenva (path, oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return myopen2 (path, oflag);

  return myopenva (path, oflag, __builtin_va_arg_pack ());
}

volatile int l0;

int
main (void)
{
  expected_char = 'a';
  if (myopen ("a", 0x43, 0644))
    abort ();
  if (myopen ("b", 0x3, 0755))
    abort ();
  if (myopen ("c", 0x2))
    abort ();
  if (myopen ("d", l0 + 0x43, 0600))
    abort ();
  if (myopen ("e", l0 + 0x3, 0700))
    abort ();
  if (myopen ("f", l0 + 0x2))
    abort ();
  /* Invalid use of myopen, but only detectable at runtime.  */
  if (myopen ("g", l0 + 0x43) != -6)
    abort ();
  return 0;
}
