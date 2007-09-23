// { dg-do compile }
// { dg-options "-O2" }

#include <stdarg.h>

extern int error_open_missing_mode (void)
  __attribute__((__error__ ("open with O_CREAT needs 3 arguments, only 2 were given")));
extern int warn_open_too_many_arguments (void)
  __attribute__((__warning__ ("open called with more than 3 arguments")));

extern int myopen2 (const char *path, int oflag);
extern int myopenva (const char *path, int oflag, ...);

extern inline __attribute__((always_inline, gnu_inline)) int
myopen (const char *path, int oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    warn_open_too_many_arguments ();	// { dg-warning "called with more than 3" }

  if (__builtin_constant_p (oflag))
    {
      if ((oflag & 0x40) != 0 && __builtin_va_arg_pack_len () < 1)
	{
	  error_open_missing_mode ();	// { dg-error "needs 3 arguments, only 2 were given" }
	  return myopen2 (path, oflag);
	}
      return myopenva (path, oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return myopen2 (path, oflag);

  return myopenva (path, oflag, __builtin_va_arg_pack ());
}

int
main (void)
{
  myopen ("h", 0x43);
  myopen ("i", 0x43, 0644, 0655);
  return 0;
}
