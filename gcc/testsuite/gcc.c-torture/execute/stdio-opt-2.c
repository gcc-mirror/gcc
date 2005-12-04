/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin printf occur and
   that we honor side effects in the arguments.

   Written by Kaveh R. Ghazi, 12/4/2000.  */

#include <stdio.h>
#include <stdarg.h>
extern int printf (const char *, ...);
extern int printf_unlocked (const char *, ...);
extern void abort(void);

int main()
{
  const char *const s1 = "hello world";
  const char *const s2[] = { s1, 0 }, *const*s3;
  
  printf ("%s\n", "hello");
  printf ("%s\n", *s2);
  s3 = s2;
  printf ("%s\n", *s3++);
  if (s3 != s2+1 || *s3 != 0)
    abort();
  
  printf ("%c", '\n');
  printf ("%c", **s2);
  s3 = s2;
  printf ("%c", **s3++);
  if (s3 != s2+1 || *s3 != 0)
    abort();
  
  printf ("");
  printf ("\n");
  printf ("hello world\n");
  
  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  __builtin_printf ("%s\n", "hello");
  /* These builtin stubs are called by __builtin_printf, ensure their
     prototypes are set correctly too.  */
  __builtin_putchar ('\n');
  __builtin_puts ("hello");
  /* Check the unlocked style, these evaluate to nothing to avoid
     problems on systems without the unlocked functions.  */
  printf_unlocked ("");
  __builtin_printf_unlocked ("");

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static int
printf (const char *string, ...)
{
  abort();
}
#endif

/* Locking stdio doesn't matter for the purposes of this test.  */
static int __attribute__ ((__noinline__))
printf_unlocked (const char *string, ...)
{
#ifdef __OPTIMIZE__
  abort();
#else
  va_list ap;
  int r;
  va_start (ap, string);
  r = vprintf (string, ap);
  va_end (ap);
  return r;
#endif
}
