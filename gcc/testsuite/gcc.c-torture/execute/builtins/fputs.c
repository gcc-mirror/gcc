/* Copyright (C) 2000, 2001  Free Software Foundation.

   Ensure all expected transformations of builtin fputs occur and that
   we honor side effects in the stream argument.

   Written by Kaveh R. Ghazi, 10/30/2000.  */

#define _GNU_SOURCE /* For fputs_unlocked.  */
#include <stdio.h>
extern void abort(void);

/* Not all systems have fputs_unlocked.  See fputs-lib.c.  */
extern int (fputs_unlocked) (const char *, FILE *);

int i;

void
main_test(void)
{
  FILE *s_array[] = {stdout, NULL}, **s_ptr = s_array;
  const char *const s1 = "hello world";
  
  fputs ("", *s_ptr);
  fputs ("\n", *s_ptr);
  fputs ("bye", *s_ptr);
  fputs (s1, *s_ptr);
  fputs (s1+5, *s_ptr);
  fputs (s1+10, *s_ptr);
  fputs (s1+11, *s_ptr);
  
  /* Check side-effects when transforming fputs -> NOP.  */
  fputs ("", *s_ptr++);
  if (s_ptr != s_array+1 || *s_ptr != 0)
    abort();

  /* Check side-effects when transforming fputs -> fputc.  */
  s_ptr = s_array;
  fputs ("\n", *s_ptr++);
  if (s_ptr != s_array+1 || *s_ptr != 0)
    abort();

  /* Check side-effects when transforming fputs -> fwrite.  */
  s_ptr = s_array;
  fputs ("hello\n", *s_ptr++);
  if (s_ptr != s_array+1 || *s_ptr != 0)
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  s_ptr = s_array;
  __builtin_fputs ("", *s_ptr);
  /* These builtin stubs are called by __builtin_fputs, ensure their
     prototypes are set correctly too.  */
  __builtin_fputc ('\n', *s_ptr);
  __builtin_fwrite ("hello\n", 1, 6, *s_ptr);
  /* Check the unlocked style, these evaluate to nothing to avoid
     problems on systems without the unlocked functions.  */
  fputs_unlocked ("", *s_ptr);
  __builtin_fputs_unlocked ("", *s_ptr);

  /* Check side-effects in conditional expression.  */
  s_ptr = s_array;
  fputs (i++ ? "f" : "x", *s_ptr++);
  if (s_ptr != s_array+1 || *s_ptr != 0 || i != 1)
    abort();
  fputs (--i ? "\n" : "\n", *--s_ptr);
  if (s_ptr != s_array || i != 0)
    abort();
}
