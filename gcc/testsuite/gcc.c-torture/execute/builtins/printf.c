/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin printf occur and
   that we honor side effects in the arguments.

   Written by Kaveh R. Ghazi, 12/4/2000.  */

extern int printf (const char *, ...);
extern int printf_unlocked (const char *, ...);
extern void abort(void);

void
main_test (void)
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
}
