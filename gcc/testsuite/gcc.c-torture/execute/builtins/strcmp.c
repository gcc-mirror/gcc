/* Copyright (C) 2000, 2003, 2004  Free Software Foundation.

   Ensure all expected transformations of builtin strcmp
   occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern int strcmp (const char *, const char *);

int x = 7;
char *bar = "hi world";

void
main_test (void)
{
  const char *const foo = "hello world";

  if (strcmp (foo, "hello") <= 0)
    abort ();
  if (strcmp (foo + 2, "llo") <= 0)
    abort ();
  if (strcmp (foo, foo) != 0)
    abort ();
  if (strcmp (foo, "hello world ") >= 0)
    abort ();
  if (strcmp (foo + 10, "dx") >= 0)
    abort ();
  if (strcmp (10 + foo, "dx") >= 0)
    abort ();
  if (strcmp (bar, "") <= 0)
    abort ();
  if (strcmp ("", bar) >= 0)
    abort ();
  if (strcmp (bar+8, "") != 0)
    abort ();
  if (strcmp ("", bar+8) != 0)
    abort ();
  if (strcmp (bar+(--x), "") <= 0 || x != 6)
    abort ();
  if (strcmp ("", bar+(++x)) >= 0 || x != 7)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strcmp (foo, "hello") <= 0)
    abort ();
}
