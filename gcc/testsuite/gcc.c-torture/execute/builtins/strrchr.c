/* Copyright (C) 2000, 2003, 2004  Free Software Foundation.

   Ensure all expected transformations of builtin strrchr and rindex
   occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern char *strrchr (const char *, int);
extern char *rindex (const char *, int);

char *bar = "hi world";
int x = 7;

void
main_test (void)
{
  const char *const foo = "hello world";

  if (strrchr (foo, 'x'))
    abort ();
  if (strrchr (foo, 'o') != foo + 7)
    abort ();
  if (strrchr (foo, 'e') != foo + 1)
    abort ();
  if (strrchr (foo + 3, 'e'))
    abort ();
  if (strrchr (foo, '\0') != foo + 11)
    abort ();
  if (strrchr (bar, '\0') != bar + 8)
    abort ();
  if (strrchr (bar + 4, '\0') != bar + 8)
    abort ();
  if (strrchr (bar + (x++ & 3), '\0') != bar + 8)
    abort ();
  if (x != 8)
    abort ();
  /* Test only one instance of rindex since the code path is the same
     as that of strrchr. */
  if (rindex ("hello", 'z') != 0)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strrchr (foo, 'o') != foo + 7)
    abort ();
  if (__builtin_rindex (foo, 'o') != foo + 7)
    abort ();
}
