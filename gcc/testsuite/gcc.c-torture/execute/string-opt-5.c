/* Copyright (C) 2000  Free Software Foundation.

   Ensure builtin strlen, strcmp, strchr and strrchr perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char *);
extern int strcmp (const char *, const char *);
extern char *strchr (const char *, int);
extern char *strrchr (const char *, int);

int x = 6;
char *bar = "hi world";

int main()
{
  const char *const foo = "hello world";

  if (strlen (bar) != 8)
    abort ();
  if (strlen (bar + (++x & 2)) != 6)
    abort ();
  if (x != 7)
    abort ();
  if (strlen (foo + (x++, 6)) != 5)
    abort ();
  if (x != 8)
    abort ();
  if (strlen (foo + (++x & 1)) != 10)
    abort ();
  if (x != 9)
    abort ();
  if (strcmp (foo + (x -= 6), "lo world"))
    abort ();
  if (x != 3)
    abort ();
  if (strcmp (foo, bar) >= 0)
    abort ();
  if (strcmp (foo, bar + (x++ & 1)) >= 0)
    abort ();
  if (x != 4)
    abort ();
  if (strchr (foo + (x++ & 7), 'l') != foo + 9)
    abort ();
  if (x != 5)
    abort ();
  if (strchr (bar, 'o') != bar + 4)
    abort ();
  if (strchr (bar, '\0')  != bar + 8)
    abort ();
  if (strrchr (bar, 'x'))
    abort ();
  if (strrchr (bar, 'o') != bar + 4)
    abort ();

  return 0;
}
