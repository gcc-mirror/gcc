/* Copyright (C) 2000, 2001, 2003, 2004  Free Software Foundation.

   Ensure all expected transformations of builtin strlen
   occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.

   Additional tests written by Roger Sayle, 11/02/2001:
   Ensure all builtin strlen comparisons against zero are optimized
   and perform correctly. The multiple calls to strcpy are to prevent
   the potentially "pure" strlen calls from being removed by CSE. */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char *);
extern char *strcpy (char *, const char *);

int x = 6;

void
main_test(void)
{
  const char *const foo = "hello world";
  char str[8];
  char *ptr;

  if (strlen (foo) != 11)
    abort ();
  if (strlen (foo + 4) != 7)
    abort ();
  if (strlen (foo + (x++ & 7)) != 5)
    abort ();
  if (x != 7)
    abort ();

  ptr = str;
  strcpy (ptr, "nts");
  if (strlen (ptr) == 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr) < 1)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr) <= 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr+3) != 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (ptr+3) > 0)
    abort ();

  strcpy (ptr, "nts");
  if (strlen (str+3) >= 1)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strlen (foo) != 11)
    abort ();
}
