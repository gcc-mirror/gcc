/* Copyright (C) 2000, 2003  Free Software Foundation.

   Ensure all expected transformations of builtin strchr and index
   occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern char *strchr (const char *, int);
extern char *index (const char *, int);

void
main_test (void)
{
  const char *const foo = "hello world";

  if (strchr (foo, 'x'))
    abort ();
  if (strchr (foo, 'o') != foo + 4)
    abort ();
  if (strchr (foo + 5, 'o') != foo + 7)
    abort ();
  if (strchr (foo, '\0')  != foo + 11)
    abort ();
  /* Test only one instance of index since the code path is the same
     as that of strchr. */
  if (index ("hello", 'z')  != 0)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strchr (foo, 'o')  != foo + 4)
    abort ();
  if (__builtin_index (foo, 'o')  != foo + 4)
    abort ();
}
