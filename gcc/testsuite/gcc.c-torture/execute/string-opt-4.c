/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strchr occur and
   perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern char *strchr (const char *, int);

int main()
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

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
char *
strchr (const char *s, int c)
{
  abort ();
}
#endif
