/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strchr and index
   occur and perform correctly.

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
  /* For systems which don't have index, we test the __builtin_
     version to avoid spurious link failures at -O0.  We only need to
     test one case since everything is handled in the same code path
     as builtin strchr.  */
  if (__builtin_index ("hello", 'z')  != 0)
    abort ();

  return 0;
}

static char *
index (const char *s, int c)
{
  abort ();
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
static char *
strchr (const char *s, int c)
{
  abort ();
}
#endif
