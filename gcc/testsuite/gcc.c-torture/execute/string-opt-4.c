/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strchr and index
   occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern char *strchr (const char *, int);
extern char *index (const char *, int);

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

  return 0;
}

static char *
index (const char *s, int c)
{
  /* For systems which don't have index, we ensure no link failures
     occur by always providing a backup definition.  During
     optimization this function aborts to catch errors.  */
#ifdef __OPTIMIZE__
  abort ();
#else
  return strchr(s, c);
#endif
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
