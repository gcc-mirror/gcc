/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strlen, strcmp,
   strrchr and rindex occur and perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char *);
extern int strcmp (const char *, const char *);
extern char *strrchr (const char *, int);
extern char *rindex (const char *, int);

int x = 6;
char *bar = "hi world";

int main()
{
  const char *const foo = "hello world";

  if (strlen (foo) != 11)
    abort ();
  if (strlen (foo + 4) != 7)
    abort ();
  if (strlen (foo + (x++ & 7)) != 5)
    abort ();
  if (x != 7)
    abort ();
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
  if (__builtin_rindex (foo, 'o') != foo + 7)
    abort ();
  if (__builtin_strrchr (foo, 'o') != foo + 7)
    abort ();
  if (__builtin_strlen (foo) != 11)
    abort ();
  if (__builtin_strcmp (foo, "hello") <= 0)
    abort ();

  return 0;
}

static char *
rindex (const char *s, int c)
{
  /* For systems which don't have rindex, we ensure no link failures
     occur by always providing a backup definition.  During
     optimization this function aborts to catch errors.  */
#ifdef __OPTIMIZE__
  abort ();
#else
  return strrchr(s, c);
#endif
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
static __SIZE_TYPE__
strlen (const char *s)
{
  abort ();
}

static int
strcmp (const char *s1, const char *s2)
{
  abort ();
}

static char *
strrchr (const char *s, int c)
{
  abort ();
}
#endif
