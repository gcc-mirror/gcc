/* Copyright (C) 2000, 2003  Free Software Foundation.

   Ensure all expected transformations of builtin strstr occur and
   perform correctly in presence of redirect.  */

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern char *strstr (const char *, const char *)
  __asm ("my_strstr");
extern char *strchr (const char *, int);
extern int strcmp (const char *, const char *);
extern int strncmp (const char *, const char *, size_t);

const char *p = "rld", *q = "hello world";

int
main (void)
{
  const char *const foo = "hello world";
  
  if (strstr (foo, "") != foo)
    abort ();
  if (strstr (foo + 4, "") != foo + 4)
    abort ();
  if (strstr (foo, "h") != foo)
    abort ();
  if (strstr (foo, "w") != foo + 6)
    abort ();
  if (strstr (foo + 6, "o") != foo + 7)
    abort ();
  if (strstr (foo + 1, "world") != foo + 6)
    abort ();
  if (strstr (foo + 2, p) != foo + 8)
    abort ();
  if (strstr (q, "") != q)
    abort ();
  if (strstr (q + 1, "o") != q + 4)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strstr (foo + 1, "world") != foo + 6)
    abort ();
  
  return 0;
}

/* There should be no calls to real strstr.  */
static char *real_strstr (const char *, const char *)
  __asm ("strstr");

__attribute__ ((noinline))
static char *
real_strstr (const char *s1, const char *s2)
{
  abort ();
}

static char *
strstr (const char *s1, const char *s2)
  __asm ("my_strstr");

__attribute__ ((noinline))
static char *
strstr (const char *s1, const char *s2)
{
  size_t len = strlen (s2);

#ifdef __OPTIMIZE__
  /* If optimizing, we should be called only in the
     strstr (foo + 2, p) case above.  All other cases should
     be optimized.  */
  if (s2 != p || strcmp (s1, "hello world" + 2) != 0)
    abort ();
#endif
  if (len == 0)
    return (char *) s1;
  for (s1 = strchr (s1, *s2); s1; s1 = strchr (s1 + 1, *s2))
    if (strncmp (s1, s2, len) == 0)
      return (char *) s1;
  return (char *) 0;
}
