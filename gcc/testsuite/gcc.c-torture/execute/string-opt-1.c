/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strstr occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/6/2000.  */

extern void abort(void);
extern char *strstr (const char *, const char *);

int main()
{
  const char *const foo = "hello world";
  
  if (strstr (foo, "") != foo)
    abort();
  if (strstr (foo + 4, "") != foo + 4)
    abort();
  if (strstr (foo, "h") != foo)
    abort();
  if (strstr (foo, "w") != foo + 6)
    abort();
  if (strstr (foo + 6, "o") != foo + 7)
    abort();
  if (strstr (foo + 1, "world") != foo + 6)
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strstr (foo + 1, "world") != foo + 6)
    abort();
  
  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static char *
strstr(const char *s1, const char *s2)
{
  abort();
}
#endif
