/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strspn occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/27/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strspn (const char *, const char *);
extern char *strcpy (char *, const char *);

int main ()
{
  const char *const s1 = "hello world";
  char dst[64], *d2;
  
  if (strspn (s1, "hello") != 5)
    abort();
  if (strspn (s1+4, "hello") != 1)
    abort();
  if (strspn (s1, "z") != 0)
    abort();
  if (strspn (s1, "hello world") != 11)
    abort();
  if (strspn (s1, "") != 0)
    abort();
  strcpy (dst, s1);
  if (strspn (dst, "") != 0)
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strspn (++d2, "") != 0 || d2 != dst+1)
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strspn (++d2+5, "") != 0 || d2 != dst+1)
    abort();
  if (strspn ("", s1) != 0)
    abort();
  strcpy (dst, s1);
  if (strspn ("", dst) != 0)
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strspn ("", ++d2) != 0 || d2 != dst+1)
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strspn ("", ++d2+5) != 0 || d2 != dst+1)
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strspn (s1, "hello") != 5)
    abort();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static size_t
strspn (const char *s1, const char *s2)
{
  abort();
}
#endif
