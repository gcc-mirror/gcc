/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strncat occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/27/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern char *strncat (char *, const char *, size_t);
extern char *strcpy (char *, const char *);
extern char *strcmp (const char *, const char *);
int x = 123;

int main ()
{
  const char *const s1 = "hello world";
  const char *const s2 = "";
  char dst[64], *d2;
  
  strcpy (dst, s1);
  if (strncat (dst, "", 100) != dst || strcmp (dst, s1))
    abort();
  strcpy (dst, s1);
  if (strncat (dst, s2, 100) != dst || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2, s2, 100) != dst+1 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s2, 100) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1+11, 100) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1, 0) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, "", ++x) != dst+6 || d2 != dst+1 || x != 124
      || strcmp (dst, s1))
    abort();

  strcpy (dst, s1);
  if (strncat (dst, "foo", 3) != dst || strcmp (dst, "hello worldfoo"))
    abort();
  strcpy (dst, s1);
  if (strncat (dst, "foo", 100) != dst || strcmp (dst, "hello worldfoo"))
    abort();
  strcpy (dst, s1);
  if (strncat (dst, s1, 100) != dst || strcmp (dst, "hello worldhello world"))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2, s1, 100) != dst+1 || d2 != dst+1
      || strcmp (dst, "hello worldhello world"))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1, 100) != dst+6 || d2 != dst+1
      || strcmp (dst, "hello worldhello world"))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1+5, 100) != dst+6 || d2 != dst+1
      || strcmp (dst, "hello world world"))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  strcpy (dst, s1);
  if (__builtin_strncat (dst, "", 100) != dst || strcmp (dst, s1))
    abort();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static char *
strncat (char *s1, const char *s2, size_t n)
{
  abort();
}
#endif
