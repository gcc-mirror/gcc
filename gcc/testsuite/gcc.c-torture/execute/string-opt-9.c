/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strcat occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/27/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern char *strcat (char *, const char *);
extern char *strcpy (char *, const char *);
extern char *strcmp (const char *, const char *);

int main ()
{
  const char *const s1 = "hello world";
  const char *const s2 = "";
  char dst[64], *d2;
  
  strcpy (dst, s1);
  if (strcat (dst, "") != dst || strcmp (dst, s1))
    abort();
  strcpy (dst, s1);
  if (strcat (dst, s2) != dst || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strcat (++d2, s2) != dst+1 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strcat (++d2+5, s2) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  strcpy (dst, s1); d2 = dst;
  if (strcat (++d2+5, s1+11) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  strcpy (dst, s1);
  if (__builtin_strcat (dst, "") != dst || strcmp (dst, s1))
    abort();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static char *
strcat (char *s1, const char *s2)
{
  abort();
}
#endif
