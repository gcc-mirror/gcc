#include <stdio.h>
/* Copyright (C) 2000  Free Software Foundation.

   Ensure builtin mempcpy and stpcpy perform correctly.

   Written by Kaveh Ghazi, 4/11/2003.  */

extern void abort (void);
extern char *strcpy (char *, const char *);
extern char *stpcpy (char *, const char *);
/*typedef __SIZE_TYPE__ size_t;*/
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern void *mempcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);

const char s1[] = "123";
char p[32] = "";

int main()
{
  int i;
  const char *s;

  if (stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();
  if (stpcpy (p + 16, "vwxyz" + 1) != p + 16 + 4 || memcmp (p + 16, "wxyz", 5))
    abort ();
  if (stpcpy (p + 1, "") != p + 1 + 0 || memcmp (p, "a\0cde", 6))
    abort ();  
  if (stpcpy (p + 3, "fghij") != p + 3 + 5 || memcmp (p, "a\0cfghij", 9))
    abort ();
  if (mempcpy (p, "ABCDE", 6) != p + 6 || memcmp (p, "ABCDE", 6))
    abort ();
  if (mempcpy (p + 16, "VWX" + 1, 2) != p + 16 + 2 || memcmp (p + 16, "WXyz", 5))
    abort ();
  if (mempcpy (p + 1, "", 1) != p + 1 + 1 || memcmp (p, "A\0CDE", 6))
    abort ();  
  if (mempcpy (p + 3, "FGHI", 4) != p + 3 + 4 || memcmp (p, "A\0CFGHIj", 9))
    abort ();

  i = 8;
  memcpy (p + 20, "qrstu", 6);
  if (stpcpy ((i++, p + 20 + 1), "23") != (p + 20 + 1 + 2) || i != 9 || memcmp (p + 20, "q23\0u", 6))
    abort ();

  s = s1; i = 3;
  memcpy (p + 25, "QRSTU", 6);
  if (mempcpy (p + 25 + 1, s++, i++) != (p + 25 + 1 + 3) || i != 4 || s != s1 + 1 || memcmp (p + 25, "Q123U", 6))
    abort ();

  if (stpcpy (stpcpy (p, "ABCD"), "EFG") != p + 7 || memcmp (p, "ABCDEFG", 8))
    abort();
  if (mempcpy (mempcpy (p, "abcdEFG", 4), "efg", 4) != p + 8 || memcmp (p, "abcdefg", 8))
    abort();
  
  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();
  if (__builtin_mempcpy (p, "ABCDE", 6) != p + 6 || memcmp (p, "ABCDE", 6))
    abort ();

  return 0;
}

/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  When not optimizing, we provide fallback funcs for
   platforms that don't have mempcpy or stpcpy in libc.*/
__attribute__ ((noinline))
static char *
stpcpy (char *d, const char *s)
{
#ifdef __OPTIMIZE__
  abort ();
#else
  return strcpy (d, s) + strlen (s);
#endif
}

__attribute__ ((noinline))
static void *
mempcpy (void *dst, const void *src, size_t sz)
{
#ifdef __OPTIMIZE__
  abort ();
#else
  return (char *) memcpy (dst, src, sz) + sz;
#endif
}
