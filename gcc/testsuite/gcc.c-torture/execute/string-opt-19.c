/* Copyright (C) 2003  Free Software Foundation.

   Ensure builtin memmove and bcopy perform correctly.

   Written by Jakub Jelinek, 4/26/2003.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memmove (void *, const void *, size_t);
extern void bcopy (const void *, void *, size_t);
extern int memcmp (const void *, const void *, size_t);

const char s1[] = "123";
char p[32] = "";

int main()
{
  int i;
  const char *s;

  if (memmove (p, "abcde", 6) != p || memcmp (p, "abcde", 6))
    abort ();
  s = s1;
  if (memmove (p + 2, ++s, 0) != p + 2 || memcmp (p, "abcde", 6) || s != s1 + 1)
    abort ();
  if (__builtin_memmove (p + 3, "", 1) != p + 3 || memcmp (p, "abc\0e", 6))
    abort ();
  bcopy ("fghijk", p + 2, 4);
  if (memcmp (p, "abfghi", 7))
    abort ();
  s = s1 + 1;
  bcopy (s++, p + 1, 0);
  if (memcmp (p, "abfghi", 7) || s != s1 + 2)
    abort ();
  __builtin_bcopy ("ABCDE", p + 4, 1);
  if (memcmp (p, "abfgAi", 7))
    abort ();

  return 0;
}

/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  When not optimizing, provide memmove/bcopy implementation
   just in case target lacks these in its libc.  */
__attribute__ ((noinline))
static void *
memmove (void *d, const void *s, size_t n)
{
#ifdef __OPTIMIZE__
  abort ();
#else
  char *dst = (char *) d;
  const char *src = (const char *) s;
  if (src < dst)
    {
      dst += n;
      src += n;
      while (n--)
        *--dst = *--src;
    }
  else
    while (n--)
      *dst++ = *src++;
  return (char *) d;
#endif
}

__attribute__ ((noinline))
static void
bcopy (const void *s, void *d, size_t n)
{
#ifdef __OPTIMIZE__
  abort ();
#else
  char *dst = (char *) d;
  const char *src = (const char *) s;
  if (src < dst)
    {
      dst += n;
      src += n;
      while (n--)
        *--dst = *--src;
    }
  else
    while (n--)
      *dst++ = *src++;
#endif
}
