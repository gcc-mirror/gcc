/* Copyright (C) 2000  Free Software Foundation.

   Ensure all expected transformations of builtin strncpy occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/25/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern char *strncpy (char *, const char *, size_t);
extern int strcmp (const char *, const char *);
extern int strncmp (const char *, const char *, size_t);
extern void *memset (void *, int, size_t);

int i;

void
main_test (void)
{
  const char *const src = "hello world";
  const char *src2;
  char dst[64], *dst2;
  
  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 4) != dst || strncmp (dst, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst+16, src, 4) != dst+16 || strncmp (dst+16, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst+32, src+5, 4) != dst+32 || strncmp (dst+32, src+5, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  dst2 = dst;
  if (strncpy (++dst2, src+5, 4) != dst+1 || strncmp (dst2, src+5, 4)
      || dst2 != dst+1)
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 0) != dst || strcmp (dst, ""))
    abort();
  
  memset (dst, 0, sizeof (dst));
  dst2 = dst; src2 = src;
  if (strncpy (++dst2, ++src2, 0) != dst+1 || strcmp (dst2, "")
      || dst2 != dst+1 || src2 != src+1)
    abort();

  memset (dst, 0, sizeof (dst));
  dst2 = dst; src2 = src;
  if (strncpy (++dst2+5, ++src2+5, 0) != dst+6 || strcmp (dst2+5, "")
      || dst2 != dst+1 || src2 != src+1)
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 12) != dst || strcmp (dst, src))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  memset (dst, 0, sizeof (dst));
  if (__builtin_strncpy (dst, src, 4) != dst || strncmp (dst, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, i++ ? "xfoo" + 1 : "bar", 4) != dst
      || strcmp (dst, "bar")
      || i != 1)
    abort ();

  return 0;
}
