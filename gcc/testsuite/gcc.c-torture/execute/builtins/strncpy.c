/* Copyright (C) 2000, 2005  Free Software Foundation.

   Ensure all expected transformations of builtin strncpy occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/25/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern char *strncpy (char *, const char *, size_t);
extern int memcmp (const void *, const void *, size_t);
extern void *memset (void *, int, size_t);

/* Reset the destination buffer to a known state. */
#define RESET_DST memset(dst, 'X', sizeof(dst))

int i;

void
main_test (void)
{
  const char *const src = "hello world";
  const char *src2;
  char dst[64], *dst2;
  
  RESET_DST;
  if (strncpy (dst, src, 4) != dst || memcmp (dst, "hellXXX", 7))
    abort();

  RESET_DST;
  if (strncpy (dst+16, src, 4) != dst+16 || memcmp (dst+16, "hellXXX", 7))
    abort();

  RESET_DST;
  if (strncpy (dst+32, src+5, 4) != dst+32 || memcmp (dst+32, " worXXX", 7))
    abort();

  RESET_DST;
  dst2 = dst;
  if (strncpy (++dst2, src+5, 4) != dst+1 || memcmp (dst2, " worXXX", 7)
      || dst2 != dst+1)
    abort();

  RESET_DST;
  if (strncpy (dst, src, 0) != dst || memcmp (dst, "XXX", 3))
    abort();
  
  RESET_DST;
  dst2 = dst; src2 = src;
  if (strncpy (++dst2, ++src2, 0) != dst+1 || memcmp (dst2, "XXX", 3)
      || dst2 != dst+1 || src2 != src+1)
    abort();

  RESET_DST;
  dst2 = dst; src2 = src;
  if (strncpy (++dst2+5, ++src2+5, 0) != dst+6 || memcmp (dst2+5, "XXX", 3)
      || dst2 != dst+1 || src2 != src+1)
    abort();

  RESET_DST;
  if (strncpy (dst, src, 12) != dst || memcmp (dst, "hello world\0XXX", 15))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  RESET_DST;
  if (__builtin_strncpy (dst, src, 4) != dst || memcmp (dst, "hellXXX", 7))
    abort();

  RESET_DST;
  if (strncpy (dst, i++ ? "xfoo" + 1 : "bar", 4) != dst
      || memcmp (dst, "bar\0XXX", 7)
      || i != 1)
    abort ();
}
