/* Copyright (C) 2000, 2003  Free Software Foundation.

   Ensure all expected transformations of builtin strcat occur and
   perform correctly.

   Written by Kaveh R. Ghazi, 11/27/2000.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern char *strcat (char *, const char *);
extern char *strcpy (char *, const char *);
extern int strcmp (const char *, const char *);
extern void *memset (void *, int, size_t);
extern int memcmp (const void *, const void *, size_t);
#define RESET_DST_WITH(FILLER) \
  do { memset (dst, 'X', sizeof (dst)); strcpy (dst, (FILLER)); } while (0)

int main ()
{
  const char *const s1 = "hello world";
  const char *const s2 = "";
  char dst[64], *d2;
  
  RESET_DST_WITH (s1);
  if (strcat (dst, "") != dst || strcmp (dst, s1))
    abort();
  RESET_DST_WITH (s1);
  if (strcat (dst, s2) != dst || strcmp (dst, s1))
    abort();
  RESET_DST_WITH (s1); d2 = dst;
  if (strcat (++d2, s2) != dst+1 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  RESET_DST_WITH (s1); d2 = dst;
  if (strcat (++d2+5, s2) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();
  RESET_DST_WITH (s1); d2 = dst;
  if (strcat (++d2+5, s1+11) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort();

#ifndef __OPTIMIZE_SIZE__
  RESET_DST_WITH (s1);
  if (strcat (dst, " 1111") != dst
      || memcmp (dst, "hello world 1111\0XXX", 20))
    abort();
  
  RESET_DST_WITH (s1);
  if (strcat (dst+5, " 2222") != dst+5
      || memcmp (dst, "hello world 2222\0XXX", 20))
    abort();
  
  RESET_DST_WITH (s1); d2 = dst;
  if (strcat (++d2+5, " 3333") != dst+6 || d2 != dst+1
      || memcmp (dst, "hello world 3333\0XXX", 20))
    abort();
  
  RESET_DST_WITH (s1);
  strcat (strcat (strcat (strcat (strcat (strcat (dst, ": this "), ""),
				  "is "), "a "), "test"), ".");
  if (memcmp (dst, "hello world: this is a test.\0X", 30))
    abort();
#endif

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  RESET_DST_WITH (s1);
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
