/* Copyright (C) 2004  Free Software Foundation.

   Ensure builtin strcpy is optimized into memcpy
   even when there is more than one possible string literal
   passed to it, but all string literals passed to it
   have equal length.

   Written by Jakub Jelinek, 9/15/2004.  */

extern void abort (void);
extern char *strcpy (char *, const char *);
typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);

char buf[32], *p;
int i;

char *
__attribute__((noinline))
test (void)
{
  int j;
  const char *q = "abcdefg";
  for (j = 0; j < 3; ++j)
    {
      if (j == i)
        q = "bcdefgh";
      else if (j == i + 1)
        q = "cdefghi";
      else if (j == i + 2)
        q = "defghij";
    }
  p = strcpy (buf, q);
  return strcpy (buf + 16, q);
}

void
main_test (void)
{
#ifndef __OPTIMIZE_SIZE__
  /* For -Os, strcpy above is not replaced with
     memcpy (buf, q, 8);, as that is larger.  */
  if (test () != buf + 16 || p != buf)
    abort ();
#endif
}
