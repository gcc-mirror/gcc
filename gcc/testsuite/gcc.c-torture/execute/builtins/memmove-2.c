/* Copyright (C) 2004  Free Software Foundation.

   Check builtin memmove and bcopy optimization when length is 1.

   Written by Jakub Jelinek, 9/14/2004.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memmove (void *, const void *, size_t);
extern void bcopy (const void *, void *, size_t);
extern int memcmp (const void *, const void *, size_t);

char p[32] = "abcdefg";
char *q = p + 4;

void
main_test (void)
{
  /* memmove with length 1 can be optimized into memcpy if it can be
     expanded inline.  */
  if (memmove (p + 2, p + 3, 1) != p + 2 || memcmp (p, "abddefg", 8))
    abort ();
  if (memmove (p + 1, p + 1, 1) != p + 1 || memcmp (p, "abddefg", 8))
    abort ();
  if (memmove (q, p + 4, 1) != p + 4 || memcmp (p, "abddefg", 8))
    abort ();
  bcopy (p + 5, p + 6, 1);
  if (memcmp (p, "abddeff", 8))
    abort ();
  bcopy (p + 1, p + 1, 1);
  if (memcmp (p, "abddeff", 8))
    abort ();
  bcopy (q, p + 4, 1);
  if (memcmp (p, "abddeff", 8))
    abort ();
}
