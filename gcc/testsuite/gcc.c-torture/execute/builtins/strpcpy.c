/* Copyright (C) 2003, 2004  Free Software Foundation.

   Ensure builtin stpcpy performs correctly.

   Written by Kaveh Ghazi, 4/11/2003.  */

typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern char *strcpy (char *, const char *);
extern char *stpcpy (char *, const char *);
extern int memcmp (const void *, const void *, size_t);

extern int inside_main;

const char s1[] = "123";
char p[32] = "";
char *s2 = "defg";
char *s3 = "FGH";
size_t l1 = 1;

void
main_test (void)
{
  int i = 8;

#if !defined __i386__ && !defined __x86_64__
  /* The functions below might not be optimized into direct stores on all
     arches.  It depends on how many instructions would be generated and
     what limits the architecture chooses in STORE_BY_PIECES_P.  */
  inside_main = 0;
#endif
  if (stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();
  if (stpcpy (p + 16, "vwxyz" + 1) != p + 16 + 4 || memcmp (p + 16, "wxyz", 5))
    abort ();
  if (stpcpy (p + 1, "") != p + 1 + 0 || memcmp (p, "a\0cde", 6))
    abort ();
  if (stpcpy (p + 3, "fghij") != p + 3 + 5 || memcmp (p, "a\0cfghij", 9))
    abort ();

  if (stpcpy ((i++, p + 20 + 1), "23") != (p + 20 + 1 + 2)
      || i != 9 || memcmp (p + 19, "z\0""23\0", 5))
    abort ();

  if (stpcpy (stpcpy (p, "ABCD"), "EFG") != p + 7 || memcmp (p, "ABCDEFG", 8))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();

    /* If the result of stpcpy is ignored, gcc should use strcpy.
       This should be optimized always, so set inside_main again.  */
  inside_main = 1;
  stpcpy (p + 3, s3);
  if (memcmp (p, "abcFGH", 6))
    abort ();
}
