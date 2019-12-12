/* PR tree-optimization/90879 - fold zero-equality of strcmp between
   a longer string and a smaller array
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-string-compare -fdump-tree-optimized -ftrack-macro-expansion=0" } */

#include "strlenopt.h"

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))

void clobber (void*, ...);

struct S { char a4[4], c; };

extern char a4[4];
extern char b4[4];

/* Verify that comparison of string literals with arrays with unknown
   content but size that prevents them from comparing equal is folded
   to a constant.  */

void test_array_lit (void)
{
  A (strcmp (a4, "1234")); clobber (a4);
  A (strcmp (a4, "12345")); clobber (a4);
  A (strcmp (a4, "123456")); clobber (a4);
  A (strcmp ("1234", a4)); clobber (a4);
  A (strcmp ("12345", a4)); clobber (a4);
  A (strcmp ("123456", a4)); clobber (a4);
}

void test_memarray_lit (struct S *p)
{
  A (strcmp (p->a4, "1234"));
  A (strcmp (p->a4, "12345"));
  A (strcmp (p->a4, "123456"));

  A (strcmp ("1234", p->a4));
  A (strcmp ("12345", p->a4));
  A (strcmp ("123456", p->a4));
}

/* Verify that the equality of empty strings is folded.  */

void test_empty_string (void)
{
  A (0 == strcmp ("", ""));

  *a4 = '\0';
  A (0 == strcmp (a4, ""));
  A (0 == strcmp ("", a4));
  A (0 == strcmp (a4, a4));

  char s[8] = "";
  A (0 == strcmp (a4, s));

  a4[1] = '\0';
  b4[1] = '\0';
  A (0 == strcmp (a4 + 1, b4 + 1));

  a4[2] = '\0';
  b4[2] = '\0';
  A (0 == strcmp (&a4[2], &b4[2]));

#if 0
  /* The following isn't handled yet due to PR 92155.  */
  clobber (a4, b4);

  memset (a4, 0, sizeof a4);
  memset (b4, 0, sizeof b4);
  A (0 == strcmp (a4, b4));
#endif
}

/* Verify that comparison of dynamically created strings with unknown
   arrays is folded.  */

void test_array_copy (void)
{
  char s[8];
  strcpy (s, "1234");
  A (strcmp (a4, s));

  strcpy (s, "12345");
  A (strlen (s) == 5);
  A (strcmp (a4, s)); clobber (a4);

  strcpy (s, "123456");
  A (strcmp (a4, s)); clobber (a4);

  strcpy (s, "1234");
  A (strcmp (s, a4)); clobber (a4);

  strcpy (s, "12345");
  A (strcmp (s, a4)); clobber (a4);

  strcpy (s, "123456");
  A (strcmp (s, a4)); clobber (a4);
}


void test_array_bounded (void)
{
  A (strncmp (a4, "12345", 5)); clobber (a4);
  A (strncmp ("54321", a4, 5)); clobber (a4);

  A (strncmp (a4, "123456", 5)); clobber (a4);
  A (strncmp ("654321", a4, 5)); clobber (a4);
}

void test_array_copy_bounded (void)
{
  char s[8];
  strcpy (s, "12345");
  A (strncmp (a4, s, 5)); clobber (a4);
  strcpy (s, "54321");
  A (strncmp (s, a4, 5)); clobber (a4);

  strcpy (s, "123456");
  A (strncmp (a4, s, 5)); clobber (a4);
  strcpy (s, "654321");
  A (strncmp (s, a4, 5)); clobber (a4);
}

/* { dg-final { scan-tree-dump-not "abort|strcmp|strncmp" "optimized" } } */
