/* PR 85623 - strncmp() warns about attribute 'nonstring' incorrectly
   in -Wstringop-overflow
  { dg-do compile }
  { dg-options "-O2 -Wstringop-overread -ftrack-macro-expansion=0" } */

#include "../gcc.dg/range.h"

#if __cplusplus
extern "C" {
#endif

extern int strcmp (const char*, const char*);
extern int strncmp (const char*, const char*, size_t);
extern int strncasecmp (const char*, const char*, size_t);

extern size_t strspn (const char*, const char*);
extern size_t strcspn (const char*, const char*);

#if __cplusplus
}
#endif

#define S26 "0123456789abcdefghijklmnopqrstuvwxyz"
#define S(n) (S26 + sizeof S26 - 1 - (n))

char __attribute__ ((nonstring)) a3[3];
char __attribute__ ((nonstring)) a5[5];

void sink (int);

#define T(call)   sink (call)

void test_strcmp_cst (void)
{
  /* Verify that no warning is issued for strcmp() calls with a non-string
     array argument when the other argument is a string whose length is
     less than the size of the array.  Because the function stops reading
     at the first nul character there is no chance that it will read past
     the end of the array.  */
  T (strcmp (S (0), a3));
  T (strcmp (S (1), a3));
  T (strcmp (S (2), a3));
  /* The following reads a3[3].  */
  T (strcmp (S (3), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  /* The following also reads past the end of a3.  */
  T (strcmp (S (9), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strcmp (a3, S (0)));
  T (strcmp (a3, S (1)));
  T (strcmp (a3, S (2)));
  T (strcmp (a3, S (3)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcmp (a3, S (9)));   /* { dg-warning "\\\[-Wstringop-overread" } */
}


void test_strcmp_range (const char *s)
{
  s = signed_value () < 0 ? S (0) : S (1);
  T (strcmp (a3, s));

  s = signed_value () < 0 ? S (0) : S (2);
  T (strcmp (a3, s));

  s = signed_value () < 0 ? S (0) : S (3);
  T (strcmp (a3, s));       /* { dg-warning "\\\[-Wstringop-overread" } */

  s = signed_value () < 0 ? S (1) : S (2);
  T (strcmp (a3, s));

  s = signed_value () < 0 ? S (1) : S (3);
  T (strcmp (a3, s));       /* { dg-warning "\\\[-Wstringop-overread" } */

  s = signed_value () < 0 ? S (3) : S (4);
  T (strcmp (a3, s));       /* { dg-warning "\\\[-Wstringop-overread" } */
}


void test_strncmp_cst (void)
{
  T (strncmp (S (0), a3, 1));
  T (strncmp (S (1), a3, 2));
  T (strncmp (S (2), a3, 3));
  T (strncmp (S (3), a3, 3));
  T (strncmp (S (3), a3, 4));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncmp (S (9), a3, 3));
  T (strncmp (S (9), a3, 4));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strncmp (S (9), a3, 5));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncmp (a3, S (0), 1));
  T (strncmp (a3, S (1), 2));
  T (strncmp (a3, S (2), 3));
  T (strncmp (a3, S (3), 3));
  T (strncmp (a3, S (3), 4));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncmp (a3, S (9), 3));
  T (strncmp (a3, S (9), 4));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strncmp (a3, S (9), 5));   /* { dg-warning "\\\[-Wstringop-overread" } */
}

void test_strncmp_range (const char *s)
{
  T (strncmp (a3, S (2), UR (0, 3)));
  T (strncmp (a3, S (2), UR (1, 4)));
  T (strncmp (a3, S (2), UR (2, 5)));
  T (strncmp (a3, S (2), UR (3, 6)));
  T (strncmp (a3, S (2), UR (4, 7)));

  T (strncmp (a3, S (5), UR (0, 3)));
  T (strncmp (a3, S (5), UR (1, 4)));
  T (strncmp (a3, S (5), UR (2, 5)));
  T (strncmp (a3, S (5), UR (3, 6)));
  T (strncmp (a3, S (5), UR (4, 7)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strncmp (a3, S (5), UR (7, 9)));   /* { dg-warning "\\\[-Wstringop-overread" } */

  s = signed_value () < 0 ? S (0) : S (1);
  T (strncmp (a3, s, UR (1, 3)));
  T (strncmp (a3, s, UR (2, 5)));

  s = signed_value () < 0 ? S (2) : S (5);
  T (strncmp (a3, s, UR (1, 3)));

  s = signed_value () < 0 ? S (2) : S (5);
  T (strncmp (a3, s, UR (1, 4)));
  T (strncmp (a3, s, UR (2, 5)));
  T (strncmp (a3, s, UR (3, 6)));
  T (strncmp (a3, s, UR (4, 7)));       /* { dg-warning "\\\[-Wstringop-overread" } */
}

void test_strncasecmp (void)
{
  T (strncasecmp (S (0), a3, 1));
  T (strncasecmp (S (1), a3, 2));
  T (strncasecmp (S (2), a3, 3));
  T (strncasecmp (S (3), a3, 3));
  T (strncasecmp (S (3), a3, 4));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncasecmp (S (9), a3, 3));
  T (strncasecmp (S (9), a3, 4));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strncasecmp (S (9), a3, 5));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncasecmp (a3, S (0), 1));
  T (strncasecmp (a3, S (1), 2));
  T (strncasecmp (a3, S (2), 3));
  T (strncasecmp (a3, S (3), 3));
  T (strncasecmp (a3, S (3), 4));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strncasecmp (a3, S (9), 3));
  T (strncasecmp (a3, S (9), 4));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strncasecmp (a3, S (9), 5));   /* { dg-warning "\\\[-Wstringop-overread" } */
}

void test_strspn (void)
{
  /* strspn must traverse all characters in the second argument except
     when the first string is empty. */
  T (strspn (S (0), a3));
  T (strspn (S (1), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (S (2), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (S (3), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (S (9), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */

  /* Similarly, strspn must traverse all characters in the first argument
     except when the second string is empty. */
  T (strspn (a3, S (0)));
  T (strspn (a3, S (1)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (a3, S (2)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (a3, S (3)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strspn (a3, S (9)));   /* { dg-warning "\\\[-Wstringop-overread" } */
}

void test_strcspn (void)
{
  T (strcspn (S (0), a3));
  T (strcspn (S (1), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (S (2), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (S (3), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (S (9), a3));   /* { dg-warning "\\\[-Wstringop-overread" } */

  T (strcspn (a3, S (0)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (a3, S (1)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (a3, S (2)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (a3, S (3)));   /* { dg-warning "\\\[-Wstringop-overread" } */
  T (strcspn (a3, S (9)));   /* { dg-warning "\\\[-Wstringop-overread" } */
}
