/* PRE tree-optimization/90626 - fold strcmp(a, b) == 0 to zero when
   one string length is exact and the other is unequal
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))


__attribute__ ((noclone, noinline, noipa)) void
clobber (void *p, int x, size_t n)
{
  for (volatile unsigned char *q = p; n--; )
    *q = x;
}

__attribute__ ((noclone, noinline, noipa)) void
test_strcmp (void)
{
  char a[8], b[8];
  strcpy (a, "1235");
  strcpy (b, "1234");

  A (strcmp (a, b));

  clobber (a, 0, sizeof a);
  clobber (b, 0, sizeof b);
  clobber (b + 4, '5', 1);

  memcpy (a, "1234", 4);
  memcpy (b, "1234", 4);

  A (0 > strcmp (a, b));
  A (0 < strcmp (b, a));
}

__attribute__ ((noclone, noinline, noipa)) void
test_strncmp (void)
{
  char a[8], b[8];
  strcpy (a, "1235");
  strcpy (b, "1234");

  A (0 == strncmp (a, b, 1));
  A (0 == strncmp (a, b, 2));
  A (0 == strncmp (a, b, 3));
  A (0 <  strncmp (a, b, 4));
  A (0 >  strncmp (b, a, 4));

  clobber (a, 0, sizeof a);
  clobber (b, 0, sizeof b);
  clobber (b + 4, '5', 1);

  memcpy (a, "1234", 4);
  memcpy (b, "1234", 4);

  A (0 == strncmp (a, b, 4));
  A (0 >  strncmp (a, b, 5));
  A (0 <  strncmp (b, a, 5));
}


__attribute__ ((noclone, noinline, noipa)) void
test_strncmp_a4_cond_s5_s2_2 (const char *s, int i)
{
  char a4[4];
  strcpy (a4, s);
  A (0 == strncmp (a4, i ? "12345" : "12", 2));
}


__attribute__ ((noclone, noinline, noipa)) void
test_strncmp_a4_cond_a5_s2_5 (const char *s, const char *t, int i)
{
  char a4[4], a5[5];
  strcpy (a4, s);
  strcpy (a5, t);
  A (0 == strncmp (a4, i ? a5 : "12", 5));
}

__attribute__ ((noclone, noinline, noipa)) void
test_strncmp_a4_cond_a5_a3_n (const char *s1, const char *s2, const char *s3,
			      int i, unsigned n)
{
  char a3[3], a4[4], a5[5];
  strcpy (a3, s1);
  strcpy (a4, s2);
  strcpy (a5, s3);
  A (0 == strncmp (a4, i ? a5 : a3, n));
}


int main (void)
{
  test_strcmp ();
  test_strncmp ();
  test_strncmp_a4_cond_s5_s2_2 ("12", 0);
  test_strncmp_a4_cond_a5_s2_5 ("12", "1234", 0);

  test_strncmp_a4_cond_a5_a3_n ("12", "1", "1",    0, 1);
  test_strncmp_a4_cond_a5_a3_n ("",   "1", "1234", 1, 1);

  test_strncmp_a4_cond_a5_a3_n ("12", "12", "1",    0, 2);
  test_strncmp_a4_cond_a5_a3_n ("",   "12", "1234", 1, 2);

  test_strncmp_a4_cond_a5_a3_n ("12", "123", "1",    0, 2);
  test_strncmp_a4_cond_a5_a3_n ("",   "123", "1234", 1, 3);
}
