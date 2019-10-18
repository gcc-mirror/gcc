/* PR tree-optimization/92157 - incorrect strcmp() == 0 result for unknown
   strings​
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"


char a2[2], a3[3];


static inline __attribute__ ((always_inline)) int
verify_not_equal (const char *s, const char *t, int x)
{
  int n = x < 0 ? strlen (s) : 0 < x ? strlen (t) : strlen (s) + strlen (t);

  if (strcmp (t, s) == 0)
    abort ();

  return n;
}

__attribute__ ((noipa)) int test_a2_s (const char *s)
{
  return verify_not_equal (a2, s, 0);
}

__attribute__ ((noipa)) int test_a2_a3 (void)
{
  return verify_not_equal (a2, a3, 0);
}

__attribute__ ((noipa)) int test_a3_a2 (void)
{
  return verify_not_equal (a3, a2, 0);
}

__attribute__ ((noipa)) int test_s_a2 (const char *s)
{
  return verify_not_equal (s, a2, 0);
}


__attribute__ ((noipa)) int test_a2_s_1 (const char *s)
{
  return verify_not_equal (a2, s, -1);
}

__attribute__ ((noipa)) int test_a2_a3_1 (void)
{
  return verify_not_equal (a2, a3, -1);
}

__attribute__ ((noipa)) int test_a3_a2_1 (void)
{
  return verify_not_equal (a3, a2, -1);
}

__attribute__ ((noipa)) int test_s_a2_1 (const char *s)
{
  return verify_not_equal (s, a2, -1);
}


__attribute__ ((noipa)) int test_a2_s_2 (const char *s)
{
  return verify_not_equal (a2, s, +1);
}

__attribute__ ((noipa)) int test_a2_a3_2 (void)
{
  return verify_not_equal (a2, a3, +1);
}

__attribute__ ((noipa)) int test_a3_a2_2 (void)
{
  return verify_not_equal (a3, a2, +1);
}

__attribute__ ((noipa)) int test_s_a2_2 (const char *s)
{
  return verify_not_equal (s, a2, +1);
}

int main (void)
{
  a2[0] = '1';
  a3[0] = '1';
  a3[0] = '2';

  test_a2_s ("");
  test_a2_a3 ();
  test_a3_a2 ();
  test_s_a2 ("");

  test_a2_s_1 ("");
  test_a2_a3_1 ();
  test_a3_a2_1 ();
  test_s_a2_1 ("");

  test_a2_s_2 ("");
  test_a2_a3_2 ();
  test_a3_a2_2 ();
  test_s_a2_2 ("");
}
