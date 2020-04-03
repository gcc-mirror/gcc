/* PR c/84919 - bogus -Wrestrict on sprintf %p with destination as argument
   { dg-do compile }
   -O2 isn't strictly necessary but setting also verifies that the sprintf/
   strlen pass doesn't warn with non-constant arguments.
   { dg-options "-O2 -Wall" } */

extern int sprintf (char* restrict, const char* restrict, ...);
extern int snprintf (char* restrict, __SIZE_TYPE__, const char* restrict, ...);

char a[32];

void test_warn (char *p)
{
  a[0] = 0;
  sprintf (a, "a=%s", a);     /* { dg-warning "-Wrestrict" } */

  p = a;
  char *q = p + 3;
  sprintf (p, "a=%s", q);     /* { dg-warning "-Wrestrict" } */
}

void test_nowarn_front_end (char *d)
{
  sprintf (d, "%p", d);
  snprintf (d, 32, "%p", d);

  sprintf (a, "p=%p", a);
  snprintf (a, sizeof a, "%p", a);
}

void test_nowarn_sprintf_pass (char *d)
{
  char *q = d;

  sprintf (d, "p=%p", q);
  snprintf (d, 32, "p=%p", q);

  q = a;
  sprintf (a, "a=%p", q);
  snprintf (a, sizeof a, "a=%p", q);
}
