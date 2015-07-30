/* { dg-do compile } */
/* { dg-options "-Wtautological-compare" } */
/* Test we don't warn for floats.  */

struct S { double d; float f; };

void
fn1 (int i, float f, double d, struct S *s, float *fp)
{
  if (f == f);
  if (f != f);
  if (d == d);
  if (d != d);
  if (fp[i] == fp[i]);
  if (fp[i] != fp[i]);
  if (s->f == s->f);
  if (s->f != s->f);
  if (s->d == s->d);
  if (s->d != s->d);
}
