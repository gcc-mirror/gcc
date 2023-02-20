/* PR ipa/108605 */
/* { dg-do compile { target { lp64 || llp64 } } } */
/* { dg-options "-O2" } */

struct S {
  char a, b, c;
  int d[__INT_MAX__], e;
};

void
foo (struct S *s)
{
  if (s->b && s->c != 0)
    __builtin_abort ();
}

void
bar (void)
{
  struct S s[2];
  s[0].a = 0;
  s[0].e = 0;
  foo (s);
}
