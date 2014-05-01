/* PR c/43245 */
/* { dg-do compile } */
/* { dg-options "-Wno-discarded-qualifiers" } */

void
foo (char *p)
{
}

char *
bar (void)
{
  const char *s = "foo";
  char *s1 = s;
  s1 = s;
  foo (s);
  return s;
}
