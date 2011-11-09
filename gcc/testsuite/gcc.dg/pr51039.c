/* { dg-do compile } */
/* { dg-options "-O -finline-small-functions -fno-ipa-pure-const" } */

float baz (void)
{
  return 0;
}

static inline int bar (int (*ibaz) (void))
{
  return ibaz ();
}

void foo (void)
{
  bar((int (*)(void))baz);
}
