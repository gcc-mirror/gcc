/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

int jalape\N{LATIN SMALL LETTER N WITH TILDE}o = 42;

int
caf\N{LATIN SMALL LETTER E WITH ACUTE} (void)
{
  return jalape\u00F1o;
}

int
test (void)
{
  return caf\u00e9 ();
}
