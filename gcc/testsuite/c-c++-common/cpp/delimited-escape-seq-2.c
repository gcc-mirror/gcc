/* P2290R3 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

int jalape\u{f1}o = 42;

int
caf\u{000e9} (void)
{
  return jalape\u00F1o;
}

int
test (void)
{
  return caf\u00e9 ();
}
