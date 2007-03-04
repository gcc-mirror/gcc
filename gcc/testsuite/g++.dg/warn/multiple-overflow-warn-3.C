/* PR 30465 : Test for duplicated warnings in a conversion.  */
/* { dg-do compile } */
/* { dg-options "-Woverflow" } */

wchar_t
g (void)
{
  wchar_t wc = ((wchar_t)1 << 31) - 1; /* { dg-bogus "overflow .* overflow" } */
  /* { dg-warning "overflow" "" { target *-*-* } 8 } */
  return wc;
}

