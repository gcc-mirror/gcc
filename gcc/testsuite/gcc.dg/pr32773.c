/* { dg-do compile } */
/* { dg-options "-O -fprofile-use -fopt-info-missed-ipa -Wno-missing-profile" } */
/* { dg-options "-O -m4 -fprofile-use -fopt-info-missed-ipa -Wno-missing-profile" { target sh-*-* } } */

void foo (int *p)
{
  if (p)
    *p = 0;
} /* { dg-missed "\[^\n\]*execution counts estimated" } */
