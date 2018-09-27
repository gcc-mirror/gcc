/* { dg-do compile } */
/* { dg-options "-O -fprofile-use -fopt-info -Wno-missing-profile" } */
/* { dg-options "-O -m4 -fprofile-use -fopt-info -Wno-missing-profile" { target sh-*-* } } */

void foo (int *p)
{
  if (p)
    *p = 0;
} /* { dg-message "note: \[^\n\]*execution counts estimated" } */
