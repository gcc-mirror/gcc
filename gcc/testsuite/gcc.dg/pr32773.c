/* { dg-do compile } */
/* { dg-options "-O -fprofile-use" } */
/* { dg-options "-O -m4 -fprofile-use" { target sh-*-* } } */

void foo (int *p)
{
  if (p)
    *p = 0;
} /* { dg-message "note: \[^\n\]*execution counts estimated" } */
