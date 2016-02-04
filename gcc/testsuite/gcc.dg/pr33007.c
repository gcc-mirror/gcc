/* { dg-do compile } */
/* { dg-options "-O -ffast-math -ftrapping-math" } */
/* { dg-warning "-fassociative-math disabled" "" { target *-*-* } 0 } */

long
foo (int i)
{
  float x;
  x = i;
  return __builtin_lroundf (x);
}
