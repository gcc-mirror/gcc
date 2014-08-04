/* PR c++/61455 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void foo ()
{
  int a[2];
  int b = a[:]; /* { dg-error "cannot be scalar" } */
}
