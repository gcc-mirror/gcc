/* PR other/62008 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void f(int *a, int w, int h)
{
  int tmp[w][h];
  tmp[:][:] = a[0:w][0:h]; /* { dg-error "base of array section must be pointer or array type" } */
  /* { dg-error "start-index and length fields necessary" "" { target c } .-1 } */
}
