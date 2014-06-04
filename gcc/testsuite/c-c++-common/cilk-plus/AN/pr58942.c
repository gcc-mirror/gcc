/* PR c/58942 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int foo (int*p, int i)
{
  return __sec_reduce_max_ind(p[1:i]);
}
