/* PR c/61191 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

double f(double * A, double * B)
{
  return __sec_reduce_add((B[0:500])(;
/* { dg-error "expected expression before ';' token" "" {target *-*-*} 7 } */
/* { dg-error "called object" "" {target *-*-*} 7 } */
} /* { dg-error "expected" } */
