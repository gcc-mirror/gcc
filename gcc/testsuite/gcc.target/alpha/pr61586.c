/* { dg-do compile } */
/* { dg-options "-mieee" } */

void foo (int *dimensions, double **params, int hh)
{
  if (params[hh])
    ;
  else if (dimensions[hh] > 0)
    params[hh][0] = 1.0f;
}
