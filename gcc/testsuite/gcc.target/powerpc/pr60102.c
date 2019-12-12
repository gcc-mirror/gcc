/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=8548 -mspe -mabi=spe -g -mfloat-gprs=double" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } } */

double
pr60102 (double x, int m)
{
  double y;
  y =  m % 2 ? x : 1;
  return y;
}
