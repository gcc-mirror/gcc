/* { dg-do compile } */
/* { dg-options "-mcpu=cortex-m55" } */

_Accum sa;
char c;

void
div_csa ()
{
  c /= sa;
}
