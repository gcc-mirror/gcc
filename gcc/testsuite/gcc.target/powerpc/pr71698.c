/* Test for a reload ICE arising from trying to direct move a TDmode value.  */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target dfp } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-O1 -mcpu=power9" } */

extern void testvad128 (int n, ...);
void
testitd128 (_Decimal128 g01d128)
{
  testvad128 (1, g01d128);
}
