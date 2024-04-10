/* Test for a reload ICE arising from trying to direct move a TDmode value.  */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O1 -mdejagnu-cpu=power9 -mvsx" } */

extern void testvad128 (int n, ...);
void
testitd128 (_Decimal128 g01d128)
{
  testvad128 (1, g01d128);
}
