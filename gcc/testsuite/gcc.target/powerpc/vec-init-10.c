/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */

/* Check that we can optimize sldi + or to rldimi for vector int init.  */

vector unsigned int
testu (unsigned int i1, unsigned int i2, unsigned int i3, unsigned int i4)
{
  vector unsigned int v = {i1, i2, i3, i4};
  return v;
}

vector signed int
tests (signed int i1, signed int i2, signed int i3, signed int i4)
{
  vector signed int v = {i1, i2, i3, i4};
  return v;
}

/* { dg-final { scan-assembler-not {\msldi\M} } } */
/* { dg-final { scan-assembler-not {\mor\M} } } */
/* { dg-final { scan-assembler-times {\mrldimi\M} 4 } } */
