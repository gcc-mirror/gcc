/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O1 -mvsx" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

typedef float vecf __attribute__ ((vector_size (16)));
extern vecf j, k;

void fun (void)
{
  j = k;
}

