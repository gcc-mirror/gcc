/* { dg-do compile } */
/* { dg-options "-O1 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler {\m(lxvd2x|lxv)\M} } } */
/* { dg-final { scan-assembler {\m(stxvd2x|stxv)\M} } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

typedef float vecf __attribute__ ((vector_size (16)));
extern vecf j, k;

void fun (void)
{
  j = k;
}

