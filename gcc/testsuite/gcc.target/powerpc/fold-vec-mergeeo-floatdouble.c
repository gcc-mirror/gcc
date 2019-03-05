/* Verify that overloaded built-ins for vec_mergee and vec_mergeo
 with float and double inputs produce the right codegen.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mpower8-vector " } */

#include <altivec.h>

/*
	vector float foo = vec_mergee (vector float, vector float);
	vector float foo = vec_mergeo (vector float, vector float);
	vector double foo = vec_mergee (vector double , vector double);
	vector double foo = vec_mergeo (vector double , vector double);
*/

vector float
testf_ee (vector float vf1, vector float vf2)
{
  return vec_mergee (vf1, vf2);
}

vector float
testf_eo (vector float vf1, vector float vf2)
{
  return vec_mergeo (vf1, vf2);
}

vector double
testd_ee ( vector double vd1, vector double vd2)
{
  return vec_mergee (vd1, vd2);
}

vector double
testd_eo ( vector double vd1, vector double vd2)
{
  return vec_mergeo (vd1, vd2);
}
/* Doubles will generate vmrg*w instructions.  */
/* { dg-final { scan-assembler-times "vmrgow" 1 } } */
/* { dg-final { scan-assembler-times "vmrgew" 1 } } */
/* Floats will generate some number of xxpermdi instructions.  Ensure we get at least one. */
/* { dg-final { scan-assembler "xxpermdi" } } */

