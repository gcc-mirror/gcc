/* Verify that overloaded built-ins for vec_mergee and vec_mergeo
 with long long inputs produce the right codegen.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -mdejagnu-cpu=power8" } */

#include <altivec.h>

vector bool long long
testbi_ee (vector bool long long v1, vector bool long long v2)
{
  return vec_mergee (v1, v2);
}

vector bool long long
testbi_eo (vector bool long long v1, vector bool long long v2)
{
  return vec_mergeo (v1, v2);
}

vector signed long long
testsi_ee (vector signed long long v1, vector signed long long v2)
{
  return vec_mergee (v1, v2);
}

vector signed long long
testsi_eo (vector signed long long v1, vector signed long long v2)
{
  return vec_mergeo (v1, v2);
}

vector unsigned long long
testui_ee (vector unsigned long long v1, vector unsigned long long v2)
{
  return vec_mergee (v1, v2);
}

vector unsigned long long
testui_eo (vector unsigned long long v1, vector unsigned long long v2)
{
  return vec_mergeo (v1, v2);
}

/* long long ...   */
/* vec_mergee and vec_mergeo codegen will consist of some number of
 xxpermdi instructions that will vary.  Ensure we get at least one. */
/* { dg-final { scan-assembler "xxpermdi" } } */

