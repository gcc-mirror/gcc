/* Verify that overloaded built-ins for vec_mergee and vec_mergeo
 with int inputs produce the right codegen.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

#include <altivec.h>

vector bool int
testbi_ee (vector bool int v1, vector bool int v2)
{
  return vec_mergee (v1, v2);
}

vector signed int
testsi_ee (vector signed int v1, vector signed int v2)
{
  return vec_mergee (v1, v2);
}

vector unsigned int
testui_ee (vector unsigned int v1, vector unsigned int v2)
{
  return vec_mergee (v1, v2);
}

vector bool int
testbi_eo (vector bool int v1, vector bool int v2)
{
  return vec_mergeo (v1, v2);
}

vector signed int
testsi_eo (vector signed int v1, vector signed int v2)
{
  return vec_mergeo (v1, v2);
}

vector unsigned int
testui_eo (vector unsigned int v1, vector unsigned int v2)
{
  return vec_mergeo (v1, v2);
}
/* { dg-final { scan-assembler-times "vmrgew" 3 } } */
/* { dg-final { scan-assembler-times "vmrgow" 3 } } */

