/* { dg-do compile  } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=cell" } } */
/* { dg-options "-O2 -maltivec -mabi=altivec -mcpu=cell" } */
#include <altivec.h>

/* This used to ICE with reloading of a constant address. */

vector float f(void)
{
  vector float * a = (void*)16;
  return vec_lvlx (0, a);
} 
