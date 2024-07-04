/* { dg-do compile  } */
/* { dg-options "-O2 -maltivec -mabi=altivec -mdejagnu-cpu=cell" } */
/* { dg-require-effective-target powerpc_altivec } */
#include <altivec.h>

/* This used to ICE with reloading of a constant address. */

vector float f(void)
{
  vector float * a = (void*)16;
  return vec_lvlx (0, a);
} 
