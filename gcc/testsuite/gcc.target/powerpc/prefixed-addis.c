/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_assemble_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Test whether the xvrl (vector word rotate left using VSX registers insead of
   Altivec registers is generated.  */

#include <stddef.h>

size_t
prefix_addis_addi (size_t x)
{
  return x + 0x123456789ABCDEUL;	/* paddis + paddi.  */
}

size_t
prefix_addis (size_t x)
{
  return x + 0x12345600000000UL;	/* paddis.  */
}

/* { dg-final { scan-assembler-times {\mpaddis\M} 2  } } */
/* { dg-final { scan-assembler-times {\mpaddi\M}  1  } } */
