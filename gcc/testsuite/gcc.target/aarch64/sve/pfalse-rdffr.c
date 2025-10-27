/* { dg-do compile } */
/* { dg-require-effective-target elf } */
/* { dg-options "-O2 -funwind-tables" } */

#include <arm_sve.h>

svbool_t rdffr_f ()
{
  return svrdffr_z (svpfalse_b ());
}

/* { dg-final { scan-assembler-times {\t.cfi_startproc\n\tpfalse\tp0\.b\n\tret\n} 1 } } */
/* { dg-final { scan-assembler-times {\t.cfi_startproc\n} 1 } } */
