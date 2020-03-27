/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo ()
{
  return vmvnq_n_s32 (2);
}

/* { dg-final { scan-assembler "vmvn.i32"  }  } */
