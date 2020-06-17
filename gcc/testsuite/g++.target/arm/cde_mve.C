/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

/* Ensure this compiles.  */
#include "arm_cde.h"
int foo ()
{
  return 1;
}
