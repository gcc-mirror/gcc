/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

/* It doesn't really matter if this produces errors about redefinitions,
   but it mustn't trigger an ICE.  */
#pragma GCC arm "arm_mve_types.h"
#pragma GCC arm "arm_mve_types.h" /* { dg-error "duplicate definition of 'arm_mve_types.h'" } */
