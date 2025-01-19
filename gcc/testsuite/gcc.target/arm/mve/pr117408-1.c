/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

/* It doesn't really matter if this produces errors missing types,
      but it mustn't trigger an ICE.  */
#pragma GCC arm "arm_mve.h" false /* { dg-error "this definition requires MVE types, please include 'arm_mve_types.h'" } */
