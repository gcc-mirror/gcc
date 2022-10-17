/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */

int mve_pred16_t; /* { dg-message "note: previous declaration of 'mve_pred16_t'" } */
int int8x16_t; /* { dg-message "note: previous declaration of 'int8x16_t'" } */
int int16x8_t; /* { dg-message "note: previous declaration of 'int16x8_t'" } */
int int32x4_t; /* { dg-message "note: previous declaration of 'int32x4_t'" } */
int int64x2_t; /* { dg-message "note: previous declaration of 'int64x2_t'" } */
int uint8x16_t; /* { dg-message "note: previous declaration of 'uint8x16_t'" } */
int uint16x8_t; /* { dg-message "note: previous declaration of 'uint16x8_t'" } */
int uint32x4_t; /* { dg-message "note: previous declaration of 'uint32x4_t'" } */
int uint64x2_t; /* { dg-message "note: previous declaration of 'uint64x2_t'" } */
int float16x8_t; /* { dg-message "note: previous declaration of 'float16x8_t'" } */
int float32x4_t; /* { dg-message "note: previous declaration of 'float32x4_t'" } */
int int8x16x2_t; /* { dg-message "note: previous declaration of 'int8x16x2_t'" } */
int int8x16x4_t; /* { dg-message "note: previous declaration of 'int8x16x4_t'" } */
int int16x8x2_t; /* { dg-message "note: previous declaration of 'int16x8x2_t'" } */
int int16x8x4_t; /* { dg-message "note: previous declaration of 'int16x8x4_t'" } */
int int32x4x2_t; /* { dg-message "note: previous declaration of 'int32x4x2_t'" } */
int int32x4x4_t; /* { dg-message "note: previous declaration of 'int32x4x4_t'" } */
int int64x2x2_t; /* { dg-message "note: previous declaration of 'int64x2x2_t'" } */
int int64x2x4_t; /* { dg-message "note: previous declaration of 'int64x2x4_t'" } */
int uint8x16x2_t; /* { dg-message "note: previous declaration of 'uint8x16x2_t'" } */
int uint8x16x4_t; /* { dg-message "note: previous declaration of 'uint8x16x4_t'" } */
int uint16x8x2_t; /* { dg-message "note: previous declaration of 'uint16x8x2_t'" } */
int uint16x8x4_t; /* { dg-message "note: previous declaration of 'uint16x8x4_t'" } */
int uint32x4x2_t; /* { dg-message "note: previous declaration of 'uint32x4x2_t'" } */
int uint32x4x4_t; /* { dg-message "note: previous declaration of 'uint32x4x4_t'" } */
int uint64x2x2_t; /* { dg-message "note: previous declaration of 'uint64x2x2_t'" } */
int uint64x2x4_t; /* { dg-message "note: previous declaration of 'uint64x2x4_t'" } */
int float16x8x2_t; /* { dg-message "note: previous declaration of 'float16x8x2_t'" } */
int float16x8x4_t; /* { dg-message "note: previous declaration of 'float16x8x4_t'" } */
int float32x4x2_t; /* { dg-message "note: previous declaration of 'float32x4x2_t'" } */
int float32x4x4_t; /* { dg-message "note: previous declaration of 'float32x4x4_t'" } */

#pragma GCC arm "arm_mve_types.h"  /* { dg-error {'mve_pred16_t' redeclared} } */
  /* { dg-error {'int8x16_t' redeclared} "" {target *-*-*} .-1 } */
  /* { dg-error {'int16x8_t' redeclared} "" {target *-*-*} .-2 } */
  /* { dg-error {'int32x4_t' redeclared} "" {target *-*-*} .-3 } */
  /* { dg-error {'int64x2_t' redeclared} "" {target *-*-*} .-4 } */
  /* { dg-error {'uint8x16_t' redeclared} "" {target *-*-*} .-5 } */
  /* { dg-error {'uint16x8_t' redeclared} "" {target *-*-*} .-6 } */
  /* { dg-error {'uint32x4_t' redeclared} "" {target *-*-*} .-7 } */
  /* { dg-error {'uint64x2_t' redeclared} "" {target *-*-*} .-8 } */
  /* { dg-error {'float16x8_t' redeclared} "" {target *-*-*} .-9 } */
  /* { dg-error {'float32x4_t' redeclared} "" {target *-*-*} .-10 } */
  /* { dg-error {'int8x16x2_t' redeclared} "" {target *-*-*} .-11 } */
  /* { dg-error {'int8x16x4_t' redeclared} "" {target *-*-*} .-12 } */
  /* { dg-error {'int16x8x2_t' redeclared} "" {target *-*-*} .-13 } */
  /* { dg-error {'int16x8x4_t' redeclared} "" {target *-*-*} .-14 } */
  /* { dg-error {'int32x4x2_t' redeclared} "" {target *-*-*} .-15 } */
  /* { dg-error {'int32x4x4_t' redeclared} "" {target *-*-*} .-16 } */
  /* { dg-error {'int64x2x2_t' redeclared} "" {target *-*-*} .-17 } */
  /* { dg-error {'int64x2x4_t' redeclared} "" {target *-*-*} .-18 } */
  /* { dg-error {'uint8x16x2_t' redeclared} "" {target *-*-*} .-19 } */
  /* { dg-error {'uint8x16x4_t' redeclared} "" {target *-*-*} .-20 } */
  /* { dg-error {'uint16x8x2_t' redeclared} "" {target *-*-*} .-21 } */
  /* { dg-error {'uint16x8x4_t' redeclared} "" {target *-*-*} .-22 } */
  /* { dg-error {'uint32x4x2_t' redeclared} "" {target *-*-*} .-23 } */
  /* { dg-error {'uint32x4x4_t' redeclared} "" {target *-*-*} .-24 } */
  /* { dg-error {'uint64x2x2_t' redeclared} "" {target *-*-*} .-25 } */
  /* { dg-error {'uint64x2x4_t' redeclared} "" {target *-*-*} .-26 } */
  /* { dg-error {'float16x8x2_t' redeclared} "" {target *-*-*} .-27 } */
  /* { dg-error {'float16x8x4_t' redeclared} "" {target *-*-*} .-28 } */
  /* { dg-error {'float32x4x2_t' redeclared} "" {target *-*-*} .-29 } */
  /* { dg-error {'float32x4x4_t' redeclared} "" {target *-*-*} .-30 } */
