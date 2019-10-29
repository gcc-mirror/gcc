/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svuint8_t
#define DUPQ svdupq_lane_u8
#define INDEX svindex_u8
#define COUNT 16

#include "dupq_lane_1.c"
