/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svint32_t
#define DUPQ svdupq_lane_s32
#define INDEX svindex_s32
#define COUNT 4

#include "dupq_lane_1.c"
