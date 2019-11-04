/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svint16_t
#define DUPQ svdupq_lane_s16
#define INDEX svindex_s16
#define COUNT 8

#include "dupq_lane_1.c"
