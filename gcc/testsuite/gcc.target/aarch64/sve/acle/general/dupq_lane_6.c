/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svuint32_t
#define DUPQ svdupq_lane_u32
#define INDEX svindex_u32
#define COUNT 4

#include "dupq_lane_1.c"
