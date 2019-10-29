/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svuint64_t
#define DUPQ svdupq_lane_u64
#define INDEX svindex_u64
#define COUNT 2

#include "dupq_lane_1.c"
