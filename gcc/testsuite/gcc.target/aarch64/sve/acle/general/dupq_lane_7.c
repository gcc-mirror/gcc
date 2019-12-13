/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svint64_t
#define DUPQ svdupq_lane_s64
#define INDEX svindex_s64
#define COUNT 2

#include "dupq_lane_1.c"
