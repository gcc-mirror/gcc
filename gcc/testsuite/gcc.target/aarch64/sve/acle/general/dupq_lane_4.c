/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#define TYPE svuint16_t
#define DUPQ svdupq_lane_u16
#define INDEX svindex_u16
#define COUNT 8

#include "dupq_lane_1.c"
