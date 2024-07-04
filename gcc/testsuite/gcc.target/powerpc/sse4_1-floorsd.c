/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128d
#define FP_T double

#define ROUND_INTRIN(x, y) _mm_floor_sd (x, y)

#include "sse4_1-round-data.h"

static struct data2 data[] = {
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.00, IGNORED } },
    .answer = {  0.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.25, IGNORED } },
    .answer = {  0.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.50, IGNORED } },
    .answer = {  0.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.75, IGNORED } },
    .answer = {  0.0, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffcp+50, IGNORED } },
    .answer = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffdp+50, IGNORED } },
    .answer = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffep+50, IGNORED } },
    .answer = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffffffffffp+50, IGNORED } },
    .answer = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000000p+51, IGNORED } },
    .answer = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000001p+51, IGNORED } },
    .answer = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000002p+51, IGNORED } },
    .answer = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000003p+51, IGNORED } },
    .answer = {  0x1.0000000000002p+51, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffep+51, IGNORED } },
    .answer = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffffffffffp+51, IGNORED } },
    .answer = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000000p+52, IGNORED } },
    .answer = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000001p+52, IGNORED } },
    .answer = {  0x1.0000000000001p+52, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000001p+52, IGNORED } },
    .answer = { -0x1.0000000000001p+52, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000000p+52, IGNORED } },
    .answer = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffffffffffp+51, IGNORED } },
    .answer = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffep+51, IGNORED } },
    .answer = { -0x1.ffffffffffffep+51, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000003p+51, IGNORED } },
    .answer = { -0x1.0000000000004p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000002p+51, IGNORED } },
    .answer = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000001p+51, IGNORED } },
    .answer = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000000p+51, IGNORED } },
    .answer = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffcp+50, IGNORED } },
    .answer = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffep+50, IGNORED } },
    .answer = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffdp+50, IGNORED } },
    .answer = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffcp+50, IGNORED } },
    .answer = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -1.00, IGNORED } },
    .answer = { -1.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.75, IGNORED } },
    .answer = { -1.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.50, IGNORED } },
    .answer = { -1.0, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.25, IGNORED } },
    .answer = { -1.0, PASSTHROUGH } }
};

#include "sse4_1-round2.h"
