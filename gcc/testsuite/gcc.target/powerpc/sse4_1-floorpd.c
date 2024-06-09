/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128d
#define FP_T double

#define ROUND_INTRIN(x, mode) _mm_floor_pd (x)

#include "sse4_1-round-data.h"

static struct data data[] = {
  { .value = { .f = {  0.00,  0.25 } }, .answer = {  0.0,  0.0 } },
  { .value = { .f = {  0.50,  0.75 } }, .answer = {  0.0,  0.0 } },

  { { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffdp+50 } },
           {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { { .f = {  0x1.ffffffffffffep+50,  0x1.0000000000000p+51 } },
           {  0x1.ffffffffffffcp+50,  0x1.0000000000000p+51 } },
  { { .f = {  0x1.0000000000000p+51,  0x1.0000000000001p+51 } },
           {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { { .f = {  0x1.0000000000002p+51,  0x1.0000000000003p+51 } },
           {  0x1.0000000000002p+51,  0x1.0000000000002p+51 } },

  { { .f = {  0x1.ffffffffffffep+51,  0x1.fffffffffffffp+51 } },
           {  0x1.ffffffffffffep+51,  0x1.ffffffffffffep+51 } },
  { { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },
           {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },

  { { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
           { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
  { { .f = { -0x1.fffffffffffffp+51, -0x1.ffffffffffffep+52 } },
           { -0x1.0000000000000p+52, -0x1.ffffffffffffep+52 } },

  { { .f = { -0x1.0000000000003p+51, -0x1.0000000000002p+51 } },
           { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } },
  { { .f = { -0x1.0000000000001p+51, -0x1.0000000000000p+51 } },
           { -0x1.0000000000002p+51, -0x1.0000000000000p+51 } },
  { { .f = { -0x1.fffffffffffffp+50, -0x1.ffffffffffffep+50 } },
           { -0x1.0000000000000p+51, -0x1.0000000000000p+51 } },
  { { .f = { -0x1.ffffffffffffdp+50, -0x1.ffffffffffffcp+50 } },
           { -0x1.0000000000000p+51, -0x1.ffffffffffffcp+50 } },

  { { .f = { -1.00, -0.75 } }, { -1.0, -1.0 } },
  { { .f = { -0.50, -0.25 } }, { -1.0, -1.0 } }
};

#include "sse4_1-round.h"
