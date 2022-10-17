/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mpower8-vector -Wno-psabi" } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN(x, y) _mm_ceil_ss (x, y)

#include "sse4_1-round-data.h"

static struct data2 data[] = {
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.00,  IGNORED, IGNORED, IGNORED } },
    .answer = {  0.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.25, IGNORED, IGNORED, IGNORED } },
    .answer = {  1.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.50, IGNORED, IGNORED, IGNORED } },
    .answer = {  1.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.75, IGNORED, IGNORED, IGNORED } },
    .answer = {  1.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffff8p+21, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffap+21, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffcp+21, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+21, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffap+22, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffcp+22, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+22, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.000000p+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+23, IGNORED, IGNORED, IGNORED } },
    .answer = {  0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+23, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+22, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffcp+22, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffap+22, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+21, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffcp+21, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffap+21, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffff8p+21, IGNORED, IGNORED, IGNORED } },
    .answer = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -1.00, IGNORED, IGNORED, IGNORED } },
    .answer = { -1.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.75, IGNORED, IGNORED, IGNORED } },
    .answer = {  0.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.50, IGNORED, IGNORED, IGNORED } },
    .answer = {  0.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.25, IGNORED, IGNORED, IGNORED } },
    .answer = {  0.0, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } }
};

#include "sse4_1-round2.h"
