/* Test cases for ST Microelectronics Loongson-2E/2F SIMD intrinsics.
   Copyright (C) 2008 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* { dg-do run { target { ! { mips*-mti-linux* mips*-img-linux* } } } } */
/* loongson.h does not handle or check for MIPS16ness or
   microMIPSness.  There doesn't seem any good reason for it to, given
   that the Loongson processors do not support either.  The effective target
   mips_nanlegacy is required for a toolchain without the legacy NaN support
   because inclusion of some system headers e.g. stdint.h will fail due to not
   finding stubs-o32_hard.h.  */
/* { dg-require-effective-target mips_nanlegacy } */
/* { dg-options "isa=loongson -mhard-float -mno-micromips -mno-mips16 -flax-vector-conversions (REQUIRES_STDLIB)" } */

#include "loongson.h"
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <limits.h>

typedef union { int32x2_t v; int32_t a[2]; } int32x2_encap_t;
typedef union { int16x4_t v; int16_t a[4]; } int16x4_encap_t;
typedef union { int8x8_t v; int8_t a[8]; } int8x8_encap_t;
typedef union { uint32x2_t v; uint32_t a[2]; } uint32x2_encap_t;
typedef union { uint16x4_t v; uint16_t a[4]; } uint16x4_encap_t;
typedef union { uint8x8_t v; uint8_t a[8]; } uint8x8_encap_t;

#define UINT16x4_MAX USHRT_MAX
#define UINT8x8_MAX UCHAR_MAX
#define INT8x8_MAX SCHAR_MAX
#define INT16x4_MAX SHRT_MAX
#define INT32x2_MAX INT_MAX

static void test_packsswh (void)
{
  int32x2_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = INT16x4_MAX - 2;
  s.a[1] = INT16x4_MAX - 1;
  t.a[0] = INT16x4_MAX;
  t.a[1] = INT16x4_MAX + 1;
  r.v = packsswh (s.v, t.v);
  assert (r.a[0] == INT16x4_MAX - 2);
  assert (r.a[1] == INT16x4_MAX - 1);
  assert (r.a[2] == INT16x4_MAX);
  assert (r.a[3] == INT16x4_MAX);
}

static void test_packsshb (void)
{
  int16x4_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = INT8x8_MAX - 6;
  s.a[1] = INT8x8_MAX - 5;
  s.a[2] = INT8x8_MAX - 4;
  s.a[3] = INT8x8_MAX - 3;
  t.a[0] = INT8x8_MAX - 2;
  t.a[1] = INT8x8_MAX - 1;
  t.a[2] = INT8x8_MAX;
  t.a[3] = INT8x8_MAX + 1;
  r.v = packsshb (s.v, t.v);
  assert (r.a[0] == INT8x8_MAX - 6);
  assert (r.a[1] == INT8x8_MAX - 5);
  assert (r.a[2] == INT8x8_MAX - 4);
  assert (r.a[3] == INT8x8_MAX - 3);
  assert (r.a[4] == INT8x8_MAX - 2);
  assert (r.a[5] == INT8x8_MAX - 1);
  assert (r.a[6] == INT8x8_MAX);
  assert (r.a[7] == INT8x8_MAX);
}

static void test_packushb (void)
{
  uint16x4_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = UINT8x8_MAX - 6;
  s.a[1] = UINT8x8_MAX - 5;
  s.a[2] = UINT8x8_MAX - 4;
  s.a[3] = UINT8x8_MAX - 3;
  t.a[0] = UINT8x8_MAX - 2;
  t.a[1] = UINT8x8_MAX - 1;
  t.a[2] = UINT8x8_MAX;
  t.a[3] = UINT8x8_MAX + 1;
  r.v = packushb (s.v, t.v);
  assert (r.a[0] == UINT8x8_MAX - 6);
  assert (r.a[1] == UINT8x8_MAX - 5);
  assert (r.a[2] == UINT8x8_MAX - 4);
  assert (r.a[3] == UINT8x8_MAX - 3);
  assert (r.a[4] == UINT8x8_MAX - 2);
  assert (r.a[5] == UINT8x8_MAX - 1);
  assert (r.a[6] == UINT8x8_MAX);
  assert (r.a[7] == UINT8x8_MAX);
}

static void test_paddw_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  t.a[0] = 3;
  t.a[1] = 4;
  r.v = paddw_u (s.v, t.v);
  assert (r.a[0] == 4);
  assert (r.a[1] == 6);
}

static void test_paddw_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = -2;
  s.a[1] = -1;
  t.a[0] = 3;
  t.a[1] = 4;
  r.v = paddw_s (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == 3);
}

static void test_paddh_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  s.a[2] = 3;
  s.a[3] = 4;
  t.a[0] = 5;
  t.a[1] = 6;
  t.a[2] = 7;
  t.a[3] = 8;
  r.v = paddh_u (s.v, t.v);
  assert (r.a[0] == 6);
  assert (r.a[1] == 8);
  assert (r.a[2] == 10);
  assert (r.a[3] == 12);
}

static void test_paddh_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -10;
  s.a[1] = -20;
  s.a[2] = -30;
  s.a[3] = -40;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  r.v = paddh_s (s.v, t.v);
  assert (r.a[0] == -9);
  assert (r.a[1] == -18);
  assert (r.a[2] == -27);
  assert (r.a[3] == -36);
}

static void test_paddb_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  s.a[2] = 3;
  s.a[3] = 4;
  s.a[4] = 5;
  s.a[5] = 6;
  s.a[6] = 7;
  s.a[7] = 8;
  t.a[0] = 9;
  t.a[1] = 10;
  t.a[2] = 11;
  t.a[3] = 12;
  t.a[4] = 13;
  t.a[5] = 14;
  t.a[6] = 15;
  t.a[7] = 16;
  r.v = paddb_u (s.v, t.v);
  assert (r.a[0] == 10);
  assert (r.a[1] == 12);
  assert (r.a[2] == 14);
  assert (r.a[3] == 16);
  assert (r.a[4] == 18);
  assert (r.a[5] == 20);
  assert (r.a[6] == 22);
  assert (r.a[7] == 24);
}

static void test_paddb_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -10;
  s.a[1] = -20;
  s.a[2] = -30;
  s.a[3] = -40;
  s.a[4] = -50;
  s.a[5] = -60;
  s.a[6] = -70;
  s.a[7] = -80;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  t.a[4] = 5;
  t.a[5] = 6;
  t.a[6] = 7;
  t.a[7] = 8;
  r.v = paddb_s (s.v, t.v);
  assert (r.a[0] == -9);
  assert (r.a[1] == -18);
  assert (r.a[2] == -27);
  assert (r.a[3] == -36);
  assert (r.a[4] == -45);
  assert (r.a[5] == -54);
  assert (r.a[6] == -63);
  assert (r.a[7] == -72);
}

static void test_paddd_u (void)
{
  uint64_t d = 123456;
  uint64_t e = 789012;
  uint64_t r;
  r = paddd_u (d, e);
  assert (r == 912468);
}

static void test_paddd_s (void)
{
  int64_t d = 123456;
  int64_t e = -789012;
  int64_t r;
  r = paddd_s (d, e);
  assert (r == -665556);
}

static void test_paddsh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = 0;
  s.a[2] = 1;
  s.a[3] = 2;
  t.a[0] = INT16x4_MAX;
  t.a[1] = INT16x4_MAX;
  t.a[2] = INT16x4_MAX;
  t.a[3] = INT16x4_MAX;
  r.v = paddsh (s.v, t.v);
  assert (r.a[0] == INT16x4_MAX - 1);
  assert (r.a[1] == INT16x4_MAX);
  assert (r.a[2] == INT16x4_MAX);
  assert (r.a[3] == INT16x4_MAX);
}

static void test_paddsb (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -6;
  s.a[1] = -5;
  s.a[2] = -4;
  s.a[3] = -3;
  s.a[4] = -2;
  s.a[5] = -1;
  s.a[6] = 0;
  s.a[7] = 1;
  t.a[0] = INT8x8_MAX;
  t.a[1] = INT8x8_MAX;
  t.a[2] = INT8x8_MAX;
  t.a[3] = INT8x8_MAX;
  t.a[4] = INT8x8_MAX;
  t.a[5] = INT8x8_MAX;
  t.a[6] = INT8x8_MAX;
  t.a[7] = INT8x8_MAX;
  r.v = paddsb (s.v, t.v);
  assert (r.a[0] == INT8x8_MAX - 6);
  assert (r.a[1] == INT8x8_MAX - 5);
  assert (r.a[2] == INT8x8_MAX - 4);
  assert (r.a[3] == INT8x8_MAX - 3);
  assert (r.a[4] == INT8x8_MAX - 2);
  assert (r.a[5] == INT8x8_MAX - 1);
  assert (r.a[6] == INT8x8_MAX);
  assert (r.a[7] == INT8x8_MAX);
}

static void test_paddush (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 0;
  s.a[1] = 1;
  s.a[2] = 0;
  s.a[3] = 1;
  t.a[0] = UINT16x4_MAX;
  t.a[1] = UINT16x4_MAX;
  t.a[2] = UINT16x4_MAX;
  t.a[3] = UINT16x4_MAX;
  r.v = paddush (s.v, t.v);
  assert (r.a[0] == UINT16x4_MAX);
  assert (r.a[1] == UINT16x4_MAX);
  assert (r.a[2] == UINT16x4_MAX);
  assert (r.a[3] == UINT16x4_MAX);
}

static void test_paddusb (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 0;
  s.a[1] = 1;
  s.a[2] = 0;
  s.a[3] = 1;
  s.a[4] = 0;
  s.a[5] = 1;
  s.a[6] = 0;
  s.a[7] = 1;
  t.a[0] = UINT8x8_MAX;
  t.a[1] = UINT8x8_MAX;
  t.a[2] = UINT8x8_MAX;
  t.a[3] = UINT8x8_MAX;
  t.a[4] = UINT8x8_MAX;
  t.a[5] = UINT8x8_MAX;
  t.a[6] = UINT8x8_MAX;
  t.a[7] = UINT8x8_MAX;
  r.v = paddusb (s.v, t.v);
  assert (r.a[0] == UINT8x8_MAX);
  assert (r.a[1] == UINT8x8_MAX);
  assert (r.a[2] == UINT8x8_MAX);
  assert (r.a[3] == UINT8x8_MAX);
  assert (r.a[4] == UINT8x8_MAX);
  assert (r.a[5] == UINT8x8_MAX);
  assert (r.a[6] == UINT8x8_MAX);
  assert (r.a[7] == UINT8x8_MAX);
}

static void test_pandn_ud (void)
{
  uint64_t d1 = 0x0000ffff0000ffffull;
  uint64_t d2 = 0x0000ffff0000ffffull;
  uint64_t r;
  r = pandn_ud (d1, d2);
  assert (r == 0);
}

static void test_pandn_sd (void)
{
  int64_t d1 = (int64_t) 0x0000000000000000ull;
  int64_t d2 = (int64_t) 0xfffffffffffffffeull;
  int64_t r;
  r = pandn_sd (d1, d2);
  assert (r == -2);
}

static void test_pandn_uw (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 0xffffffff;
  s.a[1] = 0x00000000;
  t.a[0] = 0x00000000;
  t.a[1] = 0xffffffff;
  r.v = pandn_uw (s.v, t.v);
  assert (r.a[0] == 0x00000000);
  assert (r.a[1] == 0xffffffff);
}

static void test_pandn_sw (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = 0xffffffff;
  s.a[1] = 0x00000000;
  t.a[0] = 0xffffffff;
  t.a[1] = 0xfffffffe;
  r.v = pandn_sw (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -2);
}

static void test_pandn_uh (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 0xffff;
  s.a[1] = 0x0000;
  s.a[2] = 0xffff;
  s.a[3] = 0x0000;
  t.a[0] = 0x0000;
  t.a[1] = 0xffff;
  t.a[2] = 0x0000;
  t.a[3] = 0xffff;
  r.v = pandn_uh (s.v, t.v);
  assert (r.a[0] == 0x0000);
  assert (r.a[1] == 0xffff);
  assert (r.a[2] == 0x0000);
  assert (r.a[3] == 0xffff);
}

static void test_pandn_sh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = 0xffff;
  s.a[1] = 0x0000;
  s.a[2] = 0xffff;
  s.a[3] = 0x0000;
  t.a[0] = 0xffff;
  t.a[1] = 0xfffe;
  t.a[2] = 0xffff;
  t.a[3] = 0xfffe;
  r.v = pandn_sh (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -2);
  assert (r.a[2] == 0);
  assert (r.a[3] == -2);
}

static void test_pandn_ub (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 0xff;
  s.a[1] = 0x00;
  s.a[2] = 0xff;
  s.a[3] = 0x00;
  s.a[4] = 0xff;
  s.a[5] = 0x00;
  s.a[6] = 0xff;
  s.a[7] = 0x00;
  t.a[0] = 0x00;
  t.a[1] = 0xff;
  t.a[2] = 0x00;
  t.a[3] = 0xff;
  t.a[4] = 0x00;
  t.a[5] = 0xff;
  t.a[6] = 0x00;
  t.a[7] = 0xff;
  r.v = pandn_ub (s.v, t.v);
  assert (r.a[0] == 0x00);
  assert (r.a[1] == 0xff);
  assert (r.a[2] == 0x00);
  assert (r.a[3] == 0xff);
  assert (r.a[4] == 0x00);
  assert (r.a[5] == 0xff);
  assert (r.a[6] == 0x00);
  assert (r.a[7] == 0xff);
}

static void test_pandn_sb (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = 0xff;
  s.a[1] = 0x00;
  s.a[2] = 0xff;
  s.a[3] = 0x00;
  s.a[4] = 0xff;
  s.a[5] = 0x00;
  s.a[6] = 0xff;
  s.a[7] = 0x00;
  t.a[0] = 0xff;
  t.a[1] = 0xfe;
  t.a[2] = 0xff;
  t.a[3] = 0xfe;
  t.a[4] = 0xff;
  t.a[5] = 0xfe;
  t.a[6] = 0xff;
  t.a[7] = 0xfe;
  r.v = pandn_sb (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -2);
  assert (r.a[2] == 0);
  assert (r.a[3] == -2);
  assert (r.a[4] == 0);
  assert (r.a[5] == -2);
  assert (r.a[6] == 0);
  assert (r.a[7] == -2);
}

static void test_pavgh (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  s.a[2] = 3;
  s.a[3] = 4;
  t.a[0] = 5;
  t.a[1] = 6;
  t.a[2] = 7;
  t.a[3] = 8;
  r.v = pavgh (s.v, t.v);
  assert (r.a[0] == 3);
  assert (r.a[1] == 4);
  assert (r.a[2] == 5);
  assert (r.a[3] == 6);
}

static void test_pavgb (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  s.a[2] = 3;
  s.a[3] = 4;
  s.a[4] = 1;
  s.a[5] = 2;
  s.a[6] = 3;
  s.a[7] = 4;
  t.a[0] = 5;
  t.a[1] = 6;
  t.a[2] = 7;
  t.a[3] = 8;
  t.a[4] = 5;
  t.a[5] = 6;
  t.a[6] = 7;
  t.a[7] = 8;
  r.v = pavgb (s.v, t.v);
  assert (r.a[0] == 3);
  assert (r.a[1] == 4);
  assert (r.a[2] == 5);
  assert (r.a[3] == 6);
  assert (r.a[4] == 3);
  assert (r.a[5] == 4);
  assert (r.a[6] == 5);
  assert (r.a[7] == 6);
}

static void test_pcmpeqw_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 42;
  s.a[1] = 43;
  t.a[0] = 43;
  t.a[1] = 43;
  r.v = pcmpeqw_u (s.v, t.v);
  assert (r.a[0] == 0x00000000);
  assert (r.a[1] == 0xffffffff);
}

static void test_pcmpeqh_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 42;
  s.a[1] = 43;
  s.a[2] = 42;
  s.a[3] = 43;
  t.a[0] = 43;
  t.a[1] = 43;
  t.a[2] = 43;
  t.a[3] = 43;
  r.v = pcmpeqh_u (s.v, t.v);
  assert (r.a[0] == 0x0000);
  assert (r.a[1] == 0xffff);
  assert (r.a[2] == 0x0000);
  assert (r.a[3] == 0xffff);
}

static void test_pcmpeqb_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 42;
  s.a[1] = 43;
  s.a[2] = 42;
  s.a[3] = 43;
  s.a[4] = 42;
  s.a[5] = 43;
  s.a[6] = 42;
  s.a[7] = 43;
  t.a[0] = 43;
  t.a[1] = 43;
  t.a[2] = 43;
  t.a[3] = 43;
  t.a[4] = 43;
  t.a[5] = 43;
  t.a[6] = 43;
  t.a[7] = 43;
  r.v = pcmpeqb_u (s.v, t.v);
  assert (r.a[0] == 0x00);
  assert (r.a[1] == 0xff);
  assert (r.a[2] == 0x00);
  assert (r.a[3] == 0xff);
  assert (r.a[4] == 0x00);
  assert (r.a[5] == 0xff);
  assert (r.a[6] == 0x00);
  assert (r.a[7] == 0xff);
}

static void test_pcmpeqw_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = -42;
  s.a[1] = -42;
  t.a[0] = 42;
  t.a[1] = -42;
  r.v = pcmpeqw_s (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -1);
}

static void test_pcmpeqh_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -42;
  s.a[1] = -42;
  s.a[2] = -42;
  s.a[3] = -42;
  t.a[0] = 42;
  t.a[1] = -42;
  t.a[2] = 42;
  t.a[3] = -42;
  r.v = pcmpeqh_s (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -1);
  assert (r.a[2] == 0);
  assert (r.a[3] == -1);
}

static void test_pcmpeqb_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -42;
  s.a[1] = -42;
  s.a[2] = -42;
  s.a[3] = -42;
  s.a[4] = -42;
  s.a[5] = -42;
  s.a[6] = -42;
  s.a[7] = -42;
  t.a[0] = 42;
  t.a[1] = -42;
  t.a[2] = 42;
  t.a[3] = -42;
  t.a[4] = 42;
  t.a[5] = -42;
  t.a[6] = 42;
  t.a[7] = -42;
  r.v = pcmpeqb_s (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == -1);
  assert (r.a[2] == 0);
  assert (r.a[3] == -1);
  assert (r.a[4] == 0);
  assert (r.a[5] == -1);
  assert (r.a[6] == 0);
  assert (r.a[7] == -1);
}

static void test_pcmpgtw_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 42;
  s.a[1] = 43;
  t.a[0] = 43;
  t.a[1] = 42;
  r.v = pcmpgtw_u (s.v, t.v);
  assert (r.a[0] == 0x00000000);
  assert (r.a[1] == 0xffffffff);
}

static void test_pcmpgth_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 40;
  s.a[1] = 41;
  s.a[2] = 42;
  s.a[3] = 43;
  t.a[0] = 40;
  t.a[1] = 41;
  t.a[2] = 43;
  t.a[3] = 42;
  r.v = pcmpgth_u (s.v, t.v);
  assert (r.a[0] == 0x0000);
  assert (r.a[1] == 0x0000);
  assert (r.a[2] == 0x0000);
  assert (r.a[3] == 0xffff);
}

static void test_pcmpgtb_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 40;
  s.a[1] = 41;
  s.a[2] = 42;
  s.a[3] = 43;
  s.a[4] = 44;
  s.a[5] = 45;
  s.a[6] = 46;
  s.a[7] = 47;
  t.a[0] = 48;
  t.a[1] = 47;
  t.a[2] = 46;
  t.a[3] = 45;
  t.a[4] = 44;
  t.a[5] = 43;
  t.a[6] = 42;
  t.a[7] = 41;
  r.v = pcmpgtb_u (s.v, t.v);
  assert (r.a[0] == 0x00);
  assert (r.a[1] == 0x00);
  assert (r.a[2] == 0x00);
  assert (r.a[3] == 0x00);
  assert (r.a[4] == 0x00);
  assert (r.a[5] == 0xff);
  assert (r.a[6] == 0xff);
  assert (r.a[7] == 0xff);
}

static void test_pcmpgtw_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = 42;
  s.a[1] = -42;
  t.a[0] = -42;
  t.a[1] = -42;
  r.v = pcmpgtw_s (s.v, t.v);
  assert (r.a[0] == -1);
  assert (r.a[1] == 0);
}

static void test_pcmpgth_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -42;
  s.a[1] = -42;
  s.a[2] = -42;
  s.a[3] = -42;
  t.a[0] = 42;
  t.a[1] = 43;
  t.a[2] = 44;
  t.a[3] = -43;
  r.v = pcmpgth_s (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == -1);
}

static void test_pcmpgtb_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -42;
  s.a[1] = -42;
  s.a[2] = -42;
  s.a[3] = -42;
  s.a[4] = 42;
  s.a[5] = 42;
  s.a[6] = 42;
  s.a[7] = 42;
  t.a[0] = -45;
  t.a[1] = -44;
  t.a[2] = -43;
  t.a[3] = -42;
  t.a[4] = 42;
  t.a[5] = 43;
  t.a[6] = 41;
  t.a[7] = 40;
  r.v = pcmpgtb_s (s.v, t.v);
  assert (r.a[0] == -1);
  assert (r.a[1] == -1);
  assert (r.a[2] == -1);
  assert (r.a[3] == 0);
  assert (r.a[4] == 0);
  assert (r.a[5] == 0);
  assert (r.a[6] == -1);
  assert (r.a[7] == -1);
}

static void test_pextrh_u (void)
{
  uint16x4_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 40;
  s.a[1] = 41;
  s.a[2] = 42;
  s.a[3] = 43;
  r.v = pextrh_u (s.v, 1);
  assert (r.a[0] == 41);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
}

static void test_pextrh_s (void)
{
  int16x4_encap_t s;
  int16x4_encap_t r;
  s.a[0] = -40;
  s.a[1] = -41;
  s.a[2] = -42;
  s.a[3] = -43;
  r.v = pextrh_s (s.v, 2);
  assert (r.a[0] == -42);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
}

static void test_pinsrh_0123_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 42;
  s.a[1] = 0;
  s.a[2] = 0;
  s.a[3] = 0;
  t.a[0] = 0;
  t.a[1] = 0;
  t.a[2] = 0;
  t.a[3] = 0;
  r.v = pinsrh_0_u (t.v, s.v);
  r.v = pinsrh_1_u (r.v, s.v);
  r.v = pinsrh_2_u (r.v, s.v);
  r.v = pinsrh_3_u (r.v, s.v);
  assert (r.a[0] == 42);
  assert (r.a[1] == 42);
  assert (r.a[2] == 42);
  assert (r.a[3] == 42);
}

static void test_pinsrh_0123_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -42;
  s.a[1] = 0;
  s.a[2] = 0;
  s.a[3] = 0;
  t.a[0] = 0;
  t.a[1] = 0;
  t.a[2] = 0;
  t.a[3] = 0;
  r.v = pinsrh_0_s (t.v, s.v);
  r.v = pinsrh_1_s (r.v, s.v);
  r.v = pinsrh_2_s (r.v, s.v);
  r.v = pinsrh_3_s (r.v, s.v);
  assert (r.a[0] == -42);
  assert (r.a[1] == -42);
  assert (r.a[2] == -42);
  assert (r.a[3] == -42);
}

static void test_pmaddhw (void)
{
  int16x4_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = -5;
  s.a[1] = -4;
  s.a[2] = -3;
  s.a[3] = -2;
  t.a[0] = 10;
  t.a[1] = 11;
  t.a[2] = 12;
  t.a[3] = 13;
  r.v = pmaddhw (s.v, t.v);
  assert (r.a[0] == (-5*10 + -4*11));
  assert (r.a[1] == (-3*12 + -2*13));
}

static void test_pmaxsh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -20;
  s.a[1] = 40;
  s.a[2] = -10;
  s.a[3] = 50;
  t.a[0] = 20;
  t.a[1] = -40;
  t.a[2] = 10;
  t.a[3] = -50;
  r.v = pmaxsh (s.v, t.v);
  assert (r.a[0] == 20);
  assert (r.a[1] == 40);
  assert (r.a[2] == 10);
  assert (r.a[3] == 50);
}

static void test_pmaxub (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 10;
  s.a[1] = 20;
  s.a[2] = 30;
  s.a[3] = 40;
  s.a[4] = 50;
  s.a[5] = 60;
  s.a[6] = 70;
  s.a[7] = 80;
  t.a[0] = 80;
  t.a[1] = 70;
  t.a[2] = 60;
  t.a[3] = 50;
  t.a[4] = 40;
  t.a[5] = 30;
  t.a[6] = 20;
  t.a[7] = 10;
  r.v = pmaxub (s.v, t.v);
  assert (r.a[0] == 80);
  assert (r.a[1] == 70);
  assert (r.a[2] == 60);
  assert (r.a[3] == 50);
  assert (r.a[4] == 50);
  assert (r.a[5] == 60);
  assert (r.a[6] == 70);
  assert (r.a[7] == 80);
}

static void test_pminsh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -20;
  s.a[1] = 40;
  s.a[2] = -10;
  s.a[3] = 50;
  t.a[0] = 20;
  t.a[1] = -40;
  t.a[2] = 10;
  t.a[3] = -50;
  r.v = pminsh (s.v, t.v);
  assert (r.a[0] == -20);
  assert (r.a[1] == -40);
  assert (r.a[2] == -10);
  assert (r.a[3] == -50);
}

static void test_pminub (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 10;
  s.a[1] = 20;
  s.a[2] = 30;
  s.a[3] = 40;
  s.a[4] = 50;
  s.a[5] = 60;
  s.a[6] = 70;
  s.a[7] = 80;
  t.a[0] = 80;
  t.a[1] = 70;
  t.a[2] = 60;
  t.a[3] = 50;
  t.a[4] = 40;
  t.a[5] = 30;
  t.a[6] = 20;
  t.a[7] = 10;
  r.v = pminub (s.v, t.v);
  assert (r.a[0] == 10);
  assert (r.a[1] == 20);
  assert (r.a[2] == 30);
  assert (r.a[3] == 40);
  assert (r.a[4] == 40);
  assert (r.a[5] == 30);
  assert (r.a[6] == 20);
  assert (r.a[7] == 10);
}

static void test_pmovmskb_u (void)
{
  uint8x8_encap_t s;
  uint8x8_encap_t r;
  s.a[0] = 0xf0;
  s.a[1] = 0x40;
  s.a[2] = 0xf0;
  s.a[3] = 0x40;
  s.a[4] = 0xf0;
  s.a[5] = 0x40;
  s.a[6] = 0xf0;
  s.a[7] = 0x40;
  r.v = pmovmskb_u (s.v);
  assert (r.a[0] == 0x55);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
  assert (r.a[4] == 0);
  assert (r.a[5] == 0);
  assert (r.a[6] == 0);
  assert (r.a[7] == 0);
}

static void test_pmovmskb_s (void)
{
  int8x8_encap_t s;
  int8x8_encap_t r;
  s.a[0] = -1;
  s.a[1] = 1;
  s.a[2] = -1;
  s.a[3] = 1;
  s.a[4] = -1;
  s.a[5] = 1;
  s.a[6] = -1;
  s.a[7] = 1;
  r.v = pmovmskb_s (s.v);
  assert (r.a[0] == 0x55);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
  assert (r.a[4] == 0);
  assert (r.a[5] == 0);
  assert (r.a[6] == 0);
  assert (r.a[7] == 0);
}

static void test_pmulhuh (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 0xff00;
  s.a[1] = 0xff00;
  s.a[2] = 0xff00;
  s.a[3] = 0xff00;
  t.a[0] = 16;
  t.a[1] = 16;
  t.a[2] = 16;
  t.a[3] = 16;
  r.v = pmulhuh (s.v, t.v);
  assert (r.a[0] == 0x000f);
  assert (r.a[1] == 0x000f);
  assert (r.a[2] == 0x000f);
  assert (r.a[3] == 0x000f);
}

static void test_pmulhh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = 0x0ff0;
  s.a[1] = 0x0ff0;
  s.a[2] = 0x0ff0;
  s.a[3] = 0x0ff0;
  t.a[0] = -16*16;
  t.a[1] = -16*16;
  t.a[2] = -16*16;
  t.a[3] = -16*16;
  r.v = pmulhh (s.v, t.v);
  assert (r.a[0] == -16);
  assert (r.a[1] == -16);
  assert (r.a[2] == -16);
  assert (r.a[3] == -16);
}

static void test_pmullh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = 0x0ff0;
  s.a[1] = 0x0ff0;
  s.a[2] = 0x0ff0;
  s.a[3] = 0x0ff0;
  t.a[0] = -16*16;
  t.a[1] = -16*16;
  t.a[2] = -16*16;
  t.a[3] = -16*16;
  r.v = pmullh (s.v, t.v);
  assert (r.a[0] == 4096);
  assert (r.a[1] == 4096);
  assert (r.a[2] == 4096);
  assert (r.a[3] == 4096);
}

static void test_pmuluw (void)
{
  uint32x2_encap_t s, t;
  uint64_t r;
  s.a[0] = 0xdeadbeef;
  s.a[1] = 0;
  t.a[0] = 0x0f00baaa;
  t.a[1] = 0;
  r = pmuluw (s.v, t.v);
  assert (r == 0xd0cd08e1d1a70b6ull);
}

static void test_pasubub (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 10;
  s.a[1] = 20;
  s.a[2] = 30;
  s.a[3] = 40;
  s.a[4] = 50;
  s.a[5] = 60;
  s.a[6] = 70;
  s.a[7] = 80;
  t.a[0] = 80;
  t.a[1] = 70;
  t.a[2] = 60;
  t.a[3] = 50;
  t.a[4] = 40;
  t.a[5] = 30;
  t.a[6] = 20;
  t.a[7] = 10;
  r.v = pasubub (s.v, t.v);
  assert (r.a[0] == 70);
  assert (r.a[1] == 50);
  assert (r.a[2] == 30);
  assert (r.a[3] == 10);
  assert (r.a[4] == 10);
  assert (r.a[5] == 30);
  assert (r.a[6] == 50);
  assert (r.a[7] == 70);
}

static void test_biadd (void)
{
  uint8x8_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 10;
  s.a[1] = 20;
  s.a[2] = 30;
  s.a[3] = 40;
  s.a[4] = 50;
  s.a[5] = 60;
  s.a[6] = 70;
  s.a[7] = 80;
  r.v = biadd (s.v);
  assert (r.a[0] == 360);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
}

static void test_psadbh (void)
{
  uint8x8_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 10;
  s.a[1] = 20;
  s.a[2] = 30;
  s.a[3] = 40;
  s.a[4] = 50;
  s.a[5] = 60;
  s.a[6] = 70;
  s.a[7] = 80;
  t.a[0] = 80;
  t.a[1] = 70;
  t.a[2] = 60;
  t.a[3] = 50;
  t.a[4] = 40;
  t.a[5] = 30;
  t.a[6] = 20;
  t.a[7] = 10;
  r.v = psadbh (s.v, t.v);
  assert (r.a[0] == 0x0140);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
}

static void test_pshufh_u (void)
{
  uint16x4_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 1;
  s.a[1] = 2;
  s.a[2] = 3;
  s.a[3] = 4;
  r.a[0] = 0;
  r.a[1] = 0;
  r.a[2] = 0;
  r.a[3] = 0;
  r.v = pshufh_u (r.v, s.v, 0xe5);
  assert (r.a[0] == 2);
  assert (r.a[1] == 2);
  assert (r.a[2] == 3);
  assert (r.a[3] == 4);
}

static void test_pshufh_s (void)
{
  int16x4_encap_t s;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = 2;
  s.a[2] = -3;
  s.a[3] = 4;
  r.a[0] = 0;
  r.a[1] = 0;
  r.a[2] = 0;
  r.a[3] = 0;
  r.v = pshufh_s (r.v, s.v, 0xe5);
  assert (r.a[0] == 2);
  assert (r.a[1] == 2);
  assert (r.a[2] == -3);
  assert (r.a[3] == 4);
}

static void test_psllh_u (void)
{
  uint16x4_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 0xffff;
  s.a[1] = 0xffff;
  s.a[2] = 0xffff;
  s.a[3] = 0xffff;
  r.v = psllh_u (s.v, 1);
  assert (r.a[0] == 0xfffe);
  assert (r.a[1] == 0xfffe);
  assert (r.a[2] == 0xfffe);
  assert (r.a[3] == 0xfffe);
}

static void test_psllw_u (void)
{
  uint32x2_encap_t s;
  uint32x2_encap_t r;
  s.a[0] = 0xffffffff;
  s.a[1] = 0xffffffff;
  r.v = psllw_u (s.v, 2);
  assert (r.a[0] == 0xfffffffc);
  assert (r.a[1] == 0xfffffffc);
}

static void test_psllh_s (void)
{
  int16x4_encap_t s;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = -1;
  s.a[2] = -1;
  s.a[3] = -1;
  r.v = psllh_s (s.v, 1);
  assert (r.a[0] == -2);
  assert (r.a[1] == -2);
  assert (r.a[2] == -2);
  assert (r.a[3] == -2);
}

static void test_psllw_s (void)
{
  int32x2_encap_t s;
  int32x2_encap_t r;
  s.a[0] = -1;
  s.a[1] = -1;
  r.v = psllw_s (s.v, 2);
  assert (r.a[0] == -4);
  assert (r.a[1] == -4);
}

static void test_psrah_u (void)
{
  uint16x4_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 0xffef;
  s.a[1] = 0xffef;
  s.a[2] = 0xffef;
  s.a[3] = 0xffef;
  r.v = psrah_u (s.v, 1);
  assert (r.a[0] == 0xfff7);
  assert (r.a[1] == 0xfff7);
  assert (r.a[2] == 0xfff7);
  assert (r.a[3] == 0xfff7);
}

static void test_psraw_u (void)
{
  uint32x2_encap_t s;
  uint32x2_encap_t r;
  s.a[0] = 0xffffffef;
  s.a[1] = 0xffffffef;
  r.v = psraw_u (s.v, 1);
  assert (r.a[0] == 0xfffffff7);
  assert (r.a[1] == 0xfffffff7);
}

static void test_psrah_s (void)
{
  int16x4_encap_t s;
  int16x4_encap_t r;
  s.a[0] = -2;
  s.a[1] = -2;
  s.a[2] = -2;
  s.a[3] = -2;
  r.v = psrah_s (s.v, 1);
  assert (r.a[0] == -1);
  assert (r.a[1] == -1);
  assert (r.a[2] == -1);
  assert (r.a[3] == -1);
}

static void test_psraw_s (void)
{
  int32x2_encap_t s;
  int32x2_encap_t r;
  s.a[0] = -2;
  s.a[1] = -2;
  r.v = psraw_s (s.v, 1);
  assert (r.a[0] == -1);
  assert (r.a[1] == -1);
}

static void test_psrlh_u (void)
{
  uint16x4_encap_t s;
  uint16x4_encap_t r;
  s.a[0] = 0xffef;
  s.a[1] = 0xffef;
  s.a[2] = 0xffef;
  s.a[3] = 0xffef;
  r.v = psrlh_u (s.v, 1);
  assert (r.a[0] == 0x7ff7);
  assert (r.a[1] == 0x7ff7);
  assert (r.a[2] == 0x7ff7);
  assert (r.a[3] == 0x7ff7);
}

static void test_psrlw_u (void)
{
  uint32x2_encap_t s;
  uint32x2_encap_t r;
  s.a[0] = 0xffffffef;
  s.a[1] = 0xffffffef;
  r.v = psrlw_u (s.v, 1);
  assert (r.a[0] == 0x7ffffff7);
  assert (r.a[1] == 0x7ffffff7);
}

static void test_psrlh_s (void)
{
  int16x4_encap_t s;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = -1;
  s.a[2] = -1;
  s.a[3] = -1;
  r.v = psrlh_s (s.v, 1);
  assert (r.a[0] == INT16x4_MAX);
  assert (r.a[1] == INT16x4_MAX);
  assert (r.a[2] == INT16x4_MAX);
  assert (r.a[3] == INT16x4_MAX);
}

static void test_psrlw_s (void)
{
  int32x2_encap_t s;
  int32x2_encap_t r;
  s.a[0] = -1;
  s.a[1] = -1;
  r.v = psrlw_s (s.v, 1);
  assert (r.a[0] == INT32x2_MAX);
  assert (r.a[1] == INT32x2_MAX);
}

static void test_psubw_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 3;
  s.a[1] = 4;
  t.a[0] = 2;
  t.a[1] = 1;
  r.v = psubw_u (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == 3);
}

static void test_psubw_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = -2;
  s.a[1] = -1;
  t.a[0] = 3;
  t.a[1] = -4;
  r.v = psubw_s (s.v, t.v);
  assert (r.a[0] == -5);
  assert (r.a[1] == 3);
}

static void test_psubh_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 5;
  s.a[1] = 6;
  s.a[2] = 7;
  s.a[3] = 8;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  r.v = psubh_u (s.v, t.v);
  assert (r.a[0] == 4);
  assert (r.a[1] == 4);
  assert (r.a[2] == 4);
  assert (r.a[3] == 4);
}

static void test_psubh_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -10;
  s.a[1] = -20;
  s.a[2] = -30;
  s.a[3] = -40;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  r.v = psubh_s (s.v, t.v);
  assert (r.a[0] == -11);
  assert (r.a[1] == -22);
  assert (r.a[2] == -33);
  assert (r.a[3] == -44);
}

static void test_psubb_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 10;
  s.a[1] = 11;
  s.a[2] = 12;
  s.a[3] = 13;
  s.a[4] = 14;
  s.a[5] = 15;
  s.a[6] = 16;
  s.a[7] = 17;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  t.a[4] = 5;
  t.a[5] = 6;
  t.a[6] = 7;
  t.a[7] = 8;
  r.v = psubb_u (s.v, t.v);
  assert (r.a[0] == 9);
  assert (r.a[1] == 9);
  assert (r.a[2] == 9);
  assert (r.a[3] == 9);
  assert (r.a[4] == 9);
  assert (r.a[5] == 9);
  assert (r.a[6] == 9);
  assert (r.a[7] == 9);
}

static void test_psubb_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -10;
  s.a[1] = -20;
  s.a[2] = -30;
  s.a[3] = -40;
  s.a[4] = -50;
  s.a[5] = -60;
  s.a[6] = -70;
  s.a[7] = -80;
  t.a[0] = 1;
  t.a[1] = 2;
  t.a[2] = 3;
  t.a[3] = 4;
  t.a[4] = 5;
  t.a[5] = 6;
  t.a[6] = 7;
  t.a[7] = 8;
  r.v = psubb_s (s.v, t.v);
  assert (r.a[0] == -11);
  assert (r.a[1] == -22);
  assert (r.a[2] == -33);
  assert (r.a[3] == -44);
  assert (r.a[4] == -55);
  assert (r.a[5] == -66);
  assert (r.a[6] == -77);
  assert (r.a[7] == -88);
}

static void test_psubd_u (void)
{
  uint64_t d = 789012;
  uint64_t e = 123456;
  uint64_t r;
  r = psubd_u (d, e);
  assert (r == 665556);
}

static void test_psubd_s (void)
{
  int64_t d = 123456;
  int64_t e = -789012;
  int64_t r;
  r = psubd_s (d, e);
  assert (r == 912468);
}

static void test_psubsh (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = 0;
  s.a[2] = 1;
  s.a[3] = 2;
  t.a[0] = -INT16x4_MAX;
  t.a[1] = -INT16x4_MAX;
  t.a[2] = -INT16x4_MAX;
  t.a[3] = -INT16x4_MAX;
  r.v = psubsh (s.v, t.v);
  assert (r.a[0] == INT16x4_MAX - 1);
  assert (r.a[1] == INT16x4_MAX);
  assert (r.a[2] == INT16x4_MAX);
  assert (r.a[3] == INT16x4_MAX);
}

static void test_psubsb (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -6;
  s.a[1] = -5;
  s.a[2] = -4;
  s.a[3] = -3;
  s.a[4] = -2;
  s.a[5] = -1;
  s.a[6] = 0;
  s.a[7] = 1;
  t.a[0] = -INT8x8_MAX;
  t.a[1] = -INT8x8_MAX;
  t.a[2] = -INT8x8_MAX;
  t.a[3] = -INT8x8_MAX;
  t.a[4] = -INT8x8_MAX;
  t.a[5] = -INT8x8_MAX;
  t.a[6] = -INT8x8_MAX;
  t.a[7] = -INT8x8_MAX;
  r.v = psubsb (s.v, t.v);
  assert (r.a[0] == INT8x8_MAX - 6);
  assert (r.a[1] == INT8x8_MAX - 5);
  assert (r.a[2] == INT8x8_MAX - 4);
  assert (r.a[3] == INT8x8_MAX - 3);
  assert (r.a[4] == INT8x8_MAX - 2);
  assert (r.a[5] == INT8x8_MAX - 1);
  assert (r.a[6] == INT8x8_MAX);
  assert (r.a[7] == INT8x8_MAX);
}

static void test_psubush (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 0;
  s.a[1] = 1;
  s.a[2] = 2;
  s.a[3] = 3;
  t.a[0] = 1;
  t.a[1] = 1;
  t.a[2] = 3;
  t.a[3] = 3;
  r.v = psubush (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
}

static void test_psubusb (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 0;
  s.a[1] = 1;
  s.a[2] = 2;
  s.a[3] = 3;
  s.a[4] = 4;
  s.a[5] = 5;
  s.a[6] = 6;
  s.a[7] = 7;
  t.a[0] = 1;
  t.a[1] = 1;
  t.a[2] = 3;
  t.a[3] = 3;
  t.a[4] = 5;
  t.a[5] = 5;
  t.a[6] = 7;
  t.a[7] = 7;
  r.v = psubusb (s.v, t.v);
  assert (r.a[0] == 0);
  assert (r.a[1] == 0);
  assert (r.a[2] == 0);
  assert (r.a[3] == 0);
  assert (r.a[4] == 0);
  assert (r.a[5] == 0);
  assert (r.a[6] == 0);
  assert (r.a[7] == 0);
}

static void test_punpckhbh_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -1;
  s.a[1] = -3;
  s.a[2] = -5;
  s.a[3] = -7;
  s.a[4] = -9;
  s.a[5] = -11;
  s.a[6] = -13;
  s.a[7] = -15;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  t.a[4] = 10;
  t.a[5] = 12;
  t.a[6] = 14;
  t.a[7] = 16;
  r.v = punpckhbh_s (s.v, t.v);
  assert (r.a[0] == -9);
  assert (r.a[1] == 10);
  assert (r.a[2] == -11);
  assert (r.a[3] == 12);
  assert (r.a[4] == -13);
  assert (r.a[5] == 14);
  assert (r.a[6] == -15);
  assert (r.a[7] == 16);
}

static void test_punpckhbh_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  s.a[2] = 5;
  s.a[3] = 7;
  s.a[4] = 9;
  s.a[5] = 11;
  s.a[6] = 13;
  s.a[7] = 15;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  t.a[4] = 10;
  t.a[5] = 12;
  t.a[6] = 14;
  t.a[7] = 16;
  r.v = punpckhbh_u (s.v, t.v);
  assert (r.a[0] == 9);
  assert (r.a[1] == 10);
  assert (r.a[2] == 11);
  assert (r.a[3] == 12);
  assert (r.a[4] == 13);
  assert (r.a[5] == 14);
  assert (r.a[6] == 15);
  assert (r.a[7] == 16);
}

static void test_punpckhhw_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = 3;
  s.a[2] = -5;
  s.a[3] = 7;
  t.a[0] = -2;
  t.a[1] = 4;
  t.a[2] = -6;
  t.a[3] = 8;
  r.v = punpckhhw_s (s.v, t.v);
  assert (r.a[0] == -5);
  assert (r.a[1] == -6);
  assert (r.a[2] == 7);
  assert (r.a[3] == 8);
}

static void test_punpckhhw_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  s.a[2] = 5;
  s.a[3] = 7;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  r.v = punpckhhw_u (s.v, t.v);
  assert (r.a[0] == 5);
  assert (r.a[1] == 6);
  assert (r.a[2] == 7);
  assert (r.a[3] == 8);
}

static void test_punpckhwd_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  t.a[0] = 2;
  t.a[1] = -4;
  r.v = punpckhwd_s (s.v, t.v);
  assert (r.a[0] == 3);
  assert (r.a[1] == -4);
}

static void test_punpckhwd_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  t.a[0] = 2;
  t.a[1] = 4;
  r.v = punpckhwd_u (s.v, t.v);
  assert (r.a[0] == 3);
  assert (r.a[1] == 4);
}

static void test_punpcklbh_s (void)
{
  int8x8_encap_t s, t;
  int8x8_encap_t r;
  s.a[0] = -1;
  s.a[1] = -3;
  s.a[2] = -5;
  s.a[3] = -7;
  s.a[4] = -9;
  s.a[5] = -11;
  s.a[6] = -13;
  s.a[7] = -15;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  t.a[4] = 10;
  t.a[5] = 12;
  t.a[6] = 14;
  t.a[7] = 16;
  r.v = punpcklbh_s (s.v, t.v);
  assert (r.a[0] == -1);
  assert (r.a[1] == 2);
  assert (r.a[2] == -3);
  assert (r.a[3] == 4);
  assert (r.a[4] == -5);
  assert (r.a[5] == 6);
  assert (r.a[6] == -7);
  assert (r.a[7] == 8);
}

static void test_punpcklbh_u (void)
{
  uint8x8_encap_t s, t;
  uint8x8_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  s.a[2] = 5;
  s.a[3] = 7;
  s.a[4] = 9;
  s.a[5] = 11;
  s.a[6] = 13;
  s.a[7] = 15;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  t.a[4] = 10;
  t.a[5] = 12;
  t.a[6] = 14;
  t.a[7] = 16;
  r.v = punpcklbh_u (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == 2);
  assert (r.a[2] == 3);
  assert (r.a[3] == 4);
  assert (r.a[4] == 5);
  assert (r.a[5] == 6);
  assert (r.a[6] == 7);
  assert (r.a[7] == 8);
}

static void test_punpcklhw_s (void)
{
  int16x4_encap_t s, t;
  int16x4_encap_t r;
  s.a[0] = -1;
  s.a[1] = 3;
  s.a[2] = -5;
  s.a[3] = 7;
  t.a[0] = -2;
  t.a[1] = 4;
  t.a[2] = -6;
  t.a[3] = 8;
  r.v = punpcklhw_s (s.v, t.v);
  assert (r.a[0] == -1);
  assert (r.a[1] == -2);
  assert (r.a[2] == 3);
  assert (r.a[3] == 4);
}

static void test_punpcklhw_u (void)
{
  uint16x4_encap_t s, t;
  uint16x4_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  s.a[2] = 5;
  s.a[3] = 7;
  t.a[0] = 2;
  t.a[1] = 4;
  t.a[2] = 6;
  t.a[3] = 8;
  r.v = punpcklhw_u (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == 2);
  assert (r.a[2] == 3);
  assert (r.a[3] == 4);
}

static void test_punpcklwd_s (void)
{
  int32x2_encap_t s, t;
  int32x2_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  t.a[0] = -2;
  t.a[1] = 4;
  r.v = punpcklwd_s (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == -2);
}

static void test_punpcklwd_u (void)
{
  uint32x2_encap_t s, t;
  uint32x2_encap_t r;
  s.a[0] = 1;
  s.a[1] = 3;
  t.a[0] = 2;
  t.a[1] = 4;
  r.v = punpcklwd_u (s.v, t.v);
  assert (r.a[0] == 1);
  assert (r.a[1] == 2);
}

int main (void)
{
  test_packsswh ();
  test_packsshb ();
  test_packushb ();
  test_paddw_u ();
  test_paddw_s ();
  test_paddh_u ();
  test_paddh_s ();
  test_paddb_u ();
  test_paddb_s ();
  test_paddd_u ();
  test_paddd_s ();
  test_paddsh ();
  test_paddsb ();
  test_paddush ();
  test_paddusb ();
  test_pandn_ud ();
  test_pandn_sd ();
  test_pandn_uw ();
  test_pandn_sw ();
  test_pandn_uh ();
  test_pandn_sh ();
  test_pandn_ub ();
  test_pandn_sb ();
  test_pavgh ();
  test_pavgb ();
  test_pcmpeqw_u ();
  test_pcmpeqh_u ();
  test_pcmpeqb_u ();
  test_pcmpeqw_s ();
  test_pcmpeqh_s ();
  test_pcmpeqb_s ();
  test_pcmpgtw_u ();
  test_pcmpgth_u ();
  test_pcmpgtb_u ();
  test_pcmpgtw_s ();
  test_pcmpgth_s ();
  test_pcmpgtb_s ();
  test_pextrh_u ();
  test_pextrh_s ();
  test_pinsrh_0123_u ();
  test_pinsrh_0123_s ();
  test_pmaddhw ();
  test_pmaxsh ();
  test_pmaxub ();
  test_pminsh ();
  test_pminub ();
  test_pmovmskb_u ();
  test_pmovmskb_s ();
  test_pmulhuh ();
  test_pmulhh ();
  test_pmullh ();
  test_pmuluw ();
  test_pasubub ();
  test_biadd ();
  test_psadbh ();
  test_pshufh_u ();
  test_pshufh_s ();
  test_psllh_u ();
  test_psllw_u ();
  test_psllh_s ();
  test_psllw_s ();
  test_psrah_u ();
  test_psraw_u ();
  test_psrah_s ();
  test_psraw_s ();
  test_psrlh_u ();
  test_psrlw_u ();
  test_psrlh_s ();
  test_psrlw_s ();
  test_psubw_u ();
  test_psubw_s ();
  test_psubh_u ();
  test_psubh_s ();
  test_psubb_u ();
  test_psubb_s ();
  test_psubd_u ();
  test_psubd_s ();
  test_psubsh ();
  test_psubsb ();
  test_psubush ();
  test_psubusb ();
  test_punpckhbh_s ();
  test_punpckhbh_u ();
  test_punpckhhw_s ();
  test_punpckhhw_u ();
  test_punpckhwd_s ();
  test_punpckhwd_u ();
  test_punpcklbh_s ();
  test_punpcklbh_u ();
  test_punpcklhw_s ();
  test_punpcklhw_u ();
  test_punpcklwd_s ();
  test_punpcklwd_u ();
  return 0;
}
