
/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

extern void abort (void);

void
test1 ()
{
  int16x8_t val1, val2, val3;
  int16x8_t result;
  uint64_t act, exp;

  val1 = vcombine_s16 (vcreate_s16 (UINT64_C (0xffff9ab680000000)),
		       vcreate_s16 (UINT64_C (0x00000000ffff0000)));
  val2 = vcombine_s16 (vcreate_s16 (UINT64_C (0x32b77fffffff7fff)),
		       vcreate_s16 (UINT64_C (0x0000ffff00007fff)));
  val3 = vcombine_s16 (vcreate_s16 (UINT64_C (0x7fff00007fff0000)),
		       vcreate_s16 (UINT64_C (0x80007fff00000000)));
  result = vmlsq_laneq_s16 (val1, val2, val3, 6);

  act = vgetq_lane_u64 (vreinterpretq_u64_s16 (result), 0);
  exp = UINT64_C (0xb2b69ab5ffffffff);
  if (act != exp)
    abort ();

  act = vgetq_lane_u64 (vreinterpretq_u64_s16 (result), 1);
  exp = UINT64_C (0x00007fffffffffff);
  if (act != exp)
    abort ();
}

void
test2 ()
{
  int32x4_t val1, val2, val3;
  int32x4_t result;
  uint64_t exp, act;

  val1 = vcombine_s32 (vcreate_s32 (UINT64_C (0x00008000f46f7fff)),
		       vcreate_s32 (UINT64_C (0x7fffffffffff8000)));
  val2 = vcombine_s32 (vcreate_s32 (UINT64_C (0x7fff7fff0e700000)),
		       vcreate_s32 (UINT64_C (0xffff000080000000)));
  val3 = vcombine_s32 (vcreate_s32 (UINT64_C (0x00000000ffff0000)),
		       vcreate_s32 (UINT64_C (0xd9edea1a8000fb28)));
  result = vmlsq_laneq_s32 (val1, val2, val3, 3);

  act = vgetq_lane_u64 (vreinterpretq_u64_s32 (result), 0);
  exp = UINT64_C (0xcefb6a1a1d0f7fff);
  if (act != exp)
    abort ();

  act = vgetq_lane_u64 (vreinterpretq_u64_s32 (result), 1);
  exp = UINT64_C (0x6a19ffffffff8000);
  if (act != exp)
    abort ();
}

void
test3 ()
{
  uint16x8_t val1, val2, val3;
  uint16x8_t result;
  uint64_t act, exp;

  val1 = vcombine_u16 (vcreate_u16 (UINT64_C (0x000080008000802a)),
		       vcreate_u16 (UINT64_C (0x7fffffff00007fff)));
  val2 = vcombine_u16 (vcreate_u16 (UINT64_C (0x7fffcdf1ffff0000)),
		       vcreate_u16 (UINT64_C (0xe2550000ffffffff)));
  val3 = vcombine_u16 (vcreate_u16 (UINT64_C (0x80007fff80000000)),
		       vcreate_u16 (UINT64_C (0xbe2100007fffffff)));

  result = vmlsq_laneq_u16 (val1, val2, val3, 7);

  act = vgetq_lane_u64 (vreinterpretq_u64_u16 (result), 0);
  exp = UINT64_C (0x3e2115ef3e21802a);
  if (act != exp)
    abort ();

  act = vgetq_lane_u64 (vreinterpretq_u64_u16 (result), 1);
  exp = UINT64_C (0x3d0affffbe213e20);
  if (act != exp)
    abort ();
}

void
test4 ()
{
  uint32x4_t val1, val2, val3;
  uint32x4_t result;
  uint64_t act, exp;

  val1 = vcombine_u32 (vcreate_u32 (UINT64_C (0x3295fe3d7fff7fff)),
		       vcreate_u32 (UINT64_C (0x7fff00007fff7fff)));
  val2 = vcombine_u32 (vcreate_u32 (UINT64_C (0xffff7fff7fff8000)),
		       vcreate_u32 (UINT64_C (0x7fff80008000ffff)));
  val3 = vcombine_u32 (vcreate_u32 (UINT64_C (0x7fff7fff80008000)),
		       vcreate_u32 (UINT64_C (0x0000800053ab7fff)));

  result = vmlsq_laneq_u32 (val1, val2, val3, 2);

  act = vgetq_lane_u64 (vreinterpretq_u64_u32 (result), 0);
  exp = UINT64_C (0x4640fe3cbffeffff);
  if (act != exp)
    abort ();

  act = vgetq_lane_u64 (vreinterpretq_u64_u32 (result), 1);
  exp = UINT64_C (0xbffe8000d3abfffe);
  if (act != exp)
    abort ();
}

void
test5 ()
{
  float32x4_t val1, val2, val3;
  float32x4_t result;
  float32_t act;

  val1 = vcombine_f32 (vcreate_f32 (UINT64_C (0x3f49daf03ef3dc73)),
		       vcreate_f32 (UINT64_C (0x3f5d467a3ef3dc73)));
  val2 = vcombine_f32 (vcreate_f32 (UINT64_C (0x3d2064c83d10cd28)),
		       vcreate_f32 (UINT64_C (0x3ea7d1a23d10cd28)));
  val3 = vcombine_f32 (vcreate_f32 (UINT64_C (0x3f6131993edb1e04)),
		       vcreate_f32 (UINT64_C (0x3f37f4bf3edb1e04)));

  result = vmlsq_laneq_f32 (val1, val2, val3, 0);

  act = vgetq_lane_f32 (result, 0);
  if (act != 0.46116194128990173f)
    abort ();

  act = vgetq_lane_f32 (result, 1);
  if (act != 0.7717385292053223f)
    abort ();

  act = vgetq_lane_f32 (result, 2);
  if (act != 0.46116194128990173f)
    abort ();

  act = vgetq_lane_f32 (result, 3);
  if (act != 0.7240825295448303f)
    abort ();
}

int
main (void)
{
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();

  return 0;
}
