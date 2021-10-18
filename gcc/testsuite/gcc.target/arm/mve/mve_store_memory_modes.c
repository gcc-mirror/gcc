/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"
/*
**off_store8_0:
**	...
**	vstrb.8	q0, \[r0, #16\]
**	...
*/
uint8_t *off_store8_0 (uint8_t * a, uint8x16_t v)
{
  vst1q_u8 (a + 16, v);
  return a;
}

/*
**off_store8_1:
**	...
**	vstrb.16	q0, \[r0, #-1\]
**	...
*/
int8_t *off_store8_1 (int8_t * a, int16x8_t v)
{
  vstrbq_s16 (a - 1, v);
  return a;
}

/*
**off_store8_2:
**	...
**	vstrb.32	q0, \[r0, #-127\]
**	...
*/
uint8_t *off_store8_2 (uint8_t * a, uint32x4_t v)
{
  vstrbq_u32 (a - 127, v);
  return a;
}

/*
**off_store8_3:
**	...
**	vstrb.8	q0, \[r0, #127\]
**	...
*/
int8_t *off_store8_3 (int8_t * a, int8x16_t v)
{
  vstrbq_s8 (a + 127, v);
  return a;
}

/*
**not_off_store8_0:
**	...
**	vstrb.8	q0, \[r[0-9]+\]
**	...
*/
uint8_t *not_off_store8_0 (uint8_t * a, uint8x16_t v)
{
  vst1q_u8 (a - 128, v);
  return a;
}

/*
**off_storefp16_0:
**	...
**	vstrh.16	q0, \[r0, #250\]
**	...
*/
float16_t *off_storefp16_0 (float16_t *a, float16x8_t v)
{
  vst1q_f16 (a + 125, v);
  return a;
}

/*
**off_store16_0:
**	...
**	vstrh.16	q0, \[r0, #4\]
**	...
*/
int16_t *off_store16_0 (int16_t * a, int16x8_t v)
{
  vst1q_s16 (a + 2, v);
  return a;
}

/*
**off_store16_1:
**	...
**	vstrh.32	q0, \[r0, #-254\]
**	...
*/
int16_t *off_store16_1 (int16_t * a, int32x4_t v)
{
  vstrhq_s32 (a - 127, v);
  return a;
}

/*
**not_off_store16_0:
**	...
**	vstrh.16	q0, \[r[0-9]+\]
**	...
*/
uint8_t *not_off_store16_0 (uint8_t * a, uint16x8_t v)
{
  vst1q_u16 ((uint16_t *)(a - 1), v);
  return a;
}

/*
**not_off_store16_1:
**	...
**	vstrh.32	q0, \[r[0-9]+\]
**	...
*/
int16_t *not_off_store16_1 (int16_t * a, int32x4_t v)
{
  vstrhq_s32 ((a + 128), v);
  return a;
}

/*
**off_storefp32_0:
**	...
**	vstrw.32	q0, \[r0, #-412\]
**	...
*/
float32_t *off_storefp32_0 (float32_t *a, float32x4_t v)
{
  vst1q_f32 (a - 103, v);
  return a;
}

/*
**off_store32_0:
**	...
**	vstrw.32	q0, \[r0, #-4\]
**	...
*/
int32_t *off_store32_0 (int32_t * a, int32x4_t v)
{
  vst1q_s32 (a - 1, v);
  return a;
}

/*
**off_store32_1:
**	...
**	vstrw.32	q0, \[r0, #508\]
**	...
*/
uint32_t *off_store32_1 (uint32_t * a, uint32x4_t v)
{
  vstrwq_u32 (a + 127, v);
  return a;
}

/*
**pre_store8_0:
**	...
**	vstrb.8	q[0-9]+, \[r0, #-16\]!
**	...
*/
uint8_t* pre_store8_0 (uint8_t * a, uint8x16_t v)
{
  a -= 16;
  vst1q_u8 (a, v);
  return a;
}

/*
**pre_store8_1:
**	...
**	vstrb.16	q[0-9]+, \[r0, #4\]!
**	...
*/
int8_t* pre_store8_1 (int8_t * a, int16x8_t v)
{
  a += 4;
  vstrbq_s16 (a, v);
  return a;
}

/*
**pre_storefp16_0:
**	...
**	vstrh.16	q0, \[r0, #8\]!
**	...
*/
float16_t *pre_storefp16_0 (float16_t *a, float16x8_t v)
{
  a += 4;
  vst1q_f16 (a, v);
  return a;
}

/*
**pre_store16_0:
**	...
**	vstrh.16	q[0-9]+, \[r0, #254\]!
**	...
*/
uint16_t* pre_store16_0 (uint16_t * a, uint16x8_t v)
{
  a += 127;
  vstrhq_u16 (a, v);
  return a;
}

/*
**pre_store16_1:
**	...
**	vstrh.32	q[0-9]+, \[r0, #-52\]!
**	...
*/
int16_t* pre_store16_1 (int16_t * a, int32x4_t v)
{
  a -= 26;
  vstrhq_s32 (a, v);
  return a;
}

/*
**pre_storefp32_0:
**	...
**	vstrw.32	q0, \[r0, #-4\]!
**	...
*/
float32_t *pre_storefp32_0 (float32_t *a, float32x4_t v)
{
  a--;
  vst1q_f32 (a, v);
  return a;
}

/*
**pre_store32_0:
**	...
**	vstrw.32	q[0-9]+, \[r0, #4\]!
**	...
*/
int32_t* pre_store32_0 (int32_t * a, int32x4_t v)
{
  a += 1;
  vst1q_s32 (a, v);
  return a;
}


/*
**post_store8_0:
**	...
**	vstrb.8	q[0-9]+, \[r0\], #-26
**	...
*/
int8_t* post_store8_0 (int8_t * a, int8x16_t v)
{
  vst1q_s8 (a, v);
  a -= 26;
  return a;
}

/*
**post_store8_1:
**	...
**	vstrb.16	q[0-9]+, \[r0\], #1
**	...
*/
uint8_t* post_store8_1 (uint8_t * a, uint16x8_t v)
{
  vstrbq_u16 (a, v);
  a++;
  return a;
}

/*
**post_store8_2:
**	...
**	vstrb.8	q[0-9]+, \[r0\], #-26
**	...
*/
int8_t* post_store8_2 (int8_t * a, int8x16_t v)
{
  vst1q_s8 (a, v);
  a -= 26;
  return a;
}

/*
**post_store8_3:
**	...
**	vstrb.16	q[0-9]+, \[r0\], #7
**	...
*/
uint8_t* post_store8_3 (uint8_t * a, uint16x8_t v)
{
  vstrbq_u16 (a, v);
  a += 7;
  return a;
}

/*
**post_storefp16_0:
**	...
**	vstrh.16	q[0-9]+, \[r0\], #-16
**	...
*/
float16_t *post_storefp16_0 (float16_t *a, float16x8_t v)
{
  vst1q_f16 (a, v);
  a -= 8;
  return a;
}

/*
**post_store16_0:
**	...
**	vstrh.16	q[0-9]+, \[r0\], #126
**	...
*/
int16_t* post_store16_0 (int16_t * a, int16x8_t v)
{
  vstrhq_s16 (a, v);
  a += 63;
  return a;
}

/*
**post_store16_1:
**	...
**	vstrh.32	q[0-9]+, \[r0\], #-16
**	...
*/
uint16_t* post_store16_1 (uint16_t * a, uint32x4_t v)
{
  vstrhq_u32 (a, v);
  a -= 8;
  return a;
}

/*
**post_storefp32_0:
**	...
**	vstrw.32	q[0-9]+, \[r0\], #-16
**	...
*/
float32_t* post_storefp32_0 (float32_t * a, float32x4_t v)
{
  vst1q_f32 (a, v);
  a -= 4;
  return a;
}

/*
**post_store32_0:
**	...
**	vstrw.32	q[0-9]+, \[r0\], #16
**	...
*/
int32_t* post_store32_0 (int32_t * a, int32x4_t v)
{
  vst1q_s32 (a, v);
  a += 4;
  return a;
}
