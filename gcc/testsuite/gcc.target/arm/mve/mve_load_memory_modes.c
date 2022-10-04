/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"
/*
**off_load8_0:
**	...
**	vldrb.8	q[0-7], \[r0, #16\]
**	...
*/
int8x16_t off_load8_0 (int8_t * a)
{
  return vld1q_s8 (a + 16);
}

/*
**off_load8_1:
**	...
**	vldrb.u16	q[0-7], \[r0, #1\]
**	...
*/
uint16x8_t off_load8_1 (uint8_t * a)
{
  return vldrbq_u16 (a + 1);
}

/*
**off_load8_2:
**	...
**	vldrb.s32	q[0-7], \[r0, #127\]
**	...
*/
int32x4_t off_load8_2 (int8_t * a)
{
  return vldrbq_s32 (a + 127);
}

/*
**off_load8_3:
**	...
**	vldrb.8	q[0-7], \[r0, #-127\]
**	...
*/
uint8x16_t off_load8_3 (uint8_t * a)
{
  return vldrbq_u8 (a - 127);
}

/*
**not_off_load8_0:
**	...
**	vldrb.8	q[0-7], \[r[0-7]+\]
**	...
*/
int8x16_t not_off_load8_0 (int8_t * a)
{
  return vld1q_s8 (a + 128);
}

/*
**off_loadfp16_0:
**	...
**	vldrh.16	q[0-7], \[r0, #-244\]
**	...
*/
float16x8_t off_loadfp16_0 (float16_t *a)
{
  return vld1q_f16 (a - 122);
}

/*
**off_load16_0:
**	...
**	vldrh.16	q[0-7], \[r0, #-2\]
**	...
*/
uint16x8_t off_load16_0 (uint16_t * a)
{
  return vld1q_u16 (a - 1);
}

/*
**off_load16_1:
**	...
**	vldrh.u32	q[0-7], \[r0, #254\]
**	...
*/
uint32x4_t off_load16_1 (uint16_t * a)
{
  return vldrhq_u32 (a + 127);
}

/*
**not_off_load16_0:
**	...
**	vldrh.16	q[0-7], \[r[0-7]+\]
**	...
*/
int16x8_t not_off_load16_0 (int8_t * a)
{
  return vld1q_s16 ((int16_t *)(a + 1));
}

/*
**not_off_load16_1:
**	...
**	vldrh.u32	q[0-7], \[r[0-7]+\]
**	...
*/
uint32x4_t not_off_load16_1 (uint16_t * a)
{
  return vldrhq_u32 ((a - 128));
}

/*
**off_loadfp32_0:
**	...
**	vldrw.32	q[0-7], \[r0, #24\]
**	...
*/
float32x4_t off_loadfp32_0 (float32_t *a)
{
  return vld1q_f32 (a + 6);
}

/*
**off_load32_0:
**	...
**	vldrw.32	q[0-7], \[r0, #4\]
**	...
*/
uint32x4_t off_load32_0 (uint32_t * a)
{
  return vld1q_u32 (a + 1);
}

/*
**off_load32_1:
**	...
**	vldrw.32	q[0-7], \[r0, #-508\]
**	...
*/
int32x4_t off_load32_1 (int32_t * a)
{
  return vldrwq_s32 (a - 127);
}
/*
**pre_load8_0:
**	...
**	vldrb.8	q[0-7], \[r0, #16\]!
**	...
*/
int8_t* pre_load8_0 (int8_t * a, int8x16_t *v)
{
  a += 16;
  *v = vld1q_s8 (a);
  return a;
}

/*
**pre_load8_1:
**	...
**	vldrb.u16	q[0-7], \[r0, #4\]!
**	...
*/
uint8_t* pre_load8_1 (uint8_t * a, uint16x8_t *v)
{
  a += 4;
  *v = vldrbq_u16 (a);
  return a;
}

/*
**pre_loadfp16_0:
**	...
**	vldrh.16	q[0-7], \[r0, #128\]!
**	...
*/
float16_t* pre_loadfp16_0 (float16_t *a, float16x8_t *v)
{
  a += 64;
  *v = vld1q_f16 (a);
  return a;
}

/*
**pre_load16_0:
**	...
**	vldrh.16	q[0-7], \[r0, #-254\]!
**	...
*/
int16_t* pre_load16_0 (int16_t * a, int16x8_t *v)
{
  a -= 127;
  *v = vldrhq_s16 (a);
  return a;
}

/*
**pre_load16_1:
**	...
**	vldrh.s32	q[0-7], \[r0, #52\]!
**	...
*/
int16_t* pre_load16_1 (int16_t * a, int32x4_t *v)
{
  a += 26;
  *v = vldrhq_s32 (a);
  return a;
}

/*
**pre_loadfp32_0:
**	...
**	vldrw.32	q[0-7], \[r0, #-72\]!
**	...
*/
float32_t* pre_loadfp32_0 (float32_t *a, float32x4_t *v)
{
  a -= 18;
  *v = vld1q_f32 (a);
  return a;
}


/*
**pre_load32_0:
**	...
**	vldrw.32	q[0-7], \[r0, #-4\]!
**	...
*/
uint32_t* pre_load32_0 (uint32_t * a, uint32x4_t *v)
{
  a -= 1;
  *v = vld1q_u32 (a);
  return a;
}


/*
**post_load8_0:
**	...
**	vldrb.8	q[0-7], \[r0\], #26
**	...
*/
uint8_t* post_load8_0 (uint8_t * a, uint8x16_t *v)
{
  *v = vld1q_u8 (a);
  a += 26;
  return a;
}

/*
**post_load8_1:
**	...
**	vldrb.s16	q[0-7], \[r0\], #-1
**	...
*/
int8_t* post_load8_1 (int8_t * a, int16x8_t *v)
{
  *v = vldrbq_s16 (a);
  a--;
  return a;
}

/*
**post_load8_2:
**	...
**	vldrb.8	q[0-7], \[r0\], #26
**	...
*/
uint8_t* post_load8_2 (uint8_t * a, uint8x16_t *v)
{
  *v = vld1q_u8 (a);
  a += 26;
  return a;
}

/*
**post_load8_3:
**	...
**	vldrb.s16	q[0-7], \[r0\], #-1
**	...
*/
int8_t* post_load8_3 (int8_t * a, int16x8_t *v)
{
  *v = vldrbq_s16 (a);
  a--;
  return a;
}

/*
**post_loadfp16_0:
**	...
**	vldrh.16	q[0-7], \[r0\], #-24
**	...
*/
float16_t* post_loadfp16_0 (float16_t *a, float16x8_t *v)
{
  *v = vld1q_f16 (a);
  a -= 12;
  return a;
}

/*
**post_load16_0:
**	...
**	vldrh.16	q[0-7], \[r0\], #-126
**	...
*/
uint16_t* post_load16_0 (uint16_t * a, uint16x8_t *v)
{
  *v = vldrhq_u16 (a);
  a -= 63;
  return a;
}

/*
**post_load16_1:
**	...
**	vldrh.u32	q[0-7], \[r0\], #16
**	...
*/
uint16_t* post_load16_1 (uint16_t * a, uint32x4_t *v)
{
  *v = vldrhq_u32 (a);
  a += 8;
  return a;
}

/*
**post_loadfp32_0:
**	...
**	vldrw.32	q[0-7], \[r0\], #4
**	...
*/
float32_t* post_loadfp32_0 (float32_t *a, float32x4_t *v)
{
  *v = vld1q_f32 (a);
  a++;
  return a;
}

/*
**post_load32_0:
**	...
**	vldrw.32	q[0-7], \[r0\], #-16
**	...
*/
int32_t* post_load32_0 (int32_t * a, int32x4_t *v)
{
  *v = vld1q_s32 (a);
  a -= 4;
  return a;
}
