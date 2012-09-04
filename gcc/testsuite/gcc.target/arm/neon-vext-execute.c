/* { dg-do run } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_little_endian } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>
#include <stdlib.h>
#include <stdio.h>

uint8x8_t
tst_vext_u8 (uint8x8_t __a, uint8x8_t __b)
{
  uint8x8_t __mask1 = {2, 3, 4, 5, 6, 7, 8, 9};

  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint8x8_t
tst_vext_u8_rotate (uint8x8_t __a)
{
  uint8x8_t __mask1 = {2, 3, 4, 5, 6, 7, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x4_t
tst_vext_u16 (uint16x4_t __a, uint16x4_t __b)
{
  uint16x4_t __mask1 = {2, 3, 4, 5};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint16x4_t
tst_vext_u16_rotate (uint16x4_t __a)
{
  uint16x4_t __mask1 = {2, 3, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint32x2_t
tst_vext_u32 (uint32x2_t __a, uint32x2_t __b)
{
  uint32x2_t __mask1 = {1, 2};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

/* This one is mapped into vrev64.32.  */
uint32x2_t
tst_vext_u32_rotate (uint32x2_t __a)
{
  uint32x2_t __mask1 = {1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint8x16_t
tst_vextq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  uint8x16_t __mask1 = {4, 5, 6, 7, 8, 9, 10, 11,
			12, 13, 14, 15, 16, 17, 18, 19};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint8x16_t
tst_vextq_u8_rotate (uint8x16_t __a)
{
  uint8x16_t __mask1 = {4, 5, 6, 7, 8, 9, 10, 11,
			12, 13, 14, 15, 0, 1, 2, 3};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint16x8_t
tst_vextq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  uint16x8_t __mask1 = {2, 3, 4, 5, 6, 7, 8, 9};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint16x8_t
tst_vextq_u16_rotate (uint16x8_t __a)
{
  uint16x8_t __mask1 = {2, 3, 4, 5, 6, 7, 0, 1};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint32x4_t
tst_vextq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  uint32x4_t __mask1 = {1, 2, 3, 4};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint32x4_t
tst_vextq_u32_rotate (uint32x4_t __a)
{
  uint32x4_t __mask1 = {1, 2, 3, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

uint64x2_t
tst_vextq_u64 (uint64x2_t __a, uint64x2_t __b)
{
  uint64x2_t __mask1 = {1, 2};
  return __builtin_shuffle ( __a, __b, __mask1) ;
}

uint64x2_t
tst_vextq_u64_rotate (uint64x2_t __a)
{
  uint64x2_t __mask1 = {1, 0};
  return __builtin_shuffle ( __a, __mask1) ;
}

int main (void)
{
  uint8_t arr_u8x8[] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint8_t arr2_u8x8[] = {8, 9, 10, 11, 12, 13, 14, 15};
  uint16_t arr_u16x4[] = {0, 1, 2, 3};
  uint16_t arr2_u16x4[] = {4, 5, 6, 7};
  uint32_t arr_u32x2[] = {0, 1};
  uint32_t arr2_u32x2[] = {2, 3};
  uint8_t arr_u8x16[] = {0, 1, 2, 3, 4, 5, 6, 7,
			 8, 9, 10, 11, 12, 13, 14, 15};
  uint8_t arr2_u8x16[] = {16, 17, 18, 19, 20, 21, 22, 23,
			  24, 25, 26, 27, 28, 29, 30, 31};
  uint16_t arr_u16x8[] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint16_t arr2_u16x8[] = {8, 9, 10, 11, 12, 13, 14, 15};
  uint32_t arr_u32x4[] = {0, 1, 2, 3};
  uint32_t arr2_u32x4[] = {4, 5, 6, 7};
  uint64_t arr_u64x2[] = {0, 1};
  uint64_t arr2_u64x2[] = {2, 3};

  uint8_t expected_u8x8[] = {2, 3, 4, 5, 6, 7, 8, 9};
  uint8_t expected_rot_u8x8[] = {2, 3, 4, 5, 6, 7, 0, 1};
  uint16_t expected_u16x4[] = {2, 3, 4, 5};
  uint16_t expected_rot_u16x4[] = {2, 3, 0, 1};
  uint32_t expected_u32x2[] = {1, 2};
  uint32_t expected_rot_u32x2[] = {1, 0};
  uint8_t expected_u8x16[] = {4, 5, 6, 7, 8, 9, 10, 11,
			      12, 13, 14, 15, 16, 17, 18, 19};
  uint8_t expected_rot_u8x16[] = {4, 5, 6, 7, 8, 9, 10, 11,
				  12, 13, 14, 15, 0, 1, 2, 3,};
  uint16_t expected_u16x8[] = {2, 3, 4, 5, 6, 7, 8, 9};
  uint16_t expected_rot_u16x8[] = {2, 3, 4, 5, 6, 7, 0, 1};
  uint32_t expected_u32x4[] = {1, 2, 3, 4};
  uint32_t expected_rot_u32x4[] = {1, 2, 3, 0};
  uint64_t expected_u64x2[] = {1, 2};
  uint64_t expected_rot_u64x2[] = {1, 0};

  uint8x8_t vec_u8x8 = vld1_u8 (arr_u8x8);
  uint8x8_t vec2_u8x8 = vld1_u8 (arr2_u8x8);
  uint16x4_t vec_u16x4 = vld1_u16 (arr_u16x4);
  uint16x4_t vec2_u16x4 = vld1_u16 (arr2_u16x4);
  uint32x2_t vec_u32x2 = vld1_u32 (arr_u32x2);
  uint32x2_t vec2_u32x2 = vld1_u32 (arr2_u32x2);
  uint8x16_t vec_u8x16 = vld1q_u8 (arr_u8x16);
  uint8x16_t vec2_u8x16 = vld1q_u8 (arr2_u8x16);
  uint16x8_t vec_u16x8 = vld1q_u16 (arr_u16x8);
  uint16x8_t vec2_u16x8 = vld1q_u16 (arr2_u16x8);
  uint32x4_t vec_u32x4 = vld1q_u32 (arr_u32x4);
  uint32x4_t vec2_u32x4 = vld1q_u32 (arr2_u32x4);
  uint64x2_t vec_u64x2 = vld1q_u64 (arr_u64x2);
  uint64x2_t vec2_u64x2 = vld1q_u64 (arr2_u64x2);

  uint8x8_t result_u8x8;
  uint16x4_t result_u16x4;
  uint32x2_t result_u32x2;
  uint8x16_t result_u8x16;
  uint16x8_t result_u16x8;
  uint32x4_t result_u32x4;
  uint64x2_t result_u64x2;

  union {uint8x8_t v; uint8_t buf[8];} mem_u8x8;
  union {uint16x4_t v; uint16_t buf[4];} mem_u16x4;
  union {uint32x2_t v; uint32_t buf[2];} mem_u32x2;
  union {uint8x16_t v; uint8_t buf[16];} mem_u8x16;
  union {uint16x8_t v; uint16_t buf[8];} mem_u16x8;
  union {uint32x4_t v; uint32_t buf[4];} mem_u32x4;
  union {uint64x2_t v; uint64_t buf[2];} mem_u64x2;

  int i;

  result_u8x8 = tst_vext_u8 (vec_u8x8, vec2_u8x8);
  vst1_u8 (mem_u8x8.buf, result_u8x8);

  for (i=0; i<8; i++)
      if (mem_u8x8.buf[i] != expected_u8x8[i])
	{
	  printf ("tst_vext_u8[%d]=%d expected %d\n",
		  i, mem_u8x8.buf[i], expected_u8x8[i]);
	  abort ();
	}

  result_u8x8 = tst_vext_u8_rotate (vec_u8x8);
  vst1_u8 (mem_u8x8.buf, result_u8x8);

  for (i=0; i<8; i++)
      if (mem_u8x8.buf[i] != expected_rot_u8x8[i])
	{
	  printf ("tst_vext_u8_rotate[%d]=%d expected %d\n",
		  i, mem_u8x8.buf[i], expected_rot_u8x8[i]);
	  abort ();
	}


  result_u16x4 = tst_vext_u16 (vec_u16x4, vec2_u16x4);
  vst1_u16 (mem_u16x4.buf, result_u16x4);

  for (i=0; i<4; i++)
      if (mem_u16x4.buf[i] != expected_u16x4[i])
	{
	  printf ("tst_vext_u16[%d]=%d expected %d\n",
		  i, mem_u16x4.buf[i], expected_u16x4[i]);
	  abort ();
	}

  result_u16x4 = tst_vext_u16_rotate (vec_u16x4);
  vst1_u16 (mem_u16x4.buf, result_u16x4);

  for (i=0; i<4; i++)
      if (mem_u16x4.buf[i] != expected_rot_u16x4[i])
	{
	  printf ("tst_vext_u16_rotate[%d]=%d expected %d\n",
		  i, mem_u16x4.buf[i], expected_rot_u16x4[i]);
	  abort ();
	}


  result_u32x2 = tst_vext_u32 (vec_u32x2, vec2_u32x2);
  vst1_u32 (mem_u32x2.buf, result_u32x2);

  for (i=0; i<2; i++)
      if (mem_u32x2.buf[i] != expected_u32x2[i])
	{
	  printf ("tst_vext_u32[%d]=%d expected %d\n",
		  i, mem_u32x2.buf[i], expected_u32x2[i]);
	  abort ();
	}

  result_u32x2 = tst_vext_u32_rotate (vec_u32x2);
  vst1_u32 (mem_u32x2.buf, result_u32x2);

  for (i=0; i<2; i++)
      if (mem_u32x2.buf[i] != expected_rot_u32x2[i])
	{
	  printf ("tst_vext_u32_rotate[%d]=%d expected %d\n",
		  i, mem_u32x2.buf[i], expected_rot_u32x2[i]);
	  abort ();
	}


  result_u8x16 = tst_vextq_u8 (vec_u8x16, vec2_u8x16);
  vst1q_u8 (mem_u8x16.buf, result_u8x16);

  for (i=0; i<16; i++)
      if (mem_u8x16.buf[i] != expected_u8x16[i])
	{
	  printf ("tst_vextq_u8[%d]=%d expected %d\n",
		  i, mem_u8x16.buf[i], expected_u8x16[i]);
	  abort ();
	}

  result_u8x16 = tst_vextq_u8_rotate (vec_u8x16);
  vst1q_u8 (mem_u8x16.buf, result_u8x16);

  for (i=0; i<16; i++)
      if (mem_u8x16.buf[i] != expected_rot_u8x16[i])
	{
	  printf ("tst_vextq_u8_rotate[%d]=%d expected %d\n",
		  i, mem_u8x16.buf[i], expected_rot_u8x16[i]);
	  abort ();
	}

  result_u16x8 = tst_vextq_u16 (vec_u16x8, vec2_u16x8);
  vst1q_u16 (mem_u16x8.buf, result_u16x8);

  for (i=0; i<8; i++)
      if (mem_u16x8.buf[i] != expected_u16x8[i])
	{
	  printf ("tst_vextq_u16[%d]=%d expected %d\n",
		  i, mem_u16x8.buf[i], expected_u16x8[i]);
	  abort ();
	}

  result_u16x8 = tst_vextq_u16_rotate (vec_u16x8);
  vst1q_u16 (mem_u16x8.buf, result_u16x8);

  for (i=0; i<8; i++)
      if (mem_u16x8.buf[i] != expected_rot_u16x8[i])
	{
	  printf ("tst_vextq_u16_rotate[%d]=%d expected %d\n",
		  i, mem_u16x8.buf[i], expected_rot_u16x8[i]);
	  abort ();
	}

  result_u32x4 = tst_vextq_u32 (vec_u32x4, vec2_u32x4);
  vst1q_u32 (mem_u32x4.buf, result_u32x4);

  for (i=0; i<4; i++)
      if (mem_u32x4.buf[i] != expected_u32x4[i])
	{
	  printf ("tst_vextq_u32[%d]=%d expected %d\n",
		  i, mem_u32x4.buf[i], expected_u32x4[i]);
	  abort ();
	}

  result_u32x4 = tst_vextq_u32_rotate (vec_u32x4);
  vst1q_u32 (mem_u32x4.buf, result_u32x4);

  for (i=0; i<4; i++)
      if (mem_u32x4.buf[i] != expected_rot_u32x4[i])
	{
	  printf ("tst_vextq_u32_rotate[%d]=%d expected %d\n",
		  i, mem_u32x4.buf[i], expected_rot_u32x4[i]);
	  abort ();
	}

  result_u64x2 = tst_vextq_u64 (vec_u64x2, vec2_u64x2);
  vst1q_u64 (mem_u64x2.buf, result_u64x2);

  for (i=0; i<2; i++)
      if (mem_u64x2.buf[i] != expected_u64x2[i])
	{
	  printf ("tst_vextq_u64[%d]=%lld expected %lld\n",
		  i, mem_u64x2.buf[i], expected_u64x2[i]);
	  abort ();
	}

  result_u64x2 = tst_vextq_u64_rotate (vec_u64x2);
  vst1q_u64 (mem_u64x2.buf, result_u64x2);

  for (i=0; i<2; i++)
      if (mem_u64x2.buf[i] != expected_rot_u64x2[i])
	{
	  printf ("tst_vextq_u64_rotate[%d]=%lld expected %lld\n",
		  i, mem_u64x2.buf[i], expected_rot_u64x2[i]);
	  abort ();
	}

  return 0;
}
