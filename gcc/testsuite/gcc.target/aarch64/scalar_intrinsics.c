/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */

#include <arm_neon.h>

/* Used to force a variable to a SIMD register.  */
#define force_simd(V1)   asm volatile ("mov %d0, %1.d[0]"	\
	   : "=w"(V1)						\
	   : "w"(V1)						\
	   : /* No clobbers */);

/* { dg-final { scan-assembler-times "\\tadd\\tx\[0-9\]+" 2 } } */

uint64x1_t
test_vaddd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vaddd_u64 (a, b);
}

int64x1_t
test_vaddd_s64 (int64x1_t a, int64x1_t b)
{
  return vaddd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tadd\\td\[0-9\]+" 1 } } */

int64x1_t
test_vaddd_s64_2 (int64x1_t a, int64x1_t b, int64x1_t c, int64x1_t d)
{
  return vqaddd_s64 (vaddd_s64 (vqaddd_s64 (a, b), vqaddd_s64 (c, d)),
		     vqaddd_s64 (a, d));
}

/* { dg-final { scan-assembler-times "\\tabs\\td\[0-9\]+, d\[0-9\]+" 1 } } */

int64x1_t
test_vabs_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vabs_s64 (a);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmeq\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vceqd_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vceqd_s64 (a, b);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmeq\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vceqzd_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vceqzd_s64 (a);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmge\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

uint64x1_t
test_vcged_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcged_s64 (a, b);
  force_simd (res);
  return res;
}

uint64x1_t
test_vcled_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcled_s64 (a, b);
  force_simd (res);
  return res;
}

/* Idiom recognition will cause this testcase not to generate
   the expected cmge instruction, so do not check for it.  */

uint64x1_t
test_vcgezd_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vcgezd_s64 (a);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmhs\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vcged_u64 (uint64x1_t a, uint64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcged_u64 (a, b);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmgt\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

uint64x1_t
test_vcgtd_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcgtd_s64 (a, b);
  force_simd (res);
  return res;
}

uint64x1_t
test_vcltd_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcltd_s64 (a, b);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmgt\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vcgtzd_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vcgtzd_s64 (a);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmhi\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vcgtd_u64 (uint64x1_t a, uint64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vcgtd_u64 (a, b);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmle\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vclezd_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vclezd_s64 (a);
  force_simd (res);
  return res;
}

/* Idiom recognition will cause this testcase not to generate
   the expected cmlt instruction, so do not check for it.  */

uint64x1_t
test_vcltzd_s64 (int64x1_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vcltzd_s64 (a);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "aarch64_get_lanev16qi" 2 } } */

int8x1_t
test_vdupb_lane_s8 (int8x16_t a)
{
  int8x1_t res;
  force_simd (a);
  res = vdupb_laneq_s8 (a, 2);
  force_simd (res);
  return res;
}

uint8x1_t
test_vdupb_lane_u8 (uint8x16_t a)
{
  uint8x1_t res;
  force_simd (a);
  res = vdupb_laneq_u8 (a, 2);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "aarch64_get_lanev8hi" 2 } } */

int16x1_t
test_vduph_lane_s16 (int16x8_t a)
{
  int16x1_t res;
  force_simd (a);
  res = vduph_laneq_s16 (a, 2);
  force_simd (res);
  return res;
}

uint16x1_t
test_vduph_lane_u16 (uint16x8_t a)
{
  uint16x1_t res;
  force_simd (a);
  res = vduph_laneq_u16 (a, 2);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "aarch64_get_lanev4si" 2 } } */

int32x1_t
test_vdups_lane_s32 (int32x4_t a)
{
  int32x1_t res;
  force_simd (a);
  res = vdups_laneq_s32 (a, 2);
  force_simd (res);
  return res;
}

uint32x1_t
test_vdups_lane_u32 (uint32x4_t a)
{
  uint32x1_t res;
  force_simd (a);
  res = vdups_laneq_u32 (a, 2);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "aarch64_get_lanev2di" 2 } } */

int64x1_t
test_vdupd_lane_s64 (int64x2_t a)
{
  int64x1_t res;
  force_simd (a);
  res = vdupd_laneq_s64 (a, 1);
  force_simd (res);
  return res;
}

uint64x1_t
test_vdupd_lane_u64 (uint64x2_t a)
{
  uint64x1_t res;
  force_simd (a);
  res = vdupd_laneq_u64 (a, 1);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\tcmtst\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

int64x1_t
test_vtst_s64 (int64x1_t a, int64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vtstd_s64 (a, b);
  force_simd (res);
  return res;
}

uint64x1_t
test_vtst_u64 (uint64x1_t a, uint64x1_t b)
{
  uint64x1_t res;
  force_simd (a);
  force_simd (b);
  res = vtstd_s64 (a, b);
  force_simd (res);
  return res;
}

/* { dg-final { scan-assembler-times "\\taddp\\td\[0-9\]+, v\[0-9\]+\.2d" 1 } } */

test_vpaddd_s64 (int64x2_t a)
{
  return vpaddd_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tuqadd\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqaddd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqaddd_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqadd\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqadds_u32 (uint32x1_t a, uint32x1_t b)
{
  return vqadds_u32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqadd\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqaddh_u16 (uint16x1_t a, uint16x1_t b)
{
  return vqaddh_u16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqadd\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqaddb_u8 (uint8x1_t a, uint8x1_t b)
{
  return vqaddb_u8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqadd\\td\[0-9\]+" 5 } } */

int64x1_t
test_vqaddd_s64 (int64x1_t a, int64x1_t b)
{
  return vqaddd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqadd\\ts\[0-9\]+, s\[0-9\]+" 1 } } */

int32x1_t
test_vqadds_s32 (int32x1_t a, int32x1_t b)
{
  return vqadds_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqadd\\th\[0-9\]+, h\[0-9\]+" 1 } } */

int16x1_t
test_vqaddh_s16 (int16x1_t a, int16x1_t b)
{
  return vqaddh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqadd\\tb\[0-9\]+, b\[0-9\]+" 1 } } */

int8x1_t
test_vqaddb_s8 (int8x1_t a, int8x1_t b)
{
  return vqaddb_s8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\ts\[0-9\]+, h\[0-9\]+, h\[0-9\]+" 1 } } */

int32x1_t
test_vqdmlalh_s16 (int32x1_t a, int16x1_t b, int16x1_t c)
{
  return vqdmlalh_s16 (a, b, c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\ts\[0-9\]+, h\[0-9\]+, v" 1 } } */

int32x1_t
test_vqdmlalh_lane_s16 (int32x1_t a, int16x1_t b, int16x4_t c)
{
  return vqdmlalh_lane_s16 (a, b, c, 3);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\td\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

int64x1_t
test_vqdmlals_s32 (int64x1_t a, int32x1_t b, int32x1_t c)
{
  return vqdmlals_s32 (a, b, c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\td\[0-9\]+, s\[0-9\]+, v" 1 } } */

int64x1_t
test_vqdmlals_lane_s32 (int64x1_t a, int32x1_t b, int32x2_t c)
{
  return vqdmlals_lane_s32 (a, b, c, 1);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\ts\[0-9\]+, h\[0-9\]+, h\[0-9\]+" 1 } } */

int32x1_t
test_vqdmlslh_s16 (int32x1_t a, int16x1_t b, int16x1_t c)
{
  return vqdmlslh_s16 (a, b, c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\ts\[0-9\]+, h\[0-9\]+, v" 1 } } */

int32x1_t
test_vqdmlslh_lane_s16 (int32x1_t a, int16x1_t b, int16x4_t c)
{
  return vqdmlslh_lane_s16 (a, b, c, 3);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\td\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

int64x1_t
test_vqdmlsls_s32 (int64x1_t a, int32x1_t b, int32x1_t c)
{
  return vqdmlsls_s32 (a, b, c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\td\[0-9\]+, s\[0-9\]+, v" 1 } } */

int64x1_t
test_vqdmlsls_lane_s32 (int64x1_t a, int32x1_t b, int32x2_t c)
{
  return vqdmlsls_lane_s32 (a, b, c, 1);
}

/* { dg-final { scan-assembler-times "\\tsqdmulh\\th\[0-9\]+, h\[0-9\]+, h\[0-9\]+" 1 } } */

int16x1_t
test_vqdmulhh_s16 (int16x1_t a, int16x1_t b)
{
  return vqdmulhh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqdmulh\\th\[0-9\]+, h\[0-9\]+, v" 1 } } */

int16x1_t
test_vqdmulhh_lane_s16 (int16x1_t a, int16x4_t b)
{
  return vqdmulhh_lane_s16 (a, b, 3);
}

/* { dg-final { scan-assembler-times "\\tsqdmulh\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

int32x1_t
test_vqdmulhs_s32 (int32x1_t a, int32x1_t b)
{
  return vqdmulhs_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqdmulh\\ts\[0-9\]+, s\[0-9\]+, v" 1 } } */

int32x1_t
test_vqdmulhs_lane_s32 (int32x1_t a, int32x2_t b)
{
  return vqdmulhs_lane_s32 (a, b, 1);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\ts\[0-9\]+, h\[0-9\]+, h\[0-9\]+" 1 } } */

int32x1_t
test_vqdmullh_s16 (int16x1_t a, int16x1_t b)
{
  return vqdmullh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\ts\[0-9\]+, h\[0-9\]+, v" 1 } } */

int32x1_t
test_vqdmullh_lane_s16 (int16x1_t a, int16x4_t b)
{
  return vqdmullh_lane_s16 (a, b, 3);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\td\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

int64x1_t
test_vqdmulls_s32 (int32x1_t a, int32x1_t b)
{
  return vqdmulls_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\td\[0-9\]+, s\[0-9\]+, v" 1 } } */

int64x1_t
test_vqdmulls_lane_s32 (int32x1_t a, int32x2_t b)
{
  return vqdmulls_lane_s32 (a, b, 1);
}

/* { dg-final { scan-assembler-times "\\tsqrdmulh\\th\[0-9\]+, h\[0-9\]+, h\[0-9\]+" 1 } } */

int16x1_t
test_vqrdmulhh_s16 (int16x1_t a, int16x1_t b)
{
  return vqrdmulhh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqrdmulh\\th\[0-9\]+, h\[0-9\]+, v" 1 } } */

int16x1_t
test_vqrdmulhh_lane_s16 (int16x1_t a, int16x4_t b)
{
  return vqrdmulhh_lane_s16 (a, b, 3);
}

/* { dg-final { scan-assembler-times "\\tsqrdmulh\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

int32x1_t
test_vqrdmulhs_s32 (int32x1_t a, int32x1_t b)
{
  return vqrdmulhs_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqrdmulh\\ts\[0-9\]+, s\[0-9\]+, v" 1 } } */

int32x1_t
test_vqrdmulhs_lane_s32 (int32x1_t a, int32x2_t b)
{
  return vqrdmulhs_lane_s32 (a, b, 1);
}

/* { dg-final { scan-assembler-times "\\tsuqadd\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vuqaddb_s8 (int8x1_t a, int8x1_t b)
{
  return vuqaddb_s8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsuqadd\\th\[0-9\]+" 1 } } */

int16x1_t
test_vuqaddh_s16 (int16x1_t a, int8x1_t b)
{
  return vuqaddh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsuqadd\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vuqadds_s32 (int32x1_t a, int8x1_t b)
{
  return vuqadds_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsuqadd\\td\[0-9\]+" 1 } } */

int64x1_t
test_vuqaddd_s64 (int64x1_t a, int8x1_t b)
{
  return vuqaddd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tusqadd\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vsqaddb_u8 (uint8x1_t a, int8x1_t b)
{
  return vsqaddb_u8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tusqadd\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vsqaddh_u16 (uint16x1_t a, int8x1_t b)
{
  return vsqaddh_u16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tusqadd\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vsqadds_u32 (uint32x1_t a, int8x1_t b)
{
  return vsqadds_u32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tusqadd\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vsqaddd_u64 (uint64x1_t a, int8x1_t b)
{
  return vsqaddd_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqabs\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqabsb_s8 (int8x1_t a)
{
  return vqabsb_s8 (a);
}

/* { dg-final { scan-assembler-times "\\tsqabs\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqabsh_s16 (int16x1_t a)
{
  return vqabsh_s16 (a);
}

/* { dg-final { scan-assembler-times "\\tsqabs\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqabss_s32 (int32x1_t a)
{
  return vqabss_s32 (a);
}

/* { dg-final { scan-assembler-times "\\tsqneg\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqnegb_s8 (int8x1_t a)
{
  return vqnegb_s8 (a);
}

/* { dg-final { scan-assembler-times "\\tsqneg\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqnegh_s16 (int16x1_t a)
{
  return vqnegh_s16 (a);
}

/* { dg-final { scan-assembler-times "\\tsqneg\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqnegs_s32 (int32x1_t a)
{
  return vqnegs_s32 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtun\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqmovunh_s16 (int16x1_t a)
{
  return vqmovunh_s16 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtun\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqmovuns_s32 (int32x1_t a)
{
  return vqmovuns_s32 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtun\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqmovund_s64 (int64x1_t a)
{
  return vqmovund_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtn\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqmovnh_s16 (int16x1_t a)
{
  return vqmovnh_s16 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtn\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqmovns_s32 (int32x1_t a)
{
  return vqmovns_s32 (a);
}

/* { dg-final { scan-assembler-times "\\tsqxtn\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqmovnd_s64 (int64x1_t a)
{
  return vqmovnd_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tuqxtn\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqmovnh_u16 (uint16x1_t a)
{
  return vqmovnh_u16 (a);
}

/* { dg-final { scan-assembler-times "\\tuqxtn\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqmovns_u32 (uint32x1_t a)
{
  return vqmovns_u32 (a);
}

/* { dg-final { scan-assembler-times "\\tuqxtn\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqmovnd_u64 (uint64x1_t a)
{
  return vqmovnd_u64 (a);
}

/* { dg-final { scan-assembler-times "\\tsub\\tx\[0-9\]+" 2 } } */

uint64x1_t
test_vsubd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsubd_u64 (a, b);
}

int64x1_t
test_vsubd_s64 (int64x1_t a, int64x1_t b)
{
  return vsubd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsub\\td\[0-9\]+" 1 } } */

int64x1_t
test_vsubd_s64_2 (int64x1_t a, int64x1_t b, int64x1_t c, int64x1_t d)
{
  return vqsubd_s64 (vsubd_s64 (vqsubd_s64 (a, b), vqsubd_s64 (c, d)),
		     vqsubd_s64 (a, d));
}

/* { dg-final { scan-assembler-times "\\tuqsub\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqsubd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqsubd_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqsub\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqsubs_u32 (uint32x1_t a, uint32x1_t b)
{
  return vqsubs_u32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqsub\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqsubh_u16 (uint16x1_t a, uint16x1_t b)
{
  return vqsubh_u16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqsub\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqsubb_u8 (uint8x1_t a, uint8x1_t b)
{
  return vqsubb_u8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqsub\\td\[0-9\]+" 5 } } */

int64x1_t
test_vqsubd_s64 (int64x1_t a, int64x1_t b)
{
  return vqsubd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqsub\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqsubs_s32 (int32x1_t a, int32x1_t b)
{
  return vqsubs_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqsub\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqsubh_s16 (int16x1_t a, int16x1_t b)
{
  return vqsubh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqsub\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqsubb_s8 (int8x1_t a, int8x1_t b)
{
  return vqsubb_s8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vshld_s64 (int64x1_t a, int64x1_t b)
{
  return vshld_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tushl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vshld_u64 (uint64x1_t a, uint64x1_t b)
{
  return vshld_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsrshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrshld_s64 (int64x1_t a, int64x1_t b)
{
  return vrshld_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\turshl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrshld_u64 (uint64x1_t a, uint64x1_t b)
{
  return vrshld_u64 (a, b);
}

/* Other intrinsics can generate an asr instruction (vcltzd, vcgezd),
   so we cannot check scan-assembler-times.  */

/* { dg-final { scan-assembler "\\tasr\\tx\[0-9\]+" } } */

int64x1_t
test_vshrd_n_s64 (int64x1_t a)
{
  return vshrd_n_s64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tlsr\\tx\[0-9\]+" 1 } } */

uint64x1_t
test_vshrd_n_u64 (uint64x1_t a)
{
  return vshrd_n_u64 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tssra\\td\[0-9\]+" 1 } } */

int64x1_t
test_vsrad_n_s64 (int64x1_t a, int64x1_t b)
{
  return vsrad_n_s64 (a, b, 2);
}

/* { dg-final { scan-assembler-times "\\tusra\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vsrad_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsrad_n_u64 (a, b, 5);
}

/* { dg-final { scan-assembler-times "\\tsrshr\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrshrd_n_s64 (int64x1_t a)
{
  return vrshrd_n_s64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\turshr\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrshrd_n_u64 (uint64x1_t a)
{
  return vrshrd_n_u64 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsrsra\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrsrad_n_s64 (int64x1_t a, int64x1_t b)
{
  return vrsrad_n_s64 (a, b, 3);
}

/* { dg-final { scan-assembler-times "\\tsrsra\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrsrad_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vrsrad_n_u64 (a, b, 4);
}

/* { dg-final { scan-assembler-times "\\tsqrshl\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqrshlb_s8 (int8x1_t a, int8x1_t b)
{
  return vqrshlb_s8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqrshl\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqrshlh_s16 (int16x1_t a, int16x1_t b)
{
  return vqrshlh_s16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqrshl\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqrshls_s32 (int32x1_t a, int32x1_t b)
{
  return vqrshls_s32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqrshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vqrshld_s64 (int64x1_t a, int64x1_t b)
{
  return vqrshld_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqrshl\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqrshlb_u8 (uint8x1_t a, uint8x1_t b)
{
  return vqrshlb_u8 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqrshl\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqrshlh_u16 (uint16x1_t a, uint16x1_t b)
{
  return vqrshlh_u16 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqrshl\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqrshls_u32 (uint32x1_t a, uint32x1_t b)
{
  return vqrshls_u32 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqrshl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqrshld_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqrshld_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqshlu\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqshlub_n_s8 (int8x1_t a)
{
  return vqshlub_n_s8 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqshlu\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqshluh_n_s16 (int16x1_t a)
{
  return vqshluh_n_s16 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tsqshlu\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqshlus_n_s32 (int32x1_t a)
{
  return vqshlus_n_s32 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tsqshlu\\td\[0-9\]+" 1 } } */

int64x1_t
test_vqshlud_n_s64 (int64x1_t a)
{
  return vqshlud_n_s64 (a, 6);
}

/* { dg-final { scan-assembler-times "\\tsqshl\\tb\[0-9\]+" 2 } } */

int8x1_t
test_vqshlb_s8 (int8x1_t a, int8x1_t b)
{
  return vqshlb_s8 (a, b);
}

int8x1_t
test_vqshlb_n_s8 (int8x1_t a)
{
  return vqshlb_n_s8 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tsqshl\\th\[0-9\]+" 2 } } */

int16x1_t
test_vqshlh_s16 (int16x1_t a, int16x1_t b)
{
  return vqshlh_s16 (a, b);
}

int16x1_t
test_vqshlh_n_s16 (int16x1_t a)
{
  return vqshlh_n_s16 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqshl\\ts\[0-9\]+" 2 } } */

int32x1_t
test_vqshls_s32 (int32x1_t a, int32x1_t b)
{
  return vqshls_s32 (a, b);
}

int32x1_t
test_vqshls_n_s32 (int32x1_t a)
{
  return vqshls_n_s32 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tsqshl\\td\[0-9\]+" 2 } } */

int64x1_t
test_vqshld_s64 (int64x1_t a, int64x1_t b)
{
  return vqshld_s64 (a, b);
}

int64x1_t
test_vqshld_n_s64 (int64x1_t a)
{
  return vqshld_n_s64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tuqshl\\tb\[0-9\]+" 2 } } */

uint8x1_t
test_vqshlb_u8 (uint8x1_t a, uint8x1_t b)
{
  return vqshlb_u8 (a, b);
}

uint8x1_t
test_vqshlb_n_u8 (uint8x1_t a)
{
  return vqshlb_n_u8 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tuqshl\\th\[0-9\]+" 2 } } */

uint16x1_t
test_vqshlh_u16 (uint16x1_t a, uint16x1_t b)
{
  return vqshlh_u16 (a, b);
}

uint16x1_t
test_vqshlh_n_u16 (uint16x1_t a)
{
  return vqshlh_n_u16 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tuqshl\\ts\[0-9\]+" 2 } } */

uint32x1_t
test_vqshls_u32 (uint32x1_t a, uint32x1_t b)
{
  return vqshls_u32 (a, b);
}

uint32x1_t
test_vqshls_n_u32 (uint32x1_t a)
{
  return vqshls_n_u32 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tuqshl\\td\[0-9\]+" 2 } } */

uint64x1_t
test_vqshld_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqshld_u64 (a, b);
}

uint64x1_t
test_vqshld_n_u64 (uint64x1_t a)
{
  return vqshld_n_u64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tsqshrun\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqshrunh_n_s16 (int16x1_t a)
{
  return vqshrunh_n_s16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tsqshrun\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqshruns_n_s32 (int32x1_t a)
{
  return vqshruns_n_s32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqshrun\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqshrund_n_s64 (int64x1_t a)
{
  return vqshrund_n_s64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tsqrshrun\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqrshrunh_n_s16 (int16x1_t a)
{
  return vqrshrunh_n_s16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tsqrshrun\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqrshruns_n_s32 (int32x1_t a)
{
  return vqrshruns_n_s32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqrshrun\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqrshrund_n_s64 (int64x1_t a)
{
  return vqrshrund_n_s64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tsqshrn\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqshrnh_n_s16 (int16x1_t a)
{
  return vqshrnh_n_s16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tsqshrn\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqshrns_n_s32 (int32x1_t a)
{
  return vqshrns_n_s32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqshrn\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqshrnd_n_s64 (int64x1_t a)
{
  return vqshrnd_n_s64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tuqshrn\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqshrnh_n_u16 (uint16x1_t a)
{
  return vqshrnh_n_u16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tuqshrn\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqshrns_n_u32 (uint32x1_t a)
{
  return vqshrns_n_u32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tuqshrn\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqshrnd_n_u64 (uint64x1_t a)
{
  return vqshrnd_n_u64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tsqrshrn\\tb\[0-9\]+" 1 } } */

int8x1_t
test_vqrshrnh_n_s16 (int16x1_t a)
{
  return vqrshrnh_n_s16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tsqrshrn\\th\[0-9\]+" 1 } } */

int16x1_t
test_vqrshrns_n_s32 (int32x1_t a)
{
  return vqrshrns_n_s32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsqrshrn\\ts\[0-9\]+" 1 } } */

int32x1_t
test_vqrshrnd_n_s64 (int64x1_t a)
{
  return vqrshrnd_n_s64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tuqrshrn\\tb\[0-9\]+" 1 } } */

uint8x1_t
test_vqrshrnh_n_u16 (uint16x1_t a)
{
  return vqrshrnh_n_u16 (a, 2);
}

/* { dg-final { scan-assembler-times "\\tuqrshrn\\th\[0-9\]+" 1 } } */

uint16x1_t
test_vqrshrns_n_u32 (uint32x1_t a)
{
  return vqrshrns_n_u32 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tuqrshrn\\ts\[0-9\]+" 1 } } */

uint32x1_t
test_vqrshrnd_n_u64 (uint64x1_t a)
{
  return vqrshrnd_n_u64 (a, 4);
}

/* { dg-final { scan-assembler-times "\\tlsl\\tx\[0-9\]+" 2 } } */

int64x1_t
test_vshl_n_s64 (int64x1_t a)
{
  return vshld_n_s64 (a, 9);
}

uint64x1_t
test_vshl_n_u64 (uint64x1_t a)
{
  return vshld_n_u64 (a, 9);
}

/* { dg-final { scan-assembler-times "\\tsli\\td\[0-9\]+" 2 } } */

int64x1_t
test_vsli_n_s64 (int64x1_t a, int64x1_t b)
{
  return vslid_n_s64 (a, b, 9);
}

uint64x1_t
test_vsli_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vslid_n_u64 (a, b, 9);
}

/* { dg-final { scan-assembler-times "\\tsri\\td\[0-9\]+" 2 } } */

int64x1_t
test_vsri_n_s64 (int64x1_t a, int64x1_t b)
{
  return vsrid_n_s64 (a, b, 9);
}

uint64x1_t
test_vsri_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsrid_n_u64 (a, b, 9);
}
