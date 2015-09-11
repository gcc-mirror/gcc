/* { dg-do assemble } */
/* { dg-options "-O2 -dp --save-temps" } */

/* Test the [u]int64x1_t intrinsics.  */

#include <arm_neon.h>

/* { dg-final { scan-assembler-times "\\tadd\\td\[0-9\]+" 2 } } */

uint64x1_t
test_vadd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vadd_u64 (a, b);
}

int64x1_t
test_vadd_s64 (int64x1_t a, int64x1_t b)
{
  return vadd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tabs\\td\[0-9\]+, d\[0-9\]+" 1 } } */

int64x1_t
test_vabs_s64 (int64x1_t a)
{
  return vabs_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tcmeq\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vceq_s64 (int64x1_t a, int64x1_t b)
{
  return vceq_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tcmeq\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vceqz_s64 (int64x1_t a)
{
  return vceqz_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tcmge\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

uint64x1_t
test_vcge_s64 (int64x1_t a, int64x1_t b)
{
  return vcge_s64 (a, b);
}

uint64x1_t
test_vcle_s64 (int64x1_t a, int64x1_t b)
{
  return vcle_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tcmge\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vcgez_s64 (int64x1_t a)
{
  return vcgez_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tcmhs\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vcge_u64 (uint64x1_t a, uint64x1_t b)
{
  return vcge_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tcmgt\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

uint64x1_t
test_vcgt_s64 (int64x1_t a, int64x1_t b)
{
  return vcgt_s64 (a, b);
}

uint64x1_t
test_vclt_s64 (int64x1_t a, int64x1_t b)
{
  return vclt_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tcmgt\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vcgtz_s64 (int64x1_t a)
{
  return vcgtz_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tcmhi\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

uint64x1_t
test_vcgt_u64 (uint64x1_t a, uint64x1_t b)
{
  return vcgt_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tcmle\\td\[0-9\]+, d\[0-9\]+, #?0" 1 } } */

uint64x1_t
test_vclez_s64 (int64x1_t a)
{
  return vclez_s64 (a);
}

/* Compiling with "-dp" outputs the name of each .md pattern into the assembler.
   This is what we look for here.  */
/* { dg-final { scan-assembler-times "aarch64_get_lanev2di" 2 } } */

int64x1_t
test_vdup_laneq_s64 (int64x2_t a)
{
  return vdup_laneq_s64 (a, 1);
}

uint64x1_t
test_vdup_laneq_u64 (uint64x2_t a)
{
  return vdup_laneq_u64 (a, 1);
}

/* { dg-final { scan-assembler-times "\\tcmtst\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 2 } } */

uint64x1_t
test_vtst_s64 (int64x1_t a, int64x1_t b)
{
  return vtst_s64 (a, b);
}

uint64x1_t
test_vtst_u64 (uint64x1_t a, uint64x1_t b)
{
  return vtst_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqadd\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqadd_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqadd_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqadd\\td\[0-9\]+" 1 } } */

int64x1_t
test_vqadd_s64 (int64x1_t a, int64x1_t b)
{
  return vqadd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsuqadd\\td\[0-9\]+" 1 } } */

int64x1_t
test_vuqadd_s64 (int64x1_t a, uint64x1_t b)
{
  return vuqadd_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tusqadd\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vsqadd_u64 (uint64x1_t a, int64x1_t b)
{
  return vsqadd_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsub\\td\[0-9\]+" 2 } } */

uint64x1_t
test_vsub_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsub_u64 (a, b);
}

int64x1_t
test_vsub_s64 (int64x1_t a, int64x1_t b)
{
  return vsub_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqsub\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqsub_u64 (uint64x1_t a, uint64x1_t b)
{
  return vqsub_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqsub\\td\[0-9\]+" 1 } } */

int64x1_t
test_vqsub_s64 (int64x1_t a, int64x1_t b)
{
  return vqsub_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vshl_s64 (int64x1_t a, int64x1_t b)
{
  return vshl_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tushl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vshl_u64 (uint64x1_t a, int64x1_t b)
{
  return vshl_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsrshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrshl_s64 (int64x1_t a, int64x1_t b)
{
  return vrshl_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\turshl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrshl_u64 (uint64x1_t a, int64x1_t b)
{
  return vrshl_u64 (a, b);
}

/* For int64x1_t, sshr...#63 is equivalent to cmlt...#0.  */
/* { dg-final { scan-assembler-times "\\t(?:sshr|cmlt)\\td\[0-9\]+" 2 } } */

int64x1_t
test_vshr_n_s64 (int64x1_t a)
{
  return vshr_n_s64 (a, 5);
}

uint64x1_t
test_vcltz_s64 (int64x1_t a)
{
  return vcltz_s64 (a);
}

/* { dg-final { scan-assembler-times "\\tushr\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vshr_n_u64 (uint64x1_t a)
{
  return vshr_n_u64 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tssra\\td\[0-9\]+" 1 } } */

int64x1_t
test_vsra_n_s64 (int64x1_t a, int64x1_t b)
{
  return vsra_n_s64 (a, b, 2);
}

/* { dg-final { scan-assembler-times "\\tusra\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vsra_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsra_n_u64 (a, b, 5);
}

/* { dg-final { scan-assembler-times "\\tsrshr\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrshr_n_s64 (int64x1_t a)
{
  return vrshr_n_s64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\turshr\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrshr_n_u64 (uint64x1_t a)
{
  return vrshr_n_u64 (a, 3);
}

/* { dg-final { scan-assembler-times "\\tsrsra\\td\[0-9\]+" 1 } } */

int64x1_t
test_vrsra_n_s64 (int64x1_t a, int64x1_t b)
{
  return vrsra_n_s64 (a, b, 3);
}

/* { dg-final { scan-assembler-times "\\tsrsra\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vrsra_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vrsra_n_u64 (a, b, 4);
}

/* { dg-final { scan-assembler-times "\\tsqrshl\\td\[0-9\]+" 1 } } */

int64x1_t
test_vqrshl_s64 (int64x1_t a, int64x1_t b)
{
  return vqrshl_s64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tuqrshl\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqrshl_u64 (uint64x1_t a, int64x1_t b)
{
  return vqrshl_u64 (a, b);
}

/* { dg-final { scan-assembler-times "\\tsqshlu\\td\[0-9\]+" 1 } } */

uint64x1_t
test_vqshlu_n_s64 (int64x1_t a)
{
  return vqshlu_n_s64 (a, 6);
}

/* { dg-final { scan-assembler-times "\\tsqshl\\td\[0-9\]+" 2 } } */

int64x1_t
test_vqshl_s64 (int64x1_t a, int64x1_t b)
{
  return vqshl_s64 (a, b);
}

int64x1_t
test_vqshl_n_s64 (int64x1_t a)
{
  return vqshl_n_s64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tuqshl\\td\[0-9\]+" 2 } } */

uint64x1_t
test_vqshl_u64 (uint64x1_t a, int64x1_t b)
{
  return vqshl_u64 (a, b);
}

uint64x1_t
test_vqshl_n_u64 (uint64x1_t a)
{
  return vqshl_n_u64 (a, 5);
}

/* { dg-final { scan-assembler-times "\\tshl\\td\[0-9\]+" 2 } } */

int64x1_t
test_vshl_n_s64 (int64x1_t a)
{
  return vshl_n_s64 (a, 9);
}

uint64x1_t
test_vshl_n_u64 (uint64x1_t a)
{
  return vshl_n_u64 (a, 9);
}

/* { dg-final { scan-assembler-times "\\tsli\\td\[0-9\]+" 2 } } */

int64x1_t
test_vsli_n_s64 (int64x1_t a, int64x1_t b)
{
  return vsli_n_s64 (a, b, 9);
}

uint64x1_t
test_vsli_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsli_n_u64 (a, b, 9);
}

/* { dg-final { scan-assembler-times "\\tsri\\td\[0-9\]+" 2 } } */

int64x1_t
test_vsri_n_s64 (int64x1_t a, int64x1_t b)
{
  return vsri_n_s64 (a, b, 9);
}

uint64x1_t
test_vsri_n_u64 (uint64x1_t a, uint64x1_t b)
{
  return vsri_n_u64 (a, b, 9);
}


