/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

int8x8_t
tbl_tests8_ (int8x8_t tab, int8x8_t idx)
{
  return vtbl1_s8 (tab, idx);
}

uint8x8_t
tbl_testu8_ (uint8x8_t tab, uint8x8_t idx)
{
  return vtbl1_u8 (tab, idx);
}

poly8x8_t
tbl_testp8_ (poly8x8_t tab, uint8x8_t idx)
{
  return vtbl1_p8 (tab, idx);
}

int8x8_t
tbl_tests8_2 (int8x8x2_t tab, int8x8_t idx)
{
  return vtbl2_s8 (tab, idx);
}

uint8x8_t
tbl_testu8_2 (uint8x8x2_t tab, uint8x8_t idx)
{
  return vtbl2_u8 (tab, idx);
}

poly8x8_t
tbl_testp8_2 (poly8x8x2_t tab, uint8x8_t idx)
{
  return vtbl2_p8 (tab, idx);
}

int8x8_t
tbl_tests8_3 (int8x8x3_t tab, int8x8_t idx)
{
  return vtbl3_s8 (tab, idx);
}

uint8x8_t
tbl_testu8_3 (uint8x8x3_t tab, uint8x8_t idx)
{
  return vtbl3_u8 (tab, idx);
}

poly8x8_t
tbl_testp8_3 (poly8x8x3_t tab, uint8x8_t idx)
{
  return vtbl3_p8 (tab, idx);
}

int8x8_t
tbl_tests8_4 (int8x8x4_t tab, int8x8_t idx)
{
  return vtbl4_s8 (tab, idx);
}

uint8x8_t
tbl_testu8_4 (uint8x8x4_t tab, uint8x8_t idx)
{
  return vtbl4_u8 (tab, idx);
}

poly8x8_t
tbl_testp8_4 (poly8x8x4_t tab, uint8x8_t idx)
{
  return vtbl4_p8 (tab, idx);
}

int8x8_t
tb_tests8_ (int8x8_t r, int8x8_t tab, int8x8_t idx)
{
  return vtbx1_s8 (r, tab, idx);
}

uint8x8_t
tb_testu8_ (uint8x8_t r, uint8x8_t tab, uint8x8_t idx)
{
  return vtbx1_u8 (r, tab, idx);
}

poly8x8_t
tb_testp8_ (poly8x8_t r, poly8x8_t tab, uint8x8_t idx)
{
  return vtbx1_p8 (r, tab, idx);
}

int8x8_t
tb_tests8_2 (int8x8_t r, int8x8x2_t tab, int8x8_t idx)
{
  return vtbx2_s8 (r, tab, idx);
}

uint8x8_t
tb_testu8_2 (uint8x8_t r, uint8x8x2_t tab, uint8x8_t idx)
{
  return vtbx2_u8 (r, tab, idx);
}

poly8x8_t
tb_testp8_2 (poly8x8_t r, poly8x8x2_t tab, uint8x8_t idx)
{
  return vtbx2_p8 (r, tab, idx);
}

int8x8_t
tb_tests8_3 (int8x8_t r, int8x8x3_t tab, int8x8_t idx)
{
  return vtbx3_s8 (r, tab, idx);
}

uint8x8_t
tb_testu8_3 (uint8x8_t r, uint8x8x3_t tab, uint8x8_t idx)
{
  return vtbx3_u8 (r, tab, idx);
}

poly8x8_t
tb_testp8_3 (poly8x8_t r, poly8x8x3_t tab, uint8x8_t idx)
{
  return vtbx3_p8 (r, tab, idx);
}

int8x8_t
tb_tests8_4 (int8x8_t r, int8x8x4_t tab, int8x8_t idx)
{
  return vtbx4_s8 (r, tab, idx);
}

uint8x8_t
tb_testu8_4 (uint8x8_t r, uint8x8x4_t tab, uint8x8_t idx)
{
  return vtbx4_u8 (r, tab, idx);
}

poly8x8_t
tb_testp8_4 (poly8x8_t r, poly8x8x4_t tab, uint8x8_t idx)
{
  return vtbx4_p8 (r, tab, idx);
}

int8x8_t
qtbl_tests8_ (int8x16_t tab, uint8x8_t idx)
{
  return vqtbl1_s8 (tab, idx);
}

uint8x8_t
qtbl_testu8_ (uint8x16_t tab, uint8x8_t idx)
{
  return vqtbl1_u8 (tab, idx);
}

poly8x8_t
qtbl_testp8_ (poly8x16_t tab, uint8x8_t idx)
{
  return vqtbl1_p8 (tab, idx);
}

int8x8_t
qtbl_tests8_2 (int8x16x2_t tab, uint8x8_t idx)
{
  return vqtbl2_s8 (tab, idx);
}

uint8x8_t
qtbl_testu8_2 (uint8x16x2_t tab, uint8x8_t idx)
{
  return vqtbl2_u8 (tab, idx);
}

poly8x8_t
qtbl_testp8_2 (poly8x16x2_t tab, uint8x8_t idx)
{
  return vqtbl2_p8 (tab, idx);
}

int8x8_t
qtbl_tests8_3 (int8x16x3_t tab, uint8x8_t idx)
{
  return vqtbl3_s8 (tab, idx);
}

uint8x8_t
qtbl_testu8_3 (uint8x16x3_t tab, uint8x8_t idx)
{
  return vqtbl3_u8 (tab, idx);
}

poly8x8_t
qtbl_testp8_3 (poly8x16x3_t tab, uint8x8_t idx)
{
  return vqtbl3_p8 (tab, idx);
}

int8x8_t
qtbl_tests8_4 (int8x16x4_t tab, uint8x8_t idx)
{
  return vqtbl4_s8 (tab, idx);
}

uint8x8_t
qtbl_testu8_4 (uint8x16x4_t tab, uint8x8_t idx)
{
  return vqtbl4_u8 (tab, idx);
}

poly8x8_t
qtbl_testp8_4 (poly8x16x4_t tab, uint8x8_t idx)
{
  return vqtbl4_p8 (tab, idx);
}

int8x8_t
qtb_tests8_ (int8x8_t r, int8x16_t tab, uint8x8_t idx)
{
  return vqtbx1_s8 (r, tab, idx);
}

uint8x8_t
qtb_testu8_ (uint8x8_t r, uint8x16_t tab, uint8x8_t idx)
{
  return vqtbx1_u8 (r, tab, idx);
}

poly8x8_t
qtb_testp8_ (poly8x8_t r, poly8x16_t tab, uint8x8_t idx)
{
  return vqtbx1_p8 (r, tab, idx);
}

int8x8_t
qtb_tests8_2 (int8x8_t r, int8x16x2_t tab, uint8x8_t idx)
{
  return vqtbx2_s8 (r, tab, idx);
}

uint8x8_t
qtb_testu8_2 (uint8x8_t r, uint8x16x2_t tab, uint8x8_t idx)
{
  return vqtbx2_u8 (r, tab, idx);
}

poly8x8_t
qtb_testp8_2 (poly8x8_t r, poly8x16x2_t tab, uint8x8_t idx)
{
  return vqtbx2_p8 (r, tab, idx);
}

int8x8_t
qtb_tests8_3 (int8x8_t r, int8x16x3_t tab, uint8x8_t idx)
{
  return vqtbx3_s8 (r, tab, idx);
}

uint8x8_t
qtb_testu8_3 (uint8x8_t r, uint8x16x3_t tab, uint8x8_t idx)
{
  return vqtbx3_u8 (r, tab, idx);
}

poly8x8_t
qtb_testp8_3 (poly8x8_t r, poly8x16x3_t tab, uint8x8_t idx)
{
  return vqtbx3_p8 (r, tab, idx);
}

int8x8_t
qtb_tests8_4 (int8x8_t r, int8x16x4_t tab, uint8x8_t idx)
{
  return vqtbx4_s8 (r, tab, idx);
}

uint8x8_t
qtb_testu8_4 (uint8x8_t r, uint8x16x4_t tab, uint8x8_t idx)
{
  return vqtbx4_u8 (r, tab, idx);
}

poly8x8_t
qtb_testp8_4 (poly8x8_t r, poly8x16x4_t tab, uint8x8_t idx)
{
  return vqtbx4_p8 (r, tab, idx);
}

int8x16_t
qtblq_tests8_ (int8x16_t tab, uint8x16_t idx)
{
  return vqtbl1q_s8 (tab, idx);
}

uint8x16_t
qtblq_testu8_ (uint8x16_t tab, uint8x16_t idx)
{
  return vqtbl1q_u8 (tab, idx);
}

poly8x16_t
qtblq_testp8_ (poly8x16_t tab, uint8x16_t idx)
{
  return vqtbl1q_p8 (tab, idx);
}

int8x16_t
qtblq_tests8_2 (int8x16x2_t tab, uint8x16_t idx)
{
  return vqtbl2q_s8 (tab, idx);
}

uint8x16_t
qtblq_testu8_2 (uint8x16x2_t tab, uint8x16_t idx)
{
  return vqtbl2q_u8 (tab, idx);
}

poly8x16_t
qtblq_testp8_2 (poly8x16x2_t tab, uint8x16_t idx)
{
  return vqtbl2q_p8 (tab, idx);
}

int8x16_t
qtblq_tests8_3 (int8x16x3_t tab, uint8x16_t idx)
{
  return vqtbl3q_s8 (tab, idx);
}

uint8x16_t
qtblq_testu8_3 (uint8x16x3_t tab, uint8x16_t idx)
{
  return vqtbl3q_u8 (tab, idx);
}

poly8x16_t
qtblq_testp8_3 (poly8x16x3_t tab, uint8x16_t idx)
{
  return vqtbl3q_p8 (tab, idx);
}

int8x16_t
qtblq_tests8_4 (int8x16x4_t tab, uint8x16_t idx)
{
  return vqtbl4q_s8 (tab, idx);
}

uint8x16_t
qtblq_testu8_4 (uint8x16x4_t tab, uint8x16_t idx)
{
  return vqtbl4q_u8 (tab, idx);
}

poly8x16_t
qtblq_testp8_4 (poly8x16x4_t tab, uint8x16_t idx)
{
  return vqtbl4q_p8 (tab, idx);
}

int8x16_t
qtbxq_tests8_ (int8x16_t r, int8x16_t tab, uint8x16_t idx)
{
  return vqtbx1q_s8 (r, tab, idx);
}

uint8x16_t
qtbxq_testu8_ (uint8x16_t r, uint8x16_t tab, uint8x16_t idx)
{
  return vqtbx1q_u8 (r, tab, idx);
}

poly8x16_t
qtbxq_testp8_ (poly8x16_t r, poly8x16_t tab, uint8x16_t idx)
{
  return vqtbx1q_p8 (r, tab, idx);
}

int8x16_t
qtbxq_tests8_2 (int8x16_t r, int8x16x2_t tab, uint8x16_t idx)
{
  return vqtbx2q_s8 (r, tab, idx);
}

uint8x16_t
qtbxq_testu8_2 (uint8x16_t r, uint8x16x2_t tab, uint8x16_t idx)
{
  return vqtbx2q_u8 (r, tab, idx);
}

poly8x16_t
qtbxq_testp8_2 (poly8x16_t r, poly8x16x2_t tab, uint8x16_t idx)
{
  return vqtbx2q_p8 (r, tab, idx);
}

int8x16_t
qtbxq_tests8_3 (int8x16_t r, int8x16x3_t tab, uint8x16_t idx)
{
  return vqtbx3q_s8 (r, tab, idx);
}

uint8x16_t
qtbxq_testu8_3 (uint8x16_t r, uint8x16x3_t tab, uint8x16_t idx)
{
  return vqtbx3q_u8 (r, tab, idx);
}

poly8x16_t
qtbxq_testp8_3 (poly8x16_t r, poly8x16x3_t tab, uint8x16_t idx)
{
  return vqtbx3q_p8 (r, tab, idx);
}

int8x16_t
qtbxq_tests8_4 (int8x16_t r, int8x16x4_t tab, uint8x16_t idx)
{
  return vqtbx4q_s8 (r, tab, idx);
}

uint8x16_t
qtbxq_testu8_4 (uint8x16_t r, uint8x16x4_t tab, uint8x16_t idx)
{
  return vqtbx4q_u8 (r, tab, idx);
}

poly8x16_t
qtbxq_testp8_4 (poly8x16_t r, poly8x16x4_t tab, uint8x16_t idx)
{
  return vqtbx4q_p8 (r, tab, idx);
}

/* { dg-final { scan-assembler-times "tbl\[ |\t\]*v" 42} }  */
/* { dg-final { scan-assembler-times "tbx\[ |\t\]*v" 30} }  */
